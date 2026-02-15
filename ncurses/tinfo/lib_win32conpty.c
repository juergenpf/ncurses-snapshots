/****************************************************************************
 * Copyright 2020-2024,2025 Thomas E. Dickey                                *
 * Copyright 1998-2009,2010 Free Software Foundation, Inc.                  *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/

/****************************************************************************
 *  Author: Juergen Pfeifer                                                 *
 *     and: Thomas E. Dickey                                                *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id$")

#if defined(_NC_WINDOWS_NATIVE)
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <winternl.h>
#include <io.h>
#include <fcntl.h>
#if USE_WIDEC_SUPPORT
#include <wchar.h>
#endif

// Prototypes of static function we want to use in initializers
static BOOL pty_init(int fdOut, int fdIn);
static void pty_size(int *Lines, int *Cols);
static BOOL pty_check_resize(void);
static int pty_setmode(int fd, const TTY *arg);
static int pty_getmode(int fd, TTY *arg);
static int pty_defmode(TTY *arg, BOOL isShell);
static int pty_setfilemode(TTY *arg);
static int pty_flush(int fd);
static int pty_read(SCREEN *sp, int *result);
static int pty_twait(const SCREEN *sp GCC_UNUSED,
		     int mode GCC_UNUSED,
		     int milliseconds,
		     int *timeleft,
		     long (*gettime_func)(TimeType *, int)
			 EVENTLIST_2nd(_nc_eventlist *evl));
#if USE_WIDEC_SUPPORT
static size_t wchar_to_utf8(wchar_t wc, char utf8[UTF8_MAX_BYTES]);
#endif

/*   A process can only have a single console, so it is safe
	 to maintain all the information about it in a single
	 static structure.
 */
static ConsoleInfo defaultCONSOLE = {
    .initialized = FALSE,
    .used_fdIn = -1,
    .used_fdOut = -1,
    .conhost_flags = 0,
    .ttyflags = {0, 0, 0, 0, 0, 0},
    .used_input_handle = INVALID_HANDLE_VALUE,
    .used_output_handle = INVALID_HANDLE_VALUE,
    .init = pty_init,
    .size = pty_size,
    .check_resize = pty_check_resize,
    .setmode = pty_setmode,
    .getmode = pty_getmode,
    .defmode = pty_defmode,
    .setfilemode = pty_setfilemode,
    .flush = pty_flush,
    .read = pty_read,
    .twait = pty_twait
};

/*
* Poor man's dependency injection - we maintain a pointer to the current console information,
* which is initialized to point to our default implementation. If in the future we want to 
* support other types of consoles or terminal backends on Windows, we can create additional 
* ConsoleInfo structures with different implementations of the methods, and switch the
* _nc_currentCONSOLE pointer to point to the appropriate one based on runtime detection 
* or configuration.
*/
NCURSES_EXPORT_VAR(ConsoleInfo *)
_nc_currentCONSOLE = &defaultCONSOLE;


#define UTF8_CP 65001
#define DEFAULT_UTF8_CTYPE_ENV ".UTF-8"

#define DEFAULT_ASCII_CP 1252
#define DEFAULT_ASCII_CTYPE_ENV ".1252"

static BOOL
locale_is_utf8()
{
	return GetACP() == UTF8_CP;
}

static BOOL
locale_compatible_with_ncurses()
{
#if USE_WIDEC_SUPPORT
	return locale_is_utf8();
#else
	return !locale_is_utf8();
#endif
}

static BOOL
codepage_compatible_with_ncurses(UINT cp)
{
#if USE_WIDEC_SUPPORT
	return cp == UTF8_CP; /* only UTF-8 */
#else
	return cp != UTF8_CP; /* all but UTF-8 */
#endif
}

/* Check Codepage exists */
static BOOL
valid_codepage(UINT cp)
{
	CPINFOEX info;
	return GetCPInfoEx(cp, 0, &info) != 0;
}

/* Check Windows Locale is valid */
static BOOL
valid_locale(const char *loc)
{
	if (!loc || !*loc)
		return FALSE;

	if (!setlocale(LC_CTYPE, loc))
		return FALSE;

	/* Reset to previous value */
	setlocale(LC_CTYPE, "");
	return TRUE;
}

/* Encoding setup for Windows */
static void
encoding_init(void)
{
#if USE_WIDEC_SUPPORT
	UINT default_cp = UTF8_CP;
	const char *default_ctype = DEFAULT_UTF8_CTYPE_ENV;
#else
	UINT default_cp = DEFAULT_ASCII_CP;
	const char *default_ctype = DEFAULT_ASCII_CTYPE_ENV;
#endif
	const char *env_cp = getenv("NC_WINCP");
	const char *env_ctype = getenv("NC_WIN_CTYPE");

	UINT cp = default_cp;
	const char *ctype = default_ctype;
	UINT tmp;
	UINT cur_in;
	UINT cur_out;
	const char *cur_loc;

	if (env_cp && *env_cp)
	{
		tmp = (UINT)atoi(env_cp);
		if (valid_codepage(tmp) && codepage_compatible_with_ncurses(tmp))
			cp = tmp;
	}

	if (env_ctype && *env_ctype)
	{
		if (valid_locale(env_ctype) && locale_compatible_with_ncurses(env_ctype))
			ctype = env_ctype;
	}

	cur_in = GetConsoleCP();
	cur_out = GetConsoleOutputCP();

	if (!valid_codepage(cur_in) ||
	    !valid_codepage(cur_out) ||
	    !codepage_compatible_with_ncurses(cur_in) ||
	    !codepage_compatible_with_ncurses(cur_out))
	{
		cur_in = cur_out = default_cp;
	}

	if (!env_cp && valid_codepage(cur_out) && codepage_compatible_with_ncurses(cur_out))
		cp = cur_out;

	cur_loc = setlocale(LC_CTYPE, NULL);
	if (!env_ctype && cur_loc && valid_locale(cur_loc) &&
	    locale_compatible_with_ncurses(cur_loc))
		ctype = cur_loc;

	if (valid_codepage(cp) && codepage_compatible_with_ncurses(cp))
	{
		SetConsoleCP(cp);
		SetConsoleOutputCP(cp);
		SetConsoleCP(cp);
	}
	else
	{
		SetConsoleCP(default_cp);
		SetConsoleOutputCP(default_cp);
		SetConsoleCP(default_cp);
	}

	if (!setlocale(LC_CTYPE, ctype))
	{
		/* Fallback - try alternative UTF-8 locale names for Windows */
#if USE_WIDEC_SUPPORT
		if (!setlocale(LC_CTYPE, ".UTF8") &&
		    !setlocale(LC_CTYPE, ".utf8") &&
		    !setlocale(LC_CTYPE, "en_US.UTF-8") &&
		    !setlocale(LC_CTYPE, "C.65001"))
		{
			/* Final fallback */
			setlocale(LC_CTYPE, default_ctype);
		}
#else
		setlocale(LC_CTYPE, default_ctype);
#endif
	}
}

#define REQUIRED_MAJOR_V (DWORD)10
#define REQUIRED_MINOR_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763

typedef NTSTATUS(WINAPI *RtlGetVersionPtr)(PRTL_OSVERSIONINFOW);

static bool get_real_windows_version(DWORD *major, DWORD *minor, DWORD *build)
{
	HMODULE ntdll = GetModuleHandle(TEXT("ntdll.dll"));
	if (ntdll)
	{
		RtlGetVersionPtr RtlGetVersion = (RtlGetVersionPtr)GetProcAddress(ntdll, "RtlGetVersion");
		if (RtlGetVersion)
		{
			RTL_OSVERSIONINFOW osvi = {0};
			osvi.dwOSVersionInfoSize = sizeof(osvi);
			if (RtlGetVersion(&osvi) == 0)
			{
				*major = osvi.dwMajorVersion;
				*minor = osvi.dwMinorVersion;
				*build = osvi.dwBuildNumber;
				return true;
			}
		}
	}
	return false;
}

/* Check if the current Windows version supports ConPTY, which is a 
*  requirement for the Windows Console backend of ncurses. This is 
*  because without ConPTY, the Windows Console does not provide the 
*  necessary capabilities for ncurses and escpecially the terminfo 
*  layer to function properly.
*/
static BOOL
conpty_supported(void)
{
	int result = FALSE;
	DWORD major, minor, build;

	T((T_CALLED("lib_win32conpty::conpty_supported")));

	if (!get_real_windows_version(&major, &minor, &build))
	{
		T(("RtlGetVersion failed"));
		returnBool(FALSE);
	}
	if (major >= REQUIRED_MAJOR_V)
	{
		T(("Windows version detected: %d.%d (build %d)", major, minor, build));
		if (major == REQUIRED_MAJOR_V)
		{
			if (((minor == REQUIRED_MINOR_V) &&
			     (build >= REQUIRED_BUILD)) ||
			    ((minor > REQUIRED_MINOR_V)))
				result = TRUE;
		}
		else
			result = TRUE;
	}
	returnBool(result);
}

/*
 * Reliably validate that stdout is in ConPTY mode and not in console mode.
 *
 * Returns:
 *   true  - stdout is in ConPTY mode (virtual terminal processing enabled)
 *   false - stdout is in legacy console mode or not a console at all
 */
static BOOL
output_is_conpty(void)
{
	HANDLE out_handle;
	DWORD console_mode = 0;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	BOOL result = FALSE;

	T((T_CALLED("lib_win32conpty::output_is_conpty()")));

	out_handle = defaultCONSOLE.used_output_handle;
	if (GetConsoleMode(out_handle, &console_mode) == 0)
	{
		T(("GetConsoleMode() failed"));
		returnBool(FALSE);
	}

	if (console_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	{
		if (GetConsoleScreenBufferInfo(out_handle, &csbi))
		{
			result = TRUE;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING - ConPTY detected"));
		}
		else
		{
			result = TRUE;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING (no CSBI) - ConPTY detected"));
		}
	}
	else
	{
		T(("Console mode lacks ENABLE_VIRTUAL_TERMINAL_PROCESSING - legacy console mode"));
	}

	returnBool(result);
}

/*
This initializaton function can be called multiple time, and actually it is called from within
setupterm() the first time and potentially if we enter ncurses from newterm() the next time.
The main purpose is to initialize the WINCONSOLE structure when called the first time. The
first call will alway have fdIn set to -1, as setupterm() only cares about output. Please note
that setupterm() already handles redirection of stdout and assigns stderr for output if stdout
is not a tty.

The other purpose of this routine is to manage the assignment of console handles. If the
assigned filedescriptors are NOT valid console handles, the call will return FALSE.

The function will also return FALSE, if the Windows version we run on does not support ConPTY,
which is a requirement for the Windows Console backend of ncurses. This is because without
ConPTY, the Windows Console does not provide the necessary capabilities for ncurses and
escpecially the terminfo layer to function properly.
*/
static BOOL
pty_init(int fdOut, int fdIn)
{
	BOOL result = FALSE;

	T((T_CALLED("lib_win32conpty::pty_init(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

	/* initialize once, or not at all */
	if (!defaultCONSOLE.initialized)
	{
		if (!conpty_supported())
		{
			T(("Windows version does not support ConPTY"));
			returnBool(FALSE);
		}

		// We set the console mode flags to the most basic ones that are required for ConPTY 
		// to function properly. We will let the C runtime handle any necessary translations, 
		// and we will handle UTF-8 encoding and decoding ourselves to avoid any unpredictable 
		// interferences with the various C runtimes on Windows (when using wide c)
		DWORD dwFlagIn = (ENABLE_LINE_INPUT 
			| ENABLE_PROCESSED_INPUT 
			| ENABLE_ECHO_INPUT  
			| ENABLE_EXTENDED_FLAGS);

		DWORD dwFlagOut = (ENABLE_VIRTUAL_TERMINAL_PROCESSING 
			| ENABLE_PROCESSED_OUTPUT
			| DISABLE_NEWLINE_AUTO_RETURN 
			| ENABLE_WRAP_AT_EOL_OUTPUT);

		if (fdIn != -1)
		{
			T(("In the first call fdIn is expected to be -1."));
			returnBool(FALSE);
		}

		encoding_init();

		// The NC_CONHOST_FLAGS environment variable is for future use.
		defaultCONSOLE.conhost_flags = 0;
		const char *env_flags = getenv("NC_CONHOST_FLAGS");
		if (env_flags && *env_flags)
		{
			char *endptr;
			long flags_val = strtol(env_flags, &endptr, 0);
			if (*endptr == '\0' && flags_val >= 0)
			{
				defaultCONSOLE.conhost_flags = (unsigned int)(flags_val & NC_CONHOST_FLAG_MASK);
			}
		}

		// First call is coming from setuptrm() only cares about output, so we can ignore fdIn. 
		// We will validate the input handle on the next call when fdIn is provided.
		// In the meantime, we use stdin as the input handle, which is a valid console handle 
		// as long as the process has a console.
		defaultCONSOLE.used_fdIn = _fileno(stdin);
		defaultCONSOLE.used_fdOut = fdOut;
		HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
		HANDLE out_hdl = fdOut >= 0 ? (HANDLE)_get_osfhandle(fdOut) : INVALID_HANDLE_VALUE;
		DWORD dwFlag;

		if (out_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(out_hdl, &dwFlag) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.used_output_handle = out_hdl;

		if (stdin_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(stdin_hdl, &dwFlag) == 0)
		{
			T(("StdIn handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.used_input_handle = stdin_hdl;

		SetConsoleMode(out_hdl, dwFlagOut);
		// We immediately read the console mode back to reflect any changes the
		// runtime my have added, so the saved value reflects the actual mode
		// of tghe console. 
		if (GetConsoleMode(out_hdl, &dwFlagOut) == 0)
		{
			T(("GetConsoleMode() failed for stdout"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ttyflags.dwFlagOut = dwFlagOut;

		SetConsoleMode(stdin_hdl, dwFlagIn);
		// We immediately read the console mode back to reflect any changes the
		// runtime my have added, so the saved value reflects the actual mode
		// of tghe console. 
		if (GetConsoleMode(stdin_hdl, &dwFlagIn) == 0)
		{
			T(("GetConsoleMode() failed for stdin"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ttyflags.dwFlagIn = dwFlagIn;

		/*
		* A ConPTY console is initially always in _O_TEXT mode, and we store that filemode
		* in our TTY structure so the caller knows that. Right after this initialization
		* called by setupterm(), def_shell_mode will be called first and memorize that
		* we are in text mode and Console Modes are in a "cooked mode" state. 
		*/
		defaultCONSOLE.ttyflags.InFileMode = _O_TEXT;
		defaultCONSOLE.ttyflags.OutFileMode = _O_TEXT;

		defaultCONSOLE.initialized = TRUE;
		result = TRUE;
	}
	else
	{ // This branch is called from newterm() when fdIn is provided, so we need to validate 
	  // that the provided fdIn and fdOut are valid console handles, and if so we update the 
	  // WINCONSOLE structure to use the new handles.
		DWORD dwFlagOut;
		DWORD dwFlagIn;
		defaultCONSOLE.used_fdIn = fdIn;
		defaultCONSOLE.used_fdOut = fdOut;
		HANDLE in_hdl = fdIn >= 0 ? (HANDLE)_get_osfhandle(fdIn) : INVALID_HANDLE_VALUE;
		/* Already initialized - just check if stdout is still in ConPTY mode */
		HANDLE out_hdl = fdOut >= 0 ? (HANDLE)_get_osfhandle(fdOut) : INVALID_HANDLE_VALUE;

		if (out_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(out_hdl, &dwFlagOut) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		if (in_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(in_hdl, &dwFlagIn) == 0)
		{
			T(("Input handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.used_output_handle = out_hdl;
		defaultCONSOLE.ttyflags.dwFlagOut = dwFlagOut;
		defaultCONSOLE.used_input_handle = in_hdl;
		defaultCONSOLE.ttyflags.dwFlagIn = dwFlagIn;

		_setmode(defaultCONSOLE.used_fdIn, _O_BINARY);
		_setmode(defaultCONSOLE.used_fdOut, _O_BINARY);
		defaultCONSOLE.ttyflags.InFileMode = _O_BINARY;
		defaultCONSOLE.ttyflags.OutFileMode = _O_BINARY;

		result = TRUE;
	}
	BOOL check = output_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   defaultCONSOLE.initialized,
	   check));
	returnBool(result);
}

/* Windows Console resize detection */
static int last_console_lines = -1;
static int last_console_cols = -1;

/*
 * Check if the Windows Console has been resized.
 * This provides SIGWINCH-like functionality for Windows ConPTY.
 * Returns TRUE if a resize was detected.
 */
static BOOL
pty_check_resize(void)
{
	int current_lines, current_cols;
	bool resized = FALSE;

	T((T_CALLED("lib_win32conpty::pty_check_resize()")));

	pty_size(&current_lines, &current_cols);

	if (last_console_lines == -1 || last_console_cols == -1)
	{
		last_console_lines = current_lines;
		last_console_cols = current_cols;
		returnBool(FALSE);
	}

	if (current_lines != last_console_lines || current_cols != last_console_cols)
	{
		last_console_lines = current_lines;
		last_console_cols = current_cols;

		_nc_globals.have_sigwinch = 1;

		resized = TRUE;
	}

	returnBool(resized);
}

static void
pty_size(int *Lines, int *Cols)
{
	T((T_CALLED("lib_win32conpty::pty_size(lines=%p, cols=%p)"), Lines, Cols));

	if (Lines != NULL && Cols != NULL)
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		HANDLE hdl_out = defaultCONSOLE.used_output_handle;

		if (hdl_out != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hdl_out, &csbi))
		{
			*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
			*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
		}
		else
		{
			if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi))
			{
				*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
				*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
			}
			else
			{ // Fallback to default size if we cannot get console info
				*Lines = 25;
				*Cols = 80;
			}
		}
	}
}


/*
 * WIN32_CONPTY input function (handles both widec and non-widec builds)
 * In wide mode: Assembles UTF-8 byte sequences into Unicode codepoints
 * In non-wide mode: Returns raw bytes directly (for single-byte encodings like CP1252)
 */
static int
pty_read(SCREEN *sp, int *result)
{
	unsigned char byte_buffer;
	int n;

	T((T_CALLED("lib_win32conpty::pty_read(SCREEN*=%p, result=%p)"), sp, result));

	_nc_set_read_thread(TRUE);
	n = (int)read(sp->_ifd, &byte_buffer, (size_t)1);
	_nc_set_read_thread(FALSE);

	if (n <= 0)
	{
		return n;
	}

#if USE_WIDEC_SUPPORT
	/* Wide mode: decode UTF-8 sequences */
	wchar_t wch;
	int ch;

	ch = _nc_assemble_utf8_input(byte_buffer, &wch);

	if (ch > 0)
	{
		/* Complete UTF-8 character assembled */
		*result = ch;
		return 1;
	}
	else if (ch == 0)
	{
		/* Need more bytes - recursively read next byte */
		return pty_read(sp, result);
	}
	else
	{
		/* Invalid UTF-8 sequence - return raw byte as fallback */
		*result = (int)byte_buffer;
		return 1;
	}
#else
	/* Non-wide mode: return raw bytes directly (single-byte encoding like CP1252) */
	*result = (int)byte_buffer;
	return 1;
#endif
}

/*
 * WIN32_CONPTY timeout handling function
 */
static int
pty_twait(const SCREEN *sp GCC_UNUSED,
	  int mode GCC_UNUSED,
	  int milliseconds,
	  int *timeleft,
	  long (*gettime_func)(TimeType *, int)
	      EVENTLIST_2nd(_nc_eventlist *evl))
{
	int result = TW_NONE;
	TimeType t0;

	TR(TRACE_IEVENT, ("start twait: %d milliseconds, mode: %d", milliseconds, mode));

#define min(a, b) ((a) < (b) ? (a) : (b))

	long starttime = gettime_func(&t0, TRUE);

	TR(TRACE_IEVENT, ("start WINCONSOLE.twait: %d milliseconds, mode: %d",
			  milliseconds, mode));

	if (mode & (TW_INPUT | TW_MOUSE))
	{
		HANDLE in_handle = defaultCONSOLE.used_input_handle;
		DWORD wait_result;

		if (milliseconds < 0)
		{
			while (TRUE)
			{
				wait_result = WaitForSingleObject(in_handle, 100);
				if (wait_result == WAIT_OBJECT_0)
				{
					result = TW_INPUT;
					if (mode & TW_MOUSE)
						result |= TW_MOUSE;
					break;
				}
				else if (wait_result == WAIT_TIMEOUT)
				{
					continue;
				}
				else
				{
					break;
				}
			}
		}
		else if (milliseconds == 0)
		{
			DWORD events_available = 0;

			if (GetNumberOfConsoleInputEvents(in_handle, &events_available))
			{
				if (events_available > 0)
				{
					INPUT_RECORD input_buffer[16];
					DWORD events_read = 0;

					if (PeekConsoleInput(in_handle, input_buffer,
							     min(events_available, 16), &events_read))
					{
						for (DWORD i = 0; i < events_read; i++)
						{
							if (input_buffer[i].EventType == KEY_EVENT &&
							    input_buffer[i].Event.KeyEvent.bKeyDown)
							{
								result = TW_INPUT;
								if (mode & TW_MOUSE)
									result |= TW_MOUSE;
								break;
							}
							else if (input_buffer[i].EventType == MOUSE_EVENT)
							{
								if (mode & TW_MOUSE)
									result |= TW_MOUSE;
							}
						}
					}
					else
					{
						wait_result = WaitForSingleObject(in_handle, 0);
						if (wait_result == WAIT_OBJECT_0)
						{
							result = TW_INPUT;
							if (mode & TW_MOUSE)
								result |= TW_MOUSE;
						}
					}
				}
				else
				{
					result = TW_NONE;
				}
			}
			else
			{
				wait_result = WaitForSingleObject(in_handle, 0);
				if (wait_result == WAIT_OBJECT_0)
				{
					result = TW_INPUT;
					if (mode & TW_MOUSE)
						result |= TW_MOUSE;
				}
			}
		}
		else
		{
			wait_result = WaitForSingleObject(in_handle, (DWORD)milliseconds);
			if (wait_result == WAIT_OBJECT_0)
			{
				result = TW_INPUT;
				if (mode & TW_MOUSE)
					result |= TW_MOUSE;
			}
		}
	}
	else if (milliseconds > 0)
	{
		Sleep((DWORD)milliseconds);
	}

	long elapsed = gettime_func(&t0, FALSE) - starttime;
	if (timeleft)
	{
		*timeleft = (milliseconds >= 0) ? Max(0, milliseconds - (int)elapsed) : milliseconds;
	}

	TR(TRACE_IEVENT, ("end WINCONSOLE.twait: returned %d, elapsed %ld msec",
			  result, elapsed));

#undef min
	return result;
}

typedef enum
{
	NotKnown,
	Input,
	Output
} HandleType;

static HandleType classify_handle(HANDLE hdl)
{
	HandleType type = NotKnown;
	if (hdl != INVALID_HANDLE_VALUE)
	{
		if (hdl == defaultCONSOLE.used_input_handle)
		{
			type = Input;
		}
		else if (hdl == defaultCONSOLE.used_output_handle)
		{
			type = Output;
		}
	}
	return type;
}

static int
pty_flush(int fd)
{
	int code = OK;
	HANDLE hdl = _get_osfhandle(fd);
	HandleType type = classify_handle(hdl);

	T((T_CALLED("lib_win32conpty::pty_flush(fd=%d)"), fd));

	if (type == Input)
	{
		if (!FlushConsoleInputBuffer(hdl))
			code = ERR;
	}
	else if (type == Output)
	{
		/* Flush output buffer - use FlushFileBuffers for proper VT processing */
		if (!FlushFileBuffers(hdl))
			code = ERR;
	}
	else
	{
		code = ERR;
		T(("flush not requesting a handle owned by console."));
	}
	returnCode(code);
}

static int 
pty_setfilemode(TTY* arg)
{
	T((T_CALLED("lib_win32conpty::pty_setfilemode(TTY*=%p)"), arg));

	if (!arg)
		returnCode(ERR);

	if (arg->setfMode)
	{
		if (defaultCONSOLE.used_fdIn >= 0)
			_setmode(defaultCONSOLE.used_fdIn, arg->InFileMode);
		else
			T(("Invalid input file descriptor"));

		if (defaultCONSOLE.used_fdOut >= 0)
			_setmode(defaultCONSOLE.used_fdOut, arg->OutFileMode);
		else
			T(("Invalid output file descriptor"));
	}
	returnCode(OK);
}

static int
pty_setmode(int fd, const TTY *arg)
{
	T((T_CALLED("lib_win32conpty::pty_setmode(fd=%d, TTY*=%p)"), fd, arg));
	
	if (!arg)
		returnCode(ERR);

	HANDLE in_hdl = defaultCONSOLE.used_input_handle;
	HANDLE out_hdl = defaultCONSOLE.used_output_handle;
	HANDLE fd_hdl = INVALID_HANDLE_VALUE;
	HandleType fd_type = NotKnown;

	// Get handle from fd
	if (fd >= 0)
	{
		fd_hdl = _get_osfhandle(fd);
		fd_type = classify_handle(fd_hdl);
	}
	if (fd_type == NotKnown)
	{
		T(("WINCONSOLE.setmode: fd %d does not correspond to console input or output handle", fd));
		returnCode(ERR);
	}

	// Determine which handles to use based on classification
	HANDLE input_target = INVALID_HANDLE_VALUE;
	HANDLE output_target = INVALID_HANDLE_VALUE;

	if (fd_type == Input)
	{
		input_target = fd_hdl;
		output_target = out_hdl;
	}
	else if (fd_type == Output)
	{
		input_target = in_hdl;
		output_target = fd_hdl;
	}
	else
	{
		input_target = in_hdl;
		output_target = out_hdl;
	}

	// Apply modes to appropriate handles
	bool input_ok = false, output_ok = false;

	if (input_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = ENABLE_VIRTUAL_TERMINAL_INPUT | arg->dwFlagIn;

		/*
		   ENABLE_VIRTUAL_TERMINAL_INPUT (VT) requires ENABLE_PROCESSED_INPUT to be effective.
		   If we request VT, we must ensure PROCESSED is set, otherwise SetConsoleMode fails.
		   We always allow mouse and window input events if VT input is requested, as these
		   are commonly used together and it simplifies the logic to just enable them when
		   VT is enabled.
		*/
		if (mode & ENABLE_VIRTUAL_TERMINAL_INPUT)
		{
			mode |= ENABLE_PROCESSED_INPUT | ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT;
		}

		/* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
		if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT))
		{
			mode &= ~ENABLE_ECHO_INPUT;
		}

		pty_setfilemode((TTY*)arg);

		input_ok = SetConsoleMode(input_target, mode);
		if (input_ok)
		{
			// Make sure the cached value reflects the real value we set, as the
			// caller may not have provided all necessary flags (e.g.
			// PROCESSED_INPUT when VT is requested)
			DWORD realMode;
			if (GetConsoleMode(input_target, &realMode))
			{
				defaultCONSOLE.ttyflags.dwFlagIn = realMode;
			}
			else
			{
				defaultCONSOLE.ttyflags.dwFlagIn = mode;
			}
		}
		else
		{
			T(("Invalid input file descriptor"));
		}

		// Ensure VT output is always enabled for the Windows Console backend
		mode = arg->dwFlagOut | ENABLE_VIRTUAL_TERMINAL_PROCESSING;

		if (output_target != INVALID_HANDLE_VALUE)
		{
			output_ok = SetConsoleMode(output_target, mode);
			if (output_ok)
			{
				// Make sure the cached value reflects the real value we set,
				// as the caller may not have provided all necessary flags
				// (e.g. VT output is required for the Windows Console backend)
				DWORD realMode;
				if (GetConsoleMode(output_target, &realMode))
				{
					defaultCONSOLE.ttyflags.dwFlagOut = realMode;
				}
				else
				{
					defaultCONSOLE.ttyflags.dwFlagOut = mode;
				}
			}		
			else
			{
				T(("Invalid output file descriptor"));
			}		
		}

		// Handle errors
		if (!input_ok || !output_ok)
		{
			returnCode(ERR);
		}

		returnCode(OK);
	}
	returnCode(ERR);
}

static int
pty_defmode(TTY *arg, BOOL isShell GCC_UNUSED)
{
	T((T_CALLED("lib_win32conpty::pty_defmode(TTY*=%p, isShell=%d)"), arg, isShell));

	if (NULL == arg)
		returnCode(ERR);

	arg->setfMode = TRUE;
	returnCode(OK);
}

static int
pty_getmode(int fd, TTY *arg)
{
	T((T_CALLED("lib_win32conpty::pty_getmode(fd=%d, TTY*=%p)"), fd, arg));

	if (NULL == arg)
		returnCode(ERR);

	HANDLE hdl = _get_osfhandle(fd);
	HandleType type = classify_handle(hdl);
	if (type == NotKnown)
	{
		T(("WINCONSOLE.getmode: fd %d does not correspond to console input or output handle", fd));
		returnCode(ERR);
	}
	*arg = defaultCONSOLE.ttyflags;
	arg->setfMode = FALSE;
	returnCode(OK);
}

#endif /* defined(_NC_WINDOWS_NATIVE) */
