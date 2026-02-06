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

#if defined(USE_WIN32_CONPTY)
#include <curses.priv.h>
#include <nc_win32.h>
#include <locale.h>
#include <stdio.h>

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)

/*   A process can only have a single console, so it is safe
	 to maintain all the information about it in a single
	 static structure.
 */
NCURSES_EXPORT_VAR(ConsoleInfo)
_nc_CONSOLE;

#define NOT_IMPLEMENTED                                                          \
	{                                                                        \
		fprintf(stderr, "NOT IMPLEMENTED: %s:%d\n", __FILE__, __LINE__); \
		abort();                                                         \
	}

static int
locale_is_utf8(const char *loc)
{
	if (!loc)
		return 0;
	return strstr(loc, "UTF-8") ||
	       strstr(loc, "utf8") ||
	       strstr(loc, "utf-8");
}

static int
locale_compatible_with_ncurses(const char *loc)
{
#if USE_WIDEC_SUPPORT
	return locale_is_utf8(loc);
#else
	return !locale_is_utf8(loc);
#endif
}

static int
codepage_compatible_with_ncurses(UINT cp)
{
#if USE_WIDEC_SUPPORT
	return cp == 65001; /* only UTF-8 */
#else
	return cp != 65001; /* all but UTF-8 */
#endif
}

/* Check Codepage exists */
static int valid_codepage(UINT cp)
{
	CPINFOEX info;
	return GetCPInfoEx(cp, 0, &info) != 0;
}

/* Check Windows Locale is valid */
static int valid_locale(const char *loc)
{
	if (!loc || !*loc)
		return 0;

	if (!setlocale(LC_CTYPE, loc))
		return 0;

	/* Reset to previous value */
	setlocale(LC_CTYPE, "");
	return 1;
}

/* Encoding setup for Windows */
static void
encoding_init(void)
{
#if USE_WIDEC_SUPPORT
	UINT default_cp = CP_UTF8;
	const char *default_ctype = "C.UTF-8";
#else
	UINT default_cp = 1252;
	const char *default_ctype = "English_United States.1252";
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
	}
	else
	{
		SetConsoleCP(default_cp);
		SetConsoleOutputCP(default_cp);
	}

	if (!setlocale(LC_CTYPE, ctype))
	{
		/* Fallback - try alternative UTF-8 locale names for Windows */
#if USE_WIDEC_SUPPORT
		if (!setlocale(LC_CTYPE, ".UTF8") &&
		    !setlocale(LC_CTYPE, ".utf8") &&
		    !setlocale(LC_CTYPE, "en_US.UTF-8") &&
		    !setlocale(LC_CTYPE, "English_United States.65001"))
		{
			/* Final fallback */
			setlocale(LC_CTYPE, default_ctype);
		}
#else
		setlocale(LC_CTYPE, default_ctype);
#endif
	}

	_nc_setmode(_fileno(stdin), true, false);
	_nc_setmode(_fileno(stdout), false, false);
	_nc_setmode(_fileno(stderr), false, false);
}

// Convert UNIX stty flags to Windows console flags
NCURSES_EXPORT(DWORD) 
_nc_unix_to_win32_input_flags(DWORD dwFlags, const TTY* ttyflags) {
    DWORD flags = dwFlags | ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;
    
    // Raw mode disables most processing
    if (ttyflags->c_lflag & RAW) {
        flags = ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;
        flags &= ~ENABLE_LINE_INPUT; 
    } else {
	if (ttyflags->c_lflag & CBREAK) {
	    flags &= ~ENABLE_LINE_INPUT;
	}
	if (!(ttyflags->c_lflag & ICANON)) {
	    flags &= ~ENABLE_LINE_INPUT;
	}
	if (ttyflags->c_lflag & ECHO) {
	    flags |= ENABLE_ECHO_INPUT;	
	}
	if (ttyflags->c_lflag & ISIG) {
	    flags |= ENABLE_PROCESSED_INPUT;
	}
    }    
    return flags;
}

static void win32_to_unix_input_flags(DWORD dwFlags, TTY *ttyflags) {
    ttyflags->c_lflag = 0;
    
    // Line input mode maps to canonical mode
    if (dwFlags & ENABLE_LINE_INPUT) {
        ttyflags->c_lflag |= ICANON;
    } else {
        // No line input = either raw or cbreak mode
        if (!(dwFlags & ENABLE_PROCESSED_INPUT)) {
            ttyflags->c_lflag |= RAW;
        } else {
            ttyflags->c_lflag |= CBREAK;
        }
    }
    
    // Echo processing
    if (dwFlags & ENABLE_ECHO_INPUT) {
        ttyflags->c_lflag |= ECHO;
    }
    
    // Signal processing
    if (dwFlags & ENABLE_PROCESSED_INPUT) {
        ttyflags->c_lflag |= ISIG;
    }
}

NCURSES_EXPORT(DWORD) 
_nc_unix_to_win32_output_flags(DWORD dwFlags, const TTY* ttyflags) {
    DWORD flags = dwFlags | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    
    if (!(ttyflags->c_lflag & RAW) && (ttyflags->c_lflag & ONLCR)) {
        flags |= ENABLE_PROCESSED_OUTPUT;
    }
    
    return flags;
}

static void win32_to_unix_output_flags(DWORD dwFlags, TTY *ttyflags) {
    // Output processing affects newline translation
    if (dwFlags & ENABLE_PROCESSED_OUTPUT) {
        ttyflags->c_lflag |= ONLCR;
    } else {
        ttyflags->c_lflag &= ~ONLCR;
        // No output processing could indicate raw mode
        ttyflags->c_lflag |= RAW;
    }
}

#define CON_STDIN_HANDLE _nc_console_handle(_fileno(stdin))
#define CON_STDOUT_HANDLE _nc_console_handle(_fileno(stdout))

static bool console_initialized = FALSE;

NCURSES_EXPORT(bool)
_nc_console_checkinit()
{
	bool res = FALSE;

	T((T_CALLED("lib_win32conpty::_nc_console_checkinit()")));

	/* initialize once, or not at all */
	if (!console_initialized)
	{
		int i;
		DWORD num_buttons;
		WORD a;
		BOOL b;
		DWORD dwFlagIn = CONMODE_IN_DEFAULT;
		DWORD dwFlagOut = CONMODE_OUT_DEFAULT;

		START_TRACE();

		encoding_init();

		if (GetNumberOfConsoleMouseButtons(&num_buttons))
		{
			WINCONSOLE.numButtons = (int)num_buttons;
		}
		else
		{
			WINCONSOLE.numButtons = 1;
		}

		a = _nc_console_MapColor(true, COLOR_WHITE) |
		    _nc_console_MapColor(false, COLOR_BLACK);
		for (i = 0; i < CON_NUMPAIRS; i++)
			WINCONSOLE.pairs[i] = a;

		SetConsoleMode(CON_STDIN_HANDLE, dwFlagIn);
		SetConsoleMode(CON_STDOUT_HANDLE, dwFlagOut);
				
		if (CON_STDOUT_HANDLE != INVALID_HANDLE_VALUE)
		{
			_nc_console_get_SBI();
			WINCONSOLE.save_SBI = WINCONSOLE.SBI;
			GetConsoleCursorInfo(CON_STDOUT_HANDLE, &WINCONSOLE.save_CI);
			T(("... initial cursor is %svisible, %d%%",
			   (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
			   (int)WINCONSOLE.save_CI.dwSize));
		}

		WINCONSOLE.initialized = TRUE;
		console_initialized = TRUE;
	}
	res = (CON_STDOUT_HANDLE != INVALID_HANDLE_VALUE);
	BOOL check = _nc_stdout_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   console_initialized,
	   check));
	returnBool(res);
}

#define REQUIRED_MAX_V (DWORD)10
#define REQUIRED_MIN_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763
/*
  This function returns 0 if the Windows version has no support for
  the modern Console interface, otherwise it returns 1
 */
NCURSES_EXPORT(int)
_nc_console_vt_supported(void)
{
	OSVERSIONINFO osvi;
	int res = 0;
	
	T((T_CALLED("lib_win32con::_nc_console_vt_supported")));
	ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	GetVersionEx(&osvi);
	T(("GetVersionEx returnedMajor=%lu, Minor=%lu, Build=%lu",
	   (unsigned long)osvi.dwMajorVersion,
	   (unsigned long)osvi.dwMinorVersion,
	   (unsigned long)osvi.dwBuildNumber));
	if (osvi.dwMajorVersion >= REQUIRED_MAX_V)
	{
		if (osvi.dwMajorVersion == REQUIRED_MAX_V)
		{
			if (((osvi.dwMinorVersion == REQUIRED_MIN_V) &&
			     (osvi.dwBuildNumber >= REQUIRED_BUILD)) ||
			    ((osvi.dwMinorVersion > REQUIRED_MIN_V)))
				res = 1;
		}
		else
			res = 1;
	}
	returnCode(res);
}

/*   Our replacement for the systems _isatty to include also
	 a test for mintty. This is called from the NC_ISATTY macro
	 defined in curses.priv.h

	 Return codes:
	 - 0 : Not a TTY
	 - 1 : A Windows character device detected by _isatty
	 - 2 : A future implementation may return 2 for mintty
 */
NCURSES_EXPORT(int)
_nc_console_isatty(int fd)
{
	int result = 0;

	T((T_CALLED("lib_win32conpty::_nc_console_isatty(%d"), fd));

	if (isatty(fd))
		result = 1;
	returnCode(result);
}

/* Convert a file descriptor into a HANDLE
   That's not necessarily a console HANDLE
*/
NCURSES_EXPORT(HANDLE)
_nc_console_handle(int fd)
{
	intptr_t value = _get_osfhandle(fd);
	return (HANDLE)value;
}

NCURSES_EXPORT(HANDLE)
_nc_console_fd2handle(int fd)
{
	HANDLE hdl = _nc_console_handle(fd);

	if (fd == _fileno(stdin))
	{
		T(("lib_win32con:validateHandle %d -> stdin handle", fd));
	}
	else if (fd == _fileno(stdout))
	{
		T(("lib_win32con:validateHandle %d -> stdout handle", fd));
	}
	else
	{
		T(("lib_win32con:validateHandle %d maps to unknown fd", fd));
	}
	return hdl;
}


NCURSES_EXPORT(int)
_nc_win32_tcsetattr(int fd, const TTY *arg)
{
	int code = ERR;
	if (arg)
	{
		HANDLE stdin_hdl = _nc_console_fd2handle(_fileno(stdin));
		HANDLE stdout_hdl = _nc_console_fd2handle(_fileno(stdout));
		DWORD input_flags = CONMODE_IN_DEFAULT;
		DWORD output_flags = CONMODE_OUT_DEFAULT;

		T(("lib_win32conpty:_nc_win32_tcsetattr %s", _nc_trace_ttymode(arg)));
		
		// Convert flags for both input and output (terminal is one device)
		input_flags = _nc_unix_to_win32_input_flags(input_flags, arg);
		output_flags = _nc_unix_to_win32_output_flags(output_flags, arg);
		
		// Set both input and output modes (like UNIX terminal device)
		bool input_ok = (stdin_hdl == INVALID_HANDLE_VALUE) || SetConsoleMode(stdin_hdl, input_flags);
		bool output_ok = (stdout_hdl == INVALID_HANDLE_VALUE) || SetConsoleMode(stdout_hdl, output_flags);
		
		if (input_ok && output_ok) {
			WINCONSOLE.ttyflags = *arg;
			code = OK;
		} else {
			T(("SetConsoleMode failed - input:%s output:%s", 
			   input_ok ? "OK" : "FAIL", output_ok ? "OK" : "FAIL"));
		}
		assert(_nc_stdout_is_conpty());
	}
	return (code);
}

NCURSES_EXPORT(int)
_nc_win32_tcgetattr(int fd, TTY *arg)
{
	int code = ERR;
	
	if (arg)
	{
		HANDLE stdin_hdl = _nc_console_fd2handle(_fileno(stdin));
		HANDLE stdout_hdl = _nc_console_fd2handle(_fileno(stdout));
		DWORD input_mode = 0, output_mode = 0;
		bool got_input = false, got_output = false;
		
		// Read both input and output modes (terminal is one device)
		if (stdin_hdl != INVALID_HANDLE_VALUE && GetConsoleMode(stdin_hdl, &input_mode)) {
			got_input = true;
		}
		if (stdout_hdl != INVALID_HANDLE_VALUE && GetConsoleMode(stdout_hdl, &output_mode)) {
			got_output = true;
		}
		
		if (got_input || got_output)
		{
			// Clear the structure first
			memset(arg, 0, sizeof(*arg));
			
			// Combine input and output state into unified terminal view
			if (got_input) {
				win32_to_unix_input_flags(input_mode, arg);
			}
			if (got_output) {
				TTY output_tty;
				memset(&output_tty, 0, sizeof(output_tty));
				win32_to_unix_output_flags(output_mode, &output_tty);
				// Merge output flags (mainly ONLCR) into result
				arg->c_lflag |= (output_tty.c_lflag & ONLCR);
			}
			code = OK;
		}
		else
		{
			// Fallback to cached value if we can't read console modes
			*arg = WINCONSOLE.ttyflags;
			code = OK;
		}
	}
	T(("lib_win32con:_nc_console_getmode %s", _nc_trace_ttymode(arg)));
	return (code);
}

// Handle UNIX-like signal characters in ConPTY mode
static void handle_signal_chars(wint_t ch) {
    if (_nc_stdout_is_conpty()) {
        TTY ttyflags;
        if (_nc_win32_tcgetattr(stdin, &ttyflags) != OK) {
            return;
        }
        
        switch(ch) {
            case 3:  // Ctrl+C (SIGINT)
                if (ttyflags.c_lflag & ISIG) {
                    // Raise SIGINT equivalent
                    GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
                }
                break;
            case 28: // Ctrl+\ (SIGQUIT)  
                if (ttyflags.c_lflag & ISIG) {
                    // Handle quit signal
                }
                break;
        }
    }
}

NCURSES_EXPORT(int) _nc_console_process_input(wint_t *ch) {
    TTY ttyflags;
    _nc_win32_tcgetattr(stdin, &ttyflags);
    
    // Handle special characters
    if (*ch == ttyflags.erase_char && !(ttyflags.c_lflag & RAW)) {
        // Process backspace - return actual control character
        return 8;  // Backspace (Ctrl+H)
    }
    if (*ch == ttyflags.kill_char && !(ttyflags.c_lflag & RAW)) {
        // Process kill line - return actual control character  
        return 21; // Kill line (Ctrl+U)
    }
    if (*ch == ttyflags.eof_char && !(ttyflags.c_lflag & RAW)) {
        // Process EOF
        return EOF;
    }
    
    handle_signal_chars(*ch);
    return *ch;
}

NCURSES_EXPORT(void)
_nc_console_size(int *Lines, int *Cols)
{
	if (Lines != NULL && Cols != NULL)
	{
		*Lines = (int)(WINCONSOLE.SBI.srWindow.Bottom + 1 -
			       WINCONSOLE.SBI.srWindow.Top);
		*Cols = (int)(WINCONSOLE.SBI.srWindow.Right + 1 -
			      WINCONSOLE.SBI.srWindow.Left);
	}
}

NCURSES_EXPORT(WORD)
_nc_console_MapColor(bool fore, int color)
{
	static const int _cmap[] =
	    {0, 4, 2, 6, 1, 5, 3, 7};
	int a;
	if (color < 0 || color > 7)
		a = fore ? 7 : 0;
	else
		a = _cmap[color];
	if (!fore)
		a = a << 4;
	return (WORD)a;
}

/*
 * Reliably validate that stdout is in ConPTY mode and not in console mode.
 *
 * Returns:
 *   true  - stdout is in ConPTY mode (virtual terminal processing enabled)
 *   false - stdout is in legacy console mode or not a console at all
 */
NCURSES_EXPORT(bool)
_nc_stdout_is_conpty(void)
{
	HANDLE stdout_handle;
	DWORD console_mode = 0;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	bool result = false;

	T((T_CALLED("lib_win32conpty::_nc_stdout_is_conpty()")));

	// Get stdout handle
	stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
	if (stdout_handle == INVALID_HANDLE_VALUE)
	{
		T(("GetStdHandle(STD_OUTPUT_HANDLE) failed"));
		returnCode(false);
	}

	// First check if we can get console mode at all
	if (!GetConsoleMode(stdout_handle, &console_mode))
	{
		T(("GetConsoleMode failed - not a console"));
		returnCode(false);
	}

	// Check if virtual terminal processing is enabled (ConPTY mode)
	if (console_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	{
		// Additional validation: try to get console screen buffer info
		// In ConPTY mode, this should succeed but behavior may differ
		if (GetConsoleScreenBufferInfo(stdout_handle, &csbi))
		{
			// ConPTY typically has different characteristics
			// We can be confident this is ConPTY mode if VT processing is enabled
			result = true;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING - ConPTY detected"));
		}
		else
		{
			// Even if GetConsoleScreenBufferInfo fails, VT processing flag indicates ConPTY
			result = true;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING (no CSBI) - ConPTY detected"));
		}
	}
	else
	{
		T(("Console mode lacks ENABLE_VIRTUAL_TERMINAL_PROCESSING - legacy console mode"));
	}

	returnCode(result);
}

NCURSES_EXPORT(int)
_nc_console_flush(void *hdl)
{
	int code = OK;

	T((T_CALLED("lib_win32conpty::_nc_console_flush(hdl=%p"), hdl));

	if (hdl != INVALID_HANDLE_VALUE)
	{
		if (hdl == CON_STDIN_HANDLE || hdl == CON_STDOUT_HANDLE)
		{
			if (!FlushConsoleInputBuffer(CON_STDIN_HANDLE))
				code = ERR;
		}
		else
		{
			code = ERR;
			T(("_nc_console_flush not requesting a handle owned by console."));
		}
	}
	assert(_nc_stdout_is_conpty());
	returnCode(code);
}

#define MIN_WIDE 80
#define MIN_HIGH 24

NCURSES_EXPORT(bool)
_nc_console_get_SBI(void)
{
	bool rc = FALSE;
	if (GetConsoleScreenBufferInfo(CON_STDOUT_HANDLE, &(WINCONSOLE.SBI)))
	{
		T(("GetConsoleScreenBufferInfo"));
		T(("... buffer(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwSize.X,
		   WINCONSOLE.SBI.dwSize.Y));
		T(("... window(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwMaximumWindowSize.X,
		   WINCONSOLE.SBI.dwMaximumWindowSize.Y));
		T(("... cursor(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwCursorPosition.X,
		   WINCONSOLE.SBI.dwCursorPosition.Y));
		T(("... display(Top:%d Bottom:%d Left:%d Right:%d)",
		   WINCONSOLE.SBI.srWindow.Top,
		   WINCONSOLE.SBI.srWindow.Bottom,
		   WINCONSOLE.SBI.srWindow.Left,
		   WINCONSOLE.SBI.srWindow.Right));
		WINCONSOLE.origin.X = WINCONSOLE.SBI.srWindow.Left;
		WINCONSOLE.origin.Y = WINCONSOLE.SBI.srWindow.Top;
		rc = TRUE;
	}
	else
	{
		T(("GetConsoleScreenBufferInfo ERR"));
	}
	return rc;
}


NCURSES_EXPORT(void)
_nc_console_debug(const char* headline)
{
    DWORD input_mode = 0, output_mode = 0;
    char debug_text[2048];
       
    /* Get current console modes */
    if (CON_STDIN_HANDLE != INVALID_HANDLE_VALUE) {
        GetConsoleMode(CON_STDIN_HANDLE, &input_mode);
    }
    if (CON_STDOUT_HANDLE != INVALID_HANDLE_VALUE) {
        GetConsoleMode(CON_STDOUT_HANDLE, &output_mode);
    }
    
    /* Format debug information with tabular layout */
    sprintf(debug_text, 
        "%s\n"
        "===========================================\n\n"
        "Console Handles:\n"
        "  Input Handle ................. %p\n"
        "  Output Handle ................ %p\n\n"
        "INPUT CONSOLE MODE (0x%08lX):\n"
        "  ENABLE_PROCESSED_INPUT ....... %s\n"
        "  ENABLE_LINE_INPUT ............ %s\n" 
        "  ENABLE_ECHO_INPUT ............ %s\n"
        "  ENABLE_WINDOW_INPUT .......... %s\n"
        "  ENABLE_MOUSE_INPUT ........... %s\n"
        "  ENABLE_INSERT_MODE ........... %s\n"
        "  ENABLE_QUICK_EDIT_MODE ....... %s\n"
        "  ENABLE_EXTENDED_FLAGS ........ %s\n"
        "  ENABLE_AUTO_POSITION ......... %s\n"
        "  ENABLE_VIRTUAL_TERMINAL_INPUT  %s\n\n"
        "OUTPUT CONSOLE MODE (0x%08lX):\n" 
        "  ENABLE_PROCESSED_OUTPUT ...... %s\n"
        "  ENABLE_WRAP_AT_EOL_OUTPUT .... %s\n"
        "  ENABLE_VT_PROCESSING ......... %s\n"
        "  DISABLE_NEWLINE_AUTO_RETURN .. %s\n"
        "  ENABLE_LVB_GRID_WORLDWIDE .... %s\n\n"
        "WINCONSOLE TTY FLAGS (c_lflag=0x%08lX):\n"
        "  ISIG ......................... %s\n"
        "  ICANON ....................... %s\n" 
        "  ECHO ......................... %s\n"
        "  ONLCR ........................ %s\n"
        "  CBREAK ....................... %s\n"
        "  RAW .......................... %s",
        
        headline ? headline : "NCurses ConPTY Debug Info",
        
        CON_STDIN_HANDLE, CON_STDOUT_HANDLE,
        
        input_mode,
        (input_mode & 0x0001) ? "ON " : "OFF",
        (input_mode & 0x0002) ? "ON " : "OFF",  
        (input_mode & 0x0004) ? "ON " : "OFF",
        (input_mode & 0x0008) ? "ON " : "OFF",
        (input_mode & 0x0010) ? "ON " : "OFF",
        (input_mode & 0x0020) ? "ON " : "OFF",
        (input_mode & 0x0040) ? "ON " : "OFF",
        (input_mode & 0x0080) ? "ON " : "OFF",
        (input_mode & 0x0100) ? "ON " : "OFF",
        (input_mode & 0x0200) ? "ON " : "OFF",
        
        output_mode,
        (output_mode & 0x0001) ? "ON " : "OFF",
        (output_mode & 0x0002) ? "ON " : "OFF",  
        (output_mode & 0x0004) ? "ON " : "OFF",
        (output_mode & 0x0008) ? "ON " : "OFF",
        (output_mode & 0x0010) ? "ON " : "OFF",
        
        WINCONSOLE.ttyflags.c_lflag,
        (WINCONSOLE.ttyflags.c_lflag & ISIG) ? "ON " : "OFF",
        (WINCONSOLE.ttyflags.c_lflag & ICANON) ? "ON " : "OFF", 
        (WINCONSOLE.ttyflags.c_lflag & ECHO) ? "ON " : "OFF",
        (WINCONSOLE.ttyflags.c_lflag & ONLCR) ? "ON " : "OFF",
        (WINCONSOLE.ttyflags.c_lflag & CBREAK) ? "ON " : "OFF",
        (WINCONSOLE.ttyflags.c_lflag & RAW) ? "ON " : "OFF"
    );
    
    /* Use MessageBox for reliable display */
    MessageBox(NULL, debug_text, "NCurses ConPTY Debug", MB_OK | MB_ICONINFORMATION | MB_SYSTEMMODAL);
    
    returnVoid;
}

#endif /* defined(USE_WIN32_CONPTY)) */
