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

#if defined(_NC_WINDOWS_NATIVE)
#include <locale.h>
#include <stdio.h>
#include <wchar.h>  /* For wide character functions */
#include <string.h> /* For memset */

MODULE_ID("$Id$")

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
		    !setlocale(LC_CTYPE, "C.65001"))
		{
			/* Final fallback */
			setlocale(LC_CTYPE, default_ctype);
		}
#else
		setlocale(LC_CTYPE, default_ctype);
#endif
	}

	_nc_setmode(_fileno(stdin), false);
	_nc_setmode(_fileno(stdout), false);
}


#define REQUIRED_MAX_V (DWORD)10
#define REQUIRED_MIN_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763
/*
  This function returns 0 if the Windows version has no support for
  the modern Console interface, otherwise it returns 1
 */
static bool
conpty_supported(void)
{
	OSVERSIONINFOEX osvi = {0};
	bool res = true;

	T((T_CALLED("lib_win32conpty::_nc_console_vt_supported")));

	osvi.dwOSVersionInfoSize = sizeof(osvi);
    	osvi.dwMajorVersion = 10;
    	osvi.dwMinorVersion = 0;
    	osvi.dwBuildNumber = 17763; // Windows 10 version 1809
    
    	ULONGLONG conditionMask = 0;
    	VER_SET_CONDITION(conditionMask, VER_MAJORVERSION, VER_GREATER_EQUAL);
    	VER_SET_CONDITION(conditionMask, VER_MINORVERSION, VER_GREATER_EQUAL);
    	VER_SET_CONDITION(conditionMask, VER_BUILDNUMBER, VER_GREATER_EQUAL);
    
    	res = VerifyVersionInfo(&osvi, 
                        VER_MAJORVERSION | VER_MINORVERSION | VER_BUILDNUMBER,
                        conditionMask);
	returnBool(res);
}

static bool console_initialized = FALSE;

NCURSES_EXPORT(BOOL)
_nc_console_checkinit()
{
	bool res = FALSE;

	T((T_CALLED("lib_win32conpty::_nc_console_checkinit()")));

	/* initialize once, or not at all */
	if (!console_initialized)
	{
		if (!conpty_supported())
		{
			T(("... Windows version does not support ConPTY"));
			fprintf(stderr, "ncurses: Windows version does not support ConPTY\n");
			abort;
		}
		int i;
		DWORD num_buttons;
		WORD a;
		BOOL b;
		DWORD dwFlagIn = CONMODE_IN_DEFAULT;
		DWORD dwFlagOut = CONMODE_OUT_DEFAULT;

		START_TRACE();

		encoding_init();

		WINCONSOLE.conhost_flags = 0;
		const char *env_flags = getenv("NC_CONHOST_FLAGS");
		if (env_flags && *env_flags)
		{
			char *endptr;
			long flags_val = strtol(env_flags, &endptr, 0);
			if (*endptr == '\0' && flags_val >= 0)
			{
				WINCONSOLE.conhost_flags = (unsigned int)(flags_val & NC_CONHOST_FLAG_MASK);
			}
		}

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

		WINCONSOLE.ttyflags.c_lflag = 0;
		SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), dwFlagIn);
		WINCONSOLE.last_input_mode = dwFlagIn;
		SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), dwFlagOut);
		WINCONSOLE.last_output_mode = dwFlagOut;
		_nc_win32_tcgetattr(_fileno(stdin), &WINCONSOLE.ttyflags);

		if (GetStdHandle(STD_OUTPUT_HANDLE) != INVALID_HANDLE_VALUE)
		{
			_nc_console_get_SBI();
			WINCONSOLE.save_SBI = WINCONSOLE.SBI;
			GetConsoleCursorInfo(GetStdHandle(STD_OUTPUT_HANDLE), &WINCONSOLE.save_CI);
			T(("... initial cursor is %svisible, %d%%",
			   (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
			   (int)WINCONSOLE.save_CI.dwSize));
		}

		WINCONSOLE.initialized = TRUE;
		console_initialized = TRUE;
	}
	res = (GetStdHandle(STD_OUTPUT_HANDLE) != INVALID_HANDLE_VALUE);
	BOOL check = _nc_stdout_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   console_initialized,
	   check));
	returnBool(res);
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

// Handle UNIX-like signal characters in ConPTY mode
static void handle_signal_chars(wint_t ch)
{
	if (_nc_stdout_is_conpty())
	{
		TTY ttyflags;
		if (_nc_win32_tcgetattr(stdin, &ttyflags) != OK)
		{
			return;
		}

		switch (ch)
		{
		case 3: // Ctrl+C (SIGINT)
			if (ttyflags.c_lflag & ISIG)
			{
				// Raise SIGINT equivalent
				GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
			}
			break;
		case 28: // Ctrl+\ (SIGQUIT)
			if (ttyflags.c_lflag & ISIG)
			{
				// Handle quit signal
			}
			break;
		}
	}
}

NCURSES_EXPORT(int)
_nc_console_process_input(wint_t *ch)
{
	TTY ttyflags;
	_nc_win32_tcgetattr(stdin, &ttyflags);

	// Handle special characters
	if (*ch == ttyflags.erase_char && !(ttyflags.c_lflag & RAW))
	{
		// Process backspace - return actual control character
		return 8; // Backspace (Ctrl+H)
	}
	if (*ch == ttyflags.kill_char && !(ttyflags.c_lflag & RAW))
	{
		// Process kill line - return actual control character
		return 21; // Kill line (Ctrl+U)
	}
	if (*ch == ttyflags.eof_char && !(ttyflags.c_lflag & RAW))
	{
		// Process EOF
		return EOF;
	}

	handle_signal_chars(*ch);
	return *ch;
}

/* Windows Console resize detection */
static int last_console_lines = -1;
static int last_console_cols = -1;

/*
 * Check if the Windows Console has been resized.
 * This provides SIGWINCH-like functionality for Windows ConPTY.
 * Returns TRUE if a resize was detected.
 */
NCURSES_EXPORT(BOOL)
_nc_console_check_resize(void)
{
	int current_lines, current_cols;
	bool resized = FALSE;

	/* Get current console size */
	_nc_console_size(&current_lines, &current_cols);

	/* Check if this is the first call - initialize stored size */
	if (last_console_lines == -1 || last_console_cols == -1)
	{
		last_console_lines = current_lines;
		last_console_cols = current_cols;
		return FALSE; /* Don't report resize on first call */
	}

	/* Compare with stored size */
	if (current_lines != last_console_lines || current_cols != last_console_cols)
	{
		/* Console was resized */

		/* Update stored size */
		last_console_lines = current_lines;
		last_console_cols = current_cols;

		/* Set the global SIGWINCH flag (like Unix version) */
		_nc_globals.have_sigwinch = 1;

		resized = TRUE;
	}

	return resized;
}

NCURSES_EXPORT(void)
_nc_console_size(int *Lines, int *Cols)
{
	if (Lines != NULL && Cols != NULL)
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);

		/* Get current console buffer information using GetStdHandle for Wine compatibility */
		if (hStdOut != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hStdOut, &csbi))
		{
			*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
			*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
		}
		else
		{
			/* Try the original CON_STDOUT_HANDLE as fallback */
			if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi))
			{
				*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
				*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
			}
			else
			{
				/* Final fallback to cached values */
				*Lines = (int)(WINCONSOLE.SBI.srWindow.Bottom + 1 -
					       WINCONSOLE.SBI.srWindow.Top);
				*Cols = (int)(WINCONSOLE.SBI.srWindow.Right + 1 -
					      WINCONSOLE.SBI.srWindow.Left);
			}
		}
	}
}

NCURSES_EXPORT(WORD)
_nc_console_MapColor(BOOL fore, int color)
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
NCURSES_EXPORT(BOOL)
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


#define MIN_WIDE 80
#define MIN_HIGH 24

NCURSES_EXPORT(BOOL)
_nc_console_get_SBI(void)
{
	bool rc = FALSE;
	if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &(WINCONSOLE.SBI)))
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

/* UTF-8 input assembly for ConPTY binary mode (used by both widec and non-widec) */
#define UTF8_MAX_BYTES 4 /* Maximum bytes in UTF-8 sequence */

typedef struct
{
	unsigned char buffer[UTF8_MAX_BYTES]; /* Buffer for incomplete UTF-8 sequence */
	size_t length;			      /* Current length of buffer */
#if USE_WIDEC_SUPPORT
	mbstate_t state; /* Multibyte conversion state */
#endif
} utf8_input_buffer_t;

static utf8_input_buffer_t _nc_utf8_buffer = {0};

/*
 * Simple UTF-8 decoder that doesn't require wide character functions
 * Returns Unicode codepoint or -1 for invalid sequences
 */
static int
_nc_decode_utf8_simple(const unsigned char *bytes, size_t length, uint32_t *codepoint)
{
	if (length == 0)
		return 0;

	unsigned char first = bytes[0];
	int expected_bytes;
	uint32_t cp = 0;

	/* Determine expected number of bytes from first byte */
	if ((first & 0x80) == 0)
	{
		/* ASCII (0xxxxxxx) */
		expected_bytes = 1;
		cp = first;
	}
	else if ((first & 0xE0) == 0xC0)
	{
		/* 2-byte sequence (110xxxxx) */
		expected_bytes = 2;
		cp = first & 0x1F;
	}
	else if ((first & 0xF0) == 0xE0)
	{
		/* 3-byte sequence (1110xxxx) */
		expected_bytes = 3;
		cp = first & 0x0F;
	}
	else if ((first & 0xF8) == 0xF0)
	{
		/* 4-byte sequence (11110xxx) */
		expected_bytes = 4;
		cp = first & 0x07;
	}
	else
	{
		/* Invalid UTF-8 start byte */
		return -1;
	}

	/* Check if we have enough bytes */
	if ((int)length < expected_bytes)
	{
		return 0; /* Need more bytes */
	}

	/* Process continuation bytes */
	for (int i = 1; i < expected_bytes; i++)
	{
		if ((bytes[i] & 0xC0) != 0x80)
		{
			return -1; /* Invalid continuation byte */
		}
		cp = (cp << 6) | (bytes[i] & 0x3F);
	}

	*codepoint = cp;
	return expected_bytes;
}

/*
 * Assemble incoming UTF-8 bytes into complete characters
 * Returns:
 *  > 0: Complete character assembled (Unicode codepoint)
 *  0:   Need more bytes
 * -1:   Invalid UTF-8 sequence (reset buffer)
 */
static int
_nc_assemble_utf8_input(unsigned char byte, wchar_t *wch)
{
	if (_nc_utf8_buffer.length >= UTF8_MAX_BYTES)
	{
		memset(&_nc_utf8_buffer, 0, sizeof(_nc_utf8_buffer));
		return -1;
	}

	_nc_utf8_buffer.buffer[_nc_utf8_buffer.length++] = byte;

#if USE_WIDEC_SUPPORT
	uint32_t codepoint;
	int consumed = _nc_decode_utf8_simple(_nc_utf8_buffer.buffer,
					      _nc_utf8_buffer.length,
					      &codepoint);
	if (consumed > 0)
	{
		memset(&_nc_utf8_buffer, 0, sizeof(_nc_utf8_buffer));
		*wch = (wchar_t)codepoint;
		int return_value = (int)codepoint;
		return return_value;
	}
	else if (consumed == 0)
	{
		return 0;
	}
	else
	{
		memset(&_nc_utf8_buffer, 0, sizeof(_nc_utf8_buffer));
		return -1;
	}
#else
	/* Use simple UTF-8 decoder for non-widec builds */
	uint32_t codepoint;
	int consumed = _nc_decode_utf8_simple(_nc_utf8_buffer.buffer,
					      _nc_utf8_buffer.length,
					      &codepoint);
	if (consumed > 0)
	{
		memset(&_nc_utf8_buffer, 0, sizeof(_nc_utf8_buffer));
		*wch = (wchar_t)codepoint;
		return (int)codepoint;
	}
	else if (consumed == 0)
	{
		return 0;
	}
	else
	{
		memset(&_nc_utf8_buffer, 0, sizeof(_nc_utf8_buffer));
		return -1;
	}
#endif
}

#if USE_WIDEC_SUPPORT
/*
 * WIN32_CONPTY UTF-8 aware input function (widec mode)
 */
NCURSES_EXPORT(int)
_nc_win32conpty_read(SCREEN *sp, int *result)
{
	unsigned char byte_buffer;
	int n;
	wchar_t wch;
	int ch;

	_nc_set_read_thread(TRUE);
	n = (int)read(sp->_ifd, &byte_buffer, (size_t)1);
	_nc_set_read_thread(FALSE);

	if (n <= 0)
	{
		return n;
	}

	ch = _nc_assemble_utf8_input(byte_buffer, &wch);

	if (ch > 0)
	{
		*result = ch;
		return 1;
	}
	else if (ch == 0)
	{
		return _nc_win32conpty_read(sp, result);
	}
	else
	{
		*result = (int)byte_buffer;
		return 1;
	}
}
#else /* !USE_WIDEC_SUPPORT */
/*
 * WIN32_CONPTY UTF-8 assembly function (non-widec mode)
 * Still needs UTF-8 assembly to prevent multibyte sequences from appearing as separate bytes
 */
NCURSES_EXPORT(int)
_nc_win32conpty_read(SCREEN *sp, int *result)
{
	unsigned char byte_buffer;
	int n;
	wchar_t wch;
	int ch;

	/* Read single byte from console */
	_nc_set_read_thread(TRUE);
	n = (int)read(sp->_ifd, &byte_buffer, (size_t)1);
	_nc_set_read_thread(FALSE);

	if (n <= 0)
	{
		return n;
	}

	ch = _nc_assemble_utf8_input(byte_buffer, &wch);

	if (ch > 0)
	{
		if (wch <= 0xFF)
		{
			*result = (int)wch;
		}
		else
		{
			*result = (int)wch;
		}

		return 1;
	}
	else if (ch == 0)
	{
		return _nc_win32conpty_read(sp, result);
	}
	else
	{
		*result = (int)byte_buffer;
		return 1;
	}
}

#endif /* USE_WIDEC_SUPPORT */

#endif /* defined(USE_WIN32_CONPTY)) */
