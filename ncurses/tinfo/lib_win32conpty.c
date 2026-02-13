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
#include <string.h>
#include <winternl.h>
#include <io.h>    
#include <fcntl.h>

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
	// the most conservative mode is to run in binary mode, and let us handle any necessary translations
	// we have _nc_assemble_utf8_input (see below) to handle UTF-8 decoding, and we want to ensure that 
	// we get the raw bytes as they come in without any interference from the C runtime.
	// For output we have the helper _nc_wchar_to_utf8 to encode UTF-8 characters, and we want to ensure 
	// that the C runtime does not attempt to translate line endings or perform any other transformations 
	// on the output data.
	setmode(_fileno(stdin),  O_BINARY);
	setmode(_fileno(stdout), O_BINARY);
}

#define REQUIRED_MAJOR_V (DWORD)10
#define REQUIRED_MINOR_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763
/*
  This function returns 0 if the Windows version has no support for
  the modern Console interface, otherwise it returns 1
 */

typedef NTSTATUS (WINAPI *RtlGetVersionPtr)(PRTL_OSVERSIONINFOW);

static bool get_real_windows_version(DWORD* major, DWORD* minor, DWORD* build) {
    HMODULE ntdll = GetModuleHandle(TEXT("ntdll.dll"));
    if (ntdll) {
        RtlGetVersionPtr RtlGetVersion = (RtlGetVersionPtr)GetProcAddress(ntdll, "RtlGetVersion");
        if (RtlGetVersion) {
            RTL_OSVERSIONINFOW osvi = {0};
            osvi.dwOSVersionInfoSize = sizeof(osvi);
            if (RtlGetVersion(&osvi) == 0) {
                *major = osvi.dwMajorVersion;
                *minor = osvi.dwMinorVersion; 
                *build = osvi.dwBuildNumber;
                return true;
            }
        }
    }
    return false;
}

static BOOL
conpty_supported(void)
 {
        int res = FALSE;
	DWORD major, minor, build;

        T((T_CALLED("lib_win32conpty::conpty_supported")));
 
 	if (!get_real_windows_version(&major, &minor, &build)) {
		T(("GetVersionEx failed"));
	    	returnBool(FALSE);
	}	
        if (major >= REQUIRED_MAJOR_V)
        {
                if (major == REQUIRED_MAJOR_V)
                {
                        if (((minor == REQUIRED_MINOR_V) &&
                             (build >= REQUIRED_BUILD)) ||
                            ((minor > REQUIRED_MINOR_V)))
                                res = TRUE;
                }
                else
                        res = TRUE;
        }
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
			T(("Windows version does not support ConPTY"));
			fprintf(stderr,"ncurses: Windows version does not support ConPTY\n");
			returnBool(FALSE);
		}
		
		int i;
		DWORD num_buttons;
		WORD a;
		BOOL b;
		DWORD dwFlagIn  = CONMODE_IN_DEFAULT | VT_FLAG_IN;
		DWORD dwFlagOut = CONMODE_OUT_DEFAULT | VT_FLAG_OUT;
		
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

		GetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), &WINCONSOLE.saved_ttyflags.dwFlagIn);
		GetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), &WINCONSOLE.saved_ttyflags.dwFlagOut);
		WINCONSOLE.ttyflags.dwFlagIn = dwFlagIn;
		WINCONSOLE.ttyflags.dwFlagOut = dwFlagOut;
		SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), WINCONSOLE.ttyflags.dwFlagIn);
		SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), WINCONSOLE.ttyflags.dwFlagOut);

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

#if USE_WIDEC_SUPPORT

// Helper function to adress the issue, that for pragmatic reasons we have
// to output UTF-8 encoded data to the Windows Console in O_BINARY mode.
NCURSES_EXPORT(size_t)
_nc_wchar_to_utf8(wchar_t wc, char utf8[UTF8_MAX_BYTES])
{
	wchar_t wstr[2] = {wc, L'\0'};
	int result;

	result = WideCharToMultiByte(CP_UTF8, 0, wstr, 1, utf8, 4, NULL, NULL);
	if (result > 0)
		return (size_t)result;
	else
		return 0; // signals error
}
typedef struct
{
	unsigned char buffer[UTF8_MAX_BYTES]; /* Buffer for incomplete UTF-8 sequence */
	size_t length;			      /* Current length of buffer */
	mbstate_t state;                      /* Multibyte conversion state */
} utf8_input_buffer_t;

static utf8_input_buffer_t utf8_buffer = {0};

/*
 * Convert Unicode codepoint to Windows wchar_t (UTF-16)
 * Handles surrogate pairs for codepoints > 0xFFFF
 * Returns: 1 for BMP characters, 2 for surrogate pairs, -1 for invalid
 */
static int
codepoint_to_wchar(uint32_t codepoint, wchar_t *wch)
{
	if (codepoint <= 0xFFFF)
	{
		/* Basic Multilingual Plane - direct conversion */
		if (codepoint >= 0xD800 && codepoint <= 0xDFFF)
		{
			/* Invalid: surrogate range should not appear in UTF-8 */
			return -1;
		}
		wch[0] = (wchar_t)codepoint;
		return 1;
	}
	else if (codepoint <= 0x10FFFF)
	{
		/* Supplementary planes - needs surrogate pair for Windows */
		/* Convert to UTF-16 surrogate pair */
		uint32_t code = codepoint - 0x10000;
		wch[0] = (wchar_t)(0xD800 + (code >> 10));     /* High surrogate */
		wch[1] = (wchar_t)(0xDC00 + (code & 0x3FF));   /* Low surrogate */
		return 2;
	}
	else
	{
		/* Invalid codepoint */
		return -1;
	}
}

/*
 * Enhanced UTF-8 decoder with overlong sequence detection
 * Returns Unicode codepoint or -1 for invalid sequences
 */
static int
decode_utf8_simple(const unsigned char *bytes, size_t length, uint32_t *codepoint)
{
	if (length == 0)
		return 0;

	unsigned char first = bytes[0];
	int expected_bytes;
	uint32_t cp = 0;
	uint32_t min_value; /* Minimum valid codepoint for this sequence length */

	/* Determine expected number of bytes from first byte */
	if ((first & 0x80) == 0)
	{
		/* ASCII (0xxxxxxx) */
		expected_bytes = 1;
		cp = first;
		min_value = 0x00;
	}
	else if ((first & 0xE0) == 0xC0)
	{
		/* 2-byte sequence (110xxxxx) */
		expected_bytes = 2;
		cp = first & 0x1F;
		min_value = 0x80;
	}
	else if ((first & 0xF0) == 0xE0)
	{
		/* 3-byte sequence (1110xxxx) */
		expected_bytes = 3;
		cp = first & 0x0F;
		min_value = 0x800;
	}
	else if ((first & 0xF8) == 0xF0)
	{
		/* 4-byte sequence (11110xxx) */
		expected_bytes = 4;
		cp = first & 0x07;
		min_value = 0x10000;
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

	/* Check for overlong sequences - reject if codepoint could be
	 * encoded in fewer bytes */
	if (cp < min_value)
	{
		return -1; /* Overlong encoding */
	}

	/* Check for maximum valid Unicode codepoint */
	if (cp > 0x10FFFF)
	{
		return -1; /* Beyond valid Unicode range */
	}

	*codepoint = cp;
	return expected_bytes;
}

/*
 * Assemble incoming UTF-8 bytes into complete characters
 * Returns:
 *  > 0: Complete character assembled (Unicode codepoint or special value for surrogates)
 *  0:   Need more bytes
 * -1:   Invalid UTF-8 sequence (reset buffer)
 * -2:   Surrogate pair needed but wch buffer too small (use extended function)
 */
static int
assemble_utf8_input(unsigned char byte, wchar_t *wch)
{
	if (utf8_buffer.length >= UTF8_MAX_BYTES)
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		return -1;
	}

	utf8_buffer.buffer[utf8_buffer.length++] = byte;

	/* Unified UTF-8 decoding for both widec and non-widec builds */
	uint32_t codepoint;
	int consumed = decode_utf8_simple(utf8_buffer.buffer,
					      utf8_buffer.length,
					      &codepoint);
	if (consumed > 0)
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		codepoint_to_wchar(codepoint, wch);
		/* For Windows, handle potential surrogate pairs */
		if (codepoint <= 0xFFFF)
		{
			/* BMP character - direct conversion is safe */
			*wch = (wchar_t)codepoint;
			return (int)codepoint;
		}
		else
		{
			/* Supplementary plane - would need surrogate pair on Windows */
			/* For terminal input, we'll use the codepoint value directly */
			/* The calling code should handle the Windows-specific conversion */
			*wch = 0xFFFD; /* Replacement character as fallback */
			/* Return the original codepoint for proper handling upstream */
			return (int)codepoint;
		}
	}
	else if (consumed == 0)
	{
		return 0; /* Need more bytes */
	}
	else
	{
		memset(&utf8_buffer, 0, sizeof(utf8_buffer));
		return -1; /* Invalid sequence */
	}
}
#endif /* USE_WIDEC_SUPPORT */

/*
 * WIN32_CONPTY input function (handles both widec and non-widec builds)
 * In wide mode: Assembles UTF-8 byte sequences into Unicode codepoints
 * In non-wide mode: Returns raw bytes directly (for single-byte encodings like CP1252)
 */
NCURSES_EXPORT(int)
_nc_conpty_read(SCREEN *sp, int *result)
{
	unsigned char byte_buffer;
	int n;

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

	ch = assemble_utf8_input(byte_buffer, &wch);

	if (ch > 0)
	{
		/* Complete UTF-8 character assembled */
		*result = ch;
		return 1;
	}
	else if (ch == 0)
	{
		/* Need more bytes - recursively read next byte */
		return _nc_conpty_read(sp, result);
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
NCURSES_EXPORT(int)
_nc_conpty_twait(const SCREEN *sp GCC_UNUSED, 
                      int mode GCC_UNUSED, 
                      int milliseconds, 
                      int *timeleft,
                      long (*gettime_func)(TimeType *, int)
                      EVENTLIST_2nd(_nc_eventlist *evl))
{
    int result = TW_NONE;
    TimeType t0;
    
    /* Helper macro for min */
    #define min(a, b) ((a) < (b) ? (a) : (b))
    
    long starttime = gettime_func(&t0, TRUE);
    
    TR(TRACE_IEVENT, ("start WIN32_CONPTY twait: %d milliseconds, mode: %d",
		      milliseconds, mode));

    if (mode & (TW_INPUT | TW_MOUSE)) {
        HANDLE stdin_handle = (HANDLE)_get_osfhandle(_fileno(stdin));
        DWORD wait_result;
        
        if (milliseconds < 0) {
            /* Infinite wait - but check periodically to be interruptible */
            while (TRUE) {
                wait_result = WaitForSingleObject(stdin_handle, 100);
                if (wait_result == WAIT_OBJECT_0) {
                    result = TW_INPUT;
                    /* For ConPTY, mouse events come through input stream */
                    if (mode & TW_MOUSE) result |= TW_MOUSE;
                    break;
                } else if (wait_result == WAIT_TIMEOUT) {
                    continue; /* Keep waiting */
                } else {
                    break; /* Error */
                }
            }
        } else if (milliseconds == 0) {
            /* Non-blocking check - use different approach for ConPTY */
            DWORD events_available = 0;
            
            /* First try to get number of console input events */
            if (GetNumberOfConsoleInputEvents(stdin_handle, &events_available)) {
                if (events_available > 0) {
                    /* Check if there are actual input events (not just mouse/resize) */
                    INPUT_RECORD input_buffer[16];
                    DWORD events_read = 0;
                    
                    if (PeekConsoleInput(stdin_handle, input_buffer, 
                                       min(events_available, 16), &events_read)) {
                        for (DWORD i = 0; i < events_read; i++) {
                            if (input_buffer[i].EventType == KEY_EVENT && 
                                input_buffer[i].Event.KeyEvent.bKeyDown) {
                                result = TW_INPUT;
                                if (mode & TW_MOUSE) result |= TW_MOUSE;
                                break;
                            } else if (input_buffer[i].EventType == MOUSE_EVENT) {
                                if (mode & TW_MOUSE) result |= TW_MOUSE;
                            }
                        }
                    } else {
                        /* Fallback to WaitForSingleObject if PeekConsoleInput fails */
                        wait_result = WaitForSingleObject(stdin_handle, 0);
                        if (wait_result == WAIT_OBJECT_0) {
                            result = TW_INPUT;
                            if (mode & TW_MOUSE) result |= TW_MOUSE;
                        }
                    }
                } else {
                    /* No events available - return immediately */
                    result = TW_NONE;
                }
            } else {
                /* Fallback to WaitForSingleObject if GetNumberOfConsoleInputEvents fails */
                wait_result = WaitForSingleObject(stdin_handle, 0);
                if (wait_result == WAIT_OBJECT_0) {
                    result = TW_INPUT;
                    if (mode & TW_MOUSE) result |= TW_MOUSE;
                }
            }
        } else {
            /* Timed wait */
            wait_result = WaitForSingleObject(stdin_handle, (DWORD)milliseconds);
            if (wait_result == WAIT_OBJECT_0) {
                result = TW_INPUT;
                if (mode & TW_MOUSE) result |= TW_MOUSE;
            }
        }
    } else if (milliseconds > 0) {
        /* Just sleep for the specified time */
        Sleep((DWORD)milliseconds);
    }
    
    /* Calculate remaining time */
    long elapsed = gettime_func(&t0, FALSE) - starttime;
    if (timeleft) {
        *timeleft = (milliseconds >= 0) ? Max(0, milliseconds - (int)elapsed) : milliseconds;
    }
    
    TR(TRACE_IEVENT, ("end WIN32_CONPTY twait: returned %d, elapsed %ld msec",
		      result, elapsed));
    
    #undef min
    return result;
}

#endif /* defined(USE_WIN32_CONPTY)) */
