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
_nc_unix_to_win32_input_flags(const TTY *mode) {
    DWORD flags = ENABLE_MOUSE_INPUT | VT_FLAG_IN;
    
    if (mode->unixTTYflags.icanon) {
        flags |= ENABLE_LINE_INPUT;
    }
    if (mode->unixTTYflags.echo) {
        flags |= ENABLE_ECHO_INPUT;
    }
    if (mode->unixTTYflags.isig) {
        flags |= ENABLE_PROCESSED_INPUT;
    }
    // Raw mode disables most processing
    if (mode->unixTTYflags.raw) {
        flags = ENABLE_MOUSE_INPUT | VT_FLAG_IN;
    }
    
    return flags;
}

static void win32_to_unix_input_flags(DWORD dwFlags, TTY *mode) {
    mode->unixTTYflags.raw = !(dwFlags & ENABLE_LINE_INPUT);
    mode->unixTTYflags.cbreak = !(dwFlags & ENABLE_PROCESSED_INPUT);
    mode->unixTTYflags.echo = (dwFlags & ENABLE_ECHO_INPUT) != 0;
    mode->unixTTYflags.nl = (dwFlags & ENABLE_PROCESSED_INPUT) != 0;
    mode->unixTTYflags.isig = (dwFlags & ENABLE_PROCESSED_INPUT) != 0;
    mode->unixTTYflags.icanon = !(dwFlags & ENABLE_LINE_INPUT);
}

NCURSES_EXPORT(DWORD) 
_nc_unix_to_win32_output_flags(const TTY *mode) {
    DWORD flags = ENABLE_VIRTUAL_TERMINAL_PROCESSING | VT_FLAG_OUT;
    
    if (!mode->unixTTYflags.raw && mode->unixTTYflags.nl) {
        flags |= ENABLE_PROCESSED_OUTPUT;
    }
    
    return flags;
}

static void win32_to_unix_output_flags(DWORD dwFlags, TTY *mode) {
    mode->unixTTYflags.raw = !(dwFlags & ENABLE_PROCESSED_OUTPUT);
    mode->unixTTYflags.nl = (dwFlags & ENABLE_PROCESSED_OUTPUT) != 0;
}

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

		WINCONSOLE.inp = GetStdHandle(STD_INPUT_HANDLE);
		WINCONSOLE.out = GetStdHandle(STD_OUTPUT_HANDLE);
		WINCONSOLE.hdl = WINCONSOLE.out;

		WINCONSOLE.mode.dwFlagIn = CONMODE_IN_DEFAULT | VT_FLAG_IN;
		SetConsoleMode(WINCONSOLE.inp, WINCONSOLE.mode.dwFlagIn);

		WINCONSOLE.mode.dwFlagOut = CONMODE_OUT_DEFAULT | VT_FLAG_OUT;
		SetConsoleMode(WINCONSOLE.out, WINCONSOLE.mode.dwFlagOut);
		
		win32_to_unix_input_flags(WINCONSOLE.mode.dwFlagIn, &WINCONSOLE.mode);
		win32_to_unix_output_flags(WINCONSOLE.mode.dwFlagOut, &WINCONSOLE.mode);
		
		if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE)
		{
			_nc_console_get_SBI();
			WINCONSOLE.save_SBI = WINCONSOLE.SBI;
			GetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
			T(("... initial cursor is %svisible, %d%%",
			   (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
			   (int)WINCONSOLE.save_CI.dwSize));
		}

		WINCONSOLE.initialized = TRUE;
		console_initialized = TRUE;
	}
	res = (WINCONSOLE.hdl != INVALID_HANDLE_VALUE);
	BOOL check = _nc_stdout_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   console_initialized,
	   check));
	returnBool(res);
}

static ULONGLONG
tdiff(FILETIME fstart, FILETIME fend)
{
	ULARGE_INTEGER ustart;
	ULARGE_INTEGER uend;
	ULONGLONG diff;

	ustart.LowPart = fstart.dwLowDateTime;
	ustart.HighPart = fstart.dwHighDateTime;
	uend.LowPart = fend.dwLowDateTime;
	uend.HighPart = fend.dwHighDateTime;

	diff = (uend.QuadPart - ustart.QuadPart) / 10000;
	return diff;
}

static int
Adjust(int milliseconds, int diff)
{
	if (milliseconds != NC_INFINITY)
	{
		milliseconds -= diff;
		if (milliseconds < 0)
			milliseconds = 0;
	}
	return milliseconds;
}

#define BUTTON_MASK (FROM_LEFT_1ST_BUTTON_PRESSED | \
					 FROM_LEFT_2ND_BUTTON_PRESSED | \
					 FROM_LEFT_3RD_BUTTON_PRESSED | \
					 FROM_LEFT_4TH_BUTTON_PRESSED | \
					 RIGHTMOST_BUTTON_PRESSED)

static mmask_t
decode_mouse(const SCREEN *sp, int mask)
{
	mmask_t result = 0;

	(void)sp;
	assert(sp && console_initialized);

	if (mask & FROM_LEFT_1ST_BUTTON_PRESSED)
		result |= BUTTON1_PRESSED;
	if (mask & FROM_LEFT_2ND_BUTTON_PRESSED)
		result |= BUTTON2_PRESSED;
	if (mask & FROM_LEFT_3RD_BUTTON_PRESSED)
		result |= BUTTON3_PRESSED;
	if (mask & FROM_LEFT_4TH_BUTTON_PRESSED)
		result |= BUTTON4_PRESSED;

	if (mask & RIGHTMOST_BUTTON_PRESSED)
	{
		switch (WINCONSOLE.numButtons)
		{
		case 1:
			result |= BUTTON1_PRESSED;
			break;
		case 2:
			result |= BUTTON2_PRESSED;
			break;
		case 3:
			result |= BUTTON3_PRESSED;
			break;
		case 4:
			result |= BUTTON4_PRESSED;
			break;
		}
	}

	return result;
}

#define AdjustY() ((int)WINCONSOLE.SBI.srWindow.Top)

static bool
handle_mouse(SCREEN *sp, MOUSE_EVENT_RECORD mer)
{
	MEVENT work;
	bool result = FALSE;

	assert(sp);

	sp->_drv_mouse_old_buttons = sp->_drv_mouse_new_buttons;
	sp->_drv_mouse_new_buttons = mer.dwButtonState & BUTTON_MASK;

	/*
	 * We're only interested if the button is pressed or released.
	 * FIXME: implement continuous event-tracking.
	 */
	if (sp->_drv_mouse_new_buttons != sp->_drv_mouse_old_buttons)
	{
		memset(&work, 0, sizeof(work));

		if (sp->_drv_mouse_new_buttons)
		{
			work.bstate |= decode_mouse(sp, sp->_drv_mouse_new_buttons);
		}
		else
		{
			/* cf: BUTTON_PRESSED, BUTTON_RELEASED */
			work.bstate |= (decode_mouse(sp, sp->_drv_mouse_old_buttons) >> 1);
			result = TRUE;
		}

		work.x = mer.dwMousePosition.X;
		work.y = mer.dwMousePosition.Y - AdjustY();

		sp->_drv_mouse_fifo[sp->_drv_mouse_tail] = work;
		sp->_drv_mouse_tail += 1;
	}
	return result;
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

	if (hdl == WINCONSOLE.inp)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.inp", fd));
	}
	else if (hdl == WINCONSOLE.hdl)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.hdl", fd));
	}
	else if (hdl == WINCONSOLE.out)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.out", fd));
	}
	else if (hdl == GetStdHandle(STD_INPUT_HANDLE))
	{
		T(("lib_win32con:validateHandle %d -> STD_INPUT_HANDLE", fd));
	}
	else
	{
		T(("lib_win32con:validateHandle %d maps to unknown HANDLE", fd));
		hdl = INVALID_HANDLE_VALUE;
	}
	return hdl;
}

#define OutHandle() ((WINCONSOLE.progMode) ? WINCONSOLE.hdl : WINCONSOLE.out)

/*
 * Translate Unix-style flags in ConsoleMode to Windows Console flags
 */
NCURSES_EXPORT(int)
_nc_win32_stty(int fd, ConsoleMode *mode)
{
    DWORD dwFlagIn = 0;
    DWORD dwFlagOut = 0;
    
    /* Start with default console modes */
    dwFlagIn = CONMODE_IN_DEFAULT;  
    dwFlagOut = CONMODE_OUT_DEFAULT;
    
    /* Translate Unix-style flags to Windows Console flags */
    
    /* Raw mode: disable all input processing */
    if (mode->unix_flags.unixTTYflags.raw) {
        dwFlagIn &= ~(ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT | 
                      ENABLE_ECHO_INPUT | ENABLE_WINDOW_INPUT);
        /* Keep mouse and virtual terminal input */
        dwFlagIn |= (ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT);
    }
    
    /* Cbreak mode: single character input, no line buffering */
    if (mode->unix_flags.unixTTYflags.cbreak && !mode->unix_flags.unixTTYflags.raw) {
        dwFlagIn &= ~ENABLE_LINE_INPUT;  /* Disable line input */
        dwFlagIn |= ENABLE_PROCESSED_INPUT;  /* Keep processed input for Ctrl+C etc. */
    }
    
    /* Canonical mode: line-oriented input */
    if (mode->unix_flags.unixTTYflags.icanon) {
        dwFlagIn |= ENABLE_LINE_INPUT;
    } else {
        dwFlagIn &= ~ENABLE_LINE_INPUT;
    }
    
    /* Echo mode: echo typed characters */
    if (mode->unix_flags.unixTTYflags.echo) {
        dwFlagIn |= ENABLE_ECHO_INPUT;
    } else {
        dwFlagIn &= ~ENABLE_ECHO_INPUT;
    }
    
    /* Signal processing: handle Ctrl+C, Ctrl+Break */
    if (mode->unix_flags.unixTTYflags.isig) {
        dwFlagIn |= ENABLE_PROCESSED_INPUT;
    } else if (!mode->unix_flags.unixTTYflags.raw) {
        /* In raw mode, we already disabled processed input above */
        dwFlagIn &= ~ENABLE_PROCESSED_INPUT;
    }
    
    /* Newline processing */
    if (mode->unix_flags.unixTTYflags.nl) {
        dwFlagOut |= ENABLE_PROCESSED_OUTPUT;
    } else {
        dwFlagOut &= ~ENABLE_PROCESSED_OUTPUT;
    }
    
    /* Store computed flags back into mode structure */
    mode->dwFlagIn = dwFlagIn;
    mode->dwFlagOut = dwFlagOut;
    
    /* Apply the console mode using existing ncurses Windows console functions */
    return _nc_console_setmode(fd, mode);
}

/*
 * Enhanced console mode setting that uses Unix-style flag translation
 */  
NCURSES_EXPORT(int) 
_nc_console_setmode_unix(int fd, ConsoleMode *mode)
{
    /* Translate Unix flags to Windows console flags */
    int result = _nc_win32_stty(fd, mode);
    
    /* Optional: Log the translation for debugging */
    T(("_nc_console_setmode_unix: unix_flags=[%s%s%s%s%s%s] -> dwFlagIn=0x%08lx, dwFlagOut=0x%08lx",
       mode->unix_flags.unixTTYflags.raw ? "RAW " : "",
       mode->unix_flags.unixTTYflags.cbreak ? "CBREAK " : "", 
       mode->unix_flags.unixTTYflags.icanon ? "ICANON " : "",
       mode->unix_flags.unixTTYflags.echo ? "ECHO " : "",
       mode->unix_flags.unixTTYflags.isig ? "ISIG " : "",
       mode->unix_flags.unixTTYflags.nl ? "NL " : "",
       mode->dwFlagIn, mode->dwFlagOut));
       
    return result;
}

NCURSES_EXPORT(int)
_nc_console_setmode(int fd, const TTY *arg)
{
	int code = ERR;

	if (arg)
	{
		HANDLE hdl = _nc_console_fd2handle(fd);
		DWORD dwFlag = 0;
		HANDLE alt;

		T(("lib_win32con:_nc_console_setmode %s", _nc_trace_ttymode(arg)));
		if (hdl == WINCONSOLE.inp)
		{
			dwFlag = _nc_unix_to_win32_input_flags	(arg);
			SetConsoleMode(hdl, dwFlag);

			alt = OutHandle();
			dwFlag = _nc_unix_to_win32_output_flags(arg);
			SetConsoleMode(alt, dwFlag);
		}
		else
		{
			dwFlag = _nc_unix_to_win32_output_flags(arg);
			SetConsoleMode(hdl, dwFlag);

			alt = WINCONSOLE.inp;
			dwFlag = _nc_unix_to_win32_input_flags(arg);
			SetConsoleMode(alt, dwFlag);
		}
		code = OK;
		assert(_nc_stdout_is_conpty());
	}
	return (code);
}

// Implement UNIX stty-like mode functions
NCURSES_EXPORT(int) _nc_console_raw(void) {
    TTY mode;
    if (_nc_console_getmode(WINCONSOLE.inp, &mode) == OK) {
        mode.unixTTYflags.raw = 1;
        mode.unixTTYflags.cbreak = 0;
        mode.unixTTYflags.echo = 0;
        mode.unixTTYflags.nl = 0;
        mode.unixTTYflags.isig = 0;
        mode.unixTTYflags.icanon = 0;
        return _nc_console_setmode(_nc_console_handle(STDIN_FILENO), &mode);
    }
    return ERR;
}

NCURSES_EXPORT(int) _nc_console_cbreak(void) {
    TTY mode;
    if (_nc_console_getmode(WINCONSOLE.inp, &mode) == OK) {
        mode.unixTTYflags.raw = 0;
        mode.unixTTYflags.cbreak = 1;
        mode.unixTTYflags.icanon = 0;  // Disable line buffering
        return _nc_console_setmode(_nc_console_handle(STDIN_FILENO), &mode);
    }
    return ERR;
}

NCURSES_EXPORT(int) _nc_console_noecho(void) {
    TTY mode;
    if (_nc_console_getmode(WINCONSOLE.inp, &mode) == OK) {
        mode.unixTTYflags.echo = 0;
        return _nc_console_setmode(_nc_console_handle(STDIN_FILENO), &mode);
    }
    return ERR;
}

NCURSES_EXPORT(int)
_nc_console_getmode(HANDLE hdl, TTY *arg)
{
	int code = ERR;
	
	if (arg)
	{
		DWORD dwFlag = 0;
		HANDLE alt;

		if (hdl == WINCONSOLE.inp)
		{
			if (GetConsoleMode(hdl, &dwFlag))
			{
				win32_to_unix_input_flags(dwFlag, arg);
				alt = OutHandle();
				if (GetConsoleMode(alt, &dwFlag))
				{
					win32_to_unix_output_flags(dwFlag, arg);
					code = OK;
				}
			}
		}
		else
		{
			if (GetConsoleMode(hdl, &dwFlag))
			{
				win32_to_unix_output_flags(dwFlag, arg);
				alt = WINCONSOLE.inp;
				if (GetConsoleMode(alt, &dwFlag))
				{
					win32_to_unix_input_flags(dwFlag, arg);
					code = OK;
				}
			}
		}
	}
	T(("lib_win32con:_nc_console_getmode %s", _nc_trace_ttymode(arg)));
	return (code);
}

// Handle UNIX-like signal characters in ConPTY mode
static void handle_signal_chars(wint_t ch) {
    if (_nc_stdout_is_conpty()) {
        ConsoleMode mode;
        if (_nc_console_getmode(WINCONSOLE.inp, &mode) != OK) {
            return;
        }
        
        switch(ch) {
            case 3:  // Ctrl+C (SIGINT)
                if (mode.unix_flags.unixTTYflags.isig) {
                    // Raise SIGINT equivalent
                    GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0);
                }
                break;
            case 28: // Ctrl+\ (SIGQUIT)  
                if (mode.unix_flags.unixTTYflags.isig) {
                    // Handle quit signal
                }
                break;
        }
    }
}

NCURSES_EXPORT(int) _nc_console_process_input(wint_t *ch) {
    ConsoleMode mode;
    _nc_console_getmode(WINCONSOLE.inp, &mode);
    
    // Handle special characters
    if (*ch == mode.unix_flags.erase_char && !mode.unix_flags.unixTTYflags.raw) {
        // Process backspace - return actual control character
        return 8;  // Backspace (Ctrl+H)
    }
    if (*ch == mode.unix_flags.kill_char && !mode.unix_flags.unixTTYflags.raw) {
        // Process kill line - return actual control character  
        return 21; // Kill line (Ctrl+U)
    }
    if (*ch == mode.unix_flags.eof_char && !mode.unix_flags.unixTTYflags.raw) {
        // Process EOF
        return EOF;
    }
    
    handle_signal_chars(*ch);
    return *ch;
}

NCURSES_EXPORT(void) _nc_console_show_settings(void) {
    ConsoleMode mode;
    if (_nc_console_getmode(WINCONSOLE.inp, &mode) == OK) {
        printf("ConPTY mode: %s\n", _nc_stdout_is_conpty() ? "yes" : "no");
        printf("raw: %s, cbreak: %s, echo: %s\n",
               mode.unix_flags.unixTTYflags.raw ? "on" : "off",
               mode.unix_flags.unixTTYflags.cbreak ? "on" : "off", 
               mode.unix_flags.unixTTYflags.echo ? "on" : "off");
        printf("Input flags: 0x%lx, Output flags: 0x%lx\n",
               mode.dwFlagIn, mode.dwFlagOut);
    }
}


/* Validate that a HANDLE is actually a
   console HANDLE
*/
static BOOL
IsConsoleHandle(HANDLE hdl)
{
	DWORD dwFlag = 0;
	BOOL result = FALSE;

	T((T_CALLED("lib_win32con::IsConsoleHandle(HANDLE=%p"), hdl));

	if (!GetConsoleMode(hdl, &dwFlag))
	{
		T(("GetConsoleMode failed"));
	}
	else
	{
		result = TRUE;
	}

	returnBool(result);
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
		if (hdl == WINCONSOLE.hdl ||
			hdl == WINCONSOLE.inp ||
			hdl == WINCONSOLE.out)
		{
			if (!FlushConsoleInputBuffer(WINCONSOLE.inp))
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
	if (GetConsoleScreenBufferInfo(WINCONSOLE.hdl, &(WINCONSOLE.SBI)))
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

NCURSES_EXPORT(int)
_nc_console_read(SCREEN *sp, HANDLE hdl, int *buf)
{
	int rc = -1;
	INPUT_RECORD inp_rec;
	BOOL b;
	DWORD nRead;
	WORD vk;

	assert(sp);
	assert(buf);

	memset(&inp_rec, 0, sizeof(inp_rec));

	T((T_CALLED("lib_win32con::_nc_console_read(%p)"), sp));

	while ((b = read_keycode(hdl, &inp_rec, 1, &nRead)))
	{
		if (b && nRead > 0)
		{
			if (rc < 0)
				rc = 0;
			rc = rc + (int)nRead;
			if (inp_rec.EventType == KEY_EVENT)
			{
				if (!inp_rec.Event.KeyEvent.bKeyDown)
					continue;
#if USE_WIDEC_SUPPORT
				*buf = (int)inp_rec.Event.KeyEventChar;
#else
				*buf = (int)(unsigned char)(inp_rec.Event.KeyEventChar);
#endif
				vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
				/*
				 * There are 24 virtual function-keys, and typically
				 * 12 function-keys on a keyboard.  Use the shift-modifier
				 * to provide the remaining 12 keys.
				 */
				if (vk >= VK_F1 && vk <= VK_F12)
				{
					if (inp_rec.Event.KeyEvent.dwControlKeyState &
					    SHIFT_PRESSED)
					{
						vk = (WORD)(vk + 12);
					}
				}
				if (vk == VK_BACK)
				{
					if (!(inp_rec.Event.KeyEvent.dwControlKeyState & (SHIFT_PRESSED | CONTROL_PRESSED)))
					{
						*buf = KEY_BACKSPACE;
					}
				}
				else if (vk == VK_TAB)
				{
					if ((inp_rec.Event.KeyEvent.dwControlKeyState & (SHIFT_PRESSED | CONTROL_PRESSED)))
					{
						*buf = KEY_BTAB;
					}
				}
				break;
			}
			else if (inp_rec.EventType == MOUSE_EVENT)
			{
				if (handle_mouse(sp,
						 inp_rec.Event.MouseEvent))
				{
					*buf = KEY_MOUSE;
					break;
				}
			}
			else if (inp_rec.EventType == WINDOW_BUFFER_SIZE_EVENT)
			{
				T(("_nc_console_read: WINDOW_BUFFER_SIZE_EVENT"));
				/* Console window was resized - handle like Unix SIGWINCH */
				{
					// FIXME needs to be implemented as an event in wgetch-events
				}
				break;
			}
			continue;
		}
	}
	returnCode(rc);
}

/*NCURSES_EXPORT(void)
_nc_console_selectActiveHandle(void)
{
	if (WINCONSOLE.lastOut != WINCONSOLE.hdl)
	{
		WINCONSOLE.lastOut = WINCONSOLE.hdl;
		// JPF SetConsoleActiveScreenBuffer(WINCONSOLE.lastOut);
	}
}*/

NCURSES_EXPORT(int)
_nc_console_test(int fd)
{
	int code = 0;
	HANDLE hdl = INVALID_HANDLE_VALUE;
	T((T_CALLED("lib_win32con::_nc_console_test(%d)"), fd));
	hdl = _nc_console_handle(fd);
	code = (int)IsConsoleHandle(hdl);
	returnCode(code);
}

NCURSES_EXPORT(int)
_nc_console_testmouse(
	const SCREEN *sp,
	HANDLE hdl,
	int delay
		EVENTLIST_2nd(_nc_eventlist *evl))
{
	int rc = 0;

	assert(sp);

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail)
	{
		rc = TW_MOUSE;
	}
	else
	{
		rc = _nc_console_twait(sp,
							   hdl,
							   TWAIT_MASK,
							   delay,
							   (int *)0 EVENTLIST_2nd(evl));
	}
	return rc;
}

NCURSES_EXPORT(int)
_nc_console_twait(
	const SCREEN *sp,
	HANDLE hdl,
	int mode,
	int milliseconds,
	int *timeleft
		EVENTLIST_2nd(_nc_eventlist *evl))
{
	INPUT_RECORD inp_rec;
	BOOL b;
	DWORD nRead = 0, rc = WAIT_FAILED;
	int code = 0;
	FILETIME fstart;
	FILETIME fend;
	int diff;
	bool isNoDelay = (milliseconds == 0);

#ifdef NCURSES_WGETCH_EVENTS
	(void)evl; /* TODO: implement wgetch-events */
#endif

#define IGNORE_CTRL_KEYS (SHIFT_PRESSED |                        \
						  LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED | \
						  LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)
#define CONSUME() read_keycode(hdl, &inp_rec, 1, &nRead)

	assert(sp);

	TR(TRACE_IEVENT, ("start twait: hdl=%p, %d milliseconds, mode: %d",
					  hdl, milliseconds, mode));

	if (milliseconds < 0)
		milliseconds = NC_INFINITY;

	memset(&inp_rec, 0, sizeof(inp_rec));

	while (true)
	{
		if (!isNoDelay)
		{
			GetSystemTimeAsFileTime(&fstart);
			rc = WaitForSingleObject(hdl, (DWORD)milliseconds);
			GetSystemTimeAsFileTime(&fend);
			diff = (int)tdiff(fstart, fend);
			milliseconds = Adjust(milliseconds, diff);
			if (milliseconds < 0)
				break;
		}

		if (isNoDelay || (rc == WAIT_OBJECT_0))
		{
			if (mode)
			{
				nRead = 0;
				b = GetNumberOfConsoleInputEvents(hdl, &nRead);
				if (!b)
				{
					T(("twait:err GetNumberOfConsoleInputEvents"));
				}
				if (isNoDelay && b)
				{
					T(("twait: Events Available: %lu", (unsigned long)nRead));
					if (nRead == 0)
					{
						code = 0;
						goto end;
					}
					else
					{
						DWORD n = 0;
						MakeArray(pInpRec, INPUT_RECORD, nRead);
						if (pInpRec != NULL)
						{
							DWORD i;
							BOOL f;
							memset(pInpRec, 0, sizeof(INPUT_RECORD) * nRead);
							f = PeekConsoleInput(hdl, pInpRec, nRead, &n);
							if (f)
							{
								for (i = 0; i < n; i++)
								{
									if (pInpRec[i].EventType == KEY_EVENT)
									{
										if (pInpRec[i].Event.KeyEvent.bKeyDown)
										{
											DWORD ctrlMask =
												(pInpRec[i].Event.KeyEvent.dwControlKeyState &
												 IGNORE_CTRL_KEYS);
											if (!ctrlMask)
											{
												code = TW_INPUT;
												goto end;
											}
										}
									}
								}
							}
							else
							{
								T(("twait:err PeekConsoleInput"));
							}
							code = 0;
							goto end;
						}
						else
						{
							T(("twait:err could not alloca input records"));
						}
					}
				}
				if (b && nRead > 0)
				{
					b = PeekConsoleInput(hdl, &inp_rec, 1, &nRead);
					if (!b)
					{
						T(("twait:err PeekConsoleInput"));
					}
					if (b && nRead > 0)
					{
						switch (inp_rec.EventType)
						{
						case KEY_EVENT:
							if (mode & TW_INPUT)
							{
								WORD vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
								WORD ch = inp_rec.Event.KeyEventChar;

								T(("twait:event KEY_EVENT"));
								T(("twait vk=%d, ch=%d, keydown=%d",
								   vk, ch, inp_rec.Event.KeyEvent.bKeyDown));

								if (inp_rec.Event.KeyEvent.bKeyDown)
								{
									T(("twait:event KeyDown"));
									code = TW_INPUT;
									goto end;
								}
								else
								{
									CONSUME();
								}
							}
							continue;
						case MOUSE_EVENT:
							T(("twait:event MOUSE_EVENT"));
							if (decode_mouse(sp,
											 (inp_rec.Event.MouseEvent.dwButtonState & BUTTON_MASK)) == 0)
							{
								CONSUME();
							}
							else if (mode & TW_MOUSE)
							{
								code = TW_MOUSE;
								goto end;
							}
							continue;
						case WINDOW_BUFFER_SIZE_EVENT:
							T(("twait:event WINDOW_BUFFER_SIZE_EVENT"));
							/* Console window was resized - handle like Unix SIGWINCH */
							{
								// FIXME needs to be implemented as an event in wgetch-events
							}
							CONSUME();
							continue;
						default:
							T(("twait:event Type %d", inp_rec.EventType));
							CONSUME();
							// JPF remove? _nc_console_selectActiveHandle();
							continue;
						}
					}
				}
			}
			continue;
		}
		else
		{
			if (rc != WAIT_TIMEOUT)
			{
				code = -1;
				break;
			}
			else
			{
				code = 0;
				break;
			}
		}
	}
end:

	TR(TRACE_IEVENT, ("end twait: returned %d (%lu), remaining time %d msec",
					  code, (unsigned long)GetLastError(), milliseconds));

	if (timeleft)
		*timeleft = milliseconds;

	return code;
}


#endif /* defined(USE_WIN32_CONPTY)) */
