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

#define GenMap(vKey, key) MAKELONG(key, vKey)
/* *INDENT-OFF* */
static const LONG keylist[] =
	{
		GenMap(VK_PRIOR, KEY_PPAGE),
		GenMap(VK_NEXT, KEY_NPAGE),
		GenMap(VK_END, KEY_END),
		GenMap(VK_HOME, KEY_HOME),
		GenMap(VK_LEFT, KEY_LEFT),
		GenMap(VK_UP, KEY_UP),
		GenMap(VK_RIGHT, KEY_RIGHT),
		GenMap(VK_DOWN, KEY_DOWN),
		GenMap(VK_DELETE, KEY_DC),
		GenMap(VK_INSERT, KEY_IC)};
static const LONG ansi_keys[] =
	{
		GenMap(VK_PRIOR, 'I'),
		GenMap(VK_NEXT, 'Q'),
		GenMap(VK_END, 'O'),
		GenMap(VK_HOME, 'H'),
		GenMap(VK_LEFT, 'K'),
		GenMap(VK_UP, 'H'),
		GenMap(VK_RIGHT, 'M'),
		GenMap(VK_DOWN, 'P'),
		GenMap(VK_DELETE, 'S'),
		GenMap(VK_INSERT, 'R')};
/* *INDENT-ON* */
#define array_length(a) (sizeof(a) / sizeof(a[0]))
#define N_INI ((int)array_length(keylist))
#define FKEYS 24
#define MAPSIZE (FKEYS + N_INI)

/*   A process can only have a single console, so it is safe
	 to maintain all the information about it in a single
	 static structure.
 */
NCURSES_EXPORT_VAR(ConsoleInfo)
_nc_CONSOLE;

static bool console_initialized = FALSE;
#define EnsureInit() (void)(console_initialized ? TRUE : _nc_console_checkinit(USE_NAMED_PIPES))

static int
rkeycompare(const void *el1, const void *el2)
{
	WORD key1 = (LOWORD((*((const LONG *)el1)))) & 0x7fff;
	WORD key2 = (LOWORD((*((const LONG *)el2)))) & 0x7fff;

	return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static int
keycompare(const void *el1, const void *el2)
{
	WORD key1 = HIWORD((*((const LONG *)el1)));
	WORD key2 = HIWORD((*((const LONG *)el2)));

	return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static int
MapKey(WORD vKey)
{
	int code = -1;

	if (!WINCONSOLE.isTermInfoConsole)
	{
		WORD nKey = 0;
		void *res;
		LONG key = GenMap(vKey, 0);

		res = bsearch(&key,
					  WINCONSOLE.map,
					  (size_t)(N_INI + FKEYS),
					  sizeof(keylist[0]),
					  keycompare);
		if (res)
		{
			key = *((LONG *)res);
			nKey = LOWORD(key);
			code = (int)(nKey & 0x7fff);
			if (nKey & 0x8000)
				code = -code;
		}
	}
	return code;
}

static int
AnsiKey(WORD vKey)
{
	int code = -1;

	if (!WINCONSOLE.isTermInfoConsole)
	{
		WORD nKey = 0;
		void *res;
		LONG key = GenMap(vKey, 0);

		res = bsearch(&key,
					  WINCONSOLE.ansi_map,
					  (size_t)(N_INI + FKEYS),
					  sizeof(keylist[0]),
					  keycompare);
		if (res)
		{
			key = *((LONG *)res);
			nKey = LOWORD(key);
			code = (int)(nKey & 0x7fff);
			if (nKey & 0x8000)
				code = -code;
		}
	}
	return code;
}

#define NOT_IMPLEMENTED { fprintf(stderr, "NOT IMPLEMENTED: %s:%d\n", __FILE__, __LINE__); abort(); }

static bool
save_original_screen(void)
{
	NOT_IMPLEMENTED
	return false;
}
static bool
restore_original_screen(void)
{
	NOT_IMPLEMENTED
	return false;
}


NCURSES_EXPORT(bool)
_nc_console_checkinit(bool assumeTermInfo)
{
	bool res = FALSE;

	T((T_CALLED("lib_win32con::_nc_console_checkinit(assumeTermInfo=%d)"),
	   assumeTermInfo));

	/* initialize once, or not at all */
	if (!console_initialized)
	{
		int i;
		DWORD num_buttons;
		WORD a;
		BOOL buffered = FALSE;
		BOOL b;

		START_TRACE();
		WINCONSOLE.isTermInfoConsole = assumeTermInfo;

		WINCONSOLE.map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
		WINCONSOLE.rmap = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
		WINCONSOLE.ansi_map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);

		for (i = 0; i < (N_INI + FKEYS); i++)
		{
			if (i < N_INI)
			{
				WINCONSOLE.rmap[i] = WINCONSOLE.map[i] =
					(DWORD)keylist[i];
				WINCONSOLE.ansi_map[i] = (DWORD)ansi_keys[i];
			}
			else
			{
				WINCONSOLE.rmap[i] = WINCONSOLE.map[i] =
					(DWORD)GenMap((VK_F1 + (i - N_INI)),
								  (KEY_F(1) + (i - N_INI)));
				WINCONSOLE.ansi_map[i] =
					(DWORD)GenMap((VK_F1 + (i - N_INI)),
								  (';' + (i - N_INI)));
			}
		}
		qsort(WINCONSOLE.ansi_map,
			  (size_t)(MAPSIZE),
			  sizeof(keylist[0]),
			  keycompare);
		qsort(WINCONSOLE.map,
			  (size_t)(MAPSIZE),
			  sizeof(keylist[0]),
			  keycompare);
		qsort(WINCONSOLE.rmap,
			  (size_t)(MAPSIZE),
			  sizeof(keylist[0]),
			  rkeycompare);

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

#define SaveConsoleMode(handle, value) \
	GetConsoleMode(WINCONSOLE.handle, &WINCONSOLE.originalMode.value)

		if (WINCONSOLE.isTermInfoConsole)
		{
			WINCONSOLE.inp = GetStdHandle(STD_INPUT_HANDLE);
			WINCONSOLE.out = GetStdHandle(STD_OUTPUT_HANDLE);
			WINCONSOLE.hdl = WINCONSOLE.out;

			SaveConsoleMode(inp, dwFlagIn);
			SaveConsoleMode(out, dwFlagOut);
		}
		else
		{
			b = AllocConsole();

			if (!b)
				b = AttachConsole(ATTACH_PARENT_PROCESS);

			WINCONSOLE.inp = GetDirectHandle("CONIN$", FILE_SHARE_READ);
			WINCONSOLE.out = GetDirectHandle("CONOUT$", FILE_SHARE_WRITE);

			SaveConsoleMode(inp, dwFlagIn);
			SaveConsoleMode(out, dwFlagOut);

			if (getenv("NCGDB") || getenv("NCURSES_CONSOLE2"))
			{
				WINCONSOLE.hdl = WINCONSOLE.out;
				buffered = FALSE;
				T(("... will not buffer console"));
			}
			else
			{
				T(("... creating console buffer"));
				WINCONSOLE.hdl =
					CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
											  FILE_SHARE_READ | FILE_SHARE_WRITE,
											  NULL,
											  CONSOLE_TEXTMODE_BUFFER,
											  NULL);
				buffered = TRUE;
			}
		}

		/* We set binary I/O even when using the console
		   driver to cover the situation, that the
		   TERM variable is set to #win32con, but actually
		   Windows supports virtual terminal processing.
		   So if terminfo functions are used in this setup,
		   they actually may work.
		 */
		/*_nc_setmode(fileno(stdin), true, false);
		_nc_setmode(fileno(stdout), false, false);
		_nc_setmode(fileno(stderr), false, false);*/
		if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE)
		{
			WINCONSOLE.buffered = buffered;
			_nc_console_get_SBI();
			WINCONSOLE.save_SBI = WINCONSOLE.SBI;
			if (!buffered)
			{
				save_original_screen();
				_nc_console_set_scrollback(FALSE, &WINCONSOLE.SBI);
			}
			GetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
			T(("... initial cursor is %svisible, %d%%",
			   (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
			   (int)WINCONSOLE.save_CI.dwSize));
		}

		WINCONSOLE.initialized = TRUE;
		console_initialized = TRUE;
	}
	res = (WINCONSOLE.hdl != INVALID_HANDLE_VALUE);
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
	T((T_CALLED("lib_win32con::_nc_console_isatty(%d"), fd));

	if (isatty(fd))
		result = 1;
	returnCode(result);
}
int xxx=0;

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
		if (!WINCONSOLE.isTermInfoConsole && WINCONSOLE.progMode)
		{
			hdl = WINCONSOLE.inp;
		}
	}
	else
	{
		T(("lib_win32con:validateHandle %d maps to unknown HANDLE", fd));
		hdl = INVALID_HANDLE_VALUE;
	}
	if (hdl != INVALID_HANDLE_VALUE)
	{
		if (hdl != WINCONSOLE.inp && (!WINCONSOLE.isTermInfoConsole && WINCONSOLE.progMode))
		{
			if (hdl == WINCONSOLE.out && hdl != WINCONSOLE.hdl)
			{
				T(("lib_win32con:validateHandle forcing WINCONSOLE.out -> WINCONSOLE.hdl"));
				hdl = WINCONSOLE.hdl;
			}
		}
	}
	return hdl;
}

#define OutHandle() ((WINCONSOLE.isTermInfoConsole || WINCONSOLE.progMode) ? WINCONSOLE.hdl : WINCONSOLE.out)
NCURSES_EXPORT(int)
_nc_console_setmode(int fd, const TTY *arg)
{
	HANDLE hdl = _nc_console_fd2handle(fd);
	DWORD dwFlag = 0;
	int code = ERR;
	HANDLE alt;

	if (arg)
	{
#ifdef TRACE
		TTY TRCTTY;
#define TRCTTYOUT(flag) TRCTTY.dwFlagOut = flag
#define TRCTTYIN(flag) TRCTTY.dwFlagIn = flag
#else
#define TRCTTYOUT(flag)
#define TRCTTYIN(flag)
#endif
		T(("lib_win32con:_nc_console_setmode %s", _nc_trace_ttymode(arg)));
		if (hdl == WINCONSOLE.inp)
		{
			dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT | VT_FLAG_IN;
			TRCTTYIN(dwFlag);
			SetConsoleMode(hdl, dwFlag);

			alt = OutHandle();
			dwFlag = arg->dwFlagOut;
			TRCTTYOUT(dwFlag);
			SetConsoleMode(alt, dwFlag);
		}
		else
		{
			dwFlag = arg->dwFlagOut;
			TRCTTYOUT(dwFlag);
			SetConsoleMode(hdl, dwFlag);

			alt = WINCONSOLE.inp;
			dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT;
			TRCTTYIN(dwFlag);
			SetConsoleMode(alt, dwFlag);
			T(("effective mode set %s", _nc_trace_ttymode(&TRCTTY)));
		}
		code = OK;
	}
	return (code);
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
				arg->dwFlagIn = dwFlag;
				alt = OutHandle();
				if (GetConsoleMode(alt, &dwFlag))
				{
					arg->dwFlagOut = dwFlag;
					code = OK;
				}
			}
		}
		else
		{
			if (GetConsoleMode(hdl, &dwFlag))
			{
				arg->dwFlagOut = dwFlag;
				alt = WINCONSOLE.inp;
				if (GetConsoleMode(alt, &dwFlag))
				{
					arg->dwFlagIn = dwFlag;
					code = OK;
				}
			}
		}
	}
	T(("lib_win32con:_nc_console_getmode %s", _nc_trace_ttymode(arg)));
	return (code);
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

NCURSES_EXPORT(void)
_nc_console_size(int *Lines, int *Cols)
{
	EnsureInit();
	if (Lines != NULL && Cols != NULL)
	{
		if (WINCONSOLE.buffered)
		{
			*Lines = (int)(WINCONSOLE.SBI.dwSize.Y);
			*Cols = (int)(WINCONSOLE.SBI.dwSize.X);
		}
		else
		{
			*Lines = (int)(WINCONSOLE.SBI.srWindow.Bottom + 1 -
						   WINCONSOLE.SBI.srWindow.Top);
			*Cols = (int)(WINCONSOLE.SBI.srWindow.Right + 1 -
						  WINCONSOLE.SBI.srWindow.Left);
		}
	}
}


NCURSES_EXPORT(WORD) _nc_console_MapColor(bool fore, int color) 
{
	NOT_IMPLEMENTED
	return (WORD)0;
}
NCURSES_EXPORT(int)  _nc_console_flush(void* handle) 
{
	NOT_IMPLEMENTED
	return ERR;
}
NCURSES_EXPORT(bool) _nc_console_get_SBI(void) 
{
	NOT_IMPLEMENTED
	return false;
}
NCURSES_EXPORT(bool) _nc_console_keyExist(int keycode) 
{
	NOT_IMPLEMENTED
	return false;
}
NCURSES_EXPORT(int)  _nc_console_keyok(int keycode, int flag) {
	NOT_IMPLEMENTED
	return ERR;
}
NCURSES_EXPORT(int)  _nc_console_read(SCREEN *sp, HANDLE fd, int *buf) 
{
	NOT_IMPLEMENTED
	return ERR;
}
NCURSES_EXPORT(bool) _nc_console_restore(void) 
{
	NOT_IMPLEMENTED
	return false;
}
NCURSES_EXPORT(void) _nc_console_selectActiveHandle(void) 
{
	NOT_IMPLEMENTED
}
NCURSES_EXPORT(void) _nc_console_set_scrollback(bool normal, CONSOLE_SCREEN_BUFFER_INFO * info) {
	NOT_IMPLEMENTED
}
NCURSES_EXPORT(int)  _nc_console_test(int fd) {
	NOT_IMPLEMENTED
	return ERR;
}
NCURSES_EXPORT(int)  _nc_console_testmouse(const SCREEN *sp, HANDLE fd, int delay EVENTLIST_2nd(_nc_eventlist*)) 
{
	NOT_IMPLEMENTED
	return ERR;
}
NCURSES_EXPORT(int)  _nc_console_twait(const SCREEN *sp, HANDLE hdl,int mode,int msec,int *left EVENTLIST_2nd(_nc_eventlist * evl)) 
{
	NOT_IMPLEMENTED
	return ERR;
}

static int
locale_is_utf8(const char *loc)
{
    if (!loc) return 0;
    return strstr(loc, "UTF-8") ||
           strstr(loc, "utf8")  ||
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
    return cp == 65001;   /* only UTF-8 */
#else
    return cp != 65001;   /* all but UTF-8 */
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
NCURSES_EXPORT(void)
_nc_win32_encoding_init(void)
{
#if USE_WIDEC_SUPPORT
    UINT default_cp = CP_UTF8;
    const char *default_ctype = "C.UTF-8";
#else
    UINT default_cp = 1252;
    const char *default_ctype = "English_United States.1252";
#endif

    const char *env_cp    = getenv("NC_WINCP");
    const char *env_ctype = getenv("NC_WIN_CTYPE");

    UINT cp = default_cp;
    const char *ctype = default_ctype;
    UINT tmp;
    UINT cur_in;
    UINT cur_out;
    const char *cur_loc;

    if (env_cp && *env_cp) {
        tmp = (UINT)atoi(env_cp);
        if (valid_codepage(tmp) && codepage_compatible_with_ncurses(tmp))
            cp = tmp;
    }

    if (env_ctype && *env_ctype) {
        if (valid_locale(env_ctype) && locale_compatible_with_ncurses(env_ctype))
            ctype = env_ctype;
    }

    cur_in  = GetConsoleCP();
    cur_out = GetConsoleOutputCP();

    if (!valid_codepage(cur_in) ||
	!valid_codepage(cur_out) ||
	!codepage_compatible_with_ncurses(cur_in) ||
	!codepage_compatible_with_ncurses(cur_out)) {
        cur_in = cur_out = default_cp;
    }

    if (!env_cp && valid_codepage(cur_out) && codepage_compatible_with_ncurses(cur_out))
        cp = cur_out;

    cur_loc = setlocale(LC_CTYPE, NULL);
    if (!env_ctype && cur_loc && valid_locale(cur_loc) &&
			locale_compatible_with_ncurses(cur_loc))
        ctype = cur_loc;

    if (valid_codepage(cp) && codepage_compatible_with_ncurses(cp)) {
        SetConsoleCP(cp);
        SetConsoleOutputCP(cp);
    } else {
        SetConsoleCP(default_cp);
        SetConsoleOutputCP(default_cp);
    }

    if (!setlocale(LC_CTYPE, ctype)) {
        /* Fallback - try alternative UTF-8 locale names for Windows */
#if USE_WIDEC_SUPPORT
        if (!setlocale(LC_CTYPE, ".UTF8") && 
            !setlocale(LC_CTYPE, ".utf8") &&
            !setlocale(LC_CTYPE, "en_US.UTF-8") &&
            !setlocale(LC_CTYPE, "English_United States.65001")) {
            /* Final fallback */
            setlocale(LC_CTYPE, default_ctype);
        }
#else
        setlocale(LC_CTYPE, default_ctype);
#endif
    }

    _nc_setmode(_fileno(stdin),  true, false);
    _nc_setmode(_fileno(stdout), false, false);
    _nc_setmode(_fileno(stderr), false, false);
}

#endif /* defined(USE_WIN32_CONPTY)) */
