/****************************************************************************
 * Copyright 2018-2024,2025 Thomas E. Dickey                                *
 * Copyright 2008-2016,2017 Free Software Foundation, Inc.                  *
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

/*
 * TODO - improve screen-repainting performance, using implied wraparound to reduce write's
 * TODO - make it optional whether screen is restored or not when non-buffered
 */
#if JPF || 1
#include <curses.priv.h>
#include <windows.h>

#define CUR TerminalType(my_term).

MODULE_ID("$Id: win32_driver.c,v 1.20 2025/12/30 19:34:50 tom Exp $")

#define WINMAGIC NCDRV_MAGIC(NCDRV_WINCONSOLE)
#define EXP_OPTIMIZE 0

static bool console_initialized = false;

#define AssertTCB() assert(TCB != NULL && (TCB->magic == WINMAGIC))
#define validateConsoleHandle() (AssertTCB(), console_initialized || \
                                 (console_initialized = \
                                  _nc_console_checkinit(USE_NAMED_PIPES)))
#define SetSP() assert(TCB->csp != NULL); sp = TCB->csp; (void) sp
#define EnsureInit() (void)(console_initialized ? TRUE : _nc_console_checkinit(USE_NAMED_PIPES))

#define AdjustY() (WINCONSOLE.buffered \
                   ? 0 \
                   : (int) WINCONSOLE.SBI.srWindow.Top)

#define RevAttr(attr) (WORD) (((attr) & 0xff00) | \
		      ((((attr) & 0x07) << 4) | \
		       (((attr) & 0x70) >> 4)))

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)

NCURSES_EXPORT_VAR(LegacyConsoleInterface) _nc_CONSOLE;

static BOOL get_SBI(void);
static void get_console_size(int *Lines, int *Cols);
static int console_setmode(HANDLE hdl, const TTY * arg);
static int console_getmode(HANDLE hdl, TTY * arg);
static void selectActiveHandle(void);
static void set_scrollback(bool normal, CONSOLE_SCREEN_BUFFER_INFO * info);
static int console_keyok(int keycode, int flag);
static BOOL console_keyExist(int keycode);
static WORD console_MapColor(bool fore, int color);
static int console_twait(
		     const SCREEN *sp,
		     HANDLE hdl,
		     int mode,
		     int milliseconds,
		     int *timeleft
		     EVENTLIST_2nd(_nc_eventlist * evl));
static int console_testmouse(
			 const SCREEN *sp,
			 HANDLE hdl,
			 int delay
			 EVENTLIST_2nd(_nc_eventlist * evl));
static int console_read(
		    SCREEN *sp,
		    HANDLE hdl,
		    int *buf);

static WORD
MapAttr(WORD res, attr_t ch)
{
    if (ch & A_COLOR) {
	int p;

	p = PairNumber(ch);
	if (p > 0 && p < CON_NUMPAIRS) {
	    WORD a;
	    a = console_MapColor(TRUE, p);
	    res = (WORD) ((res & 0xff00) | a);
	}
    }

    if (ch & A_REVERSE) {
	res = RevAttr(res);
    }

    if (ch & A_STANDOUT) {
	res = RevAttr(res) | BACKGROUND_INTENSITY;
    }

    if (ch & A_BOLD)
	res |= FOREGROUND_INTENSITY;

    if (ch & A_DIM)
	res |= BACKGROUND_INTENSITY;

    return res;
}

// ----------------------------- Keyboard related definitions and functions ---------------------------------

#define GenMap(vKey,key) MAKELONG(key, vKey)
/* *INDENT-OFF* */
static const LONG keylist[] =
{
    GenMap(VK_PRIOR,  KEY_PPAGE),
    GenMap(VK_NEXT,   KEY_NPAGE),
    GenMap(VK_END,    KEY_END),
    GenMap(VK_HOME,   KEY_HOME),
    GenMap(VK_LEFT,   KEY_LEFT),
    GenMap(VK_UP,     KEY_UP),
    GenMap(VK_RIGHT,  KEY_RIGHT),
    GenMap(VK_DOWN,   KEY_DOWN),
    GenMap(VK_DELETE, KEY_DC),
    GenMap(VK_INSERT, KEY_IC)
};
static const LONG ansi_keys[] =
{
    GenMap(VK_PRIOR,  'I'),
    GenMap(VK_NEXT,   'Q'),
    GenMap(VK_END,    'O'),
    GenMap(VK_HOME,   'H'),
    GenMap(VK_LEFT,   'K'),
    GenMap(VK_UP,     'H'),
    GenMap(VK_RIGHT,  'M'),
    GenMap(VK_DOWN,   'P'),
    GenMap(VK_DELETE, 'S'),
    GenMap(VK_INSERT, 'R')
};
/* *INDENT-ON* */
#define array_length(a) (sizeof(a)/sizeof(a[0]))
#define N_INI ((int)array_length(keylist))
#define FKEYS 24
#define MAPSIZE (FKEYS + N_INI)

static int
rkeycompare(const void *el1, const void *el2)
{
    WORD key1 = (LOWORD((*((const LONG *) el1)))) & 0x7fff;
    WORD key2 = (LOWORD((*((const LONG *) el2)))) & 0x7fff;

    return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static int
keycompare(const void *el1, const void *el2)
{
    WORD key1 = HIWORD((*((const LONG *) el1)));
    WORD key2 = HIWORD((*((const LONG *) el2)));

    return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static int
MapKey(WORD vKey)
{
    int code = -1;

    if (!WINCONSOLE.isTermInfoConsole) {
	WORD nKey = 0;
	void *res;
	LONG key = GenMap(vKey, 0);

	res = bsearch(&key,
		      WINCONSOLE.map,
		      (size_t) (N_INI + FKEYS),
		      sizeof(keylist[0]),
		      keycompare);
	if (res) {
	    key = *((LONG *) res);
	    nKey = LOWORD(key);
	    code = (int) (nKey & 0x7fff);
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

    if (!WINCONSOLE.isTermInfoConsole) {
	WORD nKey = 0;
	void *res;
	LONG key = GenMap(vKey, 0);

	res = bsearch(&key,
		      WINCONSOLE.ansi_map,
		      (size_t) (N_INI + FKEYS),
		      sizeof(keylist[0]),
		      keycompare);
	if (res) {
	    key = *((LONG *) res);
	    nKey = LOWORD(key);
	    code = (int) (nKey & 0x7fff);
	    if (nKey & 0x8000)
		code = -code;
	}
    }
    return code;
}

static int
console_keyok(int keycode, int flag)
{
    int code = ERR;
    WORD nKey;
    WORD vKey;
    void *res;
    LONG key = GenMap(0, (WORD) keycode);

    T((T_CALLED("lib_win32con::_nc_console_keyok(%d, %d)"), keycode, flag));

    res = bsearch(&key,
		  WINCONSOLE.rmap,
		  (size_t) (N_INI + FKEYS),
		  sizeof(keylist[0]),
		  rkeycompare);
    if (res) {
	key = *((LONG *) res);
	vKey = HIWORD(key);
	nKey = (LOWORD(key)) & 0x7fff;
	if (!flag)
	    nKey |= 0x8000;
	*(LONG *) res = GenMap(vKey, nKey);
    }
    returnCode(code);
}

static BOOL
console_keyExist(int keycode)
{
    WORD nKey;
    void *res;
    bool found = FALSE;
    LONG key = GenMap(0, (WORD) keycode);

    T((T_CALLED("lib_win32con::_nc_console_keyExist(%d)"), keycode));
    res = bsearch(&key,
		  WINCONSOLE.rmap,
		  (size_t) (N_INI + FKEYS),
		  sizeof(keylist[0]),
		  rkeycompare);
    if (res) {
	key = *((LONG *) res);
	nKey = LOWORD(key);
	if (!(nKey & 0x8000))
	    found = TRUE;
    }
    returnCode(found);
}

static bool
wcon_kyExist(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int keycode)
{
    bool found = FALSE;

    T((T_CALLED("win32con::wcon_kyExist(%d)"), keycode));
    found = console_keyExist(keycode);
    returnBool(found);
}

static int
wcon_kpad(TERMINAL_CONTROL_BLOCK * TCB, int flag GCC_UNUSED)
{
    SCREEN *sp;
    int code = ERR;

    T((T_CALLED("win32con::wcon_kpad(%p, %d)"), TCB, flag));

    if (validateConsoleHandle()) {
	SetSP();

	if (sp) {
	    code = OK;
	}
    }
    returnCode(code);
}

static int
wcon_keyok(TERMINAL_CONTROL_BLOCK * TCB,
	   int keycode,
	   int flag)
{
    int code = ERR;
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_keyok(%p, %d, %d)"), TCB, keycode, flag));

    if (validateConsoleHandle()) {
	SetSP();
	if (sp) {
	    code = console_keyok(keycode, flag);
	}
    }
    returnCode(code);
}

// ------------------------------- Color related definitions and functions -----------------------------------

static WORD
console_MapColor(bool fore, int color)
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
    return (WORD) a;
}

static void
wcon_setcolor(TERMINAL_CONTROL_BLOCK * TCB,
	      int fore,
	      int color,
	      int (*outc) (SCREEN *, int) GCC_UNUSED)
{
    (void) TCB;
    if (validateConsoleHandle()) {
	WORD a = console_MapColor(fore, color);
	a |= (WORD) ((WINCONSOLE.SBI.wAttributes) & (fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(WINCONSOLE.hdl, a);
	get_SBI();
    }
}

static int
wcon_defaultcolors(TERMINAL_CONTROL_BLOCK * TCB,
		   int fg GCC_UNUSED,
		   int bg GCC_UNUSED)
{
    SCREEN *sp;
    int code = ERR;

    AssertTCB();
    SetSP();

    return (code);
}

static bool
wcon_rescolors(TERMINAL_CONTROL_BLOCK * TCB)
{
    int result = FALSE;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    return result;
}

static void
wcon_do_color(TERMINAL_CONTROL_BLOCK * TCB,
	      int old_pair GCC_UNUSED,
	      int pair GCC_UNUSED,
	      int reverse GCC_UNUSED,
	      int (*outc) (SCREEN *, int) GCC_UNUSED
)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_initpair(TERMINAL_CONTROL_BLOCK * TCB,
	      int pair,
	      int f,
	      int b)
{
    SCREEN *sp;

    if (validateConsoleHandle()) {
	SetSP();

	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < 8)
	    && (b >= 0) && (b < 8)) {
	    WINCONSOLE.pairs[pair] =
		console_MapColor(true, f) |
		console_MapColor(false, b);
	}
    }
}

static void
wcon_initcolor(TERMINAL_CONTROL_BLOCK * TCB,
	       int color GCC_UNUSED,
	       int r GCC_UNUSED,
	       int g GCC_UNUSED,
	       int b GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

// ------------------------------- HANDLE related definitions and functions ----------------------------------

/* Convert a file descriptor into a HANDLE
   That's not necessarily a console HANDLE
*/
NCURSES_EXPORT(HANDLE)
_nc_console_handle(int fd)
{
    intptr_t value = _get_osfhandle(fd);
    return (HANDLE) value;
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

    EnsureInit();

    if (!GetConsoleMode(hdl, &dwFlag)) {
	T(("GetConsoleMode failed"));
    } else {
	result = TRUE;
    }

    returnBool(result);
}

/*   This is used when running in terminfo mode to discover,
     whether or not the "terminal" is actually a Windows
     Console. It is the responsibility of the console to deal
     with the terminal escape sequences that are sent by
     terminfo.
 */
NCURSES_EXPORT(int)
_nc_console_test(int fd)
{
    int code = 0;
    HANDLE hdl = INVALID_HANDLE_VALUE;
    T((T_CALLED("lib_win32con::_nc_console_test(%d)"), fd));
    hdl = _nc_console_handle(fd);
    code = (int) IsConsoleHandle(hdl);
    returnCode(code);
}

#define OutHandle() ((WINCONSOLE.isTermInfoConsole || WINCONSOLE.progMode) ? WINCONSOLE.hdl : WINCONSOLE.core.ConsoleHandleOut)

static void
selectActiveHandle(void)
{
    if (WINCONSOLE.lastOut != WINCONSOLE.hdl) {
	WINCONSOLE.lastOut = WINCONSOLE.hdl;
	SetConsoleActiveScreenBuffer(WINCONSOLE.lastOut);
    }
}

NCURSES_EXPORT(HANDLE)
_nc_console_fd2handle(int fd)
{
    HANDLE hdl = _nc_console_handle(fd);
    if (hdl == WINCONSOLE.core.ConsoleHandleIn) {
	T(("lib_win32con:validateHandle %d -> WINCONSOLE.core.ConsoleHandleIn", fd));
    } else if (hdl == WINCONSOLE.hdl) {
	T(("lib_win32con:validateHandle %d -> WINCONSOLE.hdl", fd));
    } else if (hdl == WINCONSOLE.core.ConsoleHandleOut) {
	T(("lib_win32con:validateHandle %d -> WINCONSOLE.core.ConsoleHandleOut", fd));
    } else if (hdl == GetStdHandle(STD_INPUT_HANDLE)) {
	T(("lib_win32con:validateHandle %d -> STD_INPUT_HANDLE", fd));
	if (!WINCONSOLE.isTermInfoConsole && WINCONSOLE.progMode) {
	    hdl = WINCONSOLE.core.ConsoleHandleIn;
	}
    } else {
	T(("lib_win32con:validateHandle %d maps to unknown HANDLE", fd));
	hdl = INVALID_HANDLE_VALUE;
    }
    if (hdl != INVALID_HANDLE_VALUE) {
	if (hdl != WINCONSOLE.core.ConsoleHandleIn && (!WINCONSOLE.isTermInfoConsole && WINCONSOLE.progMode)) {
	    if (hdl == WINCONSOLE.core.ConsoleHandleOut && hdl != WINCONSOLE.hdl) {
		T(("lib_win32con:validateHandle forcing WINCONSOLE.core.ConsoleHandleOut -> WINCONSOLE.hdl"));
		hdl = WINCONSOLE.hdl;
	    }
	}
    }
    return hdl;
}

NCURSES_EXPORT(int)
_nc_console_flush(HANDLE hdl)
{
    int code = OK;

    T((T_CALLED("lib_win32con::_nc_console_flush(hdl=%p"), hdl));

    if (hdl != INVALID_HANDLE_VALUE) {
	if (hdl == WINCONSOLE.hdl ||
	    hdl == WINCONSOLE.core.ConsoleHandleIn ||
	    hdl == WINCONSOLE.core.ConsoleHandleOut) {
	    if (!FlushConsoleInputBuffer(WINCONSOLE.core.ConsoleHandleIn))
		code = ERR;
	} else {
	    code = ERR;
	    T(("_nc_console_flush not requesting a handle owned by console."));
	}
    }
    returnCode(code);
}

#if USE_TERM_DRIVER && (USE_NAMED_PIPES || defined(USE_WIN32CON_DRIVER))
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
#ifdef _NC_CHECK_MINTTY
    else {
	if (_nc_console_checkmintty(fd, NULL)) {
	    result = 2;
	    fprintf(stderr,
		    "ncurses on Windows must run in a Windows console.\n"
		    "On newer versions of Windows, the calling program should create a PTY-like.\n"
		    "device using the CreatePseudoConsole Windows API call.\n");
	    exit(EXIT_FAILURE);
	}
    }
#endif
    returnCode(result);
}
#endif /* USE_TERM_DRIVER && (USE_NAMED_PIPES || defined(USE_WIN32CON_DRIVER)) */

// -------------------------------- Mode related definitions and functions -----------------------------------

static int
console_setmode(HANDLE hdl, const TTY * arg)
{
    DWORD dwFlag = 0;
    int code = ERR;
    HANDLE alt;

    if (arg) {
#ifdef TRACE
	TTY TRCTTY;
#define TRCTTYOUT(flag) TRCTTY.dwFlagOut = flag
#define TRCTTYIN(flag)  TRCTTY.dwFlagIn = flag
#else
#define TRCTTYOUT(flag)
#define TRCTTYIN(flag)
#endif
	T(("lib_win32con:_nc_console_setmode %s", _nc_trace_ttymode(arg)));
	if (hdl == WINCONSOLE.core.ConsoleHandleIn) {
	    dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT | VT_FLAG_IN;
	    TRCTTYIN(dwFlag);
	    SetConsoleMode(hdl, dwFlag);

	    alt = OutHandle();
	    dwFlag = arg->dwFlagOut;
	    TRCTTYOUT(dwFlag);
	    SetConsoleMode(alt, dwFlag);
	} else {
	    dwFlag = arg->dwFlagOut;
	    TRCTTYOUT(dwFlag);
	    SetConsoleMode(hdl, dwFlag);

	    alt = WINCONSOLE.core.ConsoleHandleIn;
	    dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT;
	    TRCTTYIN(dwFlag);
	    SetConsoleMode(alt, dwFlag);
	    T(("effective mode set %s", _nc_trace_ttymode(&TRCTTY)));
	}
	code = OK;
    }
    return (code);
}

static int
console_getmode(HANDLE hdl, TTY * arg)
{
    int code = ERR;

    if (arg) {
	DWORD dwFlag = 0;
	HANDLE alt;

	if (hdl == WINCONSOLE.core.ConsoleHandleIn) {
	    if (GetConsoleMode(hdl, &dwFlag)) {
		arg->dwFlagIn = dwFlag;
		alt = OutHandle();
		if (GetConsoleMode(alt, &dwFlag)) {
		    arg->dwFlagOut = dwFlag;
		    code = OK;
		}
	    }
	} else {
	    if (GetConsoleMode(hdl, &dwFlag)) {
		arg->dwFlagOut = dwFlag;
		alt = WINCONSOLE.core.ConsoleHandleIn;
		if (GetConsoleMode(alt, &dwFlag)) {
		    arg->dwFlagIn = dwFlag;
		    code = OK;
		}
	    }
	}
    }
    T(("lib_win32con:_nc_console_getmode %s", _nc_trace_ttymode(arg)));
    return (code);
}

// ------------------------------ Meta-Data related definitions and functions ---------------------------------

static BOOL
get_SBI(void)
{
    bool rc = FALSE;
    if (GetConsoleScreenBufferInfo(WINCONSOLE.hdl, &(WINCONSOLE.SBI))) {
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
	if (WINCONSOLE.buffered) {
	    WINCONSOLE.origin.X = 0;
	    WINCONSOLE.origin.Y = 0;
	} else {
	    WINCONSOLE.origin.X = WINCONSOLE.SBI.srWindow.Left;
	    WINCONSOLE.origin.Y = WINCONSOLE.SBI.srWindow.Top;
	}
	rc = TRUE;
    } else {
	T(("GetConsoleScreenBufferInfo ERR"));
    }
    return rc;
}

static void
get_console_size(int *Lines, int *Cols)
{
    EnsureInit();
    if (Lines != NULL && Cols != NULL) {
	if (WINCONSOLE.buffered) {
	    *Lines = (int) (WINCONSOLE.SBI.dwSize.Y);
	    *Cols = (int) (WINCONSOLE.SBI.dwSize.X);
	} else {
	    *Lines = (int) (WINCONSOLE.SBI.srWindow.Bottom + 1 -
			    WINCONSOLE.SBI.srWindow.Top);
	    *Cols = (int) (WINCONSOLE.SBI.srWindow.Right + 1 -
			   WINCONSOLE.SBI.srWindow.Left);
	}
    }
}

static int
wcon_size(TERMINAL_CONTROL_BLOCK * TCB, int *Lines, int *Cols)
{
    int result = ERR;

    T((T_CALLED("win32con::wcon_size(%p)"), TCB));

    if (validateConsoleHandle() &&
	(Lines != NULL) && (Cols != NULL)) {
	get_console_size(Lines, Cols);
	result = OK;
    }
    returnCode(result);
}

static int
wcon_setsize(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED,
	     int l GCC_UNUSED,
	     int c GCC_UNUSED)
{
    AssertTCB();
    return ERR;
}


// ------------------------------ Screen Save/Restore definitions and functions ---------------------------------

static bool
read_screen_data(void)
{
    bool result = FALSE;
    COORD bufferCoord;
    size_t want;

    WINCONSOLE.save_size.X = (SHORT) (WINCONSOLE.save_region.Right
				      - WINCONSOLE.save_region.Left + 1);
    WINCONSOLE.save_size.Y = (SHORT) (WINCONSOLE.save_region.Bottom
				      - WINCONSOLE.save_region.Top + 1);

    want = (size_t) (WINCONSOLE.save_size.X * WINCONSOLE.save_size.Y);

    if ((WINCONSOLE.save_screen = malloc(want * sizeof(CHAR_INFO))) != NULL) {
	bufferCoord.X = (SHORT) (WINCONSOLE.window_only
				 ? WINCONSOLE.SBI.srWindow.Left
				 : 0);
	bufferCoord.Y = (SHORT) (WINCONSOLE.window_only
				 ? WINCONSOLE.SBI.srWindow.Top
				 : 0);

	T(("... reading console %s %dx%d into %d,%d - %d,%d at %d,%d",
	   WINCONSOLE.window_only ? "window" : "buffer",
	   WINCONSOLE.save_size.Y, WINCONSOLE.save_size.X,
	   WINCONSOLE.save_region.Top,
	   WINCONSOLE.save_region.Left,
	   WINCONSOLE.save_region.Bottom,
	   WINCONSOLE.save_region.Right,
	   bufferCoord.Y,
	   bufferCoord.X));

	if (read_screen(WINCONSOLE.hdl,
			WINCONSOLE.save_screen,
			WINCONSOLE.save_size,
			bufferCoord,
			&WINCONSOLE.save_region)) {
	    result = TRUE;
	} else {
	    T((" error %#lx", (unsigned long) GetLastError()));
	    FreeAndNull(WINCONSOLE.save_screen);
	}
    }

    return result;
}

/*
 * Attempt to save the screen contents.  PDCurses does this if
 * PDC_RESTORE_SCREEN is set, giving the same visual appearance on
 * restoration as if the library had allocated a console buffer.  MSDN
 * says that the data which can be read is limited to 64Kb (and may be
 * less).
 */
static bool
save_original_screen(void)
{
    bool result = FALSE;

    WINCONSOLE.save_region.Top = 0;
    WINCONSOLE.save_region.Left = 0;
    WINCONSOLE.save_region.Bottom = (SHORT) (WINCONSOLE.SBI.dwSize.Y - 1);
    WINCONSOLE.save_region.Right = (SHORT) (WINCONSOLE.SBI.dwSize.X - 1);

    if (read_screen_data()) {
	result = TRUE;
    } else {

	WINCONSOLE.save_region.Top = WINCONSOLE.SBI.srWindow.Top;
	WINCONSOLE.save_region.Left = WINCONSOLE.SBI.srWindow.Left;
	WINCONSOLE.save_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
	WINCONSOLE.save_region.Right = WINCONSOLE.SBI.srWindow.Right;

	WINCONSOLE.window_only = TRUE;

	if (read_screen_data()) {
	    result = TRUE;
	}
    }

    T(("... save original screen contents %s", result ? "ok" : "err"));
    return result;
}

static bool
restore_original_screencon(void)
{
    COORD bufferCoord;
    bool result = FALSE;
    SMALL_RECT save_region = WINCONSOLE.save_region;

    T(("... restoring %s",
       WINCONSOLE.window_only ? "window" : "entire buffer"));

    bufferCoord.X = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Left : 0);
    bufferCoord.Y = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Top : 0);

    if (write_screen(WINCONSOLE.hdl,
		     WINCONSOLE.save_screen,
		     WINCONSOLE.save_size,
		     bufferCoord,
		     &save_region)) {
	result = TRUE;
	SetConsoleCursorPosition(WINCONSOLE.hdl, WINCONSOLE.save_SBI.dwCursorPosition);
	T(("... restore original screen contents ok %dx%d (%d,%d - %d,%d)",
	   WINCONSOLE.save_size.Y,
	   WINCONSOLE.save_size.X,
	   save_region.Top,
	   save_region.Left,
	   save_region.Bottom,
	   save_region.Right));
    } else {
	T(("... restore original screen contents err"));
    }
    return result;
}

#if 0				/* def TRACE */
static void
dump_screen(const char *fn, int ln)
{
    int max_cells = (WINCONSOLE.SBI.dwSize.Y *
		     (1 + WINCONSOLE.SBI.dwSize.X)) + 1;
    char output[max_cells];
    CHAR_INFO save_screen[max_cells];
    COORD save_size;
    SMALL_RECT save_region;
    COORD bufferCoord;

    T(("dump_screen %s@%d", fn, ln));

    save_region.Top = WINCONSOLE.SBI.srWindow.Top;
    save_region.Left = WINCONSOLE.SBI.srWindow.Left;
    save_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
    save_region.Right = WINCONSOLE.SBI.srWindow.Right;

    save_size.X = (SHORT) (save_region.Right - save_region.Left + 1);
    save_size.Y = (SHORT) (save_region.Bottom - save_region.Top + 1);

    bufferCoord.X = bufferCoord.Y = 0;

    if (read_screen(WINCONSOLE.hdl,
		    save_screen,
		    save_size,
		    bufferCoord,
		    &save_region)) {
	int i, j;
	int ij = 0;
	int k = 0;

	for (i = save_region.Top; i <= save_region.Bottom; ++i) {
	    for (j = save_region.Left; j <= save_region.Right; ++j) {
		output[k++] = save_screen[ij++].CharInfoChar;
	    }
	    output[k++] = '\n';
	}
	output[k] = 0;

	T(("DUMP: %d,%d - %d,%d",
	   save_region.Top,
	   save_region.Left,
	   save_region.Bottom,
	   save_region.Right));
	T(("%s", output));
    }
}
#else
#define dump_screen(fn,ln)	/* nothing */
#endif

static bool
restore_original_screen(void)
{
    COORD bufferCoord;
    bool result = FALSE;
    SMALL_RECT save_region = WINCONSOLE.save_region;

    T(("... restoring %s", WINCONSOLE.window_only ?
       "window" : "entire buffer"));

    bufferCoord.X = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Left : 0);
    bufferCoord.Y = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Top : 0);

    if (write_screen(WINCONSOLE.hdl,
		     WINCONSOLE.save_screen,
		     WINCONSOLE.save_size,
		     bufferCoord,
		     &save_region)) {
	result = TRUE;
	mvcur(-1, -1, LINES - 2, 0);
	T(("... restore original screen contents ok %dx%d (%d,%d - %d,%d)",
	   WINCONSOLE.save_size.Y,
	   WINCONSOLE.save_size.X,
	   save_region.Top,
	   save_region.Left,
	   save_region.Bottom,
	   save_region.Right));
    } else {
	T(("... restore original screen contents err"));
    }
    return result;
}

#define MIN_WIDE 80
#define MIN_HIGH 24
/*
 * In "normal" mode, reset the buffer- and window-sizes back to their original values.
 */
static void
set_scrollback(bool normal, CONSOLE_SCREEN_BUFFER_INFO * info)
{
    SMALL_RECT rect;
    COORD coord;
    bool changed = FALSE;

    T((T_CALLED("lib_win32con::_nc_console_set_scrollback(%s)"),
       (normal
	? "normal"
	: "application")));

    T(("... SBI.srWindow %d,%d .. %d,%d",
       info->srWindow.Top,
       info->srWindow.Left,
       info->srWindow.Bottom,
       info->srWindow.Right));
    T(("... SBI.dwSize %dx%d",
       info->dwSize.Y,
       info->dwSize.X));

    if (normal) {
	rect = info->srWindow;
	coord = info->dwSize;
	if (memcmp(info, &WINCONSOLE.SBI, sizeof(*info)) != 0) {
	    changed = TRUE;
	    WINCONSOLE.SBI = *info;
	}
    } else {
	int high = info->srWindow.Bottom - info->srWindow.Top + 1;
	int wide = info->srWindow.Right - info->srWindow.Left + 1;

	if (high < MIN_HIGH) {
	    T(("... height %d < %d", high, MIN_HIGH));
	    high = MIN_HIGH;
	    changed = TRUE;
	}
	if (wide < MIN_WIDE) {
	    T(("... width %d < %d", wide, MIN_WIDE));
	    wide = MIN_WIDE;
	    changed = TRUE;
	}

	rect.Left =
	    rect.Top = 0;
	rect.Right = (SHORT) (wide - 1);
	rect.Bottom = (SHORT) (high - 1);

	coord.X = (SHORT) wide;
	coord.Y = (SHORT) high;

	if (info->dwSize.Y != high ||
	    info->dwSize.X != wide ||
	    info->srWindow.Top != 0 ||
	    info->srWindow.Left != 0) {
	    changed = TRUE;
	}

    }

    if (changed) {
	T(("... coord %d,%d", coord.Y, coord.X));
	T(("... rect %d,%d - %d,%d",
	   rect.Top, rect.Left,
	   rect.Bottom, rect.Right));
	SetConsoleScreenBufferSize(WINCONSOLE.hdl, coord);	/* dwSize */
	SetConsoleWindowInfo(WINCONSOLE.hdl, TRUE, &rect);	/* srWindow */
	get_SBI();
    }
    returnVoid;
}

NCURSES_EXPORT(bool)
_nc_console_restore(void)
{
    bool res = FALSE;

    T((T_CALLED("lib_win32con::_nc_console_restore")));
    if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE) {
	res = TRUE;
	if (!WINCONSOLE.buffered) {
	    set_scrollback(TRUE, &WINCONSOLE.save_SBI);
	    if (!restore_original_screen())
		res = FALSE;
	}
	SetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
    }
    returnBool(res);
}

// ------------------------------ Update related  definitions and functions ---------------------------------

#if USE_WIDEC_SUPPORT
/*
 * TODO: support surrogate pairs
 * TODO: support combining characters
 * TODO: support acsc
 * TODO: _nc_wacs should be part of sp.
 */
static BOOL
con_write16(TERMINAL_CONTROL_BLOCK * TCB,
	    int y, int x, cchar_t *str, int limit)
{
    int actual = 0;
    MakeArray(ci, CHAR_INFO, limit);
    COORD loc, siz;
    SMALL_RECT rec;
    int i;
    cchar_t ch;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    for (i = actual = 0; i < limit; i++) {
	ch = str[i];
	if (isWidecExt(ch))
	    continue;
	ci[actual].CharInfoChar = CharOf(ch);
	ci[actual].Attributes = MapAttr(WINCONSOLE.SBI.wAttributes,
					AttrOf(ch));
	if (AttrOf(ch) & A_ALTCHARSET) {
	    if (_nc_wacs) {
		int which = CharOf(ch);
		if (which > 0
		    && which < ACS_LEN
		    && CharOf(_nc_wacs[which]) != 0) {
		    ci[actual].CharInfoChar = CharOf(_nc_wacs[which]);
		} else {
		    ci[actual].CharInfoChar = ' ';
		}
	    }
	}
	++actual;
    }

    loc.X = (SHORT) 0;
    loc.Y = (SHORT) 0;
    siz.X = (SHORT) actual;
    siz.Y = 1;

    rec.Left = (SHORT) x;
    rec.Top = (SHORT) (y + AdjustY());
    rec.Right = (SHORT) (x + limit - 1);
    rec.Bottom = rec.Top;

    return write_screen(WINCONSOLE.hdl, ci, siz, loc, &rec);
}
#define con_write(tcb, y, x, str, n) con_write16(tcb, y, x, str, n)
#else
static BOOL
con_write8(TERMINAL_CONTROL_BLOCK * TCB, int y, int x, chtype *str, int n)
{
    MakeArray(ci, CHAR_INFO, n);
    COORD loc, siz;
    SMALL_RECT rec;
    int i;
    chtype ch;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    for (i = 0; i < n; i++) {
	ch = str[i];
	ci[i].CharInfoChar = ChCharOf(ch);
	ci[i].Attributes = MapAttr(WINCONSOLE.SBI.wAttributes,
				   ChAttrOf(ch));
	if (ChAttrOf(ch) & A_ALTCHARSET) {
	    if (sp->_acs_map)
		ci[i].CharInfoChar =
		    ChCharOf(NCURSES_SP_NAME(_nc_acs_char) (sp, ChCharOf(ch)));
	}
    }

    loc.X = (short) 0;
    loc.Y = (short) 0;
    siz.X = (short) n;
    siz.Y = 1;

    rec.Left = (short) x;
    rec.Top = (short) y;
    rec.Right = (short) (x + n - 1);
    rec.Bottom = rec.Top;

    return write_screen(WINCONSOLE.hdl, ci, siz, loc, &rec);
}
#define con_write(tcb, y, x, str, n) con_write8(tcb, y, x, str, n)
#endif

#if EXP_OPTIMIZE
/*
 * Comparing new/current screens, determine the last column-index for a change
 * beginning on the given row,col position.  Unlike a serial terminal, there is
 * no cost for "moving" the "cursor" on the line as we update it.
 */
static int
find_end_of_change(SCREEN *sp, int row, int col)
{
    int result = col;
    struct ldat *curdat = CurScreen(sp)->_line + row;
    struct ldat *newdat = NewScreen(sp)->_line + row;

    while (col <= newdat->lastchar) {
#if USE_WIDEC_SUPPORT
	if (isWidecExt(curdat->text[col]) ||
	    isWidecExt(newdat->text[col])) {
	    result = col;
	} else if (memcmp(&curdat->text[col],
			  &newdat->text[col],
			  sizeof(curdat->text[0]))) {
	    result = col;
	} else {
	    break;
	}
#else
	if (curdat->text[col] != newdat->text[col]) {
	    result = col;
	} else {
	    break;
	}
#endif
	++col;
    }
    return result;
}

/*
 * Given a row,col position at the end of a change-chunk, look for the
 * beginning of the next change-chunk.
 */
static int
find_next_change(SCREEN *sp, int row, int col)
{
    struct ldat *curdat = CurScreen(sp)->_line + row;
    struct ldat *newdat = NewScreen(sp)->_line + row;
    int result = newdat->lastchar + 1;

    while (++col <= newdat->lastchar) {
#if USE_WIDEC_SUPPORT
	if (isWidecExt(curdat->text[col]) !=
	    isWidecExt(newdat->text[col])) {
	    result = col;
	    break;
	} else if (memcmp(&curdat->text[col],
			  &newdat->text[col],
			  sizeof(curdat->text[0]))) {
	    result = col;
	    break;
	}
#else
	if (curdat->text[col] != newdat->text[col]) {
	    result = col;
	    break;
	}
#endif
    }
    return result;
}

#define EndChange(first) \
	find_end_of_change(sp, y, first)
#define NextChange(last)                        \
	find_next_change(sp, y, last)

#endif /* EXP_OPTIMIZE */


#define MARK_NOCHANGE(win,row)                 \
    win->_line[row].firstchar = _NOCHANGE;     \
    win->_line[row].lastchar  = _NOCHANGE

static int
wcon_doupdate(TERMINAL_CONTROL_BLOCK * TCB)
{
    int result = ERR;
    int y, nonempty, n, x0, x1, Width, Height;
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_doupdate(%p)"), TCB));
    if (validateConsoleHandle()) {
	SetSP();

	Width = screen_columns(sp);
	Height = screen_lines(sp);
	nonempty = Min(Height, NewScreen(sp)->_maxy + 1);

	T(("... %dx%d clear cur:%d new:%d",
	   Height, Width,
	   CurScreen(sp)->_clear,
	   NewScreen(sp)->_clear));

	if (SP_PARM->_endwin == ewSuspend) {

	    T(("coming back from shell mode"));
	    NCURSES_SP_NAME(reset_prog_mode) (NCURSES_SP_ARG);

	    NCURSES_SP_NAME(_nc_mvcur_resume) (NCURSES_SP_ARG);
	    NCURSES_SP_NAME(_nc_screen_resume) (NCURSES_SP_ARG);
	    SP_PARM->_mouse_resume(SP_PARM);

	    SP_PARM->_endwin = ewRunning;
	}

	if ((CurScreen(sp)->_clear || NewScreen(sp)->_clear)) {
	    int x;
#if USE_WIDEC_SUPPORT
	    MakeArray(empty, cchar_t, Width);
	    wchar_t blank[2] =
	    {
		L' ', L'\0'
	    };

	    for (x = 0; x < Width; x++)
		setcchar(&empty[x], blank, 0, 0, NULL);
#else
	    MakeArray(empty, chtype, Width);

	    for (x = 0; x < Width; x++)
		empty[x] = ' ';
#endif

	    for (y = 0; y < nonempty; y++) {
		con_write(TCB, y, 0, empty, Width);
		memcpy(empty,
		       CurScreen(sp)->_line[y].text,
		       (size_t) Width * sizeof(empty[0]));
	    }
	    CurScreen(sp)->_clear = FALSE;
	    NewScreen(sp)->_clear = FALSE;
	    touchwin(NewScreen(sp));
	    T(("... cleared %dx%d lines @%d of screen", nonempty, Width,
	       AdjustY()));
	}

	for (y = 0; y < nonempty; y++) {
	    x0 = NewScreen(sp)->_line[y].firstchar;
	    if (x0 != _NOCHANGE) {
#if EXP_OPTIMIZE
		int x2;
		int limit = NewScreen(sp)->_line[y].lastchar;
		while ((x1 = EndChange(x0)) <= limit) {
		    while ((x2 = NextChange(x1)) <=
			   limit && x2 <= (x1 + 2)) {
			x1 = x2;
		    }
		    n = x1 - x0 + 1;
		    memcpy(&CurScreen(sp)->_line[y].text[x0],
			   &NewScreen(sp)->_line[y].text[x0],
			   n * sizeof(CurScreen(sp)->_line[y].text[x0]));
		    con_write(TCB,
			      y,
			      x0,
			      &CurScreen(sp)->_line[y].text[x0], n);
		    x0 = NextChange(x1);
		}

		/* mark line changed successfully */
		if (y <= NewScreen(sp)->_maxy) {
		    MARK_NOCHANGE(NewScreen(sp), y);
		}
		if (y <= CurScreen(sp)->_maxy) {
		    MARK_NOCHANGE(CurScreen(sp), y);
		}
#else
		x1 = NewScreen(sp)->_line[y].lastchar;
		n = x1 - x0 + 1;
		if (n > 0) {
		    memcpy(&CurScreen(sp)->_line[y].text[x0],
			   &NewScreen(sp)->_line[y].text[x0],
			   (size_t) n *
			   sizeof(CurScreen(sp)->_line[y].text[x0]));
		    con_write(TCB,
			      y,
			      x0,
			      &CurScreen(sp)->_line[y].text[x0], n);

		    /* mark line changed successfully */
		    if (y <= NewScreen(sp)->_maxy) {
			MARK_NOCHANGE(NewScreen(sp), y);
		    }
		    if (y <= CurScreen(sp)->_maxy) {
			MARK_NOCHANGE(CurScreen(sp), y);
		    }
		}
#endif
	    }
	}

	/* put everything back in sync */
	for (y = nonempty; y <= NewScreen(sp)->_maxy; y++) {
	    MARK_NOCHANGE(NewScreen(sp), y);
	}
	for (y = nonempty; y <= CurScreen(sp)->_maxy; y++) {
	    MARK_NOCHANGE(CurScreen(sp), y);
	}

	if (!NewScreen(sp)->_leaveok) {
	    CurScreen(sp)->_curx = NewScreen(sp)->_curx;
	    CurScreen(sp)->_cury = NewScreen(sp)->_cury;

	    TCB->drv->td_hwcur(TCB,
			       0,
			       0,
			       CurScreen(sp)->_cury,
			       CurScreen(sp)->_curx);
	}
	selectActiveHandle();
	result = OK;
    }
    returnCode(result);
}




























static const char *
wcon_name(TERMINAL_CONTROL_BLOCK * TCB)
{
    (void) TCB;
    return "win32console";
}


static bool
wcon_CanHandle(TERMINAL_CONTROL_BLOCK * TCB,
	       const char *tname,
	       int *errret GCC_UNUSED)
{
    bool code = FALSE;

    T((T_CALLED("win32con::wcon_CanHandle(%p,%s,%p)"),
       (void *) TCB, NonNull(tname), (void *) errret));

    assert(TCB != NULL);

    TCB->magic = WINMAGIC;

    if (tname == NULL || *tname == 0) {
	code = TRUE;
    } else if (tname != NULL && *tname == '#') {
	/*
	 * Use "#" (a character which cannot begin a terminal's name) to
	 * select specific driver from the table.
	 *
	 * In principle, we could have more than one non-terminfo driver,
	 * e.g., "win32gui".
	 */
	size_t n = strlen(tname + 1);
	if (n != 0
	    && ((strncmp(tname + 1, "win32console", n) == 0)
		|| (strncmp(tname + 1, "win32con", n) == 0))) {
	    code = TRUE;
	}
    } else if (tname != NULL && stricmp(tname, "unknown") == 0) {
	code = TRUE;
    }
#if !USE_NAMED_PIPES
    else if (_isatty(TCB->term.Filedes)) {
	code = TRUE;
    }
#endif

    /*
     * This is intentional, to avoid unnecessary breakage of applications
     * using <term.h> symbols.
     */
    if (code && (TerminalType(&TCB->term).Booleans == 0)) {
	_nc_init_termtype(&TerminalType(&TCB->term));
#if NCURSES_EXT_NUMBERS
	_nc_export_termtype2(&TCB->term.type, &TerminalType(&TCB->term));
#endif
    }

    if (!code) {
	if (_nc_console_test(0)) {
	    T(("isTermInfoConsole=TRUE"));
	    WINCONSOLE.isTermInfoConsole = TRUE;
	}
    }
    returnBool(code);
}

static int
wcon_dobeepflash(TERMINAL_CONTROL_BLOCK * TCB,
		 int beepFlag)
{
    SCREEN *sp;
    int res = ERR;

    int high = (WINCONSOLE.SBI.srWindow.Bottom -
		WINCONSOLE.SBI.srWindow.Top + 1);
    int wide = (WINCONSOLE.SBI.srWindow.Right -
		WINCONSOLE.SBI.srWindow.Left + 1);
    int max_cells = (high * wide);
    int i;

    MakeArray(this_screen, CHAR_INFO, max_cells);
    MakeArray(that_screen, CHAR_INFO, max_cells);
    COORD this_size;
    SMALL_RECT this_region;
    COORD bufferCoord;

    if (validateConsoleHandle()) {
	SetSP();
	this_region.Top = WINCONSOLE.SBI.srWindow.Top;
	this_region.Left = WINCONSOLE.SBI.srWindow.Left;
	this_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
	this_region.Right = WINCONSOLE.SBI.srWindow.Right;

	this_size.X = (SHORT) wide;
	this_size.Y = (SHORT) high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
	    read_screen(WINCONSOLE.hdl,
			this_screen,
			this_size,
			bufferCoord,
			&this_region)) {

	    memcpy(that_screen,
		   this_screen,
		   sizeof(CHAR_INFO) * (size_t) max_cells);

	    for (i = 0; i < max_cells; i++) {
		that_screen[i].Attributes =
		    RevAttr(that_screen[i].Attributes);
	    }

	    write_screen(WINCONSOLE.hdl, that_screen, this_size,
			 bufferCoord, &this_region);
	    Sleep(200);
	    write_screen(WINCONSOLE.hdl, this_screen, this_size,
			 bufferCoord, &this_region);

	} else {
	    MessageBeep(MB_ICONWARNING);	/* MB_OK might be better */
	}
	res = OK;
    }
    return res;
}

static int
wcon_print(TERMINAL_CONTROL_BLOCK * TCB,
	   char *data GCC_UNUSED,
	   int len GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();

    return ERR;
}



static bool
wcon_rescol(TERMINAL_CONTROL_BLOCK * TCB)
{
    bool res = FALSE;

    (void) TCB;
    if (validateConsoleHandle()) {
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;
	SetConsoleTextAttribute(WINCONSOLE.hdl, a);
	get_SBI();
	res = TRUE;
    }
    return res;
}


static int
wcon_sgmode(TERMINAL_CONTROL_BLOCK * TCB, int setFlag, TTY * buf)
{
    int result = ERR;

    T((T_CALLED("win32con::wcon_sgmode(TCB=(%p),setFlag=%d,TTY=(%p)"),
       TCB, setFlag, buf));
    if (buf != NULL && validateConsoleHandle()) {

	if (setFlag) {
	    console_setmode(WINCONSOLE.hdl, buf);
	    TCB->term.Nttyb = *buf;
	} else {
	    console_getmode(WINCONSOLE.hdl, &(TCB->term.Nttyb));
	    *buf = TCB->term.Nttyb;
	}
	result = OK;
    }
    returnCode(result);
}

static int
wcon_mode(TERMINAL_CONTROL_BLOCK * TCB, int progFlag, int defFlag)
{
    SCREEN *sp;
    TERMINAL *_term = (TERMINAL *) TCB;
    int code = ERR;

    T((T_CALLED("win32con::wcon_mode(%p, progFlag=%d, defFlag=%d)"),
       TCB, progFlag, defFlag));

    if (validateConsoleHandle()) {
	sp = TCB->csp;

	WINCONSOLE.progMode = progFlag;
	WINCONSOLE.lastOut = progFlag ? WINCONSOLE.hdl : WINCONSOLE.core.ConsoleHandleOut;
	SetConsoleActiveScreenBuffer(WINCONSOLE.lastOut);

	if (progFlag) /* prog mode */  {
	    if (defFlag) {
		if ((wcon_sgmode(TCB, FALSE, &(_term->Nttyb)) == OK)) {
		    code = OK;
		}
	    } else {
		/* reset_prog_mode */
		if (wcon_sgmode(TCB, TRUE, &(_term->Nttyb)) == OK) {
		    if (sp) {
			if (sp->_keypad_on)
			    _nc_keypad(sp, TRUE);
		    }
		    if (!WINCONSOLE.buffered) {
			set_scrollback(FALSE, &WINCONSOLE.SBI);
		    }
		    code = OK;
		}
	    }
	    T(("... buffered:%d, clear:%d",
	       WINCONSOLE.buffered, CurScreen(sp)->_clear));
	} else {		/* shell mode */
	    if (defFlag) {
		/* def_shell_mode */
		if (wcon_sgmode(TCB, FALSE, &(_term->Ottyb)) == OK) {
		    code = OK;
		}
	    } else {
		/* reset_shell_mode */
		if (sp) {
		    _nc_keypad(sp, FALSE);
		    NCURSES_SP_NAME(_nc_flush) (sp);
		}
		code = wcon_sgmode(TCB, TRUE, &(_term->Ottyb));
		if (!WINCONSOLE.buffered) {
		    set_scrollback(TRUE, &WINCONSOLE.save_SBI);
		    if (!restore_original_screen())
			code = ERR;
		}
		SetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
	    }
	}

    }
    returnCode(code);
}

static void
wcon_screen_init(SCREEN *sp GCC_UNUSED)
{
}

static void
wcon_wrap(SCREEN *sp GCC_UNUSED)
{
}

static void
wcon_release(TERMINAL_CONTROL_BLOCK * TCB)
{
    T((T_CALLED("win32con::wcon_release(%p)"), TCB));

    AssertTCB();
    if (TCB->prop)
	free(TCB->prop);

    returnVoid;
}

NCURSES_EXPORT(bool)
_nc_console_checkinit(bool assumeTermInfo)
{
    bool res = FALSE;

    T((T_CALLED("lib_win32con::_nc_console_checkinit(assumeTermInfo=%d)"),
       assumeTermInfo));

    /* initialize once, or not at all */
    if (!console_initialized) {
	int i;
	DWORD num_buttons;
	WORD a;
	BOOL buffered = FALSE;
	BOOL b;

	START_TRACE();
	WINCONSOLE.isTermInfoConsole = assumeTermInfo;

	WINCONSOLE.map = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);
	WINCONSOLE.rmap = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);
	WINCONSOLE.ansi_map = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);

	for (i = 0; i < (N_INI + FKEYS); i++) {
	    if (i < N_INI) {
		WINCONSOLE.rmap[i] = WINCONSOLE.map[i] =
		    (DWORD) keylist[i];
		WINCONSOLE.ansi_map[i] = (DWORD) ansi_keys[i];
	    } else {
		WINCONSOLE.rmap[i] = WINCONSOLE.map[i] =
		    (DWORD) GenMap((VK_F1 + (i - N_INI)),
				   (KEY_F(1) + (i - N_INI)));
		WINCONSOLE.ansi_map[i] =
		    (DWORD) GenMap((VK_F1 + (i - N_INI)),
				   (';' + (i - N_INI)));
	    }
	}
	qsort(WINCONSOLE.ansi_map,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(WINCONSOLE.map,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(WINCONSOLE.rmap,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      rkeycompare);

	if (GetNumberOfConsoleMouseButtons(&num_buttons)) {
	    WINCONSOLE.numButtons = (int) num_buttons;
	} else {
	    WINCONSOLE.numButtons = 1;
	}

	a = console_MapColor(true, COLOR_WHITE) |
	    console_MapColor(false, COLOR_BLACK);
	for (i = 0; i < CON_NUMPAIRS; i++)
	    WINCONSOLE.pairs[i] = a;

#define SaveConsoleMode(handle, value) \
	GetConsoleMode(WINCONSOLE.core.handle, &WINCONSOLE.originalMode.value)

	if (WINCONSOLE.isTermInfoConsole) {
	    WINCONSOLE.core.ConsoleHandleIn = GetStdHandle(STD_INPUT_HANDLE);
	    WINCONSOLE.core.ConsoleHandleOut = GetStdHandle(STD_OUTPUT_HANDLE);
	    WINCONSOLE.hdl = WINCONSOLE.core.ConsoleHandleOut;

	    SaveConsoleMode(ConsoleHandleIn, dwFlagIn);
	    SaveConsoleMode(ConsoleHandleOut, dwFlagOut);

	} else {
	    b = AllocConsole();

	    if (!b)
		b = AttachConsole(ATTACH_PARENT_PROCESS);

	    WINCONSOLE.core.ConsoleHandleIn = GetDirectHandle("CONIN$", FILE_SHARE_READ);
	    WINCONSOLE.core.ConsoleHandleOut = GetDirectHandle("CONOUT$", FILE_SHARE_WRITE);

	    SaveConsoleMode(ConsoleHandleIn, dwFlagIn);
	    SaveConsoleMode(ConsoleHandleOut, dwFlagOut);

	    if (getenv("NCGDB") || getenv("NCURSES_CONSOLE2")) {
		WINCONSOLE.hdl = WINCONSOLE.core.ConsoleHandleOut;
		buffered = FALSE;
		T(("... will not buffer console"));
	    } else {
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
	_setmode(fileno(stdin), _O_BINARY);
	_setmode(fileno(stdout), _O_BINARY);

	if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE) {
	    WINCONSOLE.buffered = buffered;
	    get_SBI();
	    WINCONSOLE.save_SBI = WINCONSOLE.SBI;
	    if (!buffered) {
		save_original_screen();
		set_scrollback(FALSE, &WINCONSOLE.SBI);
	    }
	    GetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
	    T(("... initial cursor is %svisible, %d%%",
	       (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
	       (int) WINCONSOLE.save_CI.dwSize));
	}

	WINCONSOLE.core.initialized = TRUE;
	console_initialized = TRUE;
    }
    res = (WINCONSOLE.hdl != INVALID_HANDLE_VALUE);
    returnBool(res);
}

static void
wcon_init(TERMINAL_CONTROL_BLOCK * TCB)
{
    T((T_CALLED("win32con::wcon_init(%p)"), TCB));

    AssertTCB();

    if (!(console_initialized = _nc_console_checkinit(USE_NAMED_PIPES))) {
	returnVoid;
    }

    if (TCB) {
	TCB->info.initcolor = TRUE;
	TCB->info.canchange = FALSE;
	TCB->info.hascolor = TRUE;
	TCB->info.caninit = TRUE;

	TCB->info.maxpairs = CON_NUMPAIRS;
	TCB->info.maxcolors = 8;
	TCB->info.numlabels = 0;
	TCB->info.labelwidth = 0;
	TCB->info.labelheight = 0;
	TCB->info.nocolorvideo = 1;
	TCB->info.tabsize = 8;

	TCB->info.numbuttons = WINCONSOLE.numButtons;
	TCB->info.defaultPalette = _nc_cga_palette;

    }
    returnVoid;
}



static void
wcon_initmouse(TERMINAL_CONTROL_BLOCK * TCB)
{
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_initmouse(%p)"), TCB));

    if (validateConsoleHandle()) {
	SetSP();

	sp->_mouse_type = M_TERM_DRIVER;
    }
    returnVoid;
}

static int
wcon_testmouse(TERMINAL_CONTROL_BLOCK * TCB,
	       int delay
	       EVENTLIST_2nd(_nc_eventlist * evl))
{
    int rc = 0;
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_testmouse(%p)"), TCB));
    if (validateConsoleHandle()) {
	SetSP();

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail) {
	    rc = TW_MOUSE;
	} else {
	    rc = TCBOf(sp)->drv->td_twait(TCBOf(sp),
					  TWAIT_MASK,
					  delay,
					  (int *) 0
					  EVENTLIST_2nd(evl));
	}
    }

    returnCode(rc);
}

static int
wcon_mvcur(TERMINAL_CONTROL_BLOCK * TCB,
	   int yold GCC_UNUSED, int xold GCC_UNUSED,
	   int y, int x)
{
    int ret = ERR;

    (void) TCB;
    if (validateConsoleHandle()) {
	COORD loc;
	loc.X = (short) x;
	loc.Y = (short) (y + AdjustY());
	SetConsoleCursorPosition(WINCONSOLE.hdl, loc);
	ret = OK;
    }
    return ret;
}

static void
wcon_hwlabel(TERMINAL_CONTROL_BLOCK * TCB,
	     int labnum GCC_UNUSED,
	     char *text GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_hwlabelOnOff(TERMINAL_CONTROL_BLOCK * TCB,
		  int OnFlag GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static chtype
wcon_conattr(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED)
{
    chtype res = A_NORMAL;
    res |= (A_BOLD | A_DIM | A_REVERSE | A_STANDOUT | A_COLOR);
    return res;
}

static void
wcon_setfilter(TERMINAL_CONTROL_BLOCK * TCB)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_initacs(TERMINAL_CONTROL_BLOCK * TCB,
	     chtype *real_map GCC_UNUSED,
	     chtype *fake_map GCC_UNUSED)
{
#define DATA(a,b) { a, b }
    static struct {
	int acs_code;
	int use_code;
    } table[] = {
	DATA('a', 0xb1),	/* ACS_CKBOARD  */
	    DATA('f', 0xf8),	/* ACS_DEGREE   */
	    DATA('g', 0xf1),	/* ACS_PLMINUS  */
	    DATA('j', 0xd9),	/* ACS_LRCORNER */
	    DATA('l', 0xda),	/* ACS_ULCORNER */
	    DATA('k', 0xbf),	/* ACS_URCORNER */
	    DATA('m', 0xc0),	/* ACS_LLCORNER */
	    DATA('n', 0xc5),	/* ACS_PLUS     */
	    DATA('q', 0xc4),	/* ACS_HLINE    */
	    DATA('t', 0xc3),	/* ACS_LTEE     */
	    DATA('u', 0xb4),	/* ACS_RTEE     */
	    DATA('v', 0xc1),	/* ACS_BTEE     */
	    DATA('w', 0xc2),	/* ACS_TTEE     */
	    DATA('x', 0xb3),	/* ACS_VLINE    */
	    DATA('y', 0xf3),	/* ACS_LEQUAL   */
	    DATA('z', 0xf2),	/* ACS_GEQUAL   */
	    DATA('0', 0xdb),	/* ACS_BLOCK    */
	    DATA('{', 0xe3),	/* ACS_PI       */
	    DATA('}', 0x9c),	/* ACS_STERLING */
	    DATA(',', 0xae),	/* ACS_LARROW   */
	    DATA('+', 0xaf),	/* ACS_RARROW   */
	    DATA('~', 0xf9),	/* ACS_BULLET   */
    };
#undef DATA
    unsigned n;

    SCREEN *sp;
    if (validateConsoleHandle()) {
	SetSP();

	for (n = 0; n < SIZEOF(table); ++n) {
	    real_map[table[n].acs_code] =
		(chtype) table[n].use_code | A_ALTCHARSET;
	    if (sp != NULL)
		sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
    }
}

static int
wcon_twait(TERMINAL_CONTROL_BLOCK * TCB,
	   int mode,
	   int milliseconds,
	   int *timeleft
	   EVENTLIST_2nd(_nc_eventlist * evl))
{
    SCREEN *sp;
    int code = 0;

    if (validateConsoleHandle()) {
	SetSP();

	code = console_twait(sp,
				 WINCONSOLE.core.ConsoleHandleIn,
				 mode,
				 milliseconds,
				 timeleft EVENTLIST_2nd(evl));
    }
    return code;
}

static int
wcon_read(TERMINAL_CONTROL_BLOCK * TCB, int *buf)
{
    SCREEN *sp;
    int n = -1;

    T((T_CALLED("win32con::wcon_read(%p)"), TCB));

    assert(buf);
    if (validateConsoleHandle()) {
	SetSP();

	n = console_read(sp, WINCONSOLE.core.ConsoleHandleIn, buf);
    }
    returnCode(n);
}

static int
wcon_nap(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int ms)
{
    T((T_CALLED("win32con::wcon_nap(%p, %d)"), TCB, ms));
    Sleep((DWORD) ms);
    returnCode(OK);
}

static int
wcon_cursorSet(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int mode)
{
    int res = -1;

    T((T_CALLED("win32con:wcon_cursorSet(%d)"), mode));
    if (validateConsoleHandle()) {
	CONSOLE_CURSOR_INFO this_CI = WINCONSOLE.save_CI;
	switch (mode) {
	case 0:
	    this_CI.bVisible = FALSE;
	    break;
	case 1:
	    break;
	case 2:
	    this_CI.dwSize = 100;
	    break;
	}
	SetConsoleCursorInfo(WINCONSOLE.hdl, &this_CI);
    }
    returnCode(res);
}


#define AdjustY() (WINCONSOLE.buffered \
                   ? 0 \
                   : (int) WINCONSOLE.SBI.srWindow.Top)

#define BUTTON_MASK (FROM_LEFT_1ST_BUTTON_PRESSED | \
                     FROM_LEFT_2ND_BUTTON_PRESSED | \
                     FROM_LEFT_3RD_BUTTON_PRESSED | \
                     FROM_LEFT_4TH_BUTTON_PRESSED | \
                     RIGHTMOST_BUTTON_PRESSED)

static mmask_t
decode_mouse(const SCREEN *sp, int mask)
{
    mmask_t result = 0;

    (void) sp;
    assert(sp && console_initialized);

    if (mask & FROM_LEFT_1ST_BUTTON_PRESSED)
	result |= BUTTON1_PRESSED;
    if (mask & FROM_LEFT_2ND_BUTTON_PRESSED)
	result |= BUTTON2_PRESSED;
    if (mask & FROM_LEFT_3RD_BUTTON_PRESSED)
	result |= BUTTON3_PRESSED;
    if (mask & FROM_LEFT_4TH_BUTTON_PRESSED)
	result |= BUTTON4_PRESSED;

    if (mask & RIGHTMOST_BUTTON_PRESSED) {
	switch (WINCONSOLE.numButtons) {
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
    if (sp->_drv_mouse_new_buttons != sp->_drv_mouse_old_buttons) {
	memset(&work, 0, sizeof(work));

	if (sp->_drv_mouse_new_buttons) {
	    work.bstate |= decode_mouse(sp, sp->_drv_mouse_new_buttons);
	} else {
	    /* cf: BUTTON_PRESSED, BUTTON_RELEASED */
	    work.bstate |= (decode_mouse(sp, sp->_drv_mouse_old_buttons)
			    >> 1);
	    result = TRUE;
	}

	work.x = mer.dwMousePosition.X;
	work.y = mer.dwMousePosition.Y - AdjustY();

	sp->_drv_mouse_fifo[sp->_drv_mouse_tail] = work;
	sp->_drv_mouse_tail += 1;
    }
    return result;
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
    if (milliseconds != NC_INFINITY) {
	milliseconds -= diff;
	if (milliseconds < 0)
	    milliseconds = 0;
    }
    return milliseconds;
}


static int
console_twait(
		     const SCREEN *sp,
		     HANDLE hdl,
		     int mode,
		     int milliseconds,
		     int *timeleft
		     EVENTLIST_2nd(_nc_eventlist * evl))
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
    (void) evl;			/* TODO: implement wgetch-events */
#endif

#define IGNORE_CTRL_KEYS (SHIFT_PRESSED | \
                          LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED | \
                          LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)
#define CONSUME() read_keycode(hdl, &inp_rec, 1, &nRead)

    assert(sp);

    TR(TRACE_IEVENT, ("start twait: hdl=%p, %d milliseconds, mode: %d",
		      hdl, milliseconds, mode));

    if (milliseconds < 0)
	milliseconds = NC_INFINITY;

    memset(&inp_rec, 0, sizeof(inp_rec));

    while (true) {
	if (!isNoDelay) {
	    GetSystemTimeAsFileTime(&fstart);
	    rc = WaitForSingleObject(hdl, (DWORD) milliseconds);
	    GetSystemTimeAsFileTime(&fend);
	    diff = (int) tdiff(fstart, fend);
	    milliseconds = Adjust(milliseconds, diff);
	    if (milliseconds < 0)
		break;
	}

	if (isNoDelay || (rc == WAIT_OBJECT_0)) {
	    if (mode) {
		nRead = 0;
		b = GetNumberOfConsoleInputEvents(hdl, &nRead);
		if (!b) {
		    T(("twait:err GetNumberOfConsoleInputEvents"));
		}
		if (isNoDelay && b) {
		    T(("twait: Events Available: %lu", (unsigned long) nRead));
		    if (nRead == 0) {
			code = 0;
			goto end;
		    } else {
			DWORD n = 0;
			MakeArray(pInpRec, INPUT_RECORD, nRead);
			if (pInpRec != NULL) {
			    DWORD i;
			    BOOL f;
			    memset(pInpRec, 0, sizeof(INPUT_RECORD) * nRead);
			    f = PeekConsoleInput(hdl, pInpRec, nRead, &n);
			    if (f) {
				for (i = 0; i < n; i++) {
				    if (pInpRec[i].EventType == KEY_EVENT) {
					if (pInpRec[i].Event.KeyEvent.bKeyDown) {
					    DWORD ctrlMask =
					    (pInpRec[i].Event.KeyEvent.dwControlKeyState &
					     IGNORE_CTRL_KEYS);
					    if (!ctrlMask) {
						code = TW_INPUT;
						goto end;
					    }
					}
				    }
				}
			    } else {
				T(("twait:err PeekConsoleInput"));
			    }
			    code = 0;
			    goto end;
			} else {
			    T(("twait:err could not alloca input records"));
			}
		    }
		}
		if (b && nRead > 0) {
		    b = PeekConsoleInput(hdl, &inp_rec, 1, &nRead);
		    if (!b) {
			T(("twait:err PeekConsoleInput"));
		    }
		    if (b && nRead > 0) {
			switch (inp_rec.EventType) {
			case KEY_EVENT:
			    if (mode & TW_INPUT) {
				WORD vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
				WORD ch = inp_rec.Event.KeyEventChar;

				T(("twait:event KEY_EVENT"));
				T(("twait vk=%d, ch=%d, keydown=%d",
				   vk, ch, inp_rec.Event.KeyEvent.bKeyDown));

				if (inp_rec.Event.KeyEvent.bKeyDown) {
				    T(("twait:event KeyDown"));
				    if (!WINCONSOLE.isTermInfoConsole &&
					(0 == ch)) {
					int nKey = MapKey(vk);
					if (nKey < 0) {
					    CONSUME();
					    continue;
					}
				    }
				    code = TW_INPUT;
				    goto end;
				} else {
				    CONSUME();
				}
			    }
			    continue;
			case MOUSE_EVENT:
			    T(("twait:event MOUSE_EVENT"));
			    if (decode_mouse(sp,
					     (inp_rec.Event.MouseEvent.dwButtonState
					      & BUTTON_MASK)) == 0) {
				CONSUME();
			    } else if (mode & TW_MOUSE) {
				code = TW_MOUSE;
				goto end;
			    }
			    continue;
			    /* e.g., FOCUS_EVENT */
			default:
			    T(("twait:event Type %d", inp_rec.EventType));
			    CONSUME();
			    selectActiveHandle();
			    continue;
			}
		    }
		}
	    }
	    continue;
	} else {
	    if (rc != WAIT_TIMEOUT) {
		code = -1;
		break;
	    } else {
		code = 0;
		break;
	    }
	}
    }
  end:

    TR(TRACE_IEVENT, ("end twait: returned %d (%lu), remaining time %d msec",
		      code, (unsigned long) GetLastError(), milliseconds));

    if (timeleft)
	*timeleft = milliseconds;

    return code;
}

static int
console_testmouse(
			 const SCREEN *sp,
			 HANDLE hdl,
			 int delay
			 EVENTLIST_2nd(_nc_eventlist * evl))
{
    int rc = 0;

    assert(sp);

    if (sp->_drv_mouse_head < sp->_drv_mouse_tail) {
	rc = TW_MOUSE;
    } else {
	rc = console_twait(sp,
			       hdl,
			       TWAIT_MASK,
			       delay,
			       (int *) 0
			       EVENTLIST_2nd(evl));
    }
    return rc;
}


static int
console_read(
		    SCREEN *sp,
		    HANDLE hdl,
		    int *buf)
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

    while ((b = read_keycode(hdl, &inp_rec, 1, &nRead))) {
	if (b && nRead > 0) {
	    if (rc < 0)
		rc = 0;
	    rc = rc + (int) nRead;
	    if (inp_rec.EventType == KEY_EVENT) {
		if (!inp_rec.Event.KeyEvent.bKeyDown)
		    continue;
		*buf = (int) inp_rec.Event.KeyEventChar;
		vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
		/*
		 * There are 24 virtual function-keys, and typically
		 * 12 function-keys on a keyboard.  Use the shift-modifier
		 * to provide the remaining 12 keys.
		 */
		if (vk >= VK_F1 && vk <= VK_F12) {
		    if (inp_rec.Event.KeyEvent.dwControlKeyState &
			SHIFT_PRESSED) {
			vk = (WORD) (vk + 12);
		    }
		}
		if (*buf == 0) {
		    int key = MapKey(vk);
		    if (key < 0)
			continue;
		    if (sp->_keypad_on) {
			*buf = key;
		    } else {
			ungetch('\0');
			*buf = AnsiKey(vk);
		    }
		} else if (vk == VK_BACK) {
		    if (!(inp_rec.Event.KeyEvent.dwControlKeyState
			  & (SHIFT_PRESSED | CONTROL_PRESSED))) {
			*buf = KEY_BACKSPACE;
		    }
		} else if (vk == VK_TAB) {
		    if ((inp_rec.Event.KeyEvent.dwControlKeyState
			 & (SHIFT_PRESSED | CONTROL_PRESSED))) {
			*buf = KEY_BTAB;
		    }
		}
		break;
	    } else if (inp_rec.EventType == MOUSE_EVENT) {
		if (handle_mouse(sp,
				 inp_rec.Event.MouseEvent)) {
		    *buf = KEY_MOUSE;
		    break;
		}
	    }
	    continue;
	}
    }
    returnCode(rc);
}

NCURSES_EXPORT_VAR (TERM_DRIVER) _nc_WIN_DRIVER = {
   	FALSE,
	wcon_name,		/* Name          */
	wcon_CanHandle,		/* CanHandle     */
	wcon_init,		/* init          */
	wcon_release,		/* release       */
	wcon_size,		/* size          */
	wcon_sgmode,		/* sgmode        */
	wcon_conattr,		/* conattr       */
	wcon_mvcur,		/* hwcur         */
	wcon_mode,		/* mode          */
	wcon_rescol,		/* rescol        */
	wcon_rescolors,		/* rescolors     */
	wcon_setcolor,		/* color         */
	wcon_dobeepflash,	/* DoBeepFlash   */
	wcon_initpair,		/* initpair      */
	wcon_initcolor,		/* initcolor     */
	wcon_do_color,		/* docolor       */
	wcon_initmouse,		/* initmouse     */
	wcon_testmouse,		/* testmouse     */
	wcon_setfilter,		/* setfilter     */
	wcon_hwlabel,		/* hwlabel       */
	wcon_hwlabelOnOff,	/* hwlabelOnOff  */
	wcon_doupdate,		/* update        */
	wcon_defaultcolors,	/* defaultcolors */
	wcon_print,		/* print         */
	wcon_size,		/* getsize       */
	wcon_setsize,		/* setsize       */
	wcon_initacs,		/* initacs       */
	wcon_screen_init,	/* scinit        */
	wcon_wrap,		/* scexit        */
	wcon_twait,		/* twait         */
	wcon_read,		/* read          */
	wcon_nap,		/* nap           */
	wcon_kpad,		/* kpad          */
	wcon_keyok,		/* kyOk          */
	wcon_kyExist,		/* kyExist       */
	wcon_cursorSet		/* cursorSet     */
};
#endif