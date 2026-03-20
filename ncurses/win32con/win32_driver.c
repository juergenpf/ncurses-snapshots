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
#if USE_WIN32CON_DRIVER || 1

#include <curses.priv.h>
#include <windows.h>

#define CUR TerminalType(my_term).

MODULE_ID("$Id: win32_driver.c,v 1.20 2025/12/30 19:34:50 tom Exp $")

#define WINMAGIC NCDRV_MAGIC(NCDRV_WINCONSOLE)
#define EXP_OPTIMIZE 0

#define  console_initialized (TRUE==LEGACYCONSOLE.core.initialized)

#define AssertTCB() assert(TCB != NULL && (TCB->magic == WINMAGIC))
#define ValidateConsole() (1)
#define SetSP() assert(TCB->csp != NULL); sp = TCB->csp; (void) sp
#define EnsureInit() /* noop */

#define AdjustY() (LEGACYCONSOLE.buffered \
                   ? 0 \
                   : (int) LEGACYCONSOLE.SBI.srWindow.Top)

#define RevAttr(attr) (WORD) (((attr) & 0xff00) | \
		      ((((attr) & 0x07) << 4) | \
		       (((attr) & 0x70) >> 4)))

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)

#define DispatchMethod(name) legacy_##name
#define Dispatch(name) .name = DispatchMethod(name)
#define NoDispatch(name) .name = NULL
#define METHOD(name,type) static type DispatchMethod(name)

METHOD(init, BOOL) (int fdOut, int fdIn);
METHOD(size, void) (int *Lines, int *Cols);
METHOD(size_changed, BOOL) (void);
METHOD(getmode, int) (int fd GCC_UNUSED, TTY * arg);
METHOD(setmode, int) (int fd GCC_UNUSED, const TTY * arg);
METHOD(defmode, int) (TTY * arg, short kind);
METHOD(flush,int)(int fd);

static BOOL get_SBI(void);
static void set_scrollback(bool normal, CONSOLE_SCREEN_BUFFER_INFO * info);
static int console_keyok(int keycode, int flag);
static BOOL console_keyExist(int keycode);
static WORD console_MapColor(bool fore, int color);
static BOOL console_restore(void);
static int console_test(int fd);

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

static LegacyConsoleInterface legacyCONSOLE =
{
    .core =
    {
	.initialized = FALSE,
	.is_conpty = FALSE,
	.ttyflags = {0, 0, TTY_MODE_UNSPECIFIED},
	.ConsoleHandleIn = INVALID_HANDLE_VALUE,
	.ConsoleHandleOut = INVALID_HANDLE_VALUE,
	.sbi_lines = -1,
	.sbi_cols = -1,
	Dispatch(init),
	Dispatch(size),
	Dispatch(size_changed),
	Dispatch(setmode),
	Dispatch(getmode),
	Dispatch(defmode),
	Dispatch(flush)
    },
    .buffered = FALSE,
    .window_only = FALSE,
    .progMode = FALSE,
    .numButtons = 0,
    .ansi_map = NULL,
    .map = NULL,
    .rmap = NULL,
    .pairs = {0},
    .origin = {0, 0},
    .save_screen = NULL,
    .save_size = {0, 0},
    .save_region = {0, 0, 0, 0},
    .SBI = {0},
    .save_SBI = {0},
    .save_CI = {0},
    .originalMode = {0}
};

NCURSES_EXPORT_VAR(LegacyConsoleInterface *) 
_nc_LEGACYCONSOLE = &legacyCONSOLE;

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

    WORD nKey = 0;
    void *res;
    LONG key = GenMap(vKey, 0);

    res = bsearch(&key,
		  LEGACYCONSOLE.map,
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
    return code;
}

static int
AnsiKey(WORD vKey)
{
	int code = -1;

	WORD nKey = 0;
	void *res;
	LONG key = GenMap(vKey, 0);

	res = bsearch(&key,
				  LEGACYCONSOLE.ansi_map,
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

    T((T_CALLED("win32_driver::console_keyok(%d, %d)"), keycode, flag));

    res = bsearch(&key,
		  LEGACYCONSOLE.rmap,
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

    T((T_CALLED("win32_driver::console_keyExist(%d)"), keycode));
    res = bsearch(&key,
		  LEGACYCONSOLE.rmap,
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

    T((T_CALLED("win32_driver::wcon_kyExist(%d)"), keycode));
    found = console_keyExist(keycode);
    returnBool(found);
}

static int
wcon_kpad(TERMINAL_CONTROL_BLOCK * TCB, int flag GCC_UNUSED)
{
    SCREEN *sp;
    int code = ERR;

    T((T_CALLED("win32_driver::wcon_kpad(%p, %d)"), TCB, flag));

    if (ValidateConsole()) {
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

    T((T_CALLED("win32_driver::wcon_keyok(%p, %d, %d)"), TCB, keycode, flag));

    if (ValidateConsole()) {
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
    if (ValidateConsole()) {
	WORD a = console_MapColor(fore, color);
	a |= (WORD) ((LEGACYCONSOLE.SBI.wAttributes) & (fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
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

    if (ValidateConsole()) {
	SetSP();

	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < 8)
	    && (b >= 0) && (b < 8)) {
	    LEGACYCONSOLE.pairs[pair] =
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

/* Validate that a HANDLE is actually a
   console HANDLE
*/
static BOOL
IsConsoleHandle(HANDLE hdl)
{
    DWORD dwFlag = 0;
    BOOL result = FALSE;

    T((T_CALLED("win32_driver::IsConsoleHandle(HANDLE=%p"), hdl));

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
static int
console_test(int fd)
{
    int code = 0;
    HANDLE hdl = INVALID_HANDLE_VALUE;
    T((T_CALLED("win32_driver::console_test(%d)"), fd));
    hdl = (HANDLE)((intptr_t)_get_osfhandle(fd));
    code = (int) IsConsoleHandle(hdl);
    returnCode(code);
}

/* This function flushes the console input buffer. It is called by the main thread when it
 * wants to discard any pending input in the console. The function returns OK on success. */
METHOD(flush, int) (int fd GCC_UNUSED)
{
    int code = OK;
    T((T_CALLED("win32_driver::legacy_flush(fd=%d)"), fd));
    FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
    returnCode(code);
}


// -------------------------------- Mode related definitions and functions -----------------------------------

/* This function sets the console mode for the input and output handles. It is called by the main thread
 * when it wants to change the console mode. The function takes a TTY structure that contains the desired
 * mode flags, and it returns OK on success or ERR on failure.
 * It is also responsible for detecting switches between shell mode and program mode, and starting or
 * stopping the input subsystem accordingly. */
METHOD(setmode, int) (int fd GCC_UNUSED, const TTY * arg)
{
    HANDLE input_target = LEGACYCONSOLE.core.ConsoleHandleIn;
    HANDLE output_target = LEGACYCONSOLE.core.ConsoleHandleOut;
    BOOL input_ok = FALSE;
    BOOL output_ok = FALSE;

    T((T_CALLED("win32_driver::legacy_setmode(fd=%d, TTY*=%p)"), fd, arg));

    if (!arg)
	returnCode(ERR);

    if (input_target != INVALID_HANDLE_VALUE) {
	DWORD mode = arg->dwFlagIn;
	if (arg->kind == TTY_MODE_SHELL) {
	    /* In shell mode, we want to disable VT input and enable the basic line input, processed
	     * input and echo input modes, to provide a more traditional console input experience.
	     * This allows the user to interact with the console in a way that is consistent with
	     * what they would expect from a typical command prompt or terminal window, with
	     * features like line editing and input processing enabled. */
	    mode |= (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT);
	} else {
	    /* In program mode, we want to enable VT input. */
	    mode |= ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT;
	}

	/* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
	if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT)) {
	    mode &= ~ENABLE_ECHO_INPUT;
	}

	input_ok = SetConsoleMode(input_target, mode);
	if (input_ok) {
	    /* Make sure the cached value reflects the real value we set, as the
	     * caller may not have provided all necessary flags (e.g.
	     * ENABLE_PROCESSED_INPUT when VT is requested) */
	    DWORD realMode;
	    if (GetConsoleMode(input_target, &realMode)) {
		LEGACYCONSOLE.core.ttyflags.dwFlagIn = realMode;
	    } else {
		LEGACYCONSOLE.core.ttyflags.dwFlagIn = mode;
	    }
	} else {
	    T(("Invalid input file descriptor"));
	}
    }

    if (output_target != INVALID_HANDLE_VALUE) {
	DWORD mode = arg->dwFlagOut;
	output_ok = SetConsoleMode(output_target, mode);
	if (output_ok) {
	    /* Make sure the cached value reflects the real value we set,
	     * as the caller may not have provided all necessary flags
	     * (e.g. VT output is required for the Windows Console backend) */
	    DWORD realMode;
	    if (GetConsoleMode(output_target, &realMode)) {
		LEGACYCONSOLE.core.ttyflags.dwFlagOut = realMode;
	    } else {
		LEGACYCONSOLE.core.ttyflags.dwFlagOut = mode;
	    }
	} else {
	    T(("Invalid output file descriptor"));
	}
    }

    if (arg->kind == TTY_MODE_SHELL) {
	T(("Shell mode set"));
	LEGACYCONSOLE.progMode = FALSE;
    } else if (arg->kind == TTY_MODE_PROGRAM) {
	T(("Program mode set"));
	LEGACYCONSOLE.progMode = TRUE;
    }

    // Handle errors
    if (!input_ok || !output_ok) {
	returnCode(ERR);
    }

    returnCode(OK);
}


/* getmode always sets the kind field to TTY_MODE_UNSPECIFIED. The trick is, that
 * def_shell_mode, def_prog_mode and savetty will call above method defmode to
 * set the field right after getting it.
 * So only calls to reset_shell_mode, reset_prog_mode and resetty will have the kind
 * field in the TTY structure set to a specific mode, which means that the setmode
 * function will know that it should apply the necessary changes to the input subsystem
 * when restoring that TTY. All other calls to setmode will have the kind field in the
 * TTY structure set to TTY_MODE_UNSPECIFIED, which means that the setmode function
 * will know that it should not change the status of the input subsystem when restoring
 * that TTY. */
METHOD(getmode, int) (int fd GCC_UNUSED, TTY * arg)
{
    T((T_CALLED("win32_driver::legacy_getmode(fd=%d, TTY*=%p)"), fd, arg));

    if (NULL == arg)
	returnCode(ERR);

    *arg = LEGACYCONSOLE.core.ttyflags;
    arg->kind = TTY_MODE_UNSPECIFIED;
    returnCode(OK);
}

/* The defmode function is only called from def_shell_mode, def_prog_mode, and savetty.
 * It's only purpose is to set the kind field in the TTY structure and to set the
 * REQUIRED console mode flags for shell mode and program mode.
 * The design idea is this: the three mentioned calls are the only ones used to get the
 * TTY structure in order to store it and later on use it to restore the console to the
 * desired state. TTY changing calls like raw() or cbreak() don't do that. The implementation
 * of the getmode function will always set the kind field to TTY_MODE_UNSPECIFIED, which means
 * that if that TTY is later used in a setmode call, the setmode function will know that it
 * should not change the status of the input subsystem. Only the def_shell_mode, def_prog_mode,
 * and savetty functions will set the kind field to a specific mode, which means that the setmode
 * function will know that it should apply the necessary changes to the input subsystem when
 * restoring that TTY. */
METHOD(defmode, int) (TTY * arg, short kind)
{
    short realMode = kind;

    T((T_CALLED("win32_driver::legacy_defmode(TTY*=%p, kind=%d)"), arg, kind));

    if (NULL == arg)
	returnCode(ERR);

    if (realMode == TTY_MODE_AUTO) {
	realMode = (LEGACYCONSOLE.progMode==TRUE) ? TTY_MODE_PROGRAM : TTY_MODE_SHELL;
    }

    arg->kind = realMode;
    returnCode(OK);
}

// ------------------------------ Meta-Data related definitions and functions ---------------------------------

static BOOL
get_SBI(void)
{
    bool rc = FALSE;
    if (GetConsoleScreenBufferInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &(LEGACYCONSOLE.SBI))) {
	T(("GetConsoleScreenBufferInfo"));
	T(("... buffer(X:%d Y:%d)",
	   LEGACYCONSOLE.SBI.dwSize.X,
	   LEGACYCONSOLE.SBI.dwSize.Y));
	T(("... window(X:%d Y:%d)",
	   LEGACYCONSOLE.SBI.dwMaximumWindowSize.X,
	   LEGACYCONSOLE.SBI.dwMaximumWindowSize.Y));
	T(("... cursor(X:%d Y:%d)",
	   LEGACYCONSOLE.SBI.dwCursorPosition.X,
	   LEGACYCONSOLE.SBI.dwCursorPosition.Y));
	T(("... display(Top:%d Bottom:%d Left:%d Right:%d)",
	   LEGACYCONSOLE.SBI.srWindow.Top,
	   LEGACYCONSOLE.SBI.srWindow.Bottom,
	   LEGACYCONSOLE.SBI.srWindow.Left,
	   LEGACYCONSOLE.SBI.srWindow.Right));
	if (LEGACYCONSOLE.buffered) {
	    LEGACYCONSOLE.origin.X = 0;
	    LEGACYCONSOLE.origin.Y = 0;
	} else {
	    LEGACYCONSOLE.origin.X = LEGACYCONSOLE.SBI.srWindow.Left;
	    LEGACYCONSOLE.origin.Y = LEGACYCONSOLE.SBI.srWindow.Top;
	}
	rc = TRUE;
    } else {
	T(("GetConsoleScreenBufferInfo ERR"));
    }
    return rc;
}

METHOD(size, void) (int *Lines, int *Cols)
{
    EnsureInit();
    if (Lines != NULL && Cols != NULL) {
	if (LEGACYCONSOLE.buffered) {
	    *Lines = (int) (LEGACYCONSOLE.SBI.dwSize.Y);
	    *Cols = (int) (LEGACYCONSOLE.SBI.dwSize.X);
	} else {
	    *Lines = (int) (LEGACYCONSOLE.SBI.srWindow.Bottom + 1 -
			    LEGACYCONSOLE.SBI.srWindow.Top);
	    *Cols = (int) (LEGACYCONSOLE.SBI.srWindow.Right + 1 -
			   LEGACYCONSOLE.SBI.srWindow.Left);
	}
    }
}

/* Check if the Windows Console has been resized. Returns TRUE if a resize was detected.
 * We implement a simple throttling to ensure that we don't call GetConsoleScreenBufferInfo
 * too often, which could become expensive in a pseudo-console context because it involves
 * a round trip to the ConPTY backend. The throttling is implemented by keeping	 track of the
 * last time we checked for a resize, and if the function is called again within a certain
 * time frame, we simply return FALSE without checking. This allows us to avoid unnecessary
 * calls to GetConsoleScreenBufferInfo while still detecting resizes in a timely manner when
 * they occur. */
METHOD(size_changed, BOOL) (void)
{
    static ULONGLONG lastCheck = 0;
    int current_lines, current_cols;
    ULONGLONG now = GetTickCount64();
    bool resized = FALSE;

    T((T_CALLED("win32_driver::legacy_size_changed()")));

    if (now - lastCheck < RESIZE_CHECK_THROTTLING_MS)
	returnBool(FALSE);

    DispatchMethod(size) (&current_lines, &current_cols);

    if (CORECONSOLE.sbi_lines == -1 || CORECONSOLE.sbi_cols == -1) {
	CORECONSOLE.sbi_lines = current_lines;
	CORECONSOLE.sbi_cols  = current_cols;
    } else {
	if (current_lines != CORECONSOLE.sbi_lines || current_cols != CORECONSOLE.sbi_cols) {
	    CORECONSOLE.sbi_lines = current_lines;
	    CORECONSOLE.sbi_cols  = current_cols;

	    _nc_globals.have_sigwinch = 1;

	    resized = TRUE;
	}
    }
    lastCheck = GetTickCount64();
    returnBool(resized);
}

static int
wcon_size(TERMINAL_CONTROL_BLOCK * TCB, int *Lines, int *Cols)
{
    int result = ERR;

    T((T_CALLED("win32_driver::wcon_size(%p)"), TCB));

    if (ValidateConsole() &&
	(Lines != NULL) && (Cols != NULL)) {
	DispatchMethod(size)(Lines, Cols);
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

    LEGACYCONSOLE.save_size.X = (SHORT) (LEGACYCONSOLE.save_region.Right
				      - LEGACYCONSOLE.save_region.Left + 1);
    LEGACYCONSOLE.save_size.Y = (SHORT) (LEGACYCONSOLE.save_region.Bottom
				      - LEGACYCONSOLE.save_region.Top + 1);

    want = (size_t) (LEGACYCONSOLE.save_size.X * LEGACYCONSOLE.save_size.Y);

    if ((LEGACYCONSOLE.save_screen = malloc(want * sizeof(CHAR_INFO))) != NULL) {
	bufferCoord.X = (SHORT) (LEGACYCONSOLE.window_only
				 ? LEGACYCONSOLE.SBI.srWindow.Left
				 : 0);
	bufferCoord.Y = (SHORT) (LEGACYCONSOLE.window_only
				 ? LEGACYCONSOLE.SBI.srWindow.Top
				 : 0);

	T(("... reading console %s %dx%d into %d,%d - %d,%d at %d,%d",
	   LEGACYCONSOLE.window_only ? "window" : "buffer",
	   LEGACYCONSOLE.save_size.Y, LEGACYCONSOLE.save_size.X,
	   LEGACYCONSOLE.save_region.Top,
	   LEGACYCONSOLE.save_region.Left,
	   LEGACYCONSOLE.save_region.Bottom,
	   LEGACYCONSOLE.save_region.Right,
	   bufferCoord.Y,
	   bufferCoord.X));

	if (read_screen(LEGACYCONSOLE.core.ConsoleHandleOut,
			LEGACYCONSOLE.save_screen,
			LEGACYCONSOLE.save_size,
			bufferCoord,
			&LEGACYCONSOLE.save_region)) {
	    result = TRUE;
	} else {
	    T((" error %#lx", (unsigned long) GetLastError()));
	    FreeAndNull(LEGACYCONSOLE.save_screen);
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

    LEGACYCONSOLE.save_region.Top = 0;
    LEGACYCONSOLE.save_region.Left = 0;
    LEGACYCONSOLE.save_region.Bottom = (SHORT) (LEGACYCONSOLE.SBI.dwSize.Y - 1);
    LEGACYCONSOLE.save_region.Right = (SHORT) (LEGACYCONSOLE.SBI.dwSize.X - 1);

    if (read_screen_data()) {
	result = TRUE;
    } else {

	LEGACYCONSOLE.save_region.Top = LEGACYCONSOLE.SBI.srWindow.Top;
	LEGACYCONSOLE.save_region.Left = LEGACYCONSOLE.SBI.srWindow.Left;
	LEGACYCONSOLE.save_region.Bottom = LEGACYCONSOLE.SBI.srWindow.Bottom;
	LEGACYCONSOLE.save_region.Right = LEGACYCONSOLE.SBI.srWindow.Right;

	LEGACYCONSOLE.window_only = TRUE;

	if (read_screen_data()) {
	    result = TRUE;
	}
    }

    T(("... save original screen contents %s", result ? "ok" : "err"));
    return result;
}

static bool
restore_original_screen(void)
{
    COORD bufferCoord;
    bool result = FALSE;
    SMALL_RECT save_region = LEGACYCONSOLE.save_region;

    T(("... restoring %s",
       LEGACYCONSOLE.window_only ? "window" : "entire buffer"));

    bufferCoord.X = (SHORT) (LEGACYCONSOLE.window_only ?
			     LEGACYCONSOLE.SBI.srWindow.Left : 0);
    bufferCoord.Y = (SHORT) (LEGACYCONSOLE.window_only ?
			     LEGACYCONSOLE.SBI.srWindow.Top : 0);

    if (write_screen(LEGACYCONSOLE.core.ConsoleHandleOut,
		     LEGACYCONSOLE.save_screen,
		     LEGACYCONSOLE.save_size,
		     bufferCoord,
		     &save_region)) {
	result = TRUE;
	SetConsoleCursorPosition(LEGACYCONSOLE.core.ConsoleHandleOut, LEGACYCONSOLE.save_SBI.dwCursorPosition);
	T(("... restore original screen contents ok %dx%d (%d,%d - %d,%d)",
	   LEGACYCONSOLE.save_size.Y,
	   LEGACYCONSOLE.save_size.X,
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
    int max_cells = (LEGACYCONSOLE.SBI.dwSize.Y *
		     (1 + LEGACYCONSOLE.SBI.dwSize.X)) + 1;
    char output[max_cells];
    CHAR_INFO save_screen[max_cells];
    COORD save_size;
    SMALL_RECT save_region;
    COORD bufferCoord;

    T(("dump_screen %s@%d", fn, ln));

    save_region.Top = LEGACYCONSOLE.SBI.srWindow.Top;
    save_region.Left = LEGACYCONSOLE.SBI.srWindow.Left;
    save_region.Bottom = LEGACYCONSOLE.SBI.srWindow.Bottom;
    save_region.Right = LEGACYCONSOLE.SBI.srWindow.Right;

    save_size.X = (SHORT) (save_region.Right - save_region.Left + 1);
    save_size.Y = (SHORT) (save_region.Bottom - save_region.Top + 1);

    bufferCoord.X = bufferCoord.Y = 0;

    if (read_screen(LEGACYCONSOLE.hdl,
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

    T((T_CALLED("win32_driver::set_scrollback(%s)"),
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
	if (memcmp(info, &LEGACYCONSOLE.SBI, sizeof(*info)) != 0) {
	    changed = TRUE;
	    LEGACYCONSOLE.SBI = *info;
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
	SetConsoleScreenBufferSize(LEGACYCONSOLE.core.ConsoleHandleOut, coord);	/* dwSize */
	SetConsoleWindowInfo(LEGACYCONSOLE.core.ConsoleHandleOut, TRUE, &rect);	/* srWindow */
	get_SBI();
    }
    returnVoid;
}

static BOOL
console_restore(void)
{
    BOOL res = FALSE;

    T((T_CALLED("win32_driver::console_restore")));
    if (LEGACYCONSOLE.core.ConsoleHandleOut != INVALID_HANDLE_VALUE) {
	res = TRUE;
	if (!LEGACYCONSOLE.buffered) {
	    set_scrollback(TRUE, &LEGACYCONSOLE.save_SBI);
	    if (!restore_original_screen())
		res = FALSE;
	}
	SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
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
	ci[actual].Attributes = MapAttr(LEGACYCONSOLE.SBI.wAttributes,
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

    return write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, ci, siz, loc, &rec);
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
	ci[i].Attributes = MapAttr(LEGACYCONSOLE.SBI.wAttributes,
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

    return write_screen(LEGACYCONSOLE.hdl, ci, siz, loc, &rec);
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

    T((T_CALLED("win32_driver::wcon_doupdate(%p)"), TCB));
    if (ValidateConsole()) {
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
	// selectActiveHandle();
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

    T((T_CALLED("win32_driver::wcon_CanHandle(%p,%s,%p)"),
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
    returnBool(code);
}

static int
wcon_dobeepflash(TERMINAL_CONTROL_BLOCK * TCB,
		 int beepFlag)
{
    SCREEN *sp;
    int res = ERR;

    int high = (LEGACYCONSOLE.SBI.srWindow.Bottom -
		LEGACYCONSOLE.SBI.srWindow.Top + 1);
    int wide = (LEGACYCONSOLE.SBI.srWindow.Right -
		LEGACYCONSOLE.SBI.srWindow.Left + 1);
    int max_cells = (high * wide);
    int i;

    MakeArray(this_screen, CHAR_INFO, max_cells);
    MakeArray(that_screen, CHAR_INFO, max_cells);
    COORD this_size;
    SMALL_RECT this_region;
    COORD bufferCoord;

    if (ValidateConsole()) {
	SetSP();
	this_region.Top = LEGACYCONSOLE.SBI.srWindow.Top;
	this_region.Left = LEGACYCONSOLE.SBI.srWindow.Left;
	this_region.Bottom = LEGACYCONSOLE.SBI.srWindow.Bottom;
	this_region.Right = LEGACYCONSOLE.SBI.srWindow.Right;

	this_size.X = (SHORT) wide;
	this_size.Y = (SHORT) high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
	    read_screen(LEGACYCONSOLE.core.ConsoleHandleOut,
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

	    write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, that_screen, this_size,
			 bufferCoord, &this_region);
	    Sleep(200);
	    write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, this_screen, this_size,
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
    if (ValidateConsole()) {
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
	res = TRUE;
    }
    return res;
}


static int
wcon_sgmode(TERMINAL_CONTROL_BLOCK * TCB, int setFlag, TTY * buf)
{
    int result = ERR;

    T((T_CALLED("win32_driver::wcon_sgmode(TCB=(%p),setFlag=%d,TTY=(%p)"),
       TCB, setFlag, buf));
    if (buf != NULL && ValidateConsole()) {

	if (setFlag) {
	    DispatchMethod(setmode)(fileno(stdout), buf);
	} else {
	    DispatchMethod(getmode)(fileno(stdout), buf);
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

    T((T_CALLED("win32_driver::wcon_mode(%p, progFlag=%d, defFlag=%d)"),
       TCB, progFlag, defFlag));

    if (ValidateConsole()) {
	sp = TCB->csp;

	LEGACYCONSOLE.progMode = progFlag;
	SetConsoleActiveScreenBuffer(LEGACYCONSOLE.core.ConsoleHandleOut);

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
		    if (!LEGACYCONSOLE.buffered) {
			set_scrollback(FALSE, &LEGACYCONSOLE.SBI);
		    }
		    code = OK;
		}
	    }
	    T(("... buffered:%d, clear:%d",
	       LEGACYCONSOLE.buffered, CurScreen(sp)->_clear));
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
		if (!LEGACYCONSOLE.buffered) {
		    set_scrollback(TRUE, &LEGACYCONSOLE.save_SBI);
		    if (!restore_original_screen())
			code = ERR;
		}
		SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
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
    T((T_CALLED("win32_driver::wcon_release(%p)"), TCB));

    AssertTCB();
    if (TCB->prop)
	free(TCB->prop);

    returnVoid;
}

/* This initializaton function can be called multiple time, and actually it is called from within
 * setupterm() the first time and potentially if we enter ncurses from newterm() the next time.
 * The main purpose is to initialize the defaultCONPTY structure when called the first time. The
 * first call will alway have fdIn set to -1, as setupterm() only cares about output. Please note
 * that setupterm() already handles redirection of stdout and assigns stderr for output if stdout
 * is not a tty.
 *
 * The other purpose of this routine is to manage the assignment of pseudo-console handles. If the
 * assigned filedescriptors are NOT valid pseudo-console handles, the call will return FALSE.
 *
 * The function will also return FALSE, if the Windows version we run on does not support ConPTY,
 * which is a requirement for the Windows Console backend of ncurses. This is because without
 * ConPTY, the Windows Console does not provide the necessary capabilities for ncurses and
 * especially the terminfo layer to function properly. */
METHOD(init, BOOL) (int fdOut, int fdIn)
{
    BOOL result = FALSE;

    T((T_CALLED("win32_driver::legacy_init(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

    /* initialize once, or not at all */
    if (!LEGACYCONSOLE.core.initialized) {
	/*
	 * We set the console mode flags to the most basic ones that are required for ConPTY
	 * to function properly. */
	DWORD dwFlagIn = (ENABLE_LINE_INPUT
			  | ENABLE_PROCESSED_INPUT
			  | ENABLE_ECHO_INPUT
			  | ENABLE_EXTENDED_FLAGS);

	DWORD dwFlagOut = (ENABLE_PROCESSED_OUTPUT
			   | DISABLE_NEWLINE_AUTO_RETURN
			   | ENABLE_WRAP_AT_EOL_OUTPUT);

	DWORD dwFlag;
	HANDLE stdin_handle = INVALID_HANDLE_VALUE;
	HANDLE stdout_handle = INVALID_HANDLE_VALUE;
	BOOL hasConsole = FALSE;
	BOOL buffered = FALSE;
	WORD a;
	int i;
	DWORD num_buttons;
	DWORD last_error;

	if (_nc_conpty_supported()) {
	    T(("Windows version does support ConPTY, please don't use the legacy API!"));
	    returnBool(FALSE);
	}

	if (fdIn != -1) {
	    T(("In the first call fdIn is expected to be -1."));
	    returnBool(FALSE);
	}

	hasConsole = AllocConsole();
	if (!hasConsole) {
	    last_error = GetLastError();
	    T(("AllocConsole() failed with error %#lx", (unsigned long) last_error));
	    hasConsole = AttachConsole(ATTACH_PARENT_PROCESS);
	    if (!hasConsole) {
		last_error = GetLastError();
	    	T(("AllocConsole() and AttachConsole() failed with error %#lx", (unsigned long) last_error));
		T(("We continue and try to use any inheritedt console handles."));
	    }
	}
	
	stdin_handle  = GetDirectHandle("CONIN$", FILE_SHARE_READ);
	stdout_handle = GetDirectHandle("CONOUT$", FILE_SHARE_WRITE);

	if (getenv("NCGDB") || getenv("NCURSES_CONSOLE2")) {
	    buffered = FALSE;
	    T(("... will not buffer console"));
	} else {
	    T(("... creating console buffer"));
	    stdout_handle =
		CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
					FILE_SHARE_READ | FILE_SHARE_WRITE,
					NULL,
					CONSOLE_TEXTMODE_BUFFER,
					NULL);
	    buffered = TRUE;
	}

	/* Especially with UCRT and wide mode, make sure we use an UTF-8 capable locale.
	 * At least we set the codepage to a proper value that's either compatible with
	 * ASCII or UTF-8, to ensure that the console can display characters properly.
	 * The actual locale setting is not that important, as long as the code page is set
	 * correctly, because we handle UTF-8 encoding and decoding ourselves and we don't
	 * rely on the C runtime for that. */
	// encoding_init();

	if (stdout_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdout_handle,
								 &dwFlag) == 0) {
	    T(("Output handle is not a console"));
	    returnBool(FALSE);
	}
	LEGACYCONSOLE.core.ConsoleHandleOut = stdout_handle;

	if (stdin_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdin_handle,
								&dwFlag) == 0) 
	{
	    T(("StdIn handle is not a console"));
	    returnBool(FALSE);
	}
	LEGACYCONSOLE.core.ConsoleHandleIn = stdin_handle;

	SetConsoleMode(stdout_handle, dwFlagOut);
	/* We immediately read the console mode back to reflect any changes the
	 * runtime may have added, so the saved value reflects the actual mode
	 * of the console. */
	if (GetConsoleMode(stdout_handle, &dwFlagOut) == 0) {
	    T(("GetConsoleMode() failed for stdout"));
	    returnBool(FALSE);
	}
	LEGACYCONSOLE.core.ttyflags.dwFlagOut = dwFlagOut;

	SetConsoleMode(stdin_handle, dwFlagIn);
	/* We immediately read the console mode back to reflect any changes the
	 * runtime may have added, so the saved value reflects the actual mode
	 * of the console. */
	if (GetConsoleMode(stdin_handle, &dwFlagIn) == 0) {
	    T(("GetConsoleMode() failed for stdin"));
	    returnBool(FALSE);
	}
	LEGACYCONSOLE.core.ttyflags.dwFlagIn = dwFlagIn;

	LEGACYCONSOLE.map = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.rmap = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.ansi_map = (LPDWORD) malloc(sizeof(DWORD) * MAPSIZE);

	for (i = 0; i < (N_INI + FKEYS); i++) {
	    if (i < N_INI) {
		LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
		    (DWORD) keylist[i];
		LEGACYCONSOLE.ansi_map[i] = (DWORD) ansi_keys[i];
	    } else {
		LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
		    (DWORD) GenMap((VK_F1 + (i - N_INI)),
				   (KEY_F(1) + (i - N_INI)));
		LEGACYCONSOLE.ansi_map[i] =
		    (DWORD) GenMap((VK_F1 + (i - N_INI)),
				   (';' + (i - N_INI)));
	    }
	}
	qsort(LEGACYCONSOLE.ansi_map,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(LEGACYCONSOLE.map,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(LEGACYCONSOLE.rmap,
	      (size_t) (MAPSIZE),
	      sizeof(keylist[0]),
	      rkeycompare);

	if (GetNumberOfConsoleMouseButtons(&num_buttons))
	    LEGACYCONSOLE.numButtons = (int) num_buttons;
	else 
	    LEGACYCONSOLE.numButtons = 1;

	a = console_MapColor(true, COLOR_WHITE) |
	    console_MapColor(false, COLOR_BLACK);
	for (i = 0; i < CON_NUMPAIRS; i++)
	    LEGACYCONSOLE.pairs[i] = a;

	LEGACYCONSOLE.buffered = buffered;
	get_SBI();
	LEGACYCONSOLE.save_SBI = LEGACYCONSOLE.SBI;
	if (!buffered) {
	    save_original_screen();
	    set_scrollback(FALSE, &LEGACYCONSOLE.SBI);
	}
	GetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
	T(("... initial cursor is %svisible, %d%%",
	    (LEGACYCONSOLE.save_CI.bVisible ? "" : "not-"),
	    (int) LEGACYCONSOLE.save_CI.dwSize));

	LEGACYCONSOLE.core.initialized = TRUE;
	result = TRUE;
    } else {
	/* This branch is called from newterm() when fdIn is provided, so we need to validate
	* that the provided fdIn and fdOut are valid pseudo-console handles, and if so we
	* update the defaultCONPTY structure to use the new handles. */
	DWORD dwFlagOut;
	DWORD dwFlagIn;

	if (GetConsoleMode(LEGACYCONSOLE.core.ConsoleHandleOut, &dwFlagOut) == 0) {
	    T(("Output handle is not a console"));
	    returnBool(FALSE);
	}
	if (GetConsoleMode(LEGACYCONSOLE.core.ConsoleHandleIn, &dwFlagIn) == 0) {
	    T(("Input handle is not a console"));
	    returnBool(FALSE);
	}
	LEGACYCONSOLE.core.ttyflags.dwFlagOut = dwFlagOut;
	LEGACYCONSOLE.core.ttyflags.dwFlagIn = dwFlagIn;
	    
	result = TRUE;
    }
    returnBool(result);
}

static void
wcon_init(TERMINAL_CONTROL_BLOCK * TCB)
{
    T((T_CALLED("win32_driver::wcon_init(%p)"), TCB));

    AssertTCB();
    if (!console_initialized) {
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

	TCB->info.numbuttons = LEGACYCONSOLE.numButtons;
	TCB->info.defaultPalette = _nc_cga_palette;

    }
    returnVoid;
}


static void
wcon_initmouse(TERMINAL_CONTROL_BLOCK * TCB)
{
    SCREEN *sp;

    T((T_CALLED("win32_driver::wcon_initmouse(%p)"), TCB));

    if (ValidateConsole()) {
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

    T((T_CALLED("win32_driver::wcon_testmouse(%p)"), TCB));
    if (ValidateConsole()) {
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
    if (ValidateConsole()) {
	COORD loc;
	loc.X = (short) x;
	loc.Y = (short) (y + AdjustY());
	SetConsoleCursorPosition(LEGACYCONSOLE.core.ConsoleHandleOut, loc);
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
    if (ValidateConsole()) {
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

    if (ValidateConsole()) {
	SetSP();

	code = console_twait(sp,
				 LEGACYCONSOLE.core.ConsoleHandleIn,
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

    T((T_CALLED("win32_driver::wcon_read(%p)"), TCB));

    assert(buf);
    if (ValidateConsole()) {
	SetSP();

	n = console_read(sp, LEGACYCONSOLE.core.ConsoleHandleIn, buf);
    }
    returnCode(n);
}

static int
wcon_nap(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int ms)
{
    T((T_CALLED("win32_driver::wcon_nap(%p, %d)"), TCB, ms));
    Sleep((DWORD) ms);
    returnCode(OK);
}

static int
wcon_cursorSet(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int mode)
{
    int res = -1;

    T((T_CALLED("win32_driver::wcon_cursorSet(%d)"), mode));
    if (ValidateConsole()) {
	CONSOLE_CURSOR_INFO this_CI = LEGACYCONSOLE.save_CI;
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
	SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &this_CI);
    }
    returnCode(res);
}


#define AdjustY() (LEGACYCONSOLE.buffered \
                   ? 0 \
                   : (int) LEGACYCONSOLE.SBI.srWindow.Top)

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
	switch (LEGACYCONSOLE.numButtons) {
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
				    if ((0 == ch)) {
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
			    // selectActiveHandle();
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

    T((T_CALLED("win32_driver::console_read(%p)"), sp));

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