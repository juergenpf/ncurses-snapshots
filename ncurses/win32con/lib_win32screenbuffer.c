/****************************************************************************
 * Copyright 2026 Juergen Pfeifer                                           *
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
 *     and: Thomas E. Dickey                                                *                                     *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id$")

#if USE_SCREENBUFFERED_CONSOLE
#include <stdint.h>
#include <sys/time.h>

// #define NO_MOUSE_SUPPORT

#define DispatchMethod(name) screenbuffer_##name
#define Dispatch(name) .name = DispatchMethod(name)
#define NoDispatch(name) .name = NULL
#define METHOD(name, type) static type DispatchMethod(name)
#define T_METHOD(name,fmt) "called {lib_win32screenbuffer::screenbuffer_" #name fmt

// Core methods, if not centrally implemented.
METHOD(termname, char*) (void);
METHOD(init, bool)(int fdOut, int fdIn);
METHOD(size, void)(int *Lines, int *Cols);
METHOD(size_changed, bool)(void);
METHOD(getmode, int)(int fd GCC_UNUSED, TTY *arg);
METHOD(setmode, int)(int fd GCC_UNUSED, const TTY *arg);
METHOD(defmode, int)(TTY *arg, short kind);

METHOD(adjust_size, bool)(void);
METHOD(termattrs, chtype)(void);
METHOD(keypad, int)(bool flag);
METHOD(beeporflash, int)(bool beep);
METHOD(keyok, int)(int keycode, int flag);
METHOD(has_key, int)(int keycode);
METHOD(init_acs, void)(chtype *acs);
METHOD(reset_color_pair, bool)(void);
METHOD(reset_colors,bool)(void);
METHOD(default_colors, int)(int fg, int bg);
METHOD(init_pair, int)(int pair, int fg, int bg);
METHOD(setcolor, void)(bool fg, int color);
METHOD(initcolor, void)(int c, int r, int g, int b);
METHOD(do_color, void)(int oldpair,int pair,int reverse, NCURSES_SP_OUTC outc);
METHOD(curs_set, int)(int visibility);
METHOD(hwlabel, void)(int num, const char* label);
METHOD(hwlabelonoff, void)(bool on);
METHOD(print,int)(char* data, int len);
METHOD(read, int)(int *buf);
METHOD(twait,int)(int mode,int milliseconds,int *timeleft EVENTLIST_2nd(_nc_eventlist *evl));
METHOD(mvcur, int)(int yold, int xold, int y, int x);
#if USE_WIDEC_SUPPORT
METHOD(writeat, bool)(int y, int x, const cchar_t *str, int limit);
#else
METHOD(writeat, bool)(int y, int x, const chtype *str, int limit);
#endif
METHOD(screen_init, void)(void);
METHOD(screen_exit, void)(void);
METHOD(setfilter, void)(void);

#if USE_WIDEC_SUPPORT
#define write_screen WriteConsoleOutputW
#define read_screen  ReadConsoleOutputW
#define read_keycode ReadConsoleInputW
#define KeyEventChar KeyEvent.uChar.UnicodeChar
#define CharInfoChar Char.UnicodeChar
#define KEYMASK 0xffff
#else
#define write_screen WriteConsoleOutput
#define read_screen  ReadConsoleOutput
#define read_keycode ReadConsoleInput
#define KeyEventChar KeyEvent.uChar.AsciiChar
#define CharInfoChar Char.AsciiChar
#define KEYMASK 0xff
#endif /* USE_WIDEC_SUPPORT */

#define AssertScreenBufferedConsole() assert((!(legacyCONSOLE.core.status & CONSOLE_STATUS_IS_CONPTY)))

static ScreenBufferedConsoleInterface legacyCONSOLE =
	{
		.core =
			{
				Dispatch(termname),
				Dispatch(init),
				Dispatch(size),
				Dispatch(size_changed),
				Dispatch(setmode),
				Dispatch(getmode),
				Dispatch(defmode)},
		.hShellMode = INVALID_HANDLE_VALUE,
		.hProgMode = INVALID_HANDLE_VALUE,
		.numButtons = 0,
		.ansi_map = NULL,
		.map = NULL,
		.rmap = NULL,
		.pairs = {0},
		.SBI = {},
		.save_CI = {0},

		Dispatch(adjust_size),
		Dispatch(termattrs),
		Dispatch(keypad),
		Dispatch(beeporflash),
		Dispatch(keyok),
		Dispatch(has_key),
		Dispatch(init_acs),
		Dispatch(reset_color_pair),
		Dispatch(reset_colors),
		Dispatch(default_colors),
		Dispatch(init_pair),
		Dispatch(setcolor),
		Dispatch(initcolor),
		Dispatch(do_color),
		Dispatch(curs_set),
		Dispatch(hwlabel),
		Dispatch(hwlabelonoff),
		Dispatch(print),
		Dispatch(read),
		Dispatch(twait),
		Dispatch(mvcur),
		Dispatch(writeat),
		Dispatch(screen_init),
		Dispatch(screen_exit),
		Dispatch(setfilter)
};
NCURSES_EXPORT_VAR(ScreenBufferedConsoleInterface *)
_nc_SCREENBUFFEREDCONSOLE = &legacyCONSOLE;
#define MYSELF legacyCONSOLE

#define RevAttr(attr) (WORD)(((attr) & 0xff00) |       \
							 ((((attr) & 0x07) << 4) | \
							  (((attr) & 0x70) >> 4)))

#define GenMap(vKey, key) MAKELONG(key, vKey)
#define AdjustY() 0
#define console_initialized (IsConsoleInitialized(&MYSELF.core))

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
	return (WORD)a;
}

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

	WORD nKey = 0;
	void *res;
	LONG key = GenMap(vKey, 0);

	res = bsearch(&key,
				  MYSELF.map,
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
AnsiKey(WORD vKey)
{
	int code = -1;

	WORD nKey = 0;
	void *res;
	LONG key = GenMap(vKey, 0);

	res = bsearch(&key,
				  MYSELF.ansi_map,
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

static bool
get_SBI(void)
{
	bool rc = false;
	if (MYSELF.core.getSBI(&(MYSELF.SBI)))
	{
		T(("GetConsoleScreenBufferInfo"));
		T(("... buffer(X:%d Y:%d)",
		   MYSELF.SBI.dwSize.X,
		   MYSELF.SBI.dwSize.Y));
		T(("... window(X:%d Y:%d)",
		   MYSELF.SBI.dwMaximumWindowSize.X,
		   MYSELF.SBI.dwMaximumWindowSize.Y));
		T(("... cursor(X:%d Y:%d)",
		   MYSELF.SBI.dwCursorPosition.X,
		   MYSELF.SBI.dwCursorPosition.Y));
		T(("... display(Top:%d Bottom:%d Left:%d Right:%d)",
		   MYSELF.SBI.srWindow.Top,
		   MYSELF.SBI.srWindow.Bottom,
		   MYSELF.SBI.srWindow.Left,
		   MYSELF.SBI.srWindow.Right));
		rc = true;
	}
	else
	{
		T(("GetConsoleScreenBufferInfo ERR"));
	}
	return rc;
}

/* Helper routine to initialize the legacy console interface. This is called during 
 * the initialization of the library to set up the function pointers and data structures 
 * needed for the legacy console implementation. It populates the key mapping tables, 
 * retrieves the initial console screen buffer information, and sets up the color 
 * capabilities of the console. */
NCURSES_EXPORT(void)
_nc_screenbuffered_console_init(void)
{
	WORD a;
	int i;
	DWORD num_buttons;

	MYSELF.map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	MYSELF.rmap = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	MYSELF.ansi_map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);

	for (i = 0; i < (N_INI + FKEYS); i++)
	{
		if (i < N_INI)
		{
			MYSELF.rmap[i] = MYSELF.map[i] =
				(DWORD)keylist[i];
			MYSELF.ansi_map[i] = (DWORD)ansi_keys[i];
		}
		else
		{
			MYSELF.rmap[i] = MYSELF.map[i] =
				(DWORD)GenMap((VK_F1 + (i - N_INI)),
							  (KEY_F(1) + (i - N_INI)));
			MYSELF.ansi_map[i] =
				(DWORD)GenMap((VK_F1 + (i - N_INI)),
							  (';' + (i - N_INI)));
		}
	}
	qsort(MYSELF.ansi_map,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  keycompare);
	qsort(MYSELF.map,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  keycompare);
	qsort(MYSELF.rmap,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  rkeycompare);

	if (GetNumberOfConsoleMouseButtons(&num_buttons))
		MYSELF.numButtons = (int)num_buttons;
	else
		MYSELF.numButtons = 1;

	a = console_MapColor(TRUE, COLOR_WHITE) |
		console_MapColor(FALSE, COLOR_BLACK);
	for (i = 0; i < CON_NUMPAIRS; i++)
		MYSELF.pairs[i] = a;

	get_SBI();
	GetConsoleCursorInfo(MYSELF.core.ConsoleHandleOut, &MYSELF.save_CI);
	T(("... initial cursor is %svisible, %d%%",
	   (MYSELF.save_CI.bVisible ? "" : "not-"),
	   (int)MYSELF.save_CI.dwSize));

	MYSELF.info.initcolor = TRUE;
	MYSELF.info.canchange = FALSE;
	MYSELF.info.hascolor = TRUE;
	MYSELF.info.caninit = TRUE;
	MYSELF.info.maxpairs = CON_NUMPAIRS;
	MYSELF.info.maxcolors = 8;
	MYSELF.info.numlabels = 0;
	MYSELF.info.labelwidth = 0;
	MYSELF.info.labelheight = 0;
	MYSELF.info.nocolorvideo = 1;
	MYSELF.info.tabsize = 8;
	MYSELF.info.numbuttons = 1;
}

METHOD(adjust_size, bool)(void)
{
	bool res = false;
	COORD newSize;
	CONSOLE_SCREEN_BUFFER_INFO csbi;

	T((T_METHOD(adjust_size," ")));
	AssertScreenBufferedConsole();

	if (HasConsoleResizeLimitations(&MYSELF.core))
	{
		T(("Console has resize limitations, skipping AdjustSize"));
		returnBool(res);
	}

	/*
	 * This piece is just for Windows 10 before the introduction of the new console:.
	 * In older Versions of Windows (before Windows 10), the conhost behaves differently
	 * when resizing the console window.
	 */
	if (!GetConsoleScreenBufferInfo(MYSELF.core.ConsoleHandleOut, &csbi))
	{
		T(("GetConsoleScreenBufferInfo failed"));
		returnBool(res);
	}

	newSize.X = (short)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
	newSize.Y = (short)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);

	T(("New console size: %d x %d", newSize.X, newSize.Y));
	SetConsoleScreenBufferSize(MYSELF.core.ConsoleHandleOut, newSize);

	MYSELF.core.getSBI(&(MYSELF.SBI));
	MYSELF.core.sbi_lines = newSize.Y;
	MYSELF.core.sbi_cols = newSize.X;
	res = TRUE;

	returnBool(res);
}

METHOD(termattrs, chtype)(void)
{
	chtype res = A_NORMAL;

	AssertScreenBufferedConsole();

	res |= (A_BOLD | A_DIM | A_REVERSE | A_STANDOUT | A_COLOR);
	return res;
}

METHOD(keypad, int)(bool flag)
{
	int code = ERR;
	SCREEN *sp;

	T((T_METHOD(keypad,"(%d)"), flag));

	AssertScreenBufferedConsole();

	sp = ConsoleScreen(&legacyCONSOLE.core);
	if (sp)
	{
		sp->_keypad_on = flag;
		code = OK;
	}
	returnCode(code);
}

METHOD(beeporflash, int)(bool beepFlag)
{
	int res = ERR;

	int high, wide, max_cells;
	int i;

	AssertScreenBufferedConsole();

	high = (MYSELF.SBI.srWindow.Bottom -
				MYSELF.SBI.srWindow.Top + 1);
	wide = (MYSELF.SBI.srWindow.Right -
				MYSELF.SBI.srWindow.Left + 1);
	max_cells = (high * wide);

	MakeArray(this_screen, CHAR_INFO, max_cells);
	MakeArray(that_screen, CHAR_INFO, max_cells);
	COORD this_size;
	SMALL_RECT this_region;
	COORD bufferCoord;

	this_region.Top = MYSELF.SBI.srWindow.Top;
	this_region.Left = MYSELF.SBI.srWindow.Left;
	this_region.Bottom = MYSELF.SBI.srWindow.Bottom;
	this_region.Right = MYSELF.SBI.srWindow.Right;

	this_size.X = (SHORT)wide;
	this_size.Y = (SHORT)high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
		read_screen(MYSELF.core.ConsoleHandleOut,
					this_screen,
					this_size,
					bufferCoord,
					&this_region))
	{

		memcpy(that_screen,
			   this_screen,
			   sizeof(CHAR_INFO) * (size_t)max_cells);

		for (i = 0; i < max_cells; i++)
		{
			that_screen[i].Attributes =
				RevAttr(that_screen[i].Attributes);
		}

		write_screen(MYSELF.core.ConsoleHandleOut, that_screen, this_size,
					 bufferCoord, &this_region);
		Sleep(200);
		write_screen(MYSELF.core.ConsoleHandleOut, this_screen, this_size,
					 bufferCoord, &this_region);
	}
	else
	{
		MessageBeep(MB_ICONWARNING); /* MB_OK might be better */
	}
	res = OK;
	return res;
}

METHOD(keyok, int)(int keycode, int flag)
{
	int code = ERR;
	WORD nKey;
	WORD vKey;
	void *res;
	LONG key = GenMap(0, (WORD)keycode);

	T((T_METHOD(keyok,"(%d, %d)"), keycode, flag));

	AssertScreenBufferedConsole();

	res = bsearch(&key,
				  MYSELF.rmap,
				  (size_t)(N_INI + FKEYS),
				  sizeof(keylist[0]),
				  rkeycompare);
	if (res)
	{
		key = *((LONG *)res);
		vKey = HIWORD(key);
		nKey = (LOWORD(key)) & 0x7fff;
		if (!flag)
			nKey |= 0x8000;
		*(LONG *)res = GenMap(vKey, nKey);
	}
	returnCode(code);
}

METHOD(has_key, int)(int keycode)
{
	WORD nKey;
	void *res;
	bool found = FALSE;
	LONG key = GenMap(0, (WORD)keycode);

	T((T_METHOD(has_key,"(%d)"), keycode));

	AssertScreenBufferedConsole();

	res = bsearch(&key,
				  MYSELF.rmap,
				  (size_t)(N_INI + FKEYS),
				  sizeof(keylist[0]),
				  rkeycompare);
	if (res)
	{
		key = *((LONG *)res);
		nKey = LOWORD(key);
		if (!(nKey & 0x8000))
			found = TRUE;
	}
	returnCode(found ? OK : ERR);
}

METHOD(init_acs, void)(chtype *real_map)
{
#define DATA(a, b) \
	{              \
		a, b       \
	}
	static struct
	{
		int acs_code;
		int use_code;
	} table[] = {
		DATA('a', 0xb1), /* ACS_CKBOARD  */
		DATA('f', 0xf8), /* ACS_DEGREE   */
		DATA('g', 0xf1), /* ACS_PLMINUS  */
		DATA('j', 0xd9), /* ACS_LRCORNER */
		DATA('l', 0xda), /* ACS_ULCORNER */
		DATA('k', 0xbf), /* ACS_URCORNER */
		DATA('m', 0xc0), /* ACS_LLCORNER */
		DATA('n', 0xc5), /* ACS_PLUS     */
		DATA('q', 0xc4), /* ACS_HLINE    */
		DATA('t', 0xc3), /* ACS_LTEE     */
		DATA('u', 0xb4), /* ACS_RTEE     */
		DATA('v', 0xc1), /* ACS_BTEE     */
		DATA('w', 0xc2), /* ACS_TTEE     */
		DATA('x', 0xb3), /* ACS_VLINE    */
		DATA('y', 0xf3), /* ACS_LEQUAL   */
		DATA('z', 0xf2), /* ACS_GEQUAL   */
		DATA('0', 0xdb), /* ACS_BLOCK    */
		DATA('{', 0xe3), /* ACS_PI       */
		DATA('}', 0x9c), /* ACS_STERLING */
		DATA(',', 0xae), /* ACS_LARROW   */
		DATA('+', 0xaf), /* ACS_RARROW   */
		DATA('~', 0xf9), /* ACS_BULLET   */
	};
#undef DATA
	unsigned n;
	SCREEN *sp;

	AssertScreenBufferedConsole();
	sp = ConsoleScreen(&legacyCONSOLE.core);

	for (n = 0; n < SIZEOF(table); ++n)
	{
		real_map[table[n].acs_code] =
			(chtype)table[n].use_code | A_ALTCHARSET;
		if (sp)
		sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
}

METHOD(reset_color_pair, bool)(void)
{
	bool res = false;
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;

	AssertScreenBufferedConsole();

	SetConsoleTextAttribute(MYSELF.core.ConsoleHandleOut, a);
	get_SBI();
	res = true;
	return res;
}

METHOD(reset_colors, bool)(void)
{
	bool res = false;
	return res;
}

METHOD(init_pair, int)(int pair, int f, int b)
{
	int code = ERR;
	int num_colors;

	AssertScreenBufferedConsole();

	num_colors = MYSELF.info.maxcolors;
	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < num_colors) && (b >= 0) && (b < num_colors))
	{
		MYSELF.pairs[pair] =
			console_MapColor(TRUE, f) |
			console_MapColor(FALSE, b);
		T(("... legacy_init_pair: pair %d: fg=%d, bg=%d", pair, f, b));
		code = OK;
	}
	return code;
}

METHOD(setcolor, void)(bool fore, int color)
{
	WORD a = console_MapColor(fore, color);

	AssertScreenBufferedConsole();

	a |= (WORD)((MYSELF.SBI.wAttributes) & ((fore) ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(MYSELF.core.ConsoleHandleOut, a);
	get_SBI();
}

METHOD(initcolor, void)(int c GCC_UNUSED, int r GCC_UNUSED, int g GCC_UNUSED, int b GCC_UNUSED)
{
	return;
}

METHOD(do_color, void)(
	int oldpair GCC_UNUSED, 
	int pair GCC_UNUSED, 
	int reverse GCC_UNUSED, 
	NCURSES_SP_OUTC outc GCC_UNUSED)
{
	return;
}

METHOD(default_colors, int)(int fg, int bg)
{
	int code = ERR;
	return code;
}

METHOD(curs_set, int)(int visibility)
{
	int res = ERR;
	CONSOLE_CURSOR_INFO this_CI;

	T((T_METHOD(curs_set,"(%d)"), visibility));

	AssertScreenBufferedConsole();

	this_CI = MYSELF.save_CI;
	switch (visibility)
	{
	case 0:
		this_CI.bVisible = FALSE;
		break;
	case 1:
		break;
	case 2:
		this_CI.dwSize = 100;
		break;
	}
	SetConsoleCursorInfo(MYSELF.core.ConsoleHandleOut, &this_CI);
	returnCode(res);
}

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)
#define BUTTON_MASK (FROM_LEFT_1ST_BUTTON_PRESSED | \
					 FROM_LEFT_2ND_BUTTON_PRESSED | \
					 FROM_LEFT_3RD_BUTTON_PRESSED | \
					 FROM_LEFT_4TH_BUTTON_PRESSED | \
					 RIGHTMOST_BUTTON_PRESSED)
#define NO_BUTTONS (mmask_t)0

static mmask_t
filter_button_events(const SCREEN *sp, int mask)
{
	mmask_t result = NO_BUTTONS;

	(void)sp;
	assert(sp && console_initialized);

	T((T_CALLED("legacy_console::filter_button_events(%p, %08x)"), sp, mask));

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
		switch (MYSELF.numButtons)
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

	returnCode(result);
}

static bool
handle_mouse(SCREEN *sp, MOUSE_EVENT_RECORD mer)
{
	MEVENT work;
	bool result = false;

	assert(sp);

	T((T_CALLED("legacy_console::handle_mouse(%p, {pos=(%d,%d) mask=%08x})"),
	   sp, mer.dwMousePosition.X, mer.dwMousePosition.Y, mer.dwButtonState));

	if (!IsMouseActive(sp))
	{
		T(("... mouse is not active, ignoring event"));
		returnBool(false);
	}
#ifdef NO_MOUSE_SUPPORT
	T(("... mouse support is disabled, ignoring event"));
	returnBool(false);
#endif

	sp->_console_mouse_old_buttons = sp->_console_mouse_new_buttons;
	sp->_console_mouse_new_buttons = mer.dwButtonState & BUTTON_MASK;

	/*
	 * We're only interested if the button is pressed or released.
	 * FIXME: implement continuous event-tracking.
	 */
	if (sp->_console_mouse_new_buttons != sp->_console_mouse_old_buttons)
	{
		T(("... button state changed: old=%08x new=%08x",
		   sp->_console_mouse_old_buttons, sp->_console_mouse_new_buttons));

		memset(&work, 0, sizeof(work));

		if (sp->_console_mouse_new_buttons)
		{
			work.bstate |= filter_button_events(sp, sp->_console_mouse_new_buttons);
		}
		else
		{
			T(("... button state cleared, reporting release"));
			/* cf: BUTTON_PRESSED, BUTTON_RELEASED */
			work.bstate |= (filter_button_events(sp, sp->_console_mouse_old_buttons) >> 1);
			result = true;
		}

		work.x = mer.dwMousePosition.X;
		work.y = mer.dwMousePosition.Y - AdjustY();
		T(("... event at (%d, %d), bstate=%08x", work.x, work.y, work.bstate));

		/*
		 * Only store events for buttons that the application is
		 * actually tracking via mousemask().  If we let through events
		 * whose bstate has no bits in _mouse_mask2, _nc_mouse_parse will
		 * invalidate all events in the ring and its unbounded adjustment
		 * loop will spin forever.  xterm avoids this by not sending
		 * events for unregistered buttons; we must filter here instead.
		 */
		if (sp->_mouse_mask2 && !(work.bstate & sp->_mouse_mask2))
		{
			T(("... bstate %08x not in _mouse_mask2 %08x, dropping",
			   (unsigned)work.bstate, (unsigned)sp->_mouse_mask2));
			returnBool(false);
		}

		if (sp->_console_mouse_tail < 0 || sp->_console_mouse_tail >= FIFO_SIZE)
		{
			T(("... mouse FIFO overflow, dropping event"));
			returnBool(false);
		}
		assert(sp->_console_mouse_tail >= 0);
		assert(sp->_console_mouse_tail < FIFO_SIZE);
		sp->_console_mouse_fifo[sp->_console_mouse_tail] = work;
		sp->_console_mouse_tail += 1;
	}
	returnBool(result);
}

METHOD(hwlabel,void)(int num GCC_UNUSED, const char* label GCC_UNUSED)
{
	return;
}

METHOD(hwlabelonoff,void)(bool on GCC_UNUSED)
{
	return;
}

METHOD(print,int)(char* data, int len)
{
	int rc = ERR;
	return(rc);
}	

METHOD(read, int)(int *buf)
{
	int rc = -1;
	INPUT_RECORD inp_rec;
	bool b;
	DWORD nRead;
	WORD vk;
	HANDLE hdl = INVALID_HANDLE_VALUE;
	SCREEN *sp;

	T((T_METHOD(read,"(%p)"), buf));
	AssertScreenBufferedConsole();

	sp = ConsoleScreen(&legacyCONSOLE.core);
	assert(buf);
	assert(sp);

	hdl = MYSELF.core.ConsoleHandleIn;
	memset(&inp_rec, 0, sizeof(inp_rec));

	while ((b = (bool)(0 != read_keycode(hdl, &inp_rec, 1, &nRead))))
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
				*buf = (int)inp_rec.Event.KeyEventChar & KEYMASK;
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
				if (*buf == 0)
				{
					int key = MapKey(vk);
					if (key < 0)
						continue;
					if (sp->_keypad_on)
					{
						*buf = key;
					}
					else
					{
						ungetch('\0');
						*buf = AnsiKey(vk);
					}
				}
				else if (vk == VK_BACK)
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
#ifdef NO_MOUSE_SUPPORT
				/* Mouse support is disabled, so we ignore the event. */
				continue;
#else
				handle_mouse(sp, inp_rec.Event.MouseEvent);
				/* Only report KEY_MOUSE when handle_mouse actually stored an
				 * event.  Position-only moves (no button change) and events
				 * received while mouse is inactive are silently discarded. */
				if (!MouseFifoHasEvent(sp))
					continue;
				*buf = KEY_MOUSE;
				if ((sp->_mouse_type == M_WINDOWS_CONSOLE)
					 && (sp->_console_mouse_head < sp->_console_mouse_tail)) {
					 sp->_mouse_event(sp);
				}
				break;
#endif
			}
			else if (inp_rec.EventType == WINDOW_BUFFER_SIZE_EVENT)
			{
				SetConsolePendingResize(&MYSELF.core);
			}
			continue;
		}
	}
	returnCode(rc);
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

METHOD(twait,int)(int mode, int milliseconds, int *timeleft EVENTLIST_2nd(_nc_eventlist *evl))
{
	SCREEN *sp;
	HANDLE hdl = INVALID_HANDLE_VALUE;
	INPUT_RECORD inp_rec;
	bool b;
	DWORD nRead = 0, rc = WAIT_FAILED;
	int code = 0;
	struct timeval fstart;
	struct timeval fend;
	int diff;
	bool isNoDelay = (milliseconds == 0);

	TR(TRACE_IEVENT, ("start twait: %d milliseconds, mode: %d",
				  milliseconds, mode));

	AssertScreenBufferedConsole();

	sp = ConsoleScreen(&legacyCONSOLE.core);
	hdl = MYSELF.core.ConsoleHandleIn;
	assert(sp);

#ifdef NCURSES_WGETCH_EVENTS
	(void)evl; /* TODO: implement wgetch-events */
#endif

/* Virtual-key codes that represent standalone modifier-key presses.
	 * These produce KEY_EVENTs of their own but carry no actual character
	 * or application key; skip them so they do not trigger TW_INPUT. */
#define IS_MODIFIER_VK(vk) ((vk) == VK_SHIFT   || (vk) == VK_LSHIFT  || \
						 (vk) == VK_RSHIFT  || \
						 (vk) == VK_CONTROL || (vk) == VK_LCONTROL || \
						 (vk) == VK_RCONTROL || \
						 (vk) == VK_MENU    || (vk) == VK_LMENU   || \
						 (vk) == VK_RMENU   || \
						 (vk) == VK_CAPITAL || (vk) == VK_NUMLOCK  || \
						 (vk) == VK_SCROLL)
#define CONSUME() read_keycode(hdl, &inp_rec, 1, &nRead)

	if (milliseconds < 0)
		milliseconds = NC_INFINITY;

	memset(&inp_rec, 0, sizeof(inp_rec));

	/*
	 * TW_NONE: pure delay, no event checking.
	 * Use Sleep() directly so that events in the input queue do not
	 * keep the console handle signaled and cause a busy-loop.
	 */
	if (mode==TW_NONE) {
		if (!isNoDelay && milliseconds != NC_INFINITY)
			Sleep((DWORD)milliseconds);
		milliseconds = 0;
		code = TW_NONE;
		goto end;
	}

	while (true)
	{
		if (!isNoDelay)
		{
			gettimeofday(&fstart, NULL);
			rc = WaitForSingleObject(hdl, (DWORD)milliseconds);
			gettimeofday(&fend, NULL);
			diff = _nc_timeval_diff_in_ms(fstart, fend);
			milliseconds = Adjust(milliseconds, diff);

			if (rc == WAIT_TIMEOUT) {
				code = TW_NONE;
				break;
			}
			if (rc != WAIT_OBJECT_0) {
				code = -1;
				break;
			}
		}

		/*
		 * rc == WAIT_OBJECT_0 or isNoDelay: inspect the event queue.
		 *
		 * Strategy: scan events from the front of the queue.
		 *   - Relevant event found  -> leave it in queue (for legacy_read),
		 *                              set code and return.
		 *   - Definitely irrelevant -> consume it (ReadConsoleInput) so the
		 *                              handle does not stay permanently
		 *                              signaled and cause a busy-loop.
		 *   - Belongs to a mode we  -> stop scanning; leave it for a future
		 *     are not checking        call that uses a matching mode.
		 *
		 * "Definitely irrelevant" events (always consumed):
		 *   KEY_EVENT  with bKeyDown==FALSE  (key-up)
		 *   KEY_EVENT  with bKeyDown==TRUE but only modifier keys held
		 *   MOUSE_EVENT with no relevant button state (mouse move etc.)
		 *   WINDOW_BUFFER_SIZE_EVENT  (consumed + resize flag set)
		 *   all other event types    (FOCUS_EVENT, MENU_EVENT, ...)
		 */
		{
			DWORD n = 0;
			DWORD i;
			bool stop_scan;

			b = (bool)(0 != GetNumberOfConsoleInputEvents(hdl, &n));
			if (!b)
			{
				T(("twait:err GetNumberOfConsoleInputEvents"));
				code = TW_NONE;
				goto end;
			}
			T(("twait: %lu events in queue", (unsigned long)n));

			if (n == 0)
			{
				if (isNoDelay) {
					code = TW_NONE;
					goto end;
				}
				if (milliseconds <= 0)
					break;
				continue;
			}

			/* n > 0: peek all events and scan from the front.
			 * MakeArray must be the first declaration in this block. */
			{
				MakeArray(pInpRec, INPUT_RECORD, n);

				if (pInpRec == NULL) {
					T(("twait:err could not alloca input records"));
					code = TW_NONE;
					goto end;
				}
				memset(pInpRec, 0, sizeof(INPUT_RECORD) * n);
				b = PeekConsoleInput(hdl, pInpRec, n, &n);
				if (!b) {
					T(("twait:err PeekConsoleInput"));
					code = TW_NONE;
					goto end;
				}

				stop_scan = FALSE;
				for (i = 0; i < n && !stop_scan; i++)
				{
					switch (pInpRec[i].EventType)
					{
					case KEY_EVENT:
						if (pInpRec[i].Event.KeyEvent.bKeyDown)
						{
							if (mode & TW_INPUT)
							{
								WORD vk = pInpRec[i].Event.KeyEvent.wVirtualKeyCode;
								if (!IS_MODIFIER_VK(vk))
								{
									/* Relevant key-down (incl. Ctrl+X etc.): leave for reader */
									T(("twait:event KEY_EVENT: vk=%d, char=%d",
									   vk,
									   pInpRec[i].Event.KeyEventChar & KEYMASK));
									code |= TW_INPUT;
									goto end;
								}
								/* Standalone modifier key: consume it */
								T(("twait: consuming standalone modifier KEY_EVENT vk=%d", vk));
								CONSUME();
							}
							else
							{
								/* Key-down but TW_INPUT not requested: may be needed
								 * by a later call -- stop scanning without consuming */
								T(("twait: KEY_EVENT not in mode, stopping scan"));
								stop_scan = true;
							}
						}
						else
						{
							/* Key-up: always irrelevant, consume it */
							CONSUME();
						}
						break;

					case MOUSE_EVENT:
						if (mode & TW_MOUSE)
						{
#ifdef NO_MOUSE_SUPPORT
							/* Mouse support is disabled, so we ignore the event. */
							T(("twait: mouse support disabled, consuming MOUSE_EVENT"));
							CONSUME();
							continue;
#else
						{
							int newBtns = pInpRec[i].Event.MouseEvent.dwButtonState & BUTTON_MASK;
							/*
							 * A button event is relevant if the raw Win32 button
							 * state differs from what handle_mouse last recorded.
							 * This correctly catches both presses (newBtns != 0)
							 * and releases (newBtns == 0 when a button was down).
							 * Pure mouse moves have the same state as the last
							 * handle_mouse call and are consumed as irrelevant.
							 *
							 * Also check that the resulting ncurses bstate would
							 * have bits in _mouse_mask2.  If not, the event will
							 * be discarded by handle_mouse anyway; treat it as
							 * irrelevant here so read() is not called only to
							 * stall waiting for the next real input.
							 */
							if (newBtns != sp->_console_mouse_new_buttons)
							{
								mmask_t bstate;
								if (newBtns)
									bstate = filter_button_events(sp, newBtns);
								else
									bstate = (mmask_t)(filter_button_events(sp, sp->_console_mouse_new_buttons) >> 1);
								if (!sp->_mouse_mask2 || (bstate & sp->_mouse_mask2))
								{
									T(("twait:event MOUSE_EVENT: buttonState=%08lx (was %08x)",
									   (unsigned long)pInpRec[i].Event.MouseEvent.dwButtonState,
									   sp->_console_mouse_new_buttons));
									code |= TW_MOUSE;
									goto end;
								}
							}
							/* Irrelevant button event or unregistered button: consume it */
							T(("twait: consuming irrelevant MOUSE_EVENT (no state change or not in mask)"));
							CONSUME();
						}
#endif
						}
						else
						{
							/* Mouse event but TW_MOUSE not requested: may be needed
							 * by a later call -- stop scanning without consuming */
							T(("twait: MOUSE_EVENT not in mode, stopping scan"));
							stop_scan = true;
						}
						break;

					case WINDOW_BUFFER_SIZE_EVENT:
						T(("twait:event WINDOW_BUFFER_SIZE_EVENT"));
						CONSUME();
						SetConsolePendingResize(&MYSELF.core);
						break;

					default:
						T(("twait:event type %d, consuming", pInpRec[i].EventType));
						CONSUME();
						break;
					}
				}
			}     /* end MakeArray block */

			/* No relevant events found in this pass */
			if (isNoDelay) {
				code = TW_NONE;
				goto end;
			}
			if (milliseconds <= 0)
				break;
			/* Continue waiting */
		}
	}
end:
	TR(TRACE_IEVENT, ("end twait: returned %d (%lu), remaining time %d msec",
					  code, (unsigned long)GetLastError(), milliseconds));

	if (timeleft)
		*timeleft = milliseconds;

	return code;
}

/* This function sets the console mode for the input and output handles. It is called by the main thread
 * when it wants to change the console mode. The function takes a TTY structure that contains the desired
 * mode flags, and it returns OK on success or ERR on failure.
 * It is also responsible for detecting switches between shell mode and program mode, and starting or
 * stopping the input subsystem accordingly. */
METHOD(setmode, int)(int fd GCC_UNUSED, const TTY *arg)
{
	HANDLE input_target = INVALID_HANDLE_VALUE;
	HANDLE output_target = INVALID_HANDLE_VALUE;
	 bool input_ok = false;
	bool output_ok = false;
	SCREEN *sp = NULL;

	T((T_METHOD(setmode,"(fd=%d, TTY*=%p)"), fd, arg));

	AssertScreenBufferedConsole();

	input_target = MYSELF.core.ConsoleHandleIn;
	output_target = MYSELF.core.ConsoleHandleOut;
	sp = ConsoleScreen(&legacyCONSOLE.core);

	if (!arg)
		returnCode(ERR);

	if (input_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = arg->dwFlagIn;
		if (arg->kind == TTY_MODE_SHELL)
		{
			/* In shell mode, we want to disable VT input and enable the basic line input, processed
			 * input and echo input modes, to provide a more traditional console input experience.
			 * This allows the user to interact with the console in a way that is consistent with
			 * what they would expect from a typical command prompt or terminal window, with
			 * features like line editing and input processing enabled. */
			mode |= (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT);
			// mode &= ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);
		}
		else if (arg->kind == TTY_MODE_PROGRAM)
		{
			/* In program mode, we want to enable VT input. */
			mode |= ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT;
		}

		/* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
		if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT))
		{
			mode &= ~ENABLE_ECHO_INPUT;
		}

		input_ok = (bool)(0!=SetConsoleMode(input_target, mode));
		if (input_ok)
		{
			/* Make sure the cached value reflects the real value we set, as the
			 * caller may not have provided all necessary flags (e.g.
			 * ENABLE_PROCESSED_INPUT when VT is requested) */
			DWORD realMode;
			if (GetConsoleMode(input_target, &realMode))
			{
				MYSELF.core.ttyflags.dwFlagIn = realMode;
			}
			else
			{
				MYSELF.core.ttyflags.dwFlagIn = mode;
			}
		}
		else
		{
			T(("Invalid input file descriptor"));
		}
	}

	if (output_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = arg->dwFlagOut;
		output_ok = (bool)(0!=SetConsoleMode(output_target, mode));
		if (output_ok)
		{
			/* Make sure the cached value reflects the real value we set,
			 * as the caller may not have provided all necessary flags
			 * (e.g. VT output is required for the Windows Console backend) */
			DWORD realMode;
			if (GetConsoleMode(output_target, &realMode))
			{
				MYSELF.core.ttyflags.dwFlagOut = realMode;
			}
			else
			{
				MYSELF.core.ttyflags.dwFlagOut = mode;
			}
		}
		else
		{
			T(("Invalid output file descriptor"));
		}
	}

	if (arg->kind == TTY_MODE_SHELL)
	{
		T(("Shell mode set"));
		if (IsConsoleProgMode(&MYSELF.core))
		{
			ClearConsoleProgMode(&MYSELF.core);
			if (sp)
			{
				_nc_keypad(sp, FALSE);
				MYSELF.core.flush(sp->_ifd);
			}

			if (MYSELF.hShellMode != INVALID_HANDLE_VALUE)
			{
				T(("... MYSELF: switching to shell mode buffer"));
				MYSELF.core.ConsoleHandleOut = MYSELF.hShellMode;
				SetConsoleActiveScreenBuffer(MYSELF.core.ConsoleHandleOut);
				SetConsoleCursorInfo(MYSELF.core.ConsoleHandleOut, &MYSELF.save_CI);
			}
			else
			{
				T(("... MYSELF: no valid shell mode buffer"));
			}
		}
		else
		{
			T(("... MYSELF: Already in shell mode"));
		}
	}
	else if (arg->kind == TTY_MODE_PROGRAM)
	{
		T(("Program mode set"));
		if (!IsConsoleProgMode(&MYSELF.core))
		{
			SetConsoleProgMode(&MYSELF.core);
			if (sp)
			{
				if (sp->_keypad_on)
					_nc_keypad(sp, TRUE);
			}

			if (MYSELF.hProgMode != INVALID_HANDLE_VALUE)
			{
				T(("... MYSELF: switching to program mode buffer"));
				MYSELF.core.ConsoleHandleOut = MYSELF.hProgMode;
				SetConsoleActiveScreenBuffer(MYSELF.core.ConsoleHandleOut);
				SetConsoleCursorInfo(MYSELF.core.ConsoleHandleOut, &MYSELF.save_CI);
			}
			else
			{
				T(("... MYSELF: no valid program mode buffer"));
			}
		}
		else
		{
			T(("... MYSELF: Already in program mode"));
		}
	}

	// Handle errors
	if (!input_ok || !output_ok)
	{
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
METHOD(getmode, int)(int fd GCC_UNUSED, TTY *arg)
{
	T((T_METHOD(getmode,"(fd=%d, TTY*=%p)"), fd, arg));

	AssertScreenBufferedConsole();

	if (NULL == arg)
		returnCode(ERR);

	*arg = MYSELF.core.ttyflags;
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
METHOD(defmode, int)(TTY *arg, short kind)
{
	short realMode = kind;

	T((T_METHOD(defmode,"(TTY*=%p, kind=%d)"), arg, kind));

	AssertScreenBufferedConsole();

	if (NULL == arg)
		returnCode(ERR);

	if (realMode == TTY_MODE_AUTO)
	{
		realMode = IsConsoleProgMode(&MYSELF.core) ? TTY_MODE_PROGRAM : TTY_MODE_SHELL;
	}

	arg->kind = realMode;
	returnCode(OK);
}

METHOD(size, void)(int *Lines, int *Cols)
{

	AssertScreenBufferedConsole();

	if (Lines != NULL && Cols != NULL)
	{
		*Lines = (int)(MYSELF.SBI.srWindow.Bottom + 1 -
					   MYSELF.SBI.srWindow.Top);
		*Cols = (int)(MYSELF.SBI.srWindow.Right + 1 -
					  MYSELF.SBI.srWindow.Left);
		T(("win32_driver::legacy_size() returns %d lines, %d cols", *Lines, *Cols));
	}
}

METHOD(size_changed, bool)(void)
{
	bool resized = false;
	T((T_METHOD(size_changed,"()")));

	AssertScreenBufferedConsole();

	if (HasConsolePendingResize(&MYSELF.core))
	{
		T(("Resize event pending, returning TRUE"));
		resized = true;
		ClearConsoleResizeLimitations(&MYSELF.core);
		_nc_globals.have_sigwinch = 1;
	}
	returnBool(resized);
}

METHOD(termname, char*)(void)
{
	return CONSOLE_TERM_NAME;
}

/* This initializaton function can be called multiple times, and actually it is called from within
 * setupterm() and/or newterm(). It initializes the defaultCONPTY structure when called the first 
 * time, and on subsequent calls it just returns TRUE.
 * 
 * The other purpose of this routine is to manage the assignment of console handles. If the
 * assigned filedescriptors are NOT valid console handles, the call will return FALSE. 
 * 
 * Please note, that on systems that support both ConPTY and the legacy console, the legacy console 
 * will never be used, the use of ConPTY will be forced, and this init function will never be called. 
 * So the legacy console is basically a fallback for older Windows versions that don't support ConPTY.
 * */
METHOD(init, bool)(int fdOut, int fdIn)
{
	bool result = false;

	T((T_METHOD(init,"(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

	AssertScreenBufferedConsole();

	/* initialize once, or not at all */
	if (!IsConsoleInitialized(&MYSELF.core))
	{
		/*
		 * We set the console mode flags to the most basic ones that are required for ConPTY
		 * to function properly. */
		DWORD dwFlagIn = (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT | ENABLE_EXTENDED_FLAGS);
		DWORD dwFlagOut = (ENABLE_PROCESSED_OUTPUT | DISABLE_NEWLINE_AUTO_RETURN | ENABLE_WRAP_AT_EOL_OUTPUT);
		DWORD dwFlag;

		HANDLE stdin_handle = INVALID_HANDLE_VALUE;
		HANDLE stdout_handle = INVALID_HANDLE_VALUE;

		stdin_handle = CreateFileA(
			"CONIN$",
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_READ,
			NULL, OPEN_EXISTING, 0, NULL);

		stdout_handle = CreateFileA(
			"CONOUT$",
			GENERIC_READ | GENERIC_WRITE,
			FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, 0, NULL);

		MYSELF.hShellMode = stdout_handle;

		if (stdout_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdout_handle,
																	&dwFlag) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(false);
		}
		MYSELF.core.ConsoleHandleOut = stdout_handle;

		if (stdin_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdin_handle,
																   &dwFlag) == 0)
		{
			T(("StdIn handle is not a console"));
			returnBool(false);
		}
		MYSELF.core.ConsoleHandleIn = stdin_handle;

		SetConsoleMode(stdout_handle, dwFlagOut);
		/* We immediately read the console mode back to reflect any changes the
		 * runtime may have added, so the saved value reflects the actual mode
		 * of the console. */
		if (GetConsoleMode(stdout_handle, &dwFlagOut) == 0)
		{
			T(("GetConsoleMode() failed for stdout"));
			returnBool(false);
		}
		MYSELF.core.ttyflags.dwFlagOut = dwFlagOut;

		dwFlagIn &= ~(ENABLE_QUICK_EDIT_MODE);
		SetConsoleMode(stdin_handle, dwFlagIn);
		/* We immediately read the console mode back to reflect any changes the
		 * runtime may have added, so the saved value reflects the actual mode
		 * of the console. */
		if (GetConsoleMode(stdin_handle, &dwFlagIn) == 0)
		{
			T(("GetConsoleMode() failed for stdin"));
			returnBool(false);
		}
		MYSELF.core.ttyflags.dwFlagIn = dwFlagIn;

		_nc_screenbuffered_console_init();

		MYSELF.core.ConsoleHandleIn = stdin_handle;
		MYSELF.core.ConsoleHandleOut = stdout_handle;

		if (MYSELF.hProgMode == INVALID_HANDLE_VALUE)
		{
			T(("... creating console buffer"));
			MYSELF.hProgMode =
				CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
										  FILE_SHARE_READ | FILE_SHARE_WRITE,
										  NULL,
										  CONSOLE_TEXTMODE_BUFFER,
										  NULL);
		}
		if (MYSELF.hProgMode == INVALID_HANDLE_VALUE || GetConsoleMode(MYSELF.hProgMode, &dwFlagOut) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(false);
		}

		MarkConsoleInitialized(&MYSELF.core);
		result = true;
	} else
	{
		T(("Console already initialized"));
		result = true;
	}
	returnBool(result);
}

METHOD(mvcur,int)(int oldrow GCC_UNUSED, int oldcol GCC_UNUSED, int newrow, int newcol)
{
	int result = ERR;
	COORD pos;

	AssertScreenBufferedConsole();

	pos.X = (SHORT)newcol;
	pos.Y = (SHORT)newrow;
	if (SetConsoleCursorPosition(MYSELF.core.ConsoleHandleOut, pos))
	{
		result = OK;
	}
	return(result);
}

static WORD
MapAttr(WORD res, attr_t ch)
{
	if (ch & A_COLOR)
	{
		int p;

		p = PairNumber(ch);
		if (p >= 0 && p < CON_NUMPAIRS)
		{
			WORD a;
			a = MYSELF.pairs[p];
			res = (WORD)((res & 0xff00) | a);
		}
	}

	if (ch & A_REVERSE)
	{
		res = RevAttr(res);
	}

	if (ch & A_STANDOUT)
	{
		res = RevAttr(res) | BACKGROUND_INTENSITY;
	}

	if (ch & A_BOLD)
		res |= FOREGROUND_INTENSITY;

	if (ch & A_DIM)
		res |= BACKGROUND_INTENSITY;

	return res;
}

#if USE_WIDEC_SUPPORT
/*
 * TODO: support surrogate pairs
 * TODO: support combining characters
 * TODO: support acsc
 */
METHOD(writeat,bool)(int y, int x, const cchar_t *str, int limit)
{
	int actual = 0;
	MakeArray(ci, CHAR_INFO, limit);
	COORD loc, siz;
	SMALL_RECT rec;
	int i;
	cchar_t ch;
	SCREEN *sp = ConsoleScreen(&MYSELF.core);
	
	assert(sp);

	for (i = actual = 0; i < limit; i++)
	{
		ch = str[i];
		if (isWidecExt(ch))
			continue;
		ci[actual].CharInfoChar = CharOf(ch);
		ci[actual].Attributes = MapAttr(MYSELF.SBI.wAttributes,
										AttrOf(ch));
		if (AttrOf(ch) & A_ALTCHARSET)
		{
			if (sp->_wacs_map)
			{
				int which = CharOf(ch);
				if (which > 0 && which < ACS_LEN && CharOf(sp->_wacs_map[which]) != 0)
				{
					ci[actual].CharInfoChar = CharOf(sp->_wacs_map[which]);
				}
				else
				{
					ci[actual].CharInfoChar = ' ';
				}
			}
		}
		++actual;
	}

	loc.X = (SHORT)0;
	loc.Y = (SHORT)0;
	siz.X = (SHORT)actual;
	siz.Y = 1;

	rec.Left = (SHORT)x;
	rec.Top = (SHORT)(y + AdjustY());
	rec.Right = (SHORT)(x + limit - 1);
	rec.Bottom = rec.Top;

	return (bool)(0 != write_screen(MYSELF.core.ConsoleHandleOut, ci, siz, loc, &rec));
}


#else
#define ACS_CHAR(sp,c) (c<0 || c>=ACS_LEN ? (chtype)0 : (sp)->_acs_map[c])

METHOD(writeat,bool)(int y, int x, const chtype *str, int limit)
{
	MakeArray(ci, CHAR_INFO, limit);
	COORD loc, siz;
	SMALL_RECT rec;
	int i;
	chtype ch;
	SCREEN *sp = ConsoleScreen(DefaultConsole());

	for (i = 0; i < limit; i++)
	{
		ch = str[i];
		ci[i].CharInfoChar = ChCharOf(ch);
		ci[i].Attributes = MapAttr(MYSELF.SBI.wAttributes,
								   ChAttrOf(ch));
		if (ChAttrOf(ch) & A_ALTCHARSET)
		{
			if (sp->_acs_map)
				ci[i].CharInfoChar =
					ChCharOf(ACS_CHAR(sp, ChCharOf(ch)));
		}
	}

	loc.X = (short)0;
	loc.Y = (short)0;
	siz.X = (short)limit;
	siz.Y = 1;

	rec.Left = (short)x;
	rec.Top = (short)y;
	rec.Right = (short)(x + limit - 1);
	rec.Bottom = rec.Top;

	return (bool)(0 != write_screen(MYSELF.core.ConsoleHandleOut, ci, siz, loc, &rec));
}
#endif

METHOD(screen_init, void)(void)
{
}

METHOD(screen_exit, void)(void)
{
}

METHOD(setfilter,void)(void)
{
}

#endif /* USE_SCREENBUFFERED_CONSOLE */
