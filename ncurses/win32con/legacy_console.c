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

#if USE_LEGACY_CONSOLE
#include <stdint.h>
#include <sys/time.h>

// FIXME JPF - For now, we disable mouse support as we need to debug why it not works
#define NO_MOUSE_SUPPORT

#define EXP_OPTIMIZE 0

#define DispatchMethod(name) legacy_##name
#define Dispatch(name) .name = DispatchMethod(name)
#define NoDispatch(name) .name = NULL
#define METHOD(name, type) static type DispatchMethod(name)

METHOD(init, BOOL)(int fdOut, int fdIn);
METHOD(size, void)(int *Lines, int *Cols);
METHOD(size_changed, BOOL)(void);
METHOD(getmode, int)(int fd GCC_UNUSED, TTY *arg);
METHOD(setmode, int)(int fd GCC_UNUSED, const TTY *arg);
METHOD(defmode, int)(TTY *arg, short kind);
METHOD(AdjustSize, BOOL)(void);
METHOD(termattrs, chtype)(void);
METHOD(keypad, int)(BOOL flag);
METHOD(beeporflash, int)(BOOL beep);
METHOD(keyok, int)(int keycode, int flag);
METHOD(has_key, int)(int keycode);
METHOD(init_acs, void)(chtype *acs);
METHOD(reset_color_pair, BOOL)(void);
METHOD(init_pair, int)(int pair, int fg, int bg);
METHOD(setcolor, void)(BOOL fg, int color);
METHOD(curs_set, int)(int visibility);
METHOD(read, int)(int *buf);
METHOD(twait,int)(int mode,int milliseconds,int *timeleft EVENTLIST_2nd(_nc_eventlist *evl));
METHOD(testmouse,int)(int delay EVENTLIST_2nd(_nc_eventlist *));
METHOD(mvcur, int)(int yold, int xold, int y, int x);
METHOD(doupdate, int)(void);

static LegacyConsoleInterface legacyCONSOLE =
	{
		.core =
			{
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

		Dispatch(AdjustSize),
		Dispatch(termattrs),
		Dispatch(keypad),
		Dispatch(beeporflash),
		Dispatch(keyok),
		Dispatch(has_key),
		Dispatch(init_acs),
		Dispatch(reset_color_pair),
		Dispatch(init_pair),
		Dispatch(setcolor),
		Dispatch(curs_set),
		Dispatch(read),
		Dispatch(twait),
		Dispatch(testmouse),
		Dispatch(mvcur),
		Dispatch(doupdate)
};
NCURSES_EXPORT_VAR(LegacyConsoleInterface *)
_nc_LEGACYCONSOLE = &legacyCONSOLE;

#define RevAttr(attr) (WORD)(((attr) & 0xff00) |       \
							 ((((attr) & 0x07) << 4) | \
							  (((attr) & 0x70) >> 4)))

#define GenMap(vKey, key) MAKELONG(key, vKey)
#define AdjustY() 0
#define console_initialized (IsConsoleInitialized())

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
console_MapColor(BOOL fore, int color)
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
				  LEGACYCONSOLE.map,
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

static BOOL
get_SBI(void)
{
	bool rc = FALSE;
	if (CORECONSOLE.getSBI(&(LEGACYCONSOLE.SBI)))
	{
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
		rc = TRUE;
	}
	else
	{
		T(("GetConsoleScreenBufferInfo ERR"));
	}
	return rc;
}

NCURSES_EXPORT(void)
_nc_legacy_console_init(void)
{
	WORD a;
	int i;
	DWORD num_buttons;

	LEGACYCONSOLE.map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.rmap = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.ansi_map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);

	for (i = 0; i < (N_INI + FKEYS); i++)
	{
		if (i < N_INI)
		{
			LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
				(DWORD)keylist[i];
			LEGACYCONSOLE.ansi_map[i] = (DWORD)ansi_keys[i];
		}
		else
		{
			LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
				(DWORD)GenMap((VK_F1 + (i - N_INI)),
							  (KEY_F(1) + (i - N_INI)));
			LEGACYCONSOLE.ansi_map[i] =
				(DWORD)GenMap((VK_F1 + (i - N_INI)),
							  (';' + (i - N_INI)));
		}
	}
	qsort(LEGACYCONSOLE.ansi_map,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  keycompare);
	qsort(LEGACYCONSOLE.map,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  keycompare);
	qsort(LEGACYCONSOLE.rmap,
		  (size_t)(MAPSIZE),
		  sizeof(keylist[0]),
		  rkeycompare);

	if (GetNumberOfConsoleMouseButtons(&num_buttons))
		LEGACYCONSOLE.numButtons = (int)num_buttons;
	else
		LEGACYCONSOLE.numButtons = 1;

	a = console_MapColor(TRUE, COLOR_WHITE) |
		console_MapColor(FALSE, COLOR_BLACK);
	for (i = 0; i < CON_NUMPAIRS; i++)
		LEGACYCONSOLE.pairs[i] = a;

	get_SBI();
	GetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
	T(("... initial cursor is %svisible, %d%%",
	   (LEGACYCONSOLE.save_CI.bVisible ? "" : "not-"),
	   (int)LEGACYCONSOLE.save_CI.dwSize));

	LEGACYCONSOLE.info.initcolor = TRUE;
	LEGACYCONSOLE.info.canchange = FALSE;
	LEGACYCONSOLE.info.hascolor = TRUE;
	LEGACYCONSOLE.info.caninit = TRUE;
	LEGACYCONSOLE.info.maxpairs = CON_NUMPAIRS;
	LEGACYCONSOLE.info.maxcolors = 8;
	LEGACYCONSOLE.info.numlabels = 0;
	LEGACYCONSOLE.info.labelwidth = 0;
	LEGACYCONSOLE.info.labelheight = 0;
	LEGACYCONSOLE.info.nocolorvideo = 1;
	LEGACYCONSOLE.info.tabsize = 8;
	LEGACYCONSOLE.info.numbuttons = LEGACYCONSOLE.numButtons;
	LEGACYCONSOLE.info.defaultPalette = _nc_cga_palette;
}

METHOD(AdjustSize, BOOL)(void)
{
	BOOL res = FALSE;
	COORD newSize;
	CONSOLE_SCREEN_BUFFER_INFO csbi;

	T((T_CALLED("win32_console::legacy_AdjustSize()")));

	assert(IsLegacyConsole());

	if (HasConsoleResizeLimitations())
	{
		T(("Console has resize limitations, skipping AdjustSize"));
		returnBool(res);
	}

	/*
	 * This piece is just for Windows 10 before the introduction of the new console:.
	 * In older Versions of Windows (before Windows 10), the conhost behaves differently
	 * when resizing the console window.
	 */
	if (!GetConsoleScreenBufferInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &csbi))
	{
		T(("GetConsoleScreenBufferInfo failed"));
		returnBool(res);
	}

	newSize.X = (short)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
	newSize.Y = (short)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);

	T(("New console size: %d x %d", newSize.X, newSize.Y));
	SetConsoleScreenBufferSize(LEGACYCONSOLE.core.ConsoleHandleOut, newSize);

	LEGACYCONSOLE.core.getSBI(&(LEGACYCONSOLE.SBI));
	LEGACYCONSOLE.core.sbi_lines = newSize.Y;
	LEGACYCONSOLE.core.sbi_cols = newSize.X;
	res = TRUE;

	returnBool(res);
}

METHOD(termattrs, chtype)(void)
{
	chtype res = A_NORMAL;

	assert(IsLegacyConsole());

	res |= (A_BOLD | A_DIM | A_REVERSE | A_STANDOUT | A_COLOR);
	return res;
}

METHOD(keypad, int)(BOOL flag)
{
	int code = ERR;
	SCREEN *sp;

	T((T_CALLED("win32_console::legacy_keypad(%d)"), flag));

	assert(IsLegacyConsole());

	sp = ConsoleScreen();
	if (sp)
	{
		sp->_keypad_on = flag;
		code = OK;
	}
	returnCode(code);
}

METHOD(beeporflash, int)(BOOL beepFlag)
{
	int res = ERR;

	int high, wide, max_cells;
	int i;

	assert(IsLegacyConsole());

	high = (LEGACYCONSOLE.SBI.srWindow.Bottom -
				LEGACYCONSOLE.SBI.srWindow.Top + 1);
	wide = (LEGACYCONSOLE.SBI.srWindow.Right -
				LEGACYCONSOLE.SBI.srWindow.Left + 1);
	max_cells = (high * wide);

	MakeArray(this_screen, CHAR_INFO, max_cells);
	MakeArray(that_screen, CHAR_INFO, max_cells);
	COORD this_size;
	SMALL_RECT this_region;
	COORD bufferCoord;

	this_region.Top = LEGACYCONSOLE.SBI.srWindow.Top;
	this_region.Left = LEGACYCONSOLE.SBI.srWindow.Left;
	this_region.Bottom = LEGACYCONSOLE.SBI.srWindow.Bottom;
	this_region.Right = LEGACYCONSOLE.SBI.srWindow.Right;

	this_size.X = (SHORT)wide;
	this_size.Y = (SHORT)high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
		read_screen(LEGACYCONSOLE.core.ConsoleHandleOut,
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

		write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, that_screen, this_size,
					 bufferCoord, &this_region);
		Sleep(200);
		write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, this_screen, this_size,
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

	T((T_CALLED("win32_console::legacy_keyok(%d, %d)"), keycode, flag));

	assert(IsLegacyConsole());

	res = bsearch(&key,
				  LEGACYCONSOLE.rmap,
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

	T((T_CALLED("win32_console::legacy_has_key(%d)"), keycode));

	assert(IsLegacyConsole());

	res = bsearch(&key,
				  LEGACYCONSOLE.rmap,
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

	assert(IsLegacyConsole());

	sp = ConsoleScreen();
	for (n = 0; n < SIZEOF(table); ++n)
	{
		real_map[table[n].acs_code] =
			(chtype)table[n].use_code | A_ALTCHARSET;
		if (sp != NULL)
			sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
}

METHOD(reset_color_pair, BOOL)(void)
{
	BOOL res = FALSE;
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;

	assert(IsLegacyConsole());

	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
	res = TRUE;
	return res;
}

METHOD(init_pair, int)(int pair, int f, int b)
{
	int code = ERR;
	int num_colors;

	assert(IsLegacyConsole());

	num_colors = LEGACYCONSOLE.info.maxcolors;
	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < num_colors) && (b >= 0) && (b < num_colors))
	{
		LEGACYCONSOLE.pairs[pair] =
			console_MapColor(TRUE, f) |
			console_MapColor(FALSE, b);
		T(("... legacy_init_pair: pair %d: fg=%d, bg=%d", pair, f, b));
		code = OK;
	}
	return code;
}

METHOD(setcolor, void)(BOOL fore, int color)
{
	WORD a = console_MapColor(fore, color);

	assert(IsLegacyConsole());

	a |= (WORD)((LEGACYCONSOLE.SBI.wAttributes) & ((BOOL)fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
}

METHOD(curs_set, int)(int visibility)
{
	int res = ERR;
	CONSOLE_CURSOR_INFO this_CI;

	T((T_CALLED("win32_console::legacy_curs_set(%d)"), visibility));

	assert(IsLegacyConsole());

	this_CI = LEGACYCONSOLE.save_CI;
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
	SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &this_CI);
	returnCode(res);
}

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)
#define BUTTON_MASK (FROM_LEFT_1ST_BUTTON_PRESSED | \
					 FROM_LEFT_2ND_BUTTON_PRESSED | \
					 FROM_LEFT_3RD_BUTTON_PRESSED | \
					 FROM_LEFT_4TH_BUTTON_PRESSED | \
					 RIGHTMOST_BUTTON_PRESSED)
#define NO_BUTTONS (mmask_t)0
#define IsMouseActive(sp) (sp->_mouse_active == TRUE)

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
		switch (LEGACYCONSOLE.numButtons)
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

static BOOL
handle_mouse(SCREEN *sp, MOUSE_EVENT_RECORD mer)
{
	MEVENT work;
	BOOL result = FALSE;

	assert(sp);

	T((T_CALLED("legacy_console::handle_mouse(%p, {pos=(%d,%d) mask=%08x})"),
	   sp, mer.dwMousePosition.X, mer.dwMousePosition.Y, mer.dwButtonState));

	if (!IsMouseActive(sp))
	{
		T(("... mouse is not active, ignoring event"));
		returnBool(FALSE);
	}
#ifdef NO_MOUSE_SUPPORT
	T(("... mouse support is disabled, ignoring event"));
	returnBool(FALSE);
#endif

	sp->_drv_mouse_old_buttons = sp->_drv_mouse_new_buttons;
	sp->_drv_mouse_new_buttons = mer.dwButtonState & BUTTON_MASK;

	/*
	 * We're only interested if the button is pressed or released.
	 * FIXME: implement continuous event-tracking.
	 */
	if (sp->_drv_mouse_new_buttons != sp->_drv_mouse_old_buttons)
	{
		T(("... button state changed: old=%08x new=%08x",
		   sp->_drv_mouse_old_buttons, sp->_drv_mouse_new_buttons));

		memset(&work, 0, sizeof(work));

		if (sp->_drv_mouse_new_buttons)
		{
			work.bstate |= filter_button_events(sp, sp->_drv_mouse_new_buttons);
		}
		else
		{
			T(("... button state cleared, reporting release"));
			/* cf: BUTTON_PRESSED, BUTTON_RELEASED */
			work.bstate |= (filter_button_events(sp, sp->_drv_mouse_old_buttons) >> 1);
			result = TRUE;
		}

		work.x = mer.dwMousePosition.X;
		work.y = mer.dwMousePosition.Y - AdjustY();
		T(("... event at (%d, %d), bstate=%08x", work.x, work.y, work.bstate));

		if (sp->_drv_mouse_tail < 0 || sp->_drv_mouse_tail >= FIFO_SIZE)
		{
			T(("... mouse FIFO overflow, dropping event"));
			returnBool(FALSE);
		}
		assert(sp->_drv_mouse_tail >= 0);
		assert(sp->_drv_mouse_tail < FIFO_SIZE);
		sp->_drv_mouse_fifo[sp->_drv_mouse_tail] = work;
		sp->_drv_mouse_tail += 1;
	}
	returnBool(result);
}

METHOD(read, int)(int *buf)
{
	int rc = -1;
	INPUT_RECORD inp_rec;
	BOOL b;
	DWORD nRead;
	WORD vk;
	HANDLE hdl = INVALID_HANDLE_VALUE;
	SCREEN *sp;

	assert(IsLegacyConsole());

	sp = ConsoleScreen();
	assert(buf);
	assert(sp);

	hdl = LEGACYCONSOLE.core.ConsoleHandleIn;
	memset(&inp_rec, 0, sizeof(inp_rec));

	T((T_CALLED("win32_console::legacy_read(%p)"), buf));

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
				*buf = (int)inp_rec.Event.KeyEventChar;
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
				handle_mouse(sp, inp_rec.Event.MouseEvent);
#ifdef NO_MOUSE_SUPPORT
					/* Mouse support is disabled, so we ignore the event. */
					continue;
				#else
					/* We report mouse events as KEY_MOUSE, and the actual event data is stored in the mouse FIFO. */
				*buf = KEY_MOUSE;
				break;
#endif
			}
			else if (inp_rec.EventType == WINDOW_BUFFER_SIZE_EVENT)
			{
				SetConsolePendingResize();
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
	BOOL b;
	DWORD nRead = 0, rc = WAIT_FAILED;
	int code = 0;
	struct timeval fstart;
	struct timeval fend;
	int diff;
	bool isNoDelay = (milliseconds == 0);

	TR(TRACE_IEVENT, ("start twait: %d milliseconds, mode: %d",
				  milliseconds, mode));

	assert(IsLegacyConsole());

	sp = ConsoleScreen();
	hdl = LEGACYCONSOLE.core.ConsoleHandleIn;
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

			T(("twait: WaitForSingleObject returned %d, fstart=%llu, fend=%llu, diff=%d msec, remaining=%d msec",
			   (int)rc, fstart, fend, diff, milliseconds));

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
			BOOL stop_scan;

			b = GetNumberOfConsoleInputEvents(hdl, &n);
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
									   pInpRec[i].Event.KeyEventChar));
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
								stop_scan = TRUE;
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
							if (filter_button_events(sp,
									pInpRec[i].Event.MouseEvent.dwButtonState & BUTTON_MASK) != NO_BUTTONS)
							{
								/* Relevant button state: leave at queue head for reader */
								T(("twait:event MOUSE_EVENT: buttonState=%08lx",
								   (unsigned long)pInpRec[i].Event.MouseEvent.dwButtonState));
								code |= TW_MOUSE;
								goto end;
							}
							/* No relevant buttons (mouse move etc.): consume it */
							T(("twait: consuming irrelevant MOUSE_EVENT"));
							CONSUME();
#endif
						}
						else
						{
							/* Mouse event but TW_MOUSE not requested: may be needed
							 * by a later call -- stop scanning without consuming */
							T(("twait: MOUSE_EVENT not in mode, stopping scan"));
							stop_scan = TRUE;
						}
						break;

					case WINDOW_BUFFER_SIZE_EVENT:
						T(("twait:event WINDOW_BUFFER_SIZE_EVENT"));
						CONSUME();
						SetConsolePendingResize();
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

METHOD(testmouse,int)(int delay EVENTLIST_2nd(_nc_eventlist *))
{
	int rc = 0;
	SCREEN *sp;

	T((T_CALLED("legacy_console::legacy__testmouse(%d)"), delay));

	assert(IsLegacyConsole());

	sp = ConsoleScreen();
	assert(sp);

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail)
	{
		rc = TW_MOUSE;
	}
	else
	{
		rc = DispatchMethod(twait)(TWAIT_MASK, delay, (int *)0 EVENTLIST_2nd(evl));
	}

	returnCode(rc);
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
	BOOL input_ok = FALSE;
	BOOL output_ok = FALSE;
	SCREEN *sp = NULL;

	T((T_CALLED("win32_driver::legacy_setmode(fd=%d, TTY*=%p)"), fd, arg));

	assert(IsLegacyConsole());

	input_target = LEGACYCONSOLE.core.ConsoleHandleIn;
	output_target = LEGACYCONSOLE.core.ConsoleHandleOut;
	sp = ConsoleScreen();

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

		input_ok = SetConsoleMode(input_target, mode);
		if (input_ok)
		{
			/* Make sure the cached value reflects the real value we set, as the
			 * caller may not have provided all necessary flags (e.g.
			 * ENABLE_PROCESSED_INPUT when VT is requested) */
			DWORD realMode;
			if (GetConsoleMode(input_target, &realMode))
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagIn = realMode;
			}
			else
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagIn = mode;
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
		output_ok = SetConsoleMode(output_target, mode);
		if (output_ok)
		{
			/* Make sure the cached value reflects the real value we set,
			 * as the caller may not have provided all necessary flags
			 * (e.g. VT output is required for the Windows Console backend) */
			DWORD realMode;
			if (GetConsoleMode(output_target, &realMode))
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagOut = realMode;
			}
			else
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagOut = mode;
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
		if (IsConsoleProgMode())
		{
			ClearConsoleProgMode();
			if (sp)
			{
				_nc_keypad(sp, FALSE);
				NCURSES_SP_NAME(_nc_flush)(sp);
			}

			if (LEGACYCONSOLE.hShellMode != INVALID_HANDLE_VALUE)
			{
				T(("... LEGACYCONSOLE: switching to shell mode buffer"));
				LEGACYCONSOLE.core.ConsoleHandleOut = LEGACYCONSOLE.hShellMode;
				SetConsoleActiveScreenBuffer(LEGACYCONSOLE.core.ConsoleHandleOut);
				SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
			}
			else
			{
				T(("... LEGACYCONSOLE: no valid shell mode buffer"));
			}
		}
		else
		{
			T(("... LEGACYCONSOLE: Already in shell mode"));
		}
	}
	else if (arg->kind == TTY_MODE_PROGRAM)
	{
		T(("Program mode set"));
		if (!IsConsoleProgMode())
		{
			SetConsoleProgMode();
			if (sp)
			{
				if (sp->_keypad_on)
					_nc_keypad(sp, TRUE);
			}

			if (LEGACYCONSOLE.hProgMode != INVALID_HANDLE_VALUE)
			{
				T(("... LEGACYCONSOLE: switching to program mode buffer"));
				LEGACYCONSOLE.core.ConsoleHandleOut = LEGACYCONSOLE.hProgMode;
				SetConsoleActiveScreenBuffer(LEGACYCONSOLE.core.ConsoleHandleOut);
				SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
			}
			else
			{
				T(("... LEGACYCONSOLE: no valid program mode buffer"));
			}
		}
		else
		{
			T(("... LEGACYCONSOLE: Already in program mode"));
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
	T((T_CALLED("win32_driver::legacy_getmode(fd=%d, TTY*=%p)"), fd, arg));

	assert(IsLegacyConsole());

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
METHOD(defmode, int)(TTY *arg, short kind)
{
	short realMode = kind;

	T((T_CALLED("win32_driver::legacy_defmode(TTY*=%p, kind=%d)"), arg, kind));

	assert(IsLegacyConsole());

	if (NULL == arg)
		returnCode(ERR);

	if (realMode == TTY_MODE_AUTO)
	{
		realMode = IsConsoleProgMode() ? TTY_MODE_PROGRAM : TTY_MODE_SHELL;
	}

	arg->kind = realMode;
	returnCode(OK);
}

METHOD(size, void)(int *Lines, int *Cols)
{

	assert(IsLegacyConsole());

	if (Lines != NULL && Cols != NULL)
	{
		*Lines = (int)(LEGACYCONSOLE.SBI.srWindow.Bottom + 1 -
					   LEGACYCONSOLE.SBI.srWindow.Top);
		*Cols = (int)(LEGACYCONSOLE.SBI.srWindow.Right + 1 -
					  LEGACYCONSOLE.SBI.srWindow.Left);
		T(("win32_driver::legacy_size() returns %d lines, %d cols", *Lines, *Cols));
	}
}

METHOD(size_changed, BOOL)(void)
{
	BOOL resized = FALSE;
	T((T_CALLED("win32_console::legacy_size_changed()")));

	assert(IsLegacyConsole());

	if (HasConsolePendingResize())
	{
		T(("Resize event pending, returning TRUE"));
		resized = TRUE;
		ClearConsoleResizeLimitations();
		_nc_globals.have_sigwinch = 1;
	}
	returnBool(resized);
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
METHOD(init, BOOL)(int fdOut, int fdIn)
{
	BOOL result = FALSE;

	T((T_CALLED("win32_driver::legacy_init(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

	assert(IsLegacyConsole());

	/* initialize once, or not at all */
	if (!IsConsoleInitialized())
	{
		/*
		 * We set the console mode flags to the most basic ones that are required for ConPTY
		 * to function properly. */
		DWORD dwFlagIn = (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT | ENABLE_EXTENDED_FLAGS);
		DWORD dwFlagOut = (ENABLE_PROCESSED_OUTPUT | DISABLE_NEWLINE_AUTO_RETURN | ENABLE_WRAP_AT_EOL_OUTPUT);
		DWORD dwFlag;

		HANDLE stdin_handle = INVALID_HANDLE_VALUE;
		HANDLE stdout_handle = INVALID_HANDLE_VALUE;

		if (fdIn != -1)
		{
			T(("In the first call fdIn is expected to be -1."));
			returnBool(FALSE);
		}

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

		LEGACYCONSOLE.hShellMode = stdout_handle;

		/* Especially with UCRT and wide mode, make sure we use an UTF-8 capable locale.
		 * At least we set the codepage to a proper value that's either compatible with
		 * ASCII or UTF-8, to ensure that the console can display characters properly.
		 * The actual locale setting is not that important, as long as the code page is set
		 * correctly, because we handle UTF-8 encoding and decoding ourselves and we don't
		 * rely on the C runtime for that. */
		// encoding_init();

		if (stdout_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdout_handle,
																	&dwFlag) == 0)
		{
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
		if (GetConsoleMode(stdout_handle, &dwFlagOut) == 0)
		{
			T(("GetConsoleMode() failed for stdout"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ttyflags.dwFlagOut = dwFlagOut;

		dwFlagIn &= ~(ENABLE_QUICK_EDIT_MODE);
		SetConsoleMode(stdin_handle, dwFlagIn);
		/* We immediately read the console mode back to reflect any changes the
		 * runtime may have added, so the saved value reflects the actual mode
		 * of the console. */
		if (GetConsoleMode(stdin_handle, &dwFlagIn) == 0)
		{
			T(("GetConsoleMode() failed for stdin"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ttyflags.dwFlagIn = dwFlagIn;

		_nc_legacy_console_init();

		LEGACYCONSOLE.core.ConsoleHandleIn = stdin_handle;
		LEGACYCONSOLE.core.ConsoleHandleOut = stdout_handle;
		MarkConsoleInitialized();
		result = TRUE;
	}
	else
	{
		/* This branch is called from newterm() when fdIn is provided, so we need to validate
		 * that the provided fdIn and fdOut are valid pseudo-console handles, and if so we
		 * update the defaultCONPTY structure to use the new handles. */
		DWORD dwFlagOut;

		if (LEGACYCONSOLE.hProgMode == INVALID_HANDLE_VALUE)
		{
			T(("... creating console buffer"));
			/* Save the original active screen buffer handle so we can switch
			 * back to it when endwin() / reset_shell_mode is called. */
			LEGACYCONSOLE.hProgMode =
				CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
										  FILE_SHARE_READ | FILE_SHARE_WRITE,
										  NULL,
										  CONSOLE_TEXTMODE_BUFFER,
										  NULL);
		}
		if (LEGACYCONSOLE.hProgMode == INVALID_HANDLE_VALUE || GetConsoleMode(LEGACYCONSOLE.hProgMode, &dwFlagOut) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		result = TRUE;
	}
	returnBool(result);
}

METHOD(mvcur,int)(int oldrow GCC_UNUSED, int oldcol GCC_UNUSED, int newrow, int newcol)
{
	int result = ERR;
	COORD pos;

	assert(IsLegacyConsole());

	pos.X = (SHORT)newcol;
	pos.Y = (SHORT)newrow;
	if (SetConsoleCursorPosition(LEGACYCONSOLE.core.ConsoleHandleOut, pos))
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
			a = LEGACYCONSOLE.pairs[p];
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
 * TODO: _nc_wacs should be part of sp.
 */
static BOOL
con_write16(int y, int x, cchar_t *str, int limit)
{
	int actual = 0;
	MakeArray(ci, CHAR_INFO, limit);
	COORD loc, siz;
	SMALL_RECT rec;
	int i;
	cchar_t ch;
	SCREEN *sp = ConsoleScreen();


	for (i = actual = 0; i < limit; i++)
	{
		ch = str[i];
		if (isWidecExt(ch))
			continue;
		ci[actual].CharInfoChar = CharOf(ch);
		ci[actual].Attributes = MapAttr(LEGACYCONSOLE.SBI.wAttributes,
										AttrOf(ch));
		if (AttrOf(ch) & A_ALTCHARSET)
		{
			if (_nc_wacs)
			{
				int which = CharOf(ch);
				if (which > 0 && which < ACS_LEN && CharOf(_nc_wacs[which]) != 0)
				{
					ci[actual].CharInfoChar = CharOf(_nc_wacs[which]);
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

	return write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, ci, siz, loc, &rec);
}


#define con_write(y, x, str, n) con_write16(y, x, str, n)
#else
static BOOL
con_write8(int y, int x, chtype *str, int n)
{
	MakeArray(ci, CHAR_INFO, n);
	COORD loc, siz;
	SMALL_RECT rec;
	int i;
	chtype ch;
	SCREEN *sp = ConsoleScreen();

	for (i = 0; i < n; i++)
	{
		ch = str[i];
		ci[i].CharInfoChar = ChCharOf(ch);
		ci[i].Attributes = MapAttr(LEGACYCONSOLE.SBI.wAttributes,
								   ChAttrOf(ch));
		if (ChAttrOf(ch) & A_ALTCHARSET)
		{
			if (sp->_acs_map)
				ci[i].CharInfoChar =
					ChCharOf(NCURSES_SP_NAME(_nc_acs_char)(sp, ChCharOf(ch)));
		}
	}

	loc.X = (short)0;
	loc.Y = (short)0;
	siz.X = (short)n;
	siz.Y = 1;

	rec.Left = (short)x;
	rec.Top = (short)y;
	rec.Right = (short)(x + n - 1);
	rec.Bottom = rec.Top;

	return write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, ci, siz, loc, &rec);
}
#define con_write(y, x, str, n) con_write8(y, x, str, n)
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

	while (col <= newdat->lastchar)
	{
#if USE_WIDEC_SUPPORT
		if (isWidecExt(curdat->text[col]) ||
			isWidecExt(newdat->text[col]))
		{
			result = col;
		}
		else if (memcmp(&curdat->text[col],
						&newdat->text[col],
						sizeof(curdat->text[0])))
		{
			result = col;
		}
		else
		{
			break;
		}
#else
		if (curdat->text[col] != newdat->text[col])
		{
			result = col;
		}
		else
		{
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

	while (++col <= newdat->lastchar)
	{
#if USE_WIDEC_SUPPORT
		if (isWidecExt(curdat->text[col]) !=
			isWidecExt(newdat->text[col]))
		{
			result = col;
			break;
		}
		else if (memcmp(&curdat->text[col],
						&newdat->text[col],
						sizeof(curdat->text[0])))
		{
			result = col;
			break;
		}
#else
		if (curdat->text[col] != newdat->text[col])
		{
			result = col;
			break;
		}
#endif
	}
	return result;
}

#define EndChange(first) \
	find_end_of_change(sp, y, first)
#define NextChange(last) \
	find_next_change(sp, y, last)

#endif /* EXP_OPTIMIZE */

#define MARK_NOCHANGE(win, row)            \
	win->_line[row].firstchar = _NOCHANGE; \
	win->_line[row].lastchar = _NOCHANGE

METHOD(doupdate,int)(void)
{
	int result = ERR;
	int y, nonempty, n, x0, x1, Width, Height;
	SCREEN *sp;

	T((T_CALLED("legacy_console::legacy_doupdate()")));

	assert(IsLegacyConsole());

	sp = ConsoleScreen();

	Width = screen_columns(sp);
	Height = screen_lines(sp);
	nonempty = Min(Height, NewScreen(sp)->_maxy + 1);

	T(("... %dx%d clear cur:%d new:%d",
	   Height, Width,
	   CurScreen(sp)->_clear,
	   NewScreen(sp)->_clear));

	if (SP_PARM->_endwin == ewSuspend)
	{

		T(("coming back from shell mode"));
		NCURSES_SP_NAME(reset_prog_mode)(NCURSES_SP_ARG);

		NCURSES_SP_NAME(_nc_mvcur_resume)(NCURSES_SP_ARG);
		NCURSES_SP_NAME(_nc_screen_resume)(NCURSES_SP_ARG);
		SP_PARM->_mouse_resume(SP_PARM);

		SP_PARM->_endwin = ewRunning;
	}

	if ((CurScreen(sp)->_clear || NewScreen(sp)->_clear))
	{
		int x;
#if USE_WIDEC_SUPPORT
		MakeArray(empty, cchar_t, Width);
		wchar_t blank[2] =
			{
				L' ', L'\0'};

		for (x = 0; x < Width; x++)
			setcchar(&empty[x], blank, 0, 0, NULL);
#else
		MakeArray(empty, chtype, Width);

		for (x = 0; x < Width; x++)
			empty[x] = ' ';
#endif

		for (y = 0; y < nonempty; y++)
		{
			con_write(y, 0, empty, Width);
			memcpy(empty,
				   CurScreen(sp)->_line[y].text,
				   (size_t)Width * sizeof(empty[0]));
		}
		CurScreen(sp)->_clear = FALSE;
		NewScreen(sp)->_clear = FALSE;
		touchwin(NewScreen(sp));
		T(("... cleared %dx%d lines @%d of screen", nonempty, Width,
		   AdjustY()));
	}

	for (y = 0; y < nonempty; y++)
	{
		x0 = NewScreen(sp)->_line[y].firstchar;
		if (x0 != _NOCHANGE)
		{
#if EXP_OPTIMIZE
			int x2;
			int limit = NewScreen(sp)->_line[y].lastchar;
			while ((x1 = EndChange(x0)) <= limit)
			{
				while ((x2 = NextChange(x1)) <=
						   limit &&
					   x2 <= (x1 + 2))
				{
					x1 = x2;
				}
				n = x1 - x0 + 1;
				memcpy(&CurScreen(sp)->_line[y].text[x0],
					   &NewScreen(sp)->_line[y].text[x0],
					   n * sizeof(CurScreen(sp)->_line[y].text[x0]));
				con_write(y,
						  x0,
						  &CurScreen(sp)->_line[y].text[x0], n);
				x0 = NextChange(x1);
			}

			/* mark line changed successfully */
			if (y <= NewScreen(sp)->_maxy)
			{
				MARK_NOCHANGE(NewScreen(sp), y);
			}
			if (y <= CurScreen(sp)->_maxy)
			{
				MARK_NOCHANGE(CurScreen(sp), y);
			}
#else
			x1 = NewScreen(sp)->_line[y].lastchar;
			n = x1 - x0 + 1;
			if (n > 0)
			{
				memcpy(&CurScreen(sp)->_line[y].text[x0],
					   &NewScreen(sp)->_line[y].text[x0],
					   (size_t)n *
						   sizeof(CurScreen(sp)->_line[y].text[x0]));
				con_write(y,
						  x0,
						  &CurScreen(sp)->_line[y].text[x0], n);

				/* mark line changed successfully */
				if (y <= NewScreen(sp)->_maxy)
				{
					MARK_NOCHANGE(NewScreen(sp), y);
				}
				if (y <= CurScreen(sp)->_maxy)
				{
					MARK_NOCHANGE(CurScreen(sp), y);
				}
			}
#endif
		}
	}

	/* put everything back in sync */
	for (y = nonempty; y <= NewScreen(sp)->_maxy; y++)
	{
		MARK_NOCHANGE(NewScreen(sp), y);
	}
	for (y = nonempty; y <= CurScreen(sp)->_maxy; y++)
	{
		MARK_NOCHANGE(CurScreen(sp), y);
	}

	if (!NewScreen(sp)->_leaveok)
	{
		CurScreen(sp)->_curx = NewScreen(sp)->_curx;
		CurScreen(sp)->_cury = NewScreen(sp)->_cury;
		LEGACYCONSOLE.mvcur(0, 0, CurScreen(sp)->_cury, CurScreen(sp)->_curx);
	}
	// selectActiveHandle();
	result = OK;

	returnCode(result);
}
#endif /* USE_LEGACY_CONSOLE */
