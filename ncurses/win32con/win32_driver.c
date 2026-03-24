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
#include <curses.priv.h>

#if USE_LEGACY_CONSOLE
#include <windows.h>

#define CUR TerminalType(my_term).

MODULE_ID("$Id: win32_driver.c,v 1.20 2025/12/30 19:34:50 tom Exp $")

#define WINMAGIC NCDRV_MAGIC(NCDRV_WINCONSOLE)
#define EXP_OPTIMIZE 0

#define console_initialized (IsConsoleInitialized())

#define AssertTCB() assert(TCB != NULL && (TCB->magic == WINMAGIC))
#define SetSP()               \
	assert(TCB->csp != NULL); \
	sp = TCB->csp;            \
	(void)sp

#define AdjustY() 0

#define RevAttr(attr) (WORD)(((attr) & 0xff00) |       \
							 ((((attr) & 0x07) << 4) | \
							  (((attr) & 0x70) >> 4)))

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)


static BOOL get_SBI(void);
static WORD console_MapColor(BOOL fore, int color);
static int console_twait(
	const SCREEN *sp,
	HANDLE hdl,
	int mode,
	int milliseconds,
	int *timeleft
		EVENTLIST_2nd(_nc_eventlist *evl));
static int console_read(
	SCREEN *sp,
	HANDLE hdl,
	int *buf);


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

// ----------------------------- Keyboard related definitions and functions ---------------------------------

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

/* *INDENT-ON* */
#define array_length(a) (sizeof(a) / sizeof(a[0]))
#define N_INI ((int)array_length(keylist))
#define FKEYS 24
#define MAPSIZE (FKEYS + N_INI)

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


// ------------------------------- Color related definitions and functions -----------------------------------

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

static void
wcon_setcolor(TERMINAL_CONTROL_BLOCK *TCB,
			  int fore,
			  int color,
			  int (*outc)(SCREEN *, int) GCC_UNUSED)
{
	WORD a = console_MapColor((BOOL)fore, color);
	(void)TCB;
	a |= (WORD)((LEGACYCONSOLE.SBI.wAttributes) & ((BOOL)fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
}

static int
wcon_defaultcolors(TERMINAL_CONTROL_BLOCK *TCB,
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
wcon_rescolors(TERMINAL_CONTROL_BLOCK *TCB)
{
	int result = FALSE;
	SCREEN *sp;

	AssertTCB();
	SetSP();

	return result;
}

static void
wcon_do_color(TERMINAL_CONTROL_BLOCK *TCB,
			  int old_pair GCC_UNUSED,
			  int pair GCC_UNUSED,
			  int reverse GCC_UNUSED,
			  int (*outc)(SCREEN *, int) GCC_UNUSED)
{
	SCREEN *sp;

	AssertTCB();
	SetSP();
}

static void
wcon_initpair(TERMINAL_CONTROL_BLOCK *TCB,
			  int pair,
			  int f,
			  int b)
{
	SCREEN *sp;

	SetSP();

	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < 8) && (b >= 0) && (b < 8))
	{
		LEGACYCONSOLE.pairs[pair] =
			console_MapColor(TRUE, f) |
			console_MapColor(FALSE, b);
		T(("... wcon_initpair: pair %d: fg=%d, bg=%d", pair, f, b));
	}
}

static void
wcon_initcolor(TERMINAL_CONTROL_BLOCK *TCB,
			   int color GCC_UNUSED,
			   int r GCC_UNUSED,
			   int g GCC_UNUSED,
			   int b GCC_UNUSED)
{
	SCREEN *sp;

	AssertTCB();
	SetSP();
}

// ------------------------------ Meta-Data related definitions and functions ---------------------------------

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


static int
wcon_size(TERMINAL_CONTROL_BLOCK *TCB, int *Lines, int *Cols)
{
	int result = ERR;

	T((T_CALLED("win32_driver::wcon_size(%p)"), TCB));

	if ((Lines != NULL) && (Cols != NULL))
	{
		LEGACYCONSOLE.core.size(Lines, Cols);
		result = OK;
	}
	returnCode(result);
}

static int
wcon_setsize(TERMINAL_CONTROL_BLOCK *TCB GCC_UNUSED,
			 int l GCC_UNUSED,
			 int c GCC_UNUSED)
{
	AssertTCB();
	return ERR;
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
con_write16(TERMINAL_CONTROL_BLOCK *TCB,
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
#define con_write(tcb, y, x, str, n) con_write16(tcb, y, x, str, n)
#else
static BOOL
con_write8(TERMINAL_CONTROL_BLOCK *TCB, int y, int x, chtype *str, int n)
{
	MakeArray(ci, CHAR_INFO, n);
	COORD loc, siz;
	SMALL_RECT rec;
	int i;
	chtype ch;
	SCREEN *sp;

	AssertTCB();
	SetSP();

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

static int
wcon_doupdate(TERMINAL_CONTROL_BLOCK *TCB)
{
	int result = ERR;
	int y, nonempty, n, x0, x1, Width, Height;
	SCREEN *sp;

	T((T_CALLED("win32_driver::wcon_doupdate(%p)"), TCB));
	SetSP();

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
			con_write(TCB, y, 0, empty, Width);
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
				con_write(TCB,
						  y,
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
				con_write(TCB,
						  y,
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

		TCB->drv->td_hwcur(TCB,
						   0,
						   0,
						   CurScreen(sp)->_cury,
						   CurScreen(sp)->_curx);
	}
	// selectActiveHandle();
	result = OK;

	returnCode(result);
}

static const char *
wcon_name(TERMINAL_CONTROL_BLOCK *TCB)
{
	(void)TCB;
	return "win32console";
}

static bool
wcon_CanHandle(TERMINAL_CONTROL_BLOCK *TCB,
			   const char *tname,
			   int *errret GCC_UNUSED)
{
	bool code = FALSE;

	T((T_CALLED("win32_driver::wcon_CanHandle(%p,%s,%p)"),
	   (void *)TCB, NonNull(tname), (void *)errret));

	assert(TCB != NULL);

	TCB->magic = WINMAGIC;

	if (tname == NULL || *tname == 0)
	{
		code = TRUE;
	}
	else if (tname != NULL && *tname == '#')
	{
		/*
		 * Use "#" (a character which cannot begin a terminal's name) to
		 * select specific driver from the table.
		 *
		 * In principle, we could have more than one non-terminfo driver,
		 * e.g., "win32gui".
		 */
		size_t n = strlen(tname + 1);
		if (n != 0 && ((strncmp(tname + 1, "win32console", n) == 0) || (strncmp(tname + 1, "win32con", n) == 0)))
		{
			code = TRUE;
		}
	}
	else if (tname != NULL && stricmp(tname, "unknown") == 0)
	{
		code = TRUE;
	}

	/*
	 * This is intentional, to avoid unnecessary breakage of applications
	 * using <term.h> symbols.
	 */
	if (code && (TerminalType(&TCB->term).Booleans == 0))
	{
		_nc_init_termtype(&TerminalType(&TCB->term));
#if NCURSES_EXT_NUMBERS
		_nc_export_termtype2(&TCB->term.type, &TerminalType(&TCB->term));
#endif
	}
	returnBool(code);
}

static bool
wcon_rescol(TERMINAL_CONTROL_BLOCK *TCB)
{
	bool res = FALSE;

	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;
	(void)TCB;
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
	res = TRUE;
	return res;
}


static void
wcon_release(TERMINAL_CONTROL_BLOCK *TCB)
{
	T((T_CALLED("win32_driver::wcon_release(%p)"), TCB));

	AssertTCB();
	if (TCB->prop)
		free(TCB->prop);

	returnVoid;
}


static void
wcon_init(TERMINAL_CONTROL_BLOCK *TCB)
{
	T((T_CALLED("win32_driver::wcon_init(%p)"), TCB));

	AssertTCB();
	returnVoid;
}

static int
wcon_testmouse(TERMINAL_CONTROL_BLOCK *TCB,
			   int delay
				   EVENTLIST_2nd(_nc_eventlist *evl)) 
{
	int rc = 0;
	SCREEN *sp;

	T((T_CALLED("win32_driver::wcon_testmouse(%p)"), TCB));
	SetSP();

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail)
	{
		rc = TW_MOUSE;
	}
	else
	{
		rc = TCBOf(sp)->drv->td_twait(TCBOf(sp),
									  TWAIT_MASK,
									  delay,
									  (int *)0 EVENTLIST_2nd(evl));
	}

	returnCode(rc);
}

static int
wcon_mvcur(TERMINAL_CONTROL_BLOCK *TCB,
		   int yold GCC_UNUSED, int xold GCC_UNUSED,
		   int y, int x)
{
	COORD loc;
	int ret = ERR;

	(void)TCB;
	loc.X = (short)x;
	loc.Y = (short)(y + AdjustY());
	SetConsoleCursorPosition(LEGACYCONSOLE.core.ConsoleHandleOut, loc);
	ret = OK;
	return ret;
}

static void
wcon_setfilter(TERMINAL_CONTROL_BLOCK *TCB)
{
	SCREEN *sp;

	AssertTCB();
	SetSP();
}

static void
wcon_initacs(TERMINAL_CONTROL_BLOCK *TCB,
			 chtype *real_map GCC_UNUSED,
			 chtype *fake_map GCC_UNUSED)
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
	SetSP();

	for (n = 0; n < SIZEOF(table); ++n)
	{
		real_map[table[n].acs_code] =
			(chtype)table[n].use_code | A_ALTCHARSET;
		if (sp != NULL)
			sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
}

static int
wcon_twait(TERMINAL_CONTROL_BLOCK *TCB,
		   int mode,
		   int milliseconds,
		   int *timeleft
			   EVENTLIST_2nd(_nc_eventlist *evl))
{
	SCREEN *sp;
	int code = 0;

	SetSP();

	code = console_twait(sp,
						 LEGACYCONSOLE.core.ConsoleHandleIn,
						 mode,
						 milliseconds,
						 timeleft EVENTLIST_2nd(evl));
	return code;
}

static int
wcon_read(TERMINAL_CONTROL_BLOCK *TCB, int *buf)
{
	SCREEN *sp;
	int n = -1;

	T((T_CALLED("win32_driver::wcon_read(%p)"), TCB));

	assert(buf);
	SetSP();

	n = console_read(sp, LEGACYCONSOLE.core.ConsoleHandleIn, buf);
	returnCode(n);
}


static int
wcon_cursorSet(TERMINAL_CONTROL_BLOCK *TCB GCC_UNUSED, int mode)
{
	int res = -1;
	CONSOLE_CURSOR_INFO this_CI = LEGACYCONSOLE.save_CI;

	T((T_CALLED("win32_driver::wcon_cursorSet(%d)"), mode));
	switch (mode)
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

	T((T_CALLED("decode_mouse(%p, %08x)"), sp, mask));

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

	T((T_CALLED("handle_mouse(%p, {pos=(%d,%d) mask=%08x})"),
	   sp, mer.dwMousePosition.X, mer.dwMousePosition.Y, mer.dwButtonState));

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
			work.bstate |= decode_mouse(sp, sp->_drv_mouse_new_buttons);
		}
		else
		{
			T(("... button state cleared, reporting release"));
			/* cf: BUTTON_PRESSED, BUTTON_RELEASED */
			work.bstate |= (decode_mouse(sp, sp->_drv_mouse_old_buttons) >> 1);
			result = TRUE;
		}

		work.x = mer.dwMousePosition.X;
		work.y = mer.dwMousePosition.Y - AdjustY();

		sp->_drv_mouse_fifo[sp->_drv_mouse_tail] = work;
		sp->_drv_mouse_tail += 1;
	}
	returnBool(result);
}

static void handle_resize()
{
	if (console_initialized)
	{
		SetConsolePendingResize();
	}
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

static int
console_twait(
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
									if ((0 == ch))
									{
										int nKey = MapKey(vk);
										if (nKey < 0)
										{
											CONSUME();
											continue;
										}
									}
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
							/* e.g., FOCUS_EVENT */
						case WINDOW_BUFFER_SIZE_EVENT:
							T(("twait:event WINDOW_BUFFER_SIZE_EVENT"));
							CONSUME();
							handle_resize();
							continue;
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
				if (handle_mouse(sp,
								 inp_rec.Event.MouseEvent))
				{
					*buf = KEY_MOUSE;
					break;
				}
			}
			else if (inp_rec.EventType == WINDOW_BUFFER_SIZE_EVENT)
			{
				handle_resize();
			}
			continue;
		}
	}
	returnCode(rc);
}

NCURSES_EXPORT_VAR(TERM_DRIVER)
_nc_WIN_DRIVER = {
	FALSE,
	wcon_name,			/* Name          */
	wcon_CanHandle,		/* CanHandle     */
	wcon_init,			/* init          */
	wcon_release,		/* release       */
	wcon_size,			/* size          */
	wcon_mvcur,			/* hwcur         */
	wcon_rescol,		/* rescol        */
	wcon_rescolors,		/* rescolors     */
	wcon_setcolor,		/* color         */
	wcon_initpair,		/* initpair      */
	wcon_initcolor,		/* initcolor     */
	wcon_do_color,		/* docolor       */
	wcon_testmouse,		/* testmouse     */
	wcon_setfilter,		/* setfilter     */
	wcon_doupdate,		/* update        */
	wcon_defaultcolors, /* defaultcolors */
	wcon_setsize,		/* setsize       */
	wcon_initacs,		/* initacs       */
	wcon_twait,			/* twait         */
	wcon_read,			/* read          */
	wcon_cursorSet		/* cursorSet     */
};
#endif /* USE_LEGACY_CONSOLE */