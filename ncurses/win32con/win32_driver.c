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



static void
wcon_init(TERMINAL_CONTROL_BLOCK *TCB)
{
	T((T_CALLED("win32_driver::wcon_init(%p)"), TCB));

	AssertTCB();
	returnVoid;
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


NCURSES_EXPORT_VAR(TERM_DRIVER)
_nc_WIN_DRIVER = {
	FALSE,
	wcon_name,			/* Name          */
	wcon_CanHandle,		/* CanHandle     */
	wcon_init,			/* init          */
	wcon_mvcur,			/* hwcur         */
	wcon_setfilter,		/* setfilter     */
	wcon_doupdate		/* update        */
};
#endif /* USE_LEGACY_CONSOLE */
