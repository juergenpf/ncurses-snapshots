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

#define EXP_OPTIMIZE 0
#define AdjustY() 0

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

NCURSES_EXPORT(int)
_nc_win32con_doupdate (SCREEN *sp)
{
	int result = ERR;
	int y, nonempty, n, x0, x1, Width, Height;

	T((T_CALLED("_nc_win32con_doupdate(%p)"), (void *) sp));

	assert(ScreenIsBufferedConsole(sp));

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
			AsScreenBufferedConsole(sp)->writeat(y, 0, empty, Width);
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
				AsScreenBufferedConsole(sp)->writeat(y,
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
				AsScreenBufferedConsole(sp)->writeat(y,
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
		AsScreenBufferedConsole(sp)->mvcur(0, 0, CurScreen(sp)->_cury, CurScreen(sp)->_curx);
	}
	// selectActiveHandle();
	result = OK;

	returnCode(result);
}

#endif /* USE_SCREENBUFFERED_CONSOLE */
