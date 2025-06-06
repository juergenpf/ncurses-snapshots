/****************************************************************************
 * Copyright 2020-2023,2024 Thomas E. Dickey                                *
 * Copyright 1998-2008,2009 Free Software Foundation, Inc.                  *
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
 *  Author: Zeyd M. Ben-Halim <zmbenhal@netcom.com> 1992,1995               *
 *     and: Eric S. Raymond <esr@snark.thyrsus.com>                         *
 *     and: Thomas E. Dickey                        1996-on                 *
 *     and: Juergen Pfeifer                         2008                    *
 ****************************************************************************/

/*
**	lib_delwin.c
**
**	The routine delwin().
**
*/

#include <curses.priv.h>

MODULE_ID("$Id: lib_delwin.c,v 1.29 2024/12/07 17:28:13 tom Exp $")

static bool
cannot_delete(const WINDOW *win)
{
    bool result = TRUE;
    bool found = FALSE;
    SCREEN *scan;
    WINDOWLIST *p;

    for (each_screen(scan)) {
	for (each_window(scan, p)) {
	    if (&(p->win) == win) {
		result = FALSE;
		found = TRUE;
		break;
	    } else if (IS_SUBWIN(&(p->win))
		       && p->win._parent == win) {
		result = TRUE;
		found = TRUE;
		break;
	    }
	}
	if (found)
	    break;
    }
    return result;
}

NCURSES_EXPORT(int)
delwin(WINDOW *win)
{
    int result = ERR;

    T((T_CALLED("delwin(%p)"), (void *) win));

    if (_nc_try_global(curses) == 0) {
	if (win == NULL
	    || cannot_delete(win)) {
	    result = ERR;
	} else if (IS_PAD(win)) {
	    win->_parent = NULL;
	    result = _nc_freewin(win);
	} else {
#if NCURSES_SP_FUNCS
	    SCREEN *sp = _nc_screen_of(win);
#endif
	    if (IS_SUBWIN(win)) {
		touchwin(win->_parent);
	    } else if (CurScreen(SP_PARM) != NULL) {
		touchwin(CurScreen(SP_PARM));
	    }
	    result = _nc_freewin(win);
	}
	_nc_unlock_global(curses);
    }
    returnCode(result);
}
