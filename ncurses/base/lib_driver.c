/****************************************************************************
 * Copyright 2018-2024,2025 Thomas E. Dickey                                *
 * Copyright 2009-2012,2014 Free Software Foundation, Inc.                  *
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
 *                                                                          *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id: lib_driver.c,v 1.12 2025/10/18 19:20:33 tom Exp $")

NCURSES_EXPORT(int)
NCURSES_SP_NAME(doupdate) (SCREEN *sp)
{
    int code = ERR;

    T((T_CALLED("doupdate(%p)"), (void *) sp));

    if (IsValidScreen(sp))
	code = CallDriver(sp, td_update);

    returnCode(code);
}

NCURSES_EXPORT(int)
doupdate(void)
{
    return NCURSES_SP_NAME(doupdate) (CURRENT_SCREEN);
}

NCURSES_EXPORT(int)
NCURSES_SP_NAME(mvcur) (SCREEN *sp, int yold, int xold, int ynew, int xnew)
{
    int code = ERR;
    TR(TRACE_CALLS | TRACE_MOVE, (T_CALLED("mvcur(%p,%d,%d,%d,%d)"),
				  (void *) sp, yold, xold, ynew, xnew));
    if (HasTerminal(sp)) {
	code = CallDriver_4(sp, td_hwcur, yold, xold, ynew, xnew);
    }
    returnCode(code);
}

NCURSES_EXPORT(int)
mvcur(int yold, int xold, int ynew, int xnew)
/* optimized cursor move from (yold, xold) to (ynew, xnew) */
{
    return NCURSES_SP_NAME(mvcur) (CURRENT_SCREEN, yold, xold, ynew, xnew);
}
