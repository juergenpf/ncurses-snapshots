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



NCURSES_EXPORT_VAR(TERM_DRIVER)
_nc_WIN_DRIVER = {
	FALSE,
	wcon_name,			/* Name          */
	wcon_CanHandle,		/* CanHandle     */
	wcon_init			/* init          */
};
#endif /* USE_LEGACY_CONSOLE */
