/****************************************************************************
 * Copyright 2026 Juergen Pfeifer                                *
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
 *  Author: Juergen Pfeifer                                                *
 ****************************************************************************/

#include <curses.priv.h>

MODULE_ID("$Id$")

/*
 * This routine is a repacement for a simple getenv("TERM"). It is used for environments
 * like Windows, where we do not have terminfo and it's unusual to have an environment
 * variable TERM.
 * 
 * The idea is, that this routine initializes potential platform specific subsystems like
 * for example the Console subsystem on Windows and then returns a name that can be 
 * interpreted as the result of getenv("TERM"). 
 * 
 * IUf the resulting name starts with a "#", which cannot be a valid terminfo name, we know
 * we are in a non-terminfo environment. Any other result, including NULL or an empty string,
 * will result in a terminfo processing, e.g. lookup in the database or whatever is configured.
*/
NCURSES_EXPORT(char *)
_nc_term_select (void)
{
#if USE_CONSOLE_API
    if (!CoreConsoleInitialized()) {
	    if (!_nc_console_setup()) {
	        fprintf(stderr, CONSOLE_INIT_FAILURE_MSG);
            /* This is part of termlib and detected early in the initialization phase,
            *  so - for now - we skip formal release  of potentially allocated resources
            *  if there are any at all in that early stage. */
	        exit(EXIT_FAILURE);
	    }
    }
    assert(DefaultConsole() != NULL);
    return (DefaultConsole()->termname());
#endif
    return(getenv("TERM"));
}
