/****************************************************************************
 * Copyright 2020-2024,2025 Thomas E. Dickey                                *
 * Copyright 1998-2009,2010 Free Software Foundation, Inc.                  *
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

#include <curses.priv.h>

MODULE_ID("$Id: lib_win32util.c,v 1.7 2025/06/28 16:58:13 tom Exp $")

#ifdef _NC_WINDOWS_NATIVE

#if HAVE_GETTIMEOFDAY == 2
#define JAN1970 116444736000000000LL /* the value for 01/01/1970 00:00 */

NCURSES_EXPORT(int)
_nc_gettimeofday(struct timeval *tv, void *tz GCC_UNUSED)
{
	union
	{
		FILETIME ft;
		long long since1601; /* time since 1 Jan 1601 in 100ns units */
	} data;

	GetSystemTimeAsFileTime(&data.ft);
	tv->tv_usec = (long)((data.since1601 / 10LL) % 1000000LL);
	tv->tv_sec = (long)((data.since1601 - JAN1970) / 10000000LL);
	return (0);
}
#endif // HAVE_GETTIMEOFDAY == 2

#endif // _NC_WINDOWS_NATIVE
