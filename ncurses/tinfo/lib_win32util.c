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

#if USE_WIDEC_SUPPORT
#define mk_wcwidth(ucs)          _nc_wcwidth(ucs)
#define mk_wcswidth(pwcs, n)     _nc_wcswidth(pwcs, n)
#define mk_wcwidth_cjk(ucs)      _nc_wcwidth_cjk(ucs)
#define mk_wcswidth_cjk(pwcs, n) _nc_wcswidth_cjk(pwcs, n)

#include <stddef.h>

typedef enum {
    WcUnknown = 0
    ,WcSoftHyphen = 1		/* soft-hyphen is spacing, e.g., Latin-1 */
    ,WcPrivateFullwidth = 2	/* private-use codes can be fullwidth in CJK */
    ,WcEmojiFullwidth = 4	/* Emojis are fullwidth */
} WcModes;

NCURSES_EXPORT(int) mk_wcwidth_init(int);
NCURSES_EXPORT(int) mk_wcwidth(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_wcwidth_cjk(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth_cjk(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_is_emoji(wchar_t ucs);

#include <wcwidth.h>
#else
void _nc_empty_wcwidth(void);
void
_nc_empty_wcwidth(void)
{
}
#endif // USE_WIDEC_SUPPORT

#endif // _NC_WINDOWS_NATIVE
