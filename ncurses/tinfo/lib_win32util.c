/****************************************************************************
 * Copyright 2020-2024,2026 Thomas E. Dickey                                *
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

#if (defined(_NC_WINDOWS_NATIVE) || defined(_WIN32) || defined(_WIN64))

#include <winternl.h>

#if HAVE_GETTIMEOFDAY == 2
#define JAN1970 116444736000000000LL	/* the value for 01/01/1970 00:00 */

NCURSES_EXPORT(int)
_nc_gettimeofday(struct timeval *tv, void *tz GCC_UNUSED)
{
    union {
	FILETIME ft;
	long long since1601;	/* time since 1 Jan 1601 in 100ns units */
    } data;

    GetSystemTimeAsFileTime(&data.ft);
    tv->tv_usec = (long) ((data.since1601 / 10LL) % 1000000LL);
    tv->tv_sec = (long) ((data.since1601 - JAN1970) / 10000000LL);
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

/* *INDENT-OFF* */
NCURSES_EXPORT(int) mk_wcwidth_init(int);
NCURSES_EXPORT(int) mk_wcwidth(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_wcwidth_cjk(uint32_t);
NCURSES_EXPORT(int) mk_wcswidth_cjk(const uint32_t *, size_t);
NCURSES_EXPORT(int) mk_is_emoji(wchar_t ucs);
/* *INDENT-ON* */

#include <wcwidth.h>
#else
void _nc_empty_wcwidth(void);
void
_nc_empty_wcwidth(void)
{
}
#endif // USE_WIDEC_SUPPORT

/* AKA Windows 10 version 1809, which is the first version that introduced ConPTY.
 * We check for this version or higher to ensure that ConPTY is available, which is
 * a requirement for the Windows Console backend of ncurses. This is because without
 * ConPTY, the Windows Console does not provide the necessary capabilities for
 * ncurses and especially the terminfo layer to function properly. */
#define REQUIRED_MAJOR_V (DWORD)10
#define REQUIRED_MINOR_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763

typedef NTSTATUS(WINAPI * RtlGetVersionPtr) (PRTL_OSVERSIONINFOW);

static bool
get_real_windows_version(DWORD * major, DWORD * minor, DWORD * build)
{
    HMODULE ntdll = GetModuleHandle(TEXT("ntdll.dll"));
    if (ntdll) {
	FARPROC proc = GetProcAddress(ntdll, "RtlGetVersion");
	union {
	    FARPROC proc;
	    RtlGetVersionPtr func;
	} cast;
	RtlGetVersionPtr RtlGetVersion = NULL;
	cast.proc = proc;
	RtlGetVersion = cast.func;
	if (RtlGetVersion) {
	    RTL_OSVERSIONINFOW osvi =
	    {0};
	    osvi.dwOSVersionInfoSize = sizeof(osvi);
	    if (RtlGetVersion(&osvi) == 0) {
		*major = osvi.dwMajorVersion;
		*minor = osvi.dwMinorVersion;
		*build = osvi.dwBuildNumber;
		return true;
	    }
	}
    }
    return false;
}

/* Check if the current Windows version supports ConPTY.
 * We check for Windows 10 version 1809 or higher, which is the first version that introduced ConPTY.
 * If the version check passes, we also verify that the standard output handle is a console and that
 * it supports virtual terminal processing, which is necessary for ncurses to function properly on
 * the Windows Console backend. If any of these checks fail, we return FALSE to indicate that
 * ConPTY is not supported in the current environment.
 * Even if the Windows version supports conpty, the environment may have disabled it, for example by 
 * setting the registry key HKCU\Console\VirtualTerminalLevel to 0. In this case, we also return FALSE 
 * to indicate that ConPTY is not supported.
 */
NCURSES_EXPORT(BOOL)
_nc_conpty_supported(void)
{
    int result = FALSE;
    DWORD major, minor, build;

    T((T_CALLED("lib_win32conpty::conpty_supported")));

    if (!get_real_windows_version(&major, &minor, &build)) {
	T(("RtlGetVersion failed"));
	returnBool(FALSE);
    }
    if (major >= REQUIRED_MAJOR_V) {
	T(("Windows version detected: %d.%d (build %d)", (int) major, (int)
	   minor, (int) build));
	if (major == REQUIRED_MAJOR_V) {
	    if (((minor == REQUIRED_MINOR_V) &&
		 (build >= REQUIRED_BUILD)) ||
		((minor > REQUIRED_MINOR_V)))
		result = TRUE;
	} else
	    result = TRUE;
    }

    if (result == TRUE) {
		HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		if (hOut == INVALID_HANDLE_VALUE) {
	    	T(("GetStdHandle failed with error %lu", GetLastError()));
	    	result = FALSE;
		} else {
	    	DWORD dwFlag;
	    	if (GetConsoleMode(hOut, &dwFlag) == 0) {
				T(("Output handle is not a console"));
				result = FALSE;
	    	} else {
	    		if ((dwFlag & ENABLE_VIRTUAL_TERMINAL_PROCESSING) == 0) {
					dwFlag |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
					if (SetConsoleMode(hOut, dwFlag) == 0) {
		    			T(("SetConsoleMode failed with error %lu", GetLastError()));
		    			result = FALSE;
					}
				}
	    	}
		}
	}	
    returnBool(result);
}

#endif // _NC_WINDOWS_NATIVE
