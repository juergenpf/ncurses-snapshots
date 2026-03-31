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
 ****************************************************************************/

#include <curses.priv.h>
#include <tchar.h>

MODULE_ID("$Id$")

#if USE_CONSOLE_API

#include <winternl.h>
#include <locale.h>

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
 * the Windows Console backend. If any of these checks fail, we return false to indicate that
 * ConPTY is not supported in the current environment.
 * Even if the Windows version supports conpty, the environment may have disabled it, for example by 
 * setting the registry key HKCU\Console\VirtualTerminalLevel to 0. In this case, we also return false 
 * to indicate that ConPTY is not supported.
 */
static bool
conpty_supported(void)
{
    bool result = false;
    DWORD major, minor, build;

    T((T_CALLED("lib_win32concore::conpty_supported")));

    if (!get_real_windows_version(&major, &minor, &build)) {
	T(("RtlGetVersion failed"));
	returnBool(false);
    } else {
	T(("Windows version detected: %d.%d (build %d)", (int) major, (int) minor, (int) build));
    }
    if (major >= REQUIRED_MAJOR_V) {
	if (major == REQUIRED_MAJOR_V) {
	    if (((minor == REQUIRED_MINOR_V) &&
		 (build >= REQUIRED_BUILD)) ||
		((minor > REQUIRED_MINOR_V)))
		result = true;
	} else
	    result = true;
    }
    if (result == true) {
		HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
		if (hOut == INVALID_HANDLE_VALUE) {
	    	T(("GetStdHandle failed with error %lu", GetLastError()));
	    	result = false;
		} else {
	    	DWORD dwFlag;
	    	if (GetConsoleMode(hOut, &dwFlag) == 0) {
				T(("Output handle is not a pseudo-console"));
				result = false;
	    	} else {
	    		if ((dwFlag & ENABLE_VIRTUAL_TERMINAL_PROCESSING) == 0) {
					dwFlag |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
					if (SetConsoleMode(hOut, dwFlag) == 0) {
		    			T(("SetConsoleMode failed with error %lu", GetLastError()));
		    			result = false;
					}
				}
	    	}
		}
	}
    returnBool(result);
}

#if USE_SCREENBUFFERED_CONSOLE
/* Windows version prior to version 10 have mixed behaviour when it comes to console resizing. 
 * In some versions, the console can be resized freely, but the application is not notified of 
 * the resize events, which means that ncurses cannot update its internal state to reflect the 
 * new console size. In other versions, resizing the console is not possible at all, which
 * means that ncurses cannot function properly because it relies on being able to query the 
 * console size and receive notifications of resize events. To work around these issues, we 
 * disable resizing of the console window when running on legacy consoles, and we also check for 
 * the Windows version to determine if there are any limitations on resizing that we need to be 
 * aware of. */
static bool
has_limiuted_resize(void)
{
    bool result = true;
    DWORD major, minor, build;

    if (!get_real_windows_version(&major, &minor, &build)) {
	T(("RtlGetVersion failed"));
    } else {
	result = (major >= 10) ? false : true; 
    }
    return result;
}
#endif /* USE_SCREENBUFFERED_CONSOLE */

NCURSES_EXPORT_VAR(ConsoleCoreInterface*) 
_nc_CORECONSOLE = NULL;

/* Helper routine for getting the console size. We try to get the console size from
 * multiple handles, because in some cases (like when running in Windows Terminal)
 * the standard output handle might not be the one that actually provides the console
 * size information, but another handle does. This routine tries to get the console
 * size from the main output handle, and if that fails, it tries the standard output
 * and standard error handles as well. If all attempts fail, it returns FALSE. */
static bool
get_sbi(CONSOLE_SCREEN_BUFFER_INFO * csbi)
{
    HANDLE test_handles[] = {
		CORECONSOLE.ConsoleHandleOut, 
		GetStdHandle(STD_OUTPUT_HANDLE),
     		GetStdHandle(STD_ERROR_HANDLE)
	};
    HANDLE hdl;

    for (size_t i = 0; i < sizeof(test_handles) / sizeof(test_handles[0]); ++i) {
	hdl = test_handles[i];
	if (hdl != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hdl, csbi)) {
	    return true;
	}
    }
    return false;
}

/* This function flushes the console input buffer. It is called by the main thread when it
 * wants to discard any pending input in the console. The function returns OK on success. */
static int
flush_input(int fd GCC_UNUSED)
{
	int code = OK;
	T((T_CALLED("lib_win32concore::flush_input(fd=%d)"), fd));
	FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
	returnCode(code);
}

#define CP_UTF8 65001
#if USE_CONPTY
/* MSVCRT doesn't support UTF-8 locales, but UCRT does. However, even with UCRT we can't rely
 * on the locale being set to UTF-8 by default, so we need to set the code page explicitly for
 * the console to ensure that it uses UTF-8 encoding.
 * With UCRT, we enforce to use a proper UTF-8 capable locale, to ensure that the console can
 * display and classify characters properly. */
static void
encoding_init(void)
{
#if defined(_UCRT)
    char *newlocale = NULL;
#endif
    char *cur_loc = NULL;
    char localebuf[16];
    UINT cp;
#if USE_WIDEC_SUPPORT
    cp = CP_UTF8;
#else
    WCHAR buf[16];
    /* We query the system for the default ANSI code page */
    int len = GetLocaleInfoEx(
				 LOCALE_NAME_SYSTEM_DEFAULT,
				 LOCALE_IDEFAULTANSICODEPAGE,
				 buf,
				 16);
    if (len > 0)
	cp = (UINT) _wtoi(buf);
    else
	cp = 1252;		/* last line of defense if GetLocaleInfoEx fails is to assume a
				 * reasonable default code page, which is the most common ANSI code
				 * page on Western systems. This is not ideal, but there isn't much
				 * else we can do in this case, and it at least allows the console
				 * to function with a reasonable character set in most cases. */
#endif
    snprintf(localebuf, sizeof(localebuf), ".%u", cp);
    cur_loc = setlocale(LC_CTYPE, NULL);

    T((T_CALLED("lib_win32conpty::encoding_init() - code page will be set to %u"), cp));
    T(("conpty Current locale: %s", cur_loc ? cur_loc : "NULL"));
#if defined(_UCRT)
    T(("conpty using UCRT"));

    T(("conpty: Try setting locale according to desired codepage %s", localebuf));
    newlocale = setlocale(LC_CTYPE, localebuf);
    T(("conpty setlocale() result locale is %s", newlocale ? newlocale :
       "NULL"));

    cur_loc = setlocale(LC_CTYPE, NULL);
    T(("conpty Current locale now %s, code page %u", cur_loc ? cur_loc :
       "NULL", cp));
#else
    T(("conpty: Not using UCRT - relying on current locale for code page handling"));
#endif /* defined(_UCRT ) */

    SetConsoleCP(cp);
    SetConsoleOutputCP(cp);
}
#endif /* USE_CONPTY */

NCURSES_EXPORT(bool)
_nc_console_setup(void) {
	bool res = false;
	DWORD status = 0;

	T((T_CALLED("lib_win32concore::_nc_console_setup()")));
	if (conpty_supported()) {
#if USE_CONPTY
		_nc_CORECONSOLE = & (WINCONPTY.core);
		status |= CONSOLE_STATUS_IS_CONPTY;
		/* Especially with UCRT and wide mode, make sure we use an UTF-8 capable locale.
	 	 * At least we set the codepage to a proper value that's either compatible with
	 	 * ASCII or UTF-8, to ensure that the console can display characters properly.
	 	 * The actual locale setting is not that important, as long as the code page is set
	 	 * correctly, because we handle UTF-8 encoding and decoding ourselves and we don't
	 	 * rely on the C runtime for that. */
		encoding_init();
#else
		/* This is intentional. We want to assert best possible support for ncurses functionality
		 * on Windows is available, so this message should motivate people to use appropriate builds
		 * of ncurses. When the application is built against a DLL version of ncurses, a simple install
		 * of a new ncurses set of DLLS with ConPTY support should be sufficient to get ConPTY support 
		 * without recompiling the application. When the application is statically linked against ncurses, 
		 * then the application itself needs to be recompiled with a version of ncurses that has ConPTY 
		 * support enabled, otherwise it will not be able to use ConPTY even if it is available on the 
		 * system. In this case, we print an error message and exit, because running without ConPTY support 
		 * on a modern Windows system would lead to a degraded experience. */
		T(("lib_win32concore::_nc_console_setup - ConPTY supported, but not enabled. Exiting Program"));
		fprintf(stderr, "ERROR: ConPTY is supported on this system, but not compiled into ncurses.\n");
		fprintf(stderr, "This configuration is NOT supported.\n");
#endif
	} else {
#if USE_SCREENBUFFERED_CONSOLE
		HWND hwnd = GetConsoleWindow();
		LONG style = GetWindowLong(hwnd, GWL_STYLE);
		style &= ~(WS_SIZEBOX | WS_MAXIMIZEBOX);
		T(("lib_win32concore::_nc_console_setup - Legacy console detected, disabling resizing"));
		SetWindowLong(hwnd, GWL_STYLE, style);
		_nc_CORECONSOLE = & (SCREENBUFFEREDCONSOLE.core);
		if (has_limiuted_resize()) {
			T(("lib_win32concore::_nc_console_setup - Legacy console has resize limitations"));
			status |= CONSOLE_STATUS_LIMITED_RESIZE;
		} else {
			CONSOLE_SCREEN_BUFFER_INFO csbi;
			if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) {
			    COORD newSize;
			    newSize.X = (short)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
			    newSize.Y = (short)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);
			    SetConsoleScreenBufferSize(SCREENBUFFEREDCONSOLE.core.ConsoleHandleOut, newSize);
			}
		}
#endif
	}
	if (NULL!=_nc_CORECONSOLE) {
		CORECONSOLE.status = status;
		CORECONSOLE.ConsoleHandleIn = INVALID_HANDLE_VALUE;
		CORECONSOLE.ConsoleHandleOut = INVALID_HANDLE_VALUE;
		CORECONSOLE.ttyflags.dwFlagIn = 0;
		CORECONSOLE.ttyflags.dwFlagOut = 0;
		CORECONSOLE.ttyflags.kind = TTY_MODE_UNSPECIFIED;
		CORECONSOLE.sbi_lines = -1;
		CORECONSOLE.sbi_cols = -1;
		CORECONSOLE.sp = 0;

		CORECONSOLE.getSBI = get_sbi;
		CORECONSOLE.flush = flush_input;


		res = true;
	}
	returnBool(res);
}

/* The central routine to get the TTY state. It dispatches in dependency 
 * of the console type to the correct implementation. */
NCURSES_EXPORT(int)
_nc_console_gettty(int fd, ConsoleMode *buf) {
	assert(_nc_CORECONSOLE);
	return CORECONSOLE.getmode(fd, buf);
}

/* The central routine to set the TTY state. It dispatches in dependency 
 * of the console type to the correct implementation. */
NCURSES_EXPORT(int)
_nc_console_settty(int fd, ConsoleMode *buf) {
	assert(_nc_CORECONSOLE);
	return CORECONSOLE.setmode(fd, buf);
}

/* Helper routine to compute the difference between two timevals in milliseconds. 
 * We use this to measure the time between console resize events, to determine if 
 * we need to update the console size information in ncurses. The function takes 
 * two timeval structures as input, representing the start and end times, and 
 * returns the difference in milliseconds as an integer. We assume that the time 
 * difference is not large enough to cause an overflow of the int64_t type, which 
 * should be safe for differences for slightly more than three weeks..
 */
NCURSES_EXPORT(int)
_nc_timeval_diff_in_ms(struct timeval start, struct timeval end) 
{
    int64_t diff_sec = (int64_t)end.tv_sec - (int64_t)start.tv_sec;
    int64_t diff_usec = (int64_t)end.tv_usec - (int64_t)start.tv_usec;
    return (int)((diff_sec * 1000) + (diff_usec / 1000));
}

#endif // _NC_WINDOWS_NATIVE
