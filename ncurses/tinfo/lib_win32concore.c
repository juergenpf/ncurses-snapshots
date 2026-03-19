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
				T(("Output handle is not a pseudo-console"));
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

NCURSES_EXPORT_VAR(ConsoleCoreInterface*) 
_nc_CORECONSOLE = NULL;

NCURSES_EXPORT(BOOL)
_nc_console_setup(void) {
	BOOL res = FALSE;

	T((T_CALLED("lib_win32concore::_nc_console_setup()")));
	if (_nc_conpty_supported()) {
#if USE_CONPTY
		_nc_CORECONSOLE = & (WINCONPTY.core);
		CORECONSOLE.is_conpty = TRUE;
#endif
	} else {
#if USE_LEGACY_CONSOLE
		_nc_CORECONSOLE = & (LEGACYCONSOLE.core);
		CORECONSOLE.is_conpty = FALSE;
#endif
	}
	if (NULL!=_nc_CORECONSOLE) {
		CORECONSOLE.initialized = FALSE;;
		CORECONSOLE.ConsoleHandleIn = INVALID_HANDLE_VALUE;
		CORECONSOLE.ConsoleHandleOut = INVALID_HANDLE_VALUE;
		CORECONSOLE.ttyflags.dwFlagIn = 0;
		CORECONSOLE.ttyflags.dwFlagOut = 0;
		CORECONSOLE.ttyflags.kind = TTY_MODE_UNSPECIFIED;
		CORECONSOLE.sbi_lines = -1;
		CORECONSOLE.sbi_cols = -1;
		res = TRUE;
	}
	returnBool(res);
}

#endif // _NC_WINDOWS_NATIVE
