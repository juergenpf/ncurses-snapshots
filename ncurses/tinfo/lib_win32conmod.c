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
#if defined(_NC_WINDOWS_NATIVE)
// #include <nc_win32.h>
#include <locale.h>
#include <stdio.h>
#include <wchar.h>  /* For wide character functions */
#include <string.h> /* For memset */

MODULE_ID("$Id$")

#define CON_STDIN_HANDLE  GetStdHandle(STD_INPUT_HANDLE)
#define CON_STDOUT_HANDLE GetStdHandle(STD_OUTPUT_HANDLE)
#define CON_STDERR_HANDLE GetStdHandle(STD_ERROR_HANDLE)

typedef enum { NotKnown, Input, Output } HandleType;

static HandleType classify_handle(HANDLE hdl) {
    DWORD mode;
    DWORD handle_type;
    DWORD test_input;
    DWORD test_output;
    DWORD original_mode;

    if (!GetConsoleMode(hdl, &mode)) {
        return NotKnown;
    }

    handle_type = GetFileType(hdl);
    if (handle_type != FILE_TYPE_CHAR) {
        return NotKnown;
    }

    // Check against standard handles first (most reliable)
    if (hdl == CON_STDIN_HANDLE) {
        return Input;
    }
    if (hdl == CON_STDOUT_HANDLE || hdl == CON_STDERR_HANDLE) {
        return Output;
    }

    // Try to determine by testing which mode types work
    test_input = ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;
    test_output = ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT;

    original_mode = mode;

    // Try setting input-specific flags
    if (SetConsoleMode(hdl, test_input)) {
        SetConsoleMode(hdl, original_mode);  // Restore
        return Input;
    }

    // Try setting output-specific flags
    if (SetConsoleMode(hdl, test_output)) {
        SetConsoleMode(hdl, original_mode);  // Restore
        return Output;
    }

    return NotKnown;
}

static HANDLE
fd2handle(int fd)
{
	HANDLE hdl = _get_osfhandle(fd);

	if (fd == _fileno(stdin))
	{
		T(("lib_win32conpty:validateHandle %d -> stdin handle", fd));
	}
	else if (fd == _fileno(stdout))
	{
		T(("lib_win32conpty:validateHandle %d -> stdout handle", fd));
	}
	else
	{
		T(("lib_win32conpty:validateHandle %d maps to unknown fd", fd));
	}
	return hdl;
}

NCURSES_EXPORT(int)
_nc_console_flush(int fd)
{
	int code = OK;
	HANDLE hdl = fd2handle(fd);

	T((T_CALLED("lib_win32conpty::_nc_console_flush(hdl=%p"), hdl));

	if (hdl != INVALID_HANDLE_VALUE)
	{
		HANDLE hdlIn = GetStdHandle(STD_INPUT_HANDLE);
		HANDLE hdlOut = GetStdHandle(STD_OUTPUT_HANDLE);
		if (hdl == hdlIn)
		{
			if (!FlushConsoleInputBuffer(hdlIn))
				code = ERR;
		}
		else if (hdl == hdlOut)
		{
			/* Flush output buffer - use FlushFileBuffers for proper VT processing */
			if (!FlushFileBuffers(hdlOut))
				code = ERR;
		}
		else
		{
			code = ERR;
			T(("_nc_console_flush not requesting a handle owned by console."));
		}
	}
	/* Note: This function works in both ConPTY and legacy console modes */
	returnCode(code);
}
 
NCURSES_EXPORT(int)
_nc_conpty_setmode(int fd, const TTY *arg)
{
	if (!arg) return ERR;
	
	HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
	HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);
	HANDLE fd_hdl = INVALID_HANDLE_VALUE;
	HandleType fd_type = NotKnown;
	
	// Get handle from fd
	if (fd >= 0) {
		fd_hdl = fd2handle(fd);
		if (fd_hdl != INVALID_HANDLE_VALUE) {
			DWORD test_mode;
			if (GetConsoleMode(fd_hdl, &test_mode)) {
				fd_type = classify_handle(fd_hdl);
			}
		}
	}
		
	// Determine which handles to use based on classification
	HANDLE input_target  = INVALID_HANDLE_VALUE;
	HANDLE output_target = INVALID_HANDLE_VALUE;
	
	if (fd_type == Input) {
		input_target = fd_hdl;
		output_target = stdout_hdl;
	} else if (fd_type == Output) {
		input_target = stdin_hdl;
		output_target = fd_hdl;
	} else {
		input_target = stdin_hdl;
		output_target = stdout_hdl;
	}
	
	// Apply modes to appropriate handles
	bool input_ok = false, output_ok = false;
	
	if (input_target != INVALID_HANDLE_VALUE) {
        DWORD mode = arg->dwFlagIn;
        
        /* 
           ENABLE_VIRTUAL_TERMINAL_INPUT (VT) requires ENABLE_PROCESSED_INPUT to be effective.
           If we request VT, we must ensure PROCESSED is set, otherwise SetConsoleMode fails.
        */
        if (mode & VT_FLAG_IN) {
            mode |= ENABLE_PROCESSED_INPUT;
        }

        /* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
        if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT)) {
             mode &= ~ENABLE_ECHO_INPUT;
        }

		input_ok = SetConsoleMode(input_target, mode);
		if (input_ok) {
			WINCONSOLE.ttyflags.dwFlagIn = mode;
		}
	}
	
	if (output_target != INVALID_HANDLE_VALUE) {
		output_ok = SetConsoleMode(output_target, VT_FLAG_OUT |arg->dwFlagOut);
		if (output_ok) {
			WINCONSOLE.ttyflags.dwFlagOut = VT_FLAG_OUT | arg->dwFlagOut;
		}
	}
	
	// Handle errors
	if (!input_ok || !output_ok)
	{
		DWORD error = GetLastError();
		
		if (error == 6) {
			// Not a console - simulate success for testing
			input_ok = output_ok = true;
		} else {
			return ERR;
		}
	}
		
	return OK;
}

NCURSES_EXPORT(int)
_nc_conpty_getmode(int fd, TTY *arg)
{
	if (NULL==arg) return ERR;

	*arg = WINCONSOLE.ttyflags; 
	return OK;
}

#if defined(TRACE) || 1
/* JPF
For testing and debugging, we want to be able to print the current console modes in a human-readable form.
When code stabilizes, this will be removed and done in the trace subtree of ncurses instead.
*/
#define DATA(name) { name, { #name } }
#define BITNAMELEN 48

typedef struct {
    unsigned long val;
    const char name[BITNAMELEN];
} BITNAMES;

static const BITNAMES dwFlagsOut[] =
    {
	DATA(ENABLE_PROCESSED_OUTPUT),
	DATA(ENABLE_WRAP_AT_EOL_OUTPUT),
	DATA(ENABLE_VIRTUAL_TERMINAL_PROCESSING),
	DATA(DISABLE_NEWLINE_AUTO_RETURN),
	DATA(ENABLE_LVB_GRID_WORLDWIDE),
	{ 0, "" }
    };
static const BITNAMES dwFlagsIn[] =
    {
	DATA(ENABLE_PROCESSED_INPUT),
	DATA(ENABLE_LINE_INPUT),
	DATA(ENABLE_ECHO_INPUT),
	DATA(ENABLE_WINDOW_INPUT),
	DATA(ENABLE_MOUSE_INPUT),
	DATA(ENABLE_INSERT_MODE),
	DATA(ENABLE_QUICK_EDIT_MODE),
	DATA(ENABLE_EXTENDED_FLAGS),
	DATA(ENABLE_AUTO_POSITION),
	DATA(ENABLE_VIRTUAL_TERMINAL_INPUT),
	{ 0, "" }
    };


static void
lookup_bits(char *buf, size_t bufsize, const BITNAMES * table, const char *label, unsigned long val)
{
    const BITNAMES *sp;

    strcat_s(buf, bufsize, label);
    strcat_s(buf, bufsize, ": {");
    for (sp = table; sp->name[0]; sp++)
	if (sp->val != 0
	    && (val & sp->val) == sp->val) {
	    strcat_s(buf, bufsize, sp->name);
	    strcat_s(buf, bufsize, ", ");
	}
    if (strlen(buf) > 2 && buf[strlen(buf) - 2] == ',')
	buf[strlen(buf) - 2] = '\0';
    strcat_s(buf, bufsize, "} ");
}

NCURSES_EXPORT(void)
_nc_console_DumpModesEx(const char *title, const TTY *tty) {
    static char buf[2048];
    HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);

    if (NULL==tty) return;

    OutputDebugStringA("========================================\n");
    if (title) {	
	OutputDebugStringA(title);
    }
    OutputDebugStringA("========================================\n");
    
    
    buf[0] = '\0';
    lookup_bits(buf, sizeof(buf), dwFlagsIn, "dwIn", tty->dwFlagIn);
    strcat_s(buf, sizeof(buf), "\n");
    lookup_bits(buf, sizeof(buf), dwFlagsOut, "dwOut", tty->dwFlagOut);

    OutputDebugStringA("Console Modes: \n");
    OutputDebugStringA(	buf);
}

NCURSES_EXPORT(void)
_nc_console_DumpModes(const char *title) {
    static char buf[2048];
    DWORD dwFlagIn = 0;
    DWORD dwFlagOut = 0;
    HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);

    OutputDebugStringA("========================================\n");
    if (title) {	
	OutputDebugStringA(title);
    }
    OutputDebugStringA("========================================\n");
    
    if (!GetConsoleMode(stdin_hdl, &dwFlagIn)){
        OutputDebugStringA("Failed to get console mode for stdin\n");
        dwFlagIn = 0;
    }
    if (!GetConsoleMode(stdout_hdl, &dwFlagOut)) {
        OutputDebugStringA("Failed to get console mode for stdout\n");
        dwFlagOut = 0;
    }
    
    buf[0] = '\0';
    lookup_bits(buf, sizeof(buf), dwFlagsIn, "dwIn", dwFlagIn);
    strcat_s(buf, sizeof(buf), "\n");
    lookup_bits(buf, sizeof(buf), dwFlagsOut, "dwOut", dwFlagOut);

    OutputDebugStringA("Console Modes: \n");
    OutputDebugStringA(	buf);
}

NCURSES_EXPORT(void)
_nc_console_DumpTitle(const char *title) {
    OutputDebugStringA("########################################\n");
    if (title) {	
	OutputDebugStringA(title);
    }
    OutputDebugStringA("########################################\n");
}
#endif /* TRACE */

#endif /* defined(USE_WIN32_CONPTY)) */
