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
    HandleType type = NotKnown;
    if (hdl!= INVALID_HANDLE_VALUE) {
	if (hdl == WINCONSOLE.used_input_handle) {
	    type = Input;
	} else if (hdl == WINCONSOLE.used_output_handle) {
	    type = Output;
	}	
    }
    return type;
}

NCURSES_EXPORT(int)
_nc_conpty_flush(int fd)
{
	int code = OK;
	HANDLE hdl = _get_osfhandle(fd);
	HandleType type = classify_handle(hdl);
	HANDLE hdlIn = WINCONSOLE.used_input_handle;
	HANDLE hdlOut = WINCONSOLE.used_output_handle;

	T((T_CALLED("lib_win32conmod::_nc_conpty_flush(fd=%d)"), fd));

	if (type == Input)
	{
		if (!FlushConsoleInputBuffer(hdlIn))
			code = ERR;
	}
	else if (type == Output)
	{
		/* Flush output buffer - use FlushFileBuffers for proper VT processing */
		if (!FlushFileBuffers(hdlOut))
			code = ERR;
	}
	else
	{
		code = ERR;
		T(("_nc_conpty_flush not requesting a handle owned by console."));
	}
	returnCode(code);
}
 
NCURSES_EXPORT(int)
_nc_conpty_setmode(int fd, const TTY *arg)
{
	if (!arg) return ERR;
	
	HANDLE in_hdl = WINCONSOLE.used_input_handle;
	HANDLE out_hdl = WINCONSOLE.used_output_handle;
	HANDLE fd_hdl = INVALID_HANDLE_VALUE;
	HandleType fd_type = NotKnown;
	
	// Get handle from fd
	if (fd >= 0) {
		fd_hdl = _get_osfhandle(fd);
		fd_type = classify_handle(fd_hdl);
	}
	if (fd_type == NotKnown) {
		T(("_nc_conpty_setmode: fd %d does not correspond to console input or output handle", fd));
		return ERR;
	}

	// Determine which handles to use based on classification
	HANDLE input_target  = INVALID_HANDLE_VALUE;
	HANDLE output_target = INVALID_HANDLE_VALUE;
	
	if (fd_type == Input) {
		input_target = fd_hdl;
		output_target = out_hdl;
	} else if (fd_type == Output) {
		input_target = in_hdl;
		output_target = fd_hdl;
	} else {
		input_target = in_hdl;
		output_target = out_hdl;
	}
	
	// Apply modes to appropriate handles
	bool input_ok = false, output_ok = false;
	
	if (input_target != INVALID_HANDLE_VALUE) {
        DWORD mode = arg->dwFlagIn;
        
        /* 
           ENABLE_VIRTUAL_TERMINAL_INPUT (VT) requires ENABLE_PROCESSED_INPUT to be effective.
           If we request VT, we must ensure PROCESSED is set, otherwise SetConsoleMode fails.
	   We always allow mouse and window input events if VT input is requested, as these 
	   are commonly used together and it simplifies the logic to just enable them when 
	   VT is enabled.
        */
        if (mode & VT_FLAG_IN) {
            mode |= ENABLE_PROCESSED_INPUT | ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT;
        }

        /* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */ 
        if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT)) {
             mode &= ~ENABLE_ECHO_INPUT;
        }

	input_ok = SetConsoleMode(input_target, mode);
	if (input_ok) {
		WINCONSOLE.ttyflags.dwFlagIn = mode;	}
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

	HANDLE hdl = _get_osfhandle(fd);
	HandleType type = classify_handle(hdl);
	if (type == NotKnown) {
		T(("_nc_conpty_getmode: fd %d does not correspond to console input or output handle", fd));
		return ERR;
	}	
	*arg = WINCONSOLE.ttyflags; 
	return OK;
}
#endif