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

#define CON_STDIN_HANDLE GetStdHandle(STD_INPUT_HANDLE)
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

/* Verification macros for VT processing */
#define VERIFY_VT_INPUT(hdl) \
    do { \
        DWORD _mode; \
        if ((hdl) != INVALID_HANDLE_VALUE && GetConsoleMode((hdl), &_mode)) { \
            assert((_mode & ENABLE_VIRTUAL_TERMINAL_INPUT) && "VT input processing not enabled"); \
        } \
    } while(0)

#define VERIFY_VT_OUTPUT(hdl) \
    do { \
        DWORD _mode; \
        if ((hdl) != INVALID_HANDLE_VALUE && GetConsoleMode((hdl), &_mode)) { \
            assert((_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) && "VT output processing not enabled"); \
        } \
    } while(0)

NCURSES_EXPORT(DWORD)
_nc_unix_to_win32_input_flags(DWORD dwFlags, const TTY *ttyflags)
{
	DWORD flags = ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT;

	// Raw mode disables most processing
	if (!(ttyflags->c_lflag & RAW))
	{
		if (ttyflags->c_lflag & ICANON)
		{
			flags |= ENABLE_LINE_INPUT;
		}
		if (ttyflags->c_lflag & ISIG)
		{
			flags |= ENABLE_PROCESSED_INPUT;
		}

		// Handle ECHO - Windows requires LINE_INPUT for ECHO to work
		if (ttyflags->c_lflag & ECHO)
		{
			if (flags & ENABLE_LINE_INPUT)
			{
				flags |= ENABLE_ECHO_INPUT;
			}
			else
			{
				// Force line input to make echo work
				flags |= ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT;
			}
		}
	}
	assert(flags & ENABLE_VIRTUAL_TERMINAL_INPUT);
	return flags;
}

static void win32_to_unix_input_flags(DWORD dwFlags, TTY *ttyflags)
{
	if (dwFlags & ENABLE_LINE_INPUT)
	{
		ttyflags->c_lflag |= ICANON;
	}
	else
	{
		DWORD processing_flags = dwFlags & (ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT);
		DWORD core_flags = dwFlags & ~(ENABLE_MOUSE_INPUT | ENABLE_VIRTUAL_TERMINAL_INPUT);

		if (processing_flags != 0)
		{
			ttyflags->c_lflag |= CBREAK;
		}
		else if (core_flags == 0)
		{
			ttyflags->c_lflag |= RAW;
		}
		else
		{
			ttyflags->c_lflag |= CBREAK;
		}
	}

	if ((dwFlags & ENABLE_ECHO_INPUT) && (dwFlags & ENABLE_LINE_INPUT))
	{
		ttyflags->c_lflag |= ECHO;
	}

	if (dwFlags & ENABLE_PROCESSED_INPUT)
	{
		ttyflags->c_lflag |= ISIG;
	}
}

NCURSES_EXPORT(DWORD) 
_nc_unix_to_conpty_output_flags(DWORD dwFlags, const TTY *ttyflags)
{
    /* Windows VT processing requires both flags to work properly */
    DWORD flags = ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_PROCESSED_OUTPUT;

    if (ttyflags->c_lflag & RAW) {
        flags |= DISABLE_NEWLINE_AUTO_RETURN;
    }

    assert(flags & ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    return flags;
}

static void win32_to_unix_output_flags(DWORD dwFlags, TTY *ttyflags)
{
	if (dwFlags & ENABLE_PROCESSED_OUTPUT)
	{
		ttyflags->c_lflag |= ONLCR;
	}
}
 
NCURSES_EXPORT(int)
_nc_conpty_tcsetattr(int fd, const TTY *arg)
{
	if (!arg) return ERR;
	
	HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
	HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);
	HANDLE fd_hdl = INVALID_HANDLE_VALUE;
	HandleType fd_type = NotKnown;
	
	// Get handle from fd
	if (fd >= 0) {
		fd_hdl = (HANDLE)_get_osfhandle(fd);
		if (fd_hdl != INVALID_HANDLE_VALUE) {
			DWORD test_mode;
			if (GetConsoleMode(fd_hdl, &test_mode)) {
				fd_type = classify_handle(fd_hdl);
			}
		}
	}
	
	// Enhance flags
	TTY enhanced_tty = *arg;
	if (!(enhanced_tty.c_lflag & (RAW | ICANON | CBREAK))) {
		enhanced_tty.c_lflag |= CBREAK;
	}
	
	// Convert Unix flags to Windows console modes
	DWORD input_flags = _nc_unix_to_win32_input_flags(0, &enhanced_tty);
	DWORD output_flags = _nc_unix_to_conpty_output_flags(0, &enhanced_tty);
	
	// Determine which handles to use based on classification
	HANDLE input_target = INVALID_HANDLE_VALUE;
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
		input_ok = SetConsoleMode(input_target, input_flags);
		if (input_ok) {
			WINCONSOLE.used_input_handle = input_target;
			VERIFY_VT_INPUT(input_target);
		}
	}
	
	if (output_target != INVALID_HANDLE_VALUE) {
		output_ok = SetConsoleMode(output_target, output_flags);
		if (output_ok) {
			WINCONSOLE.used_output_handle = output_target;
			VERIFY_VT_OUTPUT(output_target);
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
	
	if (input_ok && output_ok)
	{
		TTY effective_tty = enhanced_tty;
		
		// Windows forces canonical mode when echo is used
		if ((enhanced_tty.c_lflag & ECHO) && !(enhanced_tty.c_lflag & ICANON))
		{
			effective_tty.c_lflag &= ~(CBREAK | RAW);
			effective_tty.c_lflag |= ICANON;
		}
		
		WINCONSOLE.ttyflags = effective_tty;
		WINCONSOLE.original_intent = arg->c_lflag;
		WINCONSOLE.last_input_mode = input_flags;
		WINCONSOLE.last_output_mode = output_flags;
		
		WINCONSOLE.explicitly_raw = (arg->c_lflag & RAW) != 0;
		WINCONSOLE.explicitly_cbreak = (arg->c_lflag & CBREAK) != 0;
		
		return OK;
	}
	
	return ERR;
}

NCURSES_EXPORT(int)
_nc_conpty_tcgetattr(int fd, TTY *arg)
{
	if (!arg) return ERR;
	
	HANDLE stdin_hdl = GetStdHandle(STD_INPUT_HANDLE);
	HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);
	HANDLE fd_hdl = INVALID_HANDLE_VALUE;
	HandleType fd_type = NotKnown;
	
	// Classify fd handle
	if (fd >= 0) {
		fd_hdl = (HANDLE)_get_osfhandle(fd);
		if (fd_hdl != INVALID_HANDLE_VALUE) {
			DWORD test_mode;
			if (GetConsoleMode(fd_hdl, &test_mode)) {
				fd_type = classify_handle(fd_hdl);
			}
		}
	}
	
	// Determine which handles to query based on classification
	HANDLE input_source = INVALID_HANDLE_VALUE;
	HANDLE output_source = INVALID_HANDLE_VALUE;
	
	if (fd_type == Input) {
		input_source = fd_hdl;
		output_source = WINCONSOLE.used_output_handle != INVALID_HANDLE_VALUE 
		               ? WINCONSOLE.used_output_handle : stdout_hdl;
	} else if (fd_type == Output) {
		input_source = WINCONSOLE.used_input_handle != INVALID_HANDLE_VALUE 
		              ? WINCONSOLE.used_input_handle : stdin_hdl;
		output_source = fd_hdl;
	} else {
		input_source = WINCONSOLE.used_input_handle != INVALID_HANDLE_VALUE 
		              ? WINCONSOLE.used_input_handle : stdin_hdl;
		output_source = WINCONSOLE.used_output_handle != INVALID_HANDLE_VALUE 
		               ? WINCONSOLE.used_output_handle : stdout_hdl;
	}
	
	DWORD input_mode = 0, output_mode = 0;
	bool got_input = false, got_output = false;
	
	// Read modes from appropriate handles
	if (input_source != INVALID_HANDLE_VALUE) {
		got_input = GetConsoleMode(input_source, &input_mode);
		if (!got_input && GetLastError() == 6) {
			input_mode = WINCONSOLE.last_input_mode;
			got_input = (input_mode != 0);
		}
	}
	
	if (output_source != INVALID_HANDLE_VALUE) {
		got_output = GetConsoleMode(output_source, &output_mode);
		if (!got_output && GetLastError() == 6) {
			output_mode = WINCONSOLE.last_output_mode;
			got_output = (output_mode != 0);
		}
	}
	
	// Verify VT processing is enabled
	if (got_output) {
		VERIFY_VT_OUTPUT(output_source);
	}
	if (got_input) {
		VERIFY_VT_INPUT(input_source);
	}
	
	// Reconstruct Unix flags
	if (got_input || got_output) {
		if (WINCONSOLE.original_intent != 0 || WINCONSOLE.ttyflags.c_lflag != 0) {
			*arg = WINCONSOLE.ttyflags;
			
			// Verify console hasn't been changed externally
			if ((got_input && input_mode != WINCONSOLE.last_input_mode) ||
			    (got_output && output_mode != WINCONSOLE.last_output_mode)) {
				TTY current_state;
				memset(&current_state, 0, sizeof(current_state));
				
				if (got_input) {
					win32_to_unix_input_flags(input_mode, &current_state);
				}
				if (got_output) {
					win32_to_unix_output_flags(output_mode, &current_state);
				}
				
				// Apply special mode knowledge from tracking
				if (WINCONSOLE.explicitly_raw && !(current_state.c_lflag & ICANON)) {
					current_state.c_lflag = (current_state.c_lflag & ~CBREAK) | RAW;
				}
				else if (WINCONSOLE.explicitly_cbreak && !(current_state.c_lflag & ICANON)) {
					current_state.c_lflag = (current_state.c_lflag & ~RAW) | CBREAK;
				}
				
				*arg = current_state;
			}
		} else {
			memset(arg, 0, sizeof(*arg));
			if (got_input) {
				win32_to_unix_input_flags(input_mode, arg);
			}
			if (got_output) {
				win32_to_unix_output_flags(output_mode, arg);
			}
		}
		return OK;
	}
	
	*arg = WINCONSOLE.ttyflags;
	return OK;
}

NCURSES_EXPORT(void)
NCURSES_SP_NAME(_nc_conpty_echo_sync) (NCURSES_SP_DCL0)
{
	if (SP_PARM != NULL) {
        	TERMINAL *termp = TerminalOf(SP_PARM);
        	if (termp != NULL) {
            		TTY current_tty;
            		memset(&current_tty, 0, sizeof(current_tty));
            
            		// Build TTY structure from current ncurses state
            		if (IsRaw(SP_PARM)) {
                		current_tty.c_lflag |= RAW;
            		} else if (IsCbreak(SP_PARM)) {
                		current_tty.c_lflag |= (CBREAK | ISIG);
            		} else {
                		current_tty.c_lflag |= (ICANON | ISIG | ONLCR);
            		}
            
            		if (IsEcho(SP_PARM)) {
                		current_tty.c_lflag |= ECHO;
            		}
            
            		// Apply the updated mode to console
            		_nc_conpty_tcsetattr(termp->Filedes, &current_tty);
	        }	
	}
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

static const BITNAMES unixFlags[] =
    {
	DATA(ISIG),
	DATA(ICANON),
	DATA(ECHO),
	DATA(ONLCR),
	DATA(CBREAK),
	DATA(RAW),
	{ 0, "" }
    };

static void DumpUnixFlags(void) {
    static char buf[512];
    
    buf[0] = '\0';
    lookup_bits(buf, sizeof(buf), unixFlags, "Unix c_lflag", WINCONSOLE.ttyflags.c_lflag);
    OutputDebugStringA("UNIX Flages: ");
    OutputDebugStringA(buf);
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
    DumpUnixFlags(); 
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
