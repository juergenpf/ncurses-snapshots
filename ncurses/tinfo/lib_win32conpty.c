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

MODULE_ID("$Id$")

#if defined(_NC_WINDOWS_NATIVE)
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <winternl.h>
#include <io.h>
#include <fcntl.h>
#include <process.h>
#if USE_WIDEC_SUPPORT
#include <wchar.h>
#endif

#define DispatchMethod(name) pty_##name
#define METHOD(name,type) static type DispatchMethod(name)

// Prototypes of static function we want to use in initializers
METHOD(init,BOOL)(int fdOut, int fdIn);
METHOD(size,void)(int *Lines, int *Cols);
METHOD(check_resize,BOOL)(void);
METHOD(setmode,int)(int fd, const TTY *arg);
METHOD(getmode,int)(int fd, TTY *arg);
METHOD(defmode,int)(TTY *arg, short kind);
METHOD(flush,int)(int fd);
METHOD(read,int)(int fd, int *result);
METHOD(write,int)(int fd, const void *buf, size_t count);
METHOD(start_input_subsystem,int)(void);
METHOD(stop_input_subsystem,int)(void);
METHOD(poll,int)(struct pty_pollfd *fds, nfds_t nfds, int timeout_ms);

/*   A process can only have a single console, so it is safe
	 to maintain all the information about it in a single
	 static structure.
 */
static ConsoleInfo defaultCONSOLE = {
    .initialized = FALSE,
    .conhost_flags = 0,
    .ttyflags = {0, 0, TTY_MODE_UNSPECIFIED},
    .ConsoleHandleIn = INVALID_HANDLE_VALUE,
    .ConsoleHandleOut = INVALID_HANDLE_VALUE,
    .sbi_lines = -1,
    .sbi_cols = -1,

	.init = DispatchMethod(init),
    .size = DispatchMethod(size),
    .check_resize = DispatchMethod(check_resize),
    .setmode = DispatchMethod(setmode),
    .getmode = DispatchMethod(getmode),
    .defmode = DispatchMethod(defmode),
    .flush = DispatchMethod(flush),
    .read = DispatchMethod(read),
    .write = DispatchMethod(write),
    .start_input_subsystem = DispatchMethod(start_input_subsystem),
	.stop_input_subsystem  = DispatchMethod(stop_input_subsystem),
	.poll = DispatchMethod(poll)
};

/*
* Poor man's dependency injection - we maintain a pointer to the current console information,
* which is initialized to point to our default implementation. If in the future we want to 
* support other types of consoles or terminal backends on Windows, we can create additional 
* ConsoleInfo structures with different implementations of the methods, and switch the
* _nc_currentCONSOLE pointer to point to the appropriate one based on runtime detection 
* or configuration.
*/
NCURSES_EXPORT_VAR(ConsoleInfo *)
_nc_currentCONSOLE = &defaultCONSOLE;

// ---------------------------------------------------------------------------------------
/*
* In order to stay strictly in the pipe I/O model of the Windows Console, we need to have 
* a dedicated thread that is responsible for reading input from the console handle and 
* putting it into a thread-safe buffer that the main thread can read from. This is because 
* the Windows Console does not support non-blocking reads or polling in the same way that 
* a Unix terminal does, so we need to have a separate thread that can block on ReadConsoleInput 
* and then signal the main thread when input is available.
* This input model is only used when in ncurses program mode, where we want to have direct 
* control over the console input. In shell mode, we can rely on the C runtime to handle 
* console input in the usual way, and we don't need.
* We implement a standard ring buffer for the input, and use Windows events to signal between 
* the threads when input is available or when the thread should shut down. We also implement 
* a simple lazy read model, where the input thread only reads from the console when the main 
* thread signals that it wants to read. 
*/
#define INPUT_BUFFER_SIZE 4096
typedef struct {
    uint8_t buf[INPUT_BUFFER_SIZE];
    int head;
    int tail;
    CRITICAL_SECTION lock;
} InputBuffer;

static InputBuffer g_input_buffer;
static HANDLE g_input_thread = NULL;

static HANDLE g_read_request_event = NULL;   // Signal: "Thread, bitte ReadFile aufrufen"
static HANDLE g_input_available_event = NULL; // Signal: "Daten liegen im Ringbuffer"
static HANDLE g_shutdown_event = NULL;        // Signal: "System beenden"
// Our handle is always the consoles input handle.
#define g_stdin_handle defaultCONSOLE.ConsoleHandleIn

/* Forward declaration of the input reader thread */
static unsigned __stdcall input_thread(LPVOID param);

// ---------------------------------------------------------------------------------------

#define UTF8_CP 65001

/* As long as we rely on MinGW for building, it doesn't provide support for 
* .UTF-8 locale names, so we need to use the codepage numbers directly.
* As soon as we have native compiles with clang or msvc against UCRT working, 
* we can switch to the more standard locale names and improve this 
* initialization logic to be more robust and flexible in handling different 
* types of encodings and locales on Windows.
*/
static void
encoding_init(void)
{
#if defined(_UCRT)
	char *newlocale = NULL;
#endif
	char *cur_loc = NULL;
	/* We query the system for the default ANSI code page */
	WCHAR buf[16];
	char localebuf[16];
	UINT cp;
	int len = GetLocaleInfoEx(
    		LOCALE_NAME_SYSTEM_DEFAULT,
    		LOCALE_IDEFAULTANSICODEPAGE,
    		buf,
    		16);
#if USE_WIDEC_SUPPORT
	cp = UTF8_CP;
#else
	if (len > 0)
		cp = (UINT)_wtoi(buf);
	else
		cp = 1252; /* last line of defense if GetLocaleInfoEx fails is to assume a 
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
        newlocale=setlocale(LC_CTYPE, localebuf);
	T(("conpty setlocale() result locale is %s", newlocale ? newlocale : "NULL"));

	cur_loc = setlocale(LC_CTYPE, NULL);
	T(("conpty Current locale now %s, code page %u", cur_loc ? cur_loc : "NULL", cp));
#else
	T(("conpty: Not using UCRT - relying on current locale for code page handling"));
#endif /* defined(_UCRT	) */
	SetConsoleCP(cp);
	SetConsoleOutputCP(cp);
}

#define REQUIRED_MAJOR_V (DWORD)10
#define REQUIRED_MINOR_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763

typedef NTSTATUS(WINAPI *RtlGetVersionPtr)(PRTL_OSVERSIONINFOW);

static bool get_real_windows_version(DWORD *major, DWORD *minor, DWORD *build)
{
	HMODULE ntdll = GetModuleHandle(TEXT("ntdll.dll"));
	if (ntdll)
	{
	       FARPROC proc = GetProcAddress(ntdll, "RtlGetVersion");
	       union {
		       FARPROC proc;
		       RtlGetVersionPtr func;
	       } cast;
		   RtlGetVersionPtr RtlGetVersion = NULL;
	       cast.proc = proc;
	       RtlGetVersion = cast.func;
	       if (RtlGetVersion)
	       {
		       RTL_OSVERSIONINFOW osvi = {0};
		       osvi.dwOSVersionInfoSize = sizeof(osvi);
		       if (RtlGetVersion(&osvi) == 0)
		       {
			       *major = osvi.dwMajorVersion;
			       *minor = osvi.dwMinorVersion;
			       *build = osvi.dwBuildNumber;
			       return true;
		       }
	       }
	}
	return false;
}

/* Check if the current Windows version supports ConPTY, which is a 
*  requirement for the Windows Console backend of ncurses. This is 
*  because without ConPTY, the Windows Console does not provide the 
*  necessary capabilities for ncurses and escpecially the terminfo 
*  layer to function properly.
*/
static BOOL
conpty_supported(void)
{
	int result = FALSE;
	DWORD major, minor, build;

	T((T_CALLED("lib_win32conpty::conpty_supported")));

	if (!get_real_windows_version(&major, &minor, &build))
	{
		T(("RtlGetVersion failed"));
		returnBool(FALSE);
	}
	if (major >= REQUIRED_MAJOR_V)
	{
		T(("Windows version detected: %d.%d (build %d)",(int) major, (int) minor, (int) build));
		if (major == REQUIRED_MAJOR_V)
		{
			if (((minor == REQUIRED_MINOR_V) &&
			     (build >= REQUIRED_BUILD)) ||
			    ((minor > REQUIRED_MINOR_V)))
				result = TRUE;
		}
		else
			result = TRUE;
	}
	returnBool(result);
}

/*
 * Reliably validate that stdout is in ConPTY mode and not in console mode.
 * Returns:
 *   true  - stdout is in ConPTY mode (virtual terminal processing enabled)
 *   false - stdout is in legacy console mode or not a console at all
 */
static BOOL
output_is_conpty(void)
{
	HANDLE out_handle;
	DWORD console_mode = 0;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	BOOL result = FALSE;

	T((T_CALLED("lib_win32conpty::output_is_conpty()")));

	out_handle = defaultCONSOLE.ConsoleHandleOut;
	if (GetConsoleMode(out_handle, &console_mode) == 0)
	{
		T(("GetConsoleMode() failed"));
		returnBool(FALSE);
	}

	if (console_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	{
		if (GetConsoleScreenBufferInfo(out_handle, &csbi))
		{
			result = TRUE;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING - ConPTY detected"));
		}
		else
		{
			result = TRUE;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING (no CSBI) - ConPTY detected"));
		}
	}
	else
	{
		T(("Console mode lacks ENABLE_VIRTUAL_TERMINAL_PROCESSING - legacy console mode"));
	}

	returnBool(result);
}

/*
This initializaton function can be called multiple time, and actually it is called from within
setupterm() the first time and potentially if we enter ncurses from newterm() the next time.
The main purpose is to initialize the WINCONSOLE structure when called the first time. The
first call will alway have fdIn set to -1, as setupterm() only cares about output. Please note
that setupterm() already handles redirection of stdout and assigns stderr for output if stdout
is not a tty.

The other purpose of this routine is to manage the assignment of console handles. If the
assigned filedescriptors are NOT valid console handles, the call will return FALSE.

The function will also return FALSE, if the Windows version we run on does not support ConPTY,
which is a requirement for the Windows Console backend of ncurses. This is because without
ConPTY, the Windows Console does not provide the necessary capabilities for ncurses and
escpecially the terminfo layer to function properly.
*/
METHOD(init,BOOL)(int fdOut, int fdIn)
{
	BOOL result = FALSE;
	BOOL check;

	T((T_CALLED("lib_win32conpty::pty_init(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

	/* initialize once, or not at all */
	if (!defaultCONSOLE.initialized)
	{
		// We set the console mode flags to the most basic ones that are required for ConPTY 
		// to function properly. We will let the C runtime handle any necessary translations, 
		// and we will handle UTF-8 encoding and decoding ourselves to avoid any unpredictable 
		// interferences with the various C runtimes on Windows (when using wide c)
		DWORD dwFlagIn = (ENABLE_LINE_INPUT 
			| ENABLE_PROCESSED_INPUT 
			| ENABLE_ECHO_INPUT  
			| ENABLE_EXTENDED_FLAGS);

		DWORD dwFlagOut = (ENABLE_VIRTUAL_TERMINAL_PROCESSING 
			| ENABLE_PROCESSED_OUTPUT
			| DISABLE_NEWLINE_AUTO_RETURN 
			| ENABLE_WRAP_AT_EOL_OUTPUT);

		const char *env_flags = getenv("NC_CONHOST_FLAGS");
		DWORD dwFlag;

		HANDLE stdin_hdl  = GetStdHandle(STD_INPUT_HANDLE);
		HANDLE stdout_hdl = GetStdHandle(STD_OUTPUT_HANDLE);

		if (!conpty_supported())
		{
			T(("Windows version does not support ConPTY"));
			returnBool(FALSE);
		}

		if (fdIn != -1)
		{
			T(("In the first call fdIn is expected to be -1."));
			returnBool(FALSE);
		}

		encoding_init();

		// The NC_CONHOST_FLAGS environment variable is for future use.
		defaultCONSOLE.conhost_flags = 0;
		if (env_flags && *env_flags)
		{
			char *endptr;
			long flags_val = strtol(env_flags, &endptr, 0);
			if (*endptr == '\0' && flags_val >= 0)
			{
				defaultCONSOLE.conhost_flags = (unsigned int)(flags_val & NC_CONHOST_FLAG_MASK);
			}
		}

		if (stdout_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(stdout_hdl, &dwFlag) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ConsoleHandleOut = stdout_hdl;

		if (stdin_hdl == INVALID_HANDLE_VALUE || GetConsoleMode(stdin_hdl, &dwFlag) == 0)
		{
			T(("StdIn handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ConsoleHandleIn = stdin_hdl;

		SetConsoleMode(stdout_hdl, dwFlagOut);
		// We immediately read the console mode back to reflect any changes the
		// runtime my have added, so the saved value reflects the actual mode
		// of tghe console. 
		if (GetConsoleMode(stdout_hdl, &dwFlagOut) == 0)
		{
			T(("GetConsoleMode() failed for stdout"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ttyflags.dwFlagOut = dwFlagOut;

		SetConsoleMode(stdin_hdl, dwFlagIn);
		// We immediately read the console mode back to reflect any changes the
		// runtime my have added, so the saved value reflects the actual mode
		// of tghe console. 
		if (GetConsoleMode(stdin_hdl, &dwFlagIn) == 0)
		{
			T(("GetConsoleMode() failed for stdin"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ttyflags.dwFlagIn = dwFlagIn;

		defaultCONSOLE.initialized = TRUE;
		result = TRUE;
	}
	else
	{ // This branch is called from newterm() when fdIn is provided, so we need to validate 
	  // that the provided fdIn and fdOut are valid console handles, and if so we update the 
	  // WINCONSOLE structure to use the new handles.
		DWORD dwFlagOut;
		DWORD dwFlagIn;

		if (GetConsoleMode(defaultCONSOLE.ConsoleHandleOut, &dwFlagOut) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		if (GetConsoleMode(defaultCONSOLE.ConsoleHandleIn, &dwFlagIn) == 0)
		{
			T(("Input handle is not a console"));
			returnBool(FALSE);
		}
		defaultCONSOLE.ttyflags.dwFlagOut = dwFlagOut;
		defaultCONSOLE.ttyflags.dwFlagIn = dwFlagIn;

		result = TRUE;
	}
	check = output_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   defaultCONSOLE.initialized,
	   check));
	returnBool(result);
}

/*
 * Check if the Windows Console has been resized.
 * This provides SIGWINCH-like functionality for Windows ConPTY.
 * Returns TRUE if a resize was detected.
 */
METHOD(check_resize,BOOL)(void)	
{
	int current_lines, current_cols;
	bool resized = FALSE;

	T((T_CALLED("lib_win32conpty::pty_check_resize()")));

	DispatchMethod(size)(&current_lines, &current_cols);

	if (defaultCONSOLE.sbi_lines == -1 || defaultCONSOLE.sbi_cols == -1)
	{
		defaultCONSOLE.sbi_lines = current_lines;
		defaultCONSOLE.sbi_cols = current_cols;
		returnBool(FALSE);
	}

	if (current_lines != defaultCONSOLE.sbi_lines || current_cols != defaultCONSOLE.sbi_cols)
	{
		defaultCONSOLE.sbi_lines = current_lines;
		defaultCONSOLE.sbi_cols = current_cols;

		_nc_globals.have_sigwinch = 1;

		resized = TRUE;
	}

	returnBool(resized);
}

/*
 * Get the current size of the Windows Console in lines and columns.
 * This method must not alter the cached values stored in ConsoleInfo.
 * It should report the result from the GetConsoleScreenBufferInfo 
 * API call directly. It may use the cached values as a fallback if the 
 * API call fails. The use of this API call is non-destructive in the 
 * API context.
 * This method can be safely called before the Console is initialized,
 * because we can fallback to query the standard handles.x
 */
METHOD(size,void)(int *Lines, int *Cols)
{
	T((T_CALLED("lib_win32conpty::pty_size(lines=%p, cols=%p)"), Lines, Cols));

	if (Lines != NULL && Cols != NULL)
	{
		CONSOLE_SCREEN_BUFFER_INFO csbi;
		HANDLE hdl_out = defaultCONSOLE.ConsoleHandleOut;

		if (hdl_out != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hdl_out, &csbi))
		{
			*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
			*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
		}
		else
		{
			HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
			if (hOut != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hOut, &csbi))
			{
				*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
				*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
			}
			else
			{ // Maybe stderr works...
				HANDLE hErr = GetStdHandle(STD_ERROR_HANDLE);
				if (hErr != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(hErr, &csbi))
				{
					*Lines = (int)(csbi.srWindow.Bottom + 1 - csbi.srWindow.Top);
					*Cols = (int)(csbi.srWindow.Right + 1 - csbi.srWindow.Left);
				}
				else
					// Fallback to cached values or defaults if we can't get the console size
					// We assume Windows Terminal is our host, it has modern default size of
					// 120x30, but if the cached values are set we use those instead to reflect
					// the actual size of the console.
					*Lines = defaultCONSOLE.sbi_lines != -1 ? defaultCONSOLE.sbi_lines : 120;
				*Cols = defaultCONSOLE.sbi_cols != -1 ? defaultCONSOLE.sbi_cols : 30;
			}
		}
	}
}

// ---------------------------------------------------------------------------------------
/*
* This function is called, when we enter ncurses program mode, which means we want to take 
* control over the console input and use our own input thread and buffer to manage console 
* input. We initialize the necessary synchronization primitives and start the input thread, 
* which will block on reading from the console input handle.
*/
METHOD(start_input_subsystem,int)(void)	
{
	T((T_CALLED("lib_win32conpty::start_input_subsystem()")));
	if (g_input_thread != NULL) 
		returnCode(OK); // Already initialized

	if (g_stdin_handle == INVALID_HANDLE_VALUE) 
		returnCode(ERR);
	
    g_read_request_event = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (g_read_request_event == NULL) 
		returnCode(ERR);
    
	g_input_available_event = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (g_input_available_event == NULL) {
		CloseHandle(g_read_request_event);
		returnCode(ERR);
	}
    
	g_shutdown_event = CreateEvent(NULL, TRUE, FALSE, NULL);
    if (g_shutdown_event == NULL) {
		CloseHandle(g_read_request_event);
		CloseHandle(g_input_available_event);
		returnCode(ERR);
	}

	InitializeCriticalSection(&g_input_buffer.lock);

    g_input_thread =(HANDLE) _beginthreadex(NULL, 0, input_thread, &g_input_buffer, 0, NULL);
	if (g_input_thread == NULL) 
	{
		CloseHandle(g_read_request_event);
		CloseHandle(g_input_available_event);
		CloseHandle(g_shutdown_event);
		DeleteCriticalSection(&g_input_buffer.lock);
		returnCode(ERR);
	}
	returnCode(OK);    
}

/*
* This function is called when we exit ncurses program mode, which means we want to shut down 
* our input thread and clean up the synchronization primitives and buffers. We signal the 
* input thread to shut down, and we also cancel any pending ReadFile operation in case the 
* thread is currently blocked on reading from the console, to ensure that it can exit promptly. 
* We then wait for the thread to exit. Finally, we clean up all the resources and reset the 
* global variables to their initial state.
*/
METHOD(stop_input_subsystem,int)(void)
{
	T((T_CALLED("lib_win32conpty::stop_input_subsystem()")));
	if (g_input_thread==NULL) 
		returnCode(OK);

    SetEvent(g_shutdown_event);
    
    /*
	 * Force input_thread to exit immediately if it's currently blocked in ReadFile 
	 * by cancelling the I/O operation. This is necessary to ensure that we can shut 
	 * down cleanly even if the input thread is waiting for input and no input is 
	 * coming.
	*/
    CancelSynchronousIo(g_input_thread);

	// Wait until thread really exits, with a timeout to avoid hanging indefinitely 
	// in case something goes wrong.
    if (WaitForSingleObject(g_input_thread, 2000) == WAIT_TIMEOUT) {
		// Emergency measure if the thread is extremely stubborn and does not exit 
		// in a reasonable time frame.
        TerminateThread(g_input_thread, 0); 
    }

    CloseHandle(g_input_thread);
	g_input_thread = NULL; // IMPORTANT!!

    CloseHandle(g_read_request_event);
	g_read_request_event = NULL;
	
    CloseHandle(g_input_available_event);
	g_input_available_event = NULL;

	CloseHandle(g_shutdown_event);
	g_shutdown_event = NULL;

    DeleteCriticalSection(&g_input_buffer.lock);
	g_input_buffer.lock = (CRITICAL_SECTION){0};

	g_input_buffer.head = 0;
	g_input_buffer.tail = 0;
	
	returnCode(OK);
}

/**
 * This function returns the number of bytes available in the input buffer.
 */
static int 
input_available_count(InputBuffer *pbuf) 
{
	T((T_CALLED("lib_win32conpty::input_available_count(pbuf=%p)"), pbuf));
	assert(g_input_thread != NULL);
    EnterCriticalSection(&pbuf->lock);
    int avail = (pbuf->head - pbuf->tail + INPUT_BUFFER_SIZE) % INPUT_BUFFER_SIZE;
    LeaveCriticalSection(&pbuf->lock);
	returnCode(avail);
}

/**
 * This function writes data to the input buffer. It is called by the input thread when it 
 * reads data from the console, and it needs to store that data in the buffer for the main 
 * thread to read later. The function takes care of managing the head and tail indices of 
 * the ring buffer, and it also ensures that if the buffer becomes full, it will overwrite 
 * the oldest data (by advancing the tail index). The function is protected by a critical 
 * section to ensure thread safety when accessing the buffer.
 */
static void 
ringbuffer_write(InputBuffer *pbuf, uint8_t *data, DWORD n) 
{
	T((T_CALLED("lib_win32conpty::ringbuffer_write(pbuf=%p, data=%p, n=%lu)"), pbuf, data, n));
    EnterCriticalSection(&pbuf->lock);
    for (DWORD i = 0; i < n; i++) {
        pbuf->buf[pbuf->head] = data[i];
        pbuf->head = (pbuf->head + 1) % INPUT_BUFFER_SIZE;
        if (pbuf->head == pbuf->tail)
            pbuf->tail = (pbuf->tail + 1) % INPUT_BUFFER_SIZE;
    }
    LeaveCriticalSection(&pbuf->lock);
}

/**
 * This function reads a byte from the input buffer. It is called by the main thread when it 
 * wants to read input that has been stored in the buffer by the input thread. The function 
 * takes care of managing the head and tail indices of the ring buffer, and it is protected 
 * by a critical section to ensure thread safety when accessing the buffer.
 */
static BOOL
ringbuffer_read(InputBuffer *pbuf, uint8_t *byte) 
{
	T((T_CALLED("lib_win32conpty::ringbuffer_read(pbuf=%p, byte=%p)"), pbuf, byte));
    EnterCriticalSection(&pbuf->lock);
    if (((pbuf->head - pbuf->tail + INPUT_BUFFER_SIZE) % INPUT_BUFFER_SIZE) == 0) {
        LeaveCriticalSection(&pbuf->lock);
        returnBool(FALSE);
    }
    *byte = pbuf->buf[pbuf->tail];
    pbuf->tail = (pbuf->tail + 1) % INPUT_BUFFER_SIZE;
    LeaveCriticalSection(&pbuf->lock);
    returnBool(TRUE);
}

/**
 * This function is the entry point for the input thread. It continuously waits for input
 * requests or shutdown events, reads data from the console, and writes it to the input buffer.
 */
static unsigned __stdcall
input_thread(LPVOID param) 
{
    InputBuffer *pbuf = (InputBuffer*)param;
    uint8_t tmp[256];
    DWORD n;

	T((T_CALLED("lib_win32conpty::input_thread(param=%p)"), param));
    HANDLE wait_handles[2] = { g_shutdown_event, g_read_request_event };

    while (1) {
        DWORD r = WaitForMultipleObjects(2, wait_handles, FALSE, INFINITE);
        if (r == WAIT_OBJECT_0) {
			T(("input_thread: Shutdown event received, exiting thread"));
			break; 
		}
		else if (r != WAIT_OBJECT_0 + 1) {
			T(("input_thread: Unexpected wait result: %lu, continuing...", r));
			continue;
		}

        if (ReadFile(g_stdin_handle, tmp, sizeof(tmp), &n, NULL)) {
            if (n > 0) {
                ringbuffer_write(pbuf, tmp, n);
                SetEvent(g_input_available_event);
            }
        } else {
			DWORD err = GetLastError();
            if (err == ERROR_OPERATION_ABORTED) {
				T(("input_thread: ReadFile was cancelled, exiting thread"));	 
				break;
			}
        }
        ResetEvent(g_read_request_event);
    }
	T(("input_thread: Thread exiting"));
    return 0;
}

/**
 * This function attempts to read a byte from the input buffer without blocking.
 * It is called by the main thread when it wants to read input that has been stored
 * in the buffer by the input thread. The function returns immediately if no input
 * is available.
 */
static int 
get_byte_nonblocking(void) 
{
	uint8_t byte;
	T((T_CALLED("lib_win32conpty::get_byte_nonblocking()")));
    if (ringbuffer_read(&g_input_buffer, &byte)) {
        if (input_available_count(&g_input_buffer) == 0)
            ResetEvent(g_input_available_event);
        returnCode((int)byte);
    }
    returnCode(-1);
} 

/**
 * This function attempts to read a byte from the input buffer, blocking if necessary.
 * It is called by the main thread when it wants to read input that has been stored
 * in the buffer by the input thread. The function waits until input is available.
 */
static int 
get_byte_blocking(void) 
{
    uint8_t byte;

	T((T_CALLED("lib_win32conpty::get_byte_blocking()")));

	while (input_available_count(&g_input_buffer) == 0) {
        SetEvent(g_read_request_event);
        WaitForSingleObject(g_input_available_event, INFINITE);
    }

    if (ringbuffer_read(&g_input_buffer, &byte)) {
        if (input_available_count(&g_input_buffer) == 0) {
            ResetEvent(g_input_available_event);
        }
        returnCode((int)byte);
    }

    returnCode(-1); // Should never happen
}

/**
 * This function polls the input buffer for available data. It waits for the specified
 * timeout and returns 1 if data is available, 0 if the timeout expires, and -1 on error.
 */
static int 
poll_input(DWORD timeout_ms)
{
	DWORD r;

    if (input_available_count(&g_input_buffer) > 0)
        return 1;

    SetEvent(g_read_request_event);

    r = WaitForSingleObject(g_input_available_event,timeout_ms);

    if (r == WAIT_OBJECT_0)
        return 1;

    if (r == WAIT_TIMEOUT)
        return 0;

    return -1;
}

/**
 * This function polls the input buffer for available data. It waits for the specified
 * timeout and returns 1 if data is available, 0 if the timeout expires, and -1 on error.
 * This is the implementation of the pty_poll method for the Windows Console backend, which
 * allows the main thread to wait for input without blocking indefinitely, by waiting on 
 * the input_available_event that the input thread signals when it has read data from the 
 * console and stored it in the input buffer.
 * The call has the same signature as the corresponding UNIX function, but it only supports 
 * polling on stdin (fd 0) and ignores any other file descriptors, since the Windows Console 
 * backend is designed to work with the console input handle directly. If the caller tries 
 * to poll on any other file descriptor or more than one file descriptor, the function 
 * will return -1 to indicate an error.
 * We implement it that way, so that we can use it in module lib_twait.c with minimal changes 
 * to the original UNIX based des, and we can also support the standard ncurses polling 
 * mechanism in a way that is consistent with the rest of the Windows Console backend design.
 */
METHOD(poll,int)(struct pty_pollfd *fds, nfds_t nfds, int timeout_ms) 
{
	T((T_CALLED("lib_win32conpty::pty_poll(fds=%p, nfds=%zu, timeout_ms=%d)"), fds, nfds, timeout_ms));

	int code = -1;
	// We only support polling stdin
    if (nfds != 1 || fds[0].fd != 0) 
		return -1;;

	code = poll_input(timeout_ms);
	if (code < 0)
		return -1;
	if (code == 0)
		fds[0].revents = 0;
	else
		fds[0].revents = POLLIN;
	return 1;
}

/**
 * This function reads a byte of input from the console. It is called by the main thread when 
 * it wants to read input that has been stored in the buffer by the input thread. The function 
 * blocks until input is available, and then it returns the byte that was read. If there is an 
 * error, it returns -1.
 */
METHOD(read,int)(int fd GCC_UNUSED, int *result)
{
	uint8_t byte;

	T((T_CALLED("lib_win32conpty::pty_read(fd=%d, result=%p)"), fd, result));

	if (!result)
		return -1;

	if (g_input_thread == NULL) {
		// If the input thread is not running, we are likely in a non-ncurses context,  
		// so we can read directly from the console handle.
		HANDLE hIn = defaultCONSOLE.ConsoleHandleIn;
		DWORD n;
		if (!ReadFile(hIn, &byte, sizeof(byte), &n, NULL) || n == 0)
			return -1;
	} else {
		// If the input thread is running, we read from the ring buffer it fills
		byte = get_byte_blocking();
		if (byte == (uint8_t)-1)
			return -1;
	}
	*result = (int)byte;
	return 1;
}

/*
 * This function writes data to the console output. It is called by the main thread when it 
 * wants to write output to the console. The function takes a buffer and a count of bytes to 
 * write, and it returns the number of bytes that were actually written, or -1 on error.
 */
METHOD(write,int)(int fd GCC_UNUSED, const void *buf, size_t count)
{
	HANDLE hOut = defaultCONSOLE.ConsoleHandleOut;
	DWORD written = 0;

	T((T_CALLED("lib_win32conpty::pty_write(fd=%d, buf=%p, count=%zu)"), fd, buf, count));

	if (!WriteFile(hOut, buf, (DWORD)count, &written, NULL))
	{
		T(("WriteFile failed with error %lu", GetLastError()));
		return -1;
	}

	return (int)written;
}

/**
 * This function flushes the console input buffer. It is called by the main thread when it 
 * wants to discard any pending input in the console. The function returns OK on success.
 */
METHOD(flush,int)(int fd GCC_UNUSED)
{
	int code = OK;
	T((T_CALLED("lib_win32conpty::pty_flush(fd=%d)"), fd));
	FlushConsoleInputBuffer(GetStdHandle(STD_INPUT_HANDLE));
	returnCode(code);
}

/*
 * This function sets the console mode for the input and output handles. It is called by the main thread
 * when it wants to change the console mode. The function takes a TTY structure that contains the desired
 * mode flags, and it returns OK on success or ERR on failure.
 * It is also responsible for detecting switches between shell mode and program mode, and starting or 
 * stopping the input subsystem accordingly.
 */
METHOD(setmode,int)(int fd GCC_UNUSED, const TTY *arg)
{
	HANDLE input_target = defaultCONSOLE.ConsoleHandleIn;
	HANDLE output_target = defaultCONSOLE.ConsoleHandleOut;
	BOOL input_ok = FALSE;
	BOOL output_ok = FALSE;

	T((T_CALLED("lib_win32conpty::pty_setmode(fd=%d, TTY*=%p)"), fd, arg));
	
	if (!arg)
		returnCode(ERR);

	if (input_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = ENABLE_VIRTUAL_TERMINAL_INPUT | arg->dwFlagIn;

		/*
		   ENABLE_VIRTUAL_TERMINAL_INPUT (VT) requires ENABLE_PROCESSED_INPUT to be effective.
		   If we request VT, we must ensure PROCESSED is set, otherwise SetConsoleMode fails.
		   We always allow mouse and window input events if VT input is requested, as these
		   are commonly used together and it simplifies the logic to just enable them when
		   VT is enabled.
		*/
		if (mode & ENABLE_VIRTUAL_TERMINAL_INPUT)
		{
			mode |= ENABLE_PROCESSED_INPUT;
		}

		/* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
		if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT))
		{
			mode &= ~ENABLE_ECHO_INPUT;
		}

		input_ok = SetConsoleMode(input_target, mode);
		if (input_ok)
		{
			// Make sure the cached value reflects the real value we set, as the
			// caller may not have provided all necessary flags (e.g.
			// PROCESSED_INPUT when VT is requested)
			DWORD realMode;
			if (GetConsoleMode(input_target, &realMode))
			{
				defaultCONSOLE.ttyflags.dwFlagIn = realMode;
			}
			else
			{
				defaultCONSOLE.ttyflags.dwFlagIn = mode;
			}		}
		else
		{
			T(("Invalid input file descriptor"));
		}
	}
		
	if (output_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = ENABLE_VIRTUAL_TERMINAL_PROCESSING | arg->dwFlagOut;
		output_ok = SetConsoleMode(output_target, mode);
		if (output_ok)
		{
			// Make sure the cached value reflects the real value we set,
			// as the caller may not have provided all necessary flags
			// (e.g. VT output is required for the Windows Console backend)
			DWORD realMode;
			if (GetConsoleMode(output_target, &realMode))
			{
				defaultCONSOLE.ttyflags.dwFlagOut = realMode;
			}
			else
			{
				defaultCONSOLE.ttyflags.dwFlagOut = mode;
			}
		}		
		else
		{
			T(("Invalid output file descriptor"));
		}		
	}

	if (arg->kind == TTY_MODE_SHELL)
	{
		T(("Shell mode set"));
		DispatchMethod(stop_input_subsystem)();
	}
	else if (arg->kind == TTY_MODE_PROGRAM)
	{
		T(("Program mode set"));
		DispatchMethod(start_input_subsystem)();
	}

	// Handle errors
	if (!input_ok || !output_ok)
	{
		returnCode(ERR);
	}

	returnCode(OK);
}

/*
* The defmode function is only called from def_shell_mode and def_prog_mode, and it's only
* purpose is to set the setfMode flag to true. If this flag is set, the setmode function
* above will actually apply the file mode settings to the console streams.
* This approach should help to avoid excessive unnecessary calls to _setmode, which can be 
* expensive and may cause issues with some C runtimes on Windows if called too frequently or 
* with incompatible modes.
*/
METHOD(defmode,int)(TTY *arg, short kind)
{
	short realMode = kind;

	T((T_CALLED("lib_win32conpty::pty_defmode(TTY*=%p, kind=%d)"), arg, kind));

	if (NULL == arg)
		returnCode(ERR);

	if (realMode == TTY_MODE_AUTO)
		realMode = (g_input_thread != NULL) ? TTY_MODE_PROGRAM : TTY_MODE_SHELL;

	if (realMode == TTY_MODE_SHELL)
	{
		arg->dwFlagIn &= ~(ENABLE_VIRTUAL_TERMINAL_INPUT);
		arg->dwFlagOut |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	}
	else if (realMode == TTY_MODE_PROGRAM)
	{
		arg->dwFlagIn |= ENABLE_VIRTUAL_TERMINAL_INPUT;
		arg->dwFlagOut |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
	}
	arg->kind = realMode;
	returnCode(OK);
}

/*
* getmode always sets the setfMode flag to FALSE. The trick is, that def_shell_mode and
* def_prog_mode will call abov method defmode to set the flag right after getting it.
* So only calls to reset_shell_mode and reset_prog_mode will have the setfMode flag 
* set to TRUE, which means that the next call to setmode will apply any file mode changes 
* to the console streams.
* By that approach the Windows specific filemode handling is finally reduced to the calls
* in the lib_ttyflag.c functions. 
*/
METHOD(getmode,int)(int fd GCC_UNUSED, TTY *arg)
{
	T((T_CALLED("lib_win32conpty::pty_getmode(fd=%d, TTY*=%p)"), fd, arg));

	if (NULL == arg)
		returnCode(ERR);

	*arg = defaultCONSOLE.ttyflags;
	arg->kind = TTY_MODE_UNSPECIFIED;
	returnCode(OK);
}

#endif /* defined(_NC_WINDOWS_NATIVE) */
