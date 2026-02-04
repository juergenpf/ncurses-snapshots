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

#if defined(USE_WIN32_CONPTY)
#include <curses.priv.h>
#include <nc_win32.h>
#include <locale.h>

#define CONTROL_PRESSED (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)

/*   A process can only have a single console, so it is safe
	 to maintain all the information about it in a single
	 static structure.
 */
NCURSES_EXPORT_VAR(ConsoleInfo)
_nc_CONSOLE;

static bool console_initialized = FALSE;
#define EnsureInit() (void)(console_initialized ? TRUE : _nc_console_checkinit())

#define NOT_IMPLEMENTED                                                          \
	{                                                                        \
		fprintf(stderr, "NOT IMPLEMENTED: %s:%d\n", __FILE__, __LINE__); \
		abort();                                                         \
	}

static ULONGLONG
tdiff(FILETIME fstart, FILETIME fend)
{
	ULARGE_INTEGER ustart;
	ULARGE_INTEGER uend;
	ULONGLONG diff;

	ustart.LowPart = fstart.dwLowDateTime;
	ustart.HighPart = fstart.dwHighDateTime;
	uend.LowPart = fend.dwLowDateTime;
	uend.HighPart = fend.dwHighDateTime;

	diff = (uend.QuadPart - ustart.QuadPart) / 10000;
	return diff;
}

static int
Adjust(int milliseconds, int diff)
{
	if (milliseconds != NC_INFINITY)
	{
		milliseconds -= diff;
		if (milliseconds < 0)
			milliseconds = 0;
	}
	return milliseconds;
}

#define BUTTON_MASK (FROM_LEFT_1ST_BUTTON_PRESSED | \
					 FROM_LEFT_2ND_BUTTON_PRESSED | \
					 FROM_LEFT_3RD_BUTTON_PRESSED | \
					 FROM_LEFT_4TH_BUTTON_PRESSED | \
					 RIGHTMOST_BUTTON_PRESSED)

static mmask_t
decode_mouse(const SCREEN *sp, int mask)
{
	mmask_t result = 0;

	(void)sp;
	assert(sp && console_initialized);

	if (mask & FROM_LEFT_1ST_BUTTON_PRESSED)
		result |= BUTTON1_PRESSED;
	if (mask & FROM_LEFT_2ND_BUTTON_PRESSED)
		result |= BUTTON2_PRESSED;
	if (mask & FROM_LEFT_3RD_BUTTON_PRESSED)
		result |= BUTTON3_PRESSED;
	if (mask & FROM_LEFT_4TH_BUTTON_PRESSED)
		result |= BUTTON4_PRESSED;

	if (mask & RIGHTMOST_BUTTON_PRESSED)
	{
		switch (WINCONSOLE.numButtons)
		{
		case 1:
			result |= BUTTON1_PRESSED;
			break;
		case 2:
			result |= BUTTON2_PRESSED;
			break;
		case 3:
			result |= BUTTON3_PRESSED;
			break;
		case 4:
			result |= BUTTON4_PRESSED;
			break;
		}
	}

	return result;
}

#define AdjustY() ((int)WINCONSOLE.SBI.srWindow.Top)

static bool
handle_mouse(SCREEN *sp, MOUSE_EVENT_RECORD mer)
{
	MEVENT work;
	bool result = FALSE;

	assert(sp);

	sp->_drv_mouse_old_buttons = sp->_drv_mouse_new_buttons;
	sp->_drv_mouse_new_buttons = mer.dwButtonState & BUTTON_MASK;

	/*
	 * We're only interested if the button is pressed or released.
	 * FIXME: implement continuous event-tracking.
	 */
	if (sp->_drv_mouse_new_buttons != sp->_drv_mouse_old_buttons)
	{
		memset(&work, 0, sizeof(work));

		if (sp->_drv_mouse_new_buttons)
		{
			work.bstate |= decode_mouse(sp, sp->_drv_mouse_new_buttons);
		}
		else
		{
			/* cf: BUTTON_PRESSED, BUTTON_RELEASED */
			work.bstate |= (decode_mouse(sp, sp->_drv_mouse_old_buttons) >> 1);
			result = TRUE;
		}

		work.x = mer.dwMousePosition.X;
		work.y = mer.dwMousePosition.Y - AdjustY();

		sp->_drv_mouse_fifo[sp->_drv_mouse_tail] = work;
		sp->_drv_mouse_tail += 1;
	}
	return result;
}

static bool
read_screen_data(void)
{
	bool result = FALSE;
	COORD bufferCoord;
	size_t want;

	WINCONSOLE.save_size.X = (SHORT)(WINCONSOLE.save_region.Right - WINCONSOLE.save_region.Left + 1);
	WINCONSOLE.save_size.Y = (SHORT)(WINCONSOLE.save_region.Bottom - WINCONSOLE.save_region.Top + 1);

	want = (size_t)(WINCONSOLE.save_size.X * WINCONSOLE.save_size.Y);

	if ((WINCONSOLE.save_screen = malloc(want * sizeof(CHAR_INFO))) != NULL)
	{
		bufferCoord.X = (SHORT)(WINCONSOLE.window_only
					    ? WINCONSOLE.SBI.srWindow.Left
					    : 0);
		bufferCoord.Y = (SHORT)(WINCONSOLE.window_only
					    ? WINCONSOLE.SBI.srWindow.Top
					    : 0);

		T(("... reading console %s %dx%d into %d,%d - %d,%d at %d,%d",
		   WINCONSOLE.window_only ? "window" : "buffer",
		   WINCONSOLE.save_size.Y, WINCONSOLE.save_size.X,
		   WINCONSOLE.save_region.Top,
		   WINCONSOLE.save_region.Left,
		   WINCONSOLE.save_region.Bottom,
		   WINCONSOLE.save_region.Right,
		   bufferCoord.Y,
		   bufferCoord.X));

		if (read_screen(WINCONSOLE.hdl,
				WINCONSOLE.save_screen,
				WINCONSOLE.save_size,
				bufferCoord,
				&WINCONSOLE.save_region))
		{
			result = TRUE;
		}
		else
		{
			T((" error %#lx", (unsigned long)GetLastError()));
			FreeAndNull(WINCONSOLE.save_screen);
		}
	}

	return result;
}

static bool
save_original_screen(void)
{
	bool result = FALSE;

	WINCONSOLE.save_region.Top = 0;
	WINCONSOLE.save_region.Left = 0;
	WINCONSOLE.save_region.Bottom = (SHORT)(WINCONSOLE.SBI.dwSize.Y - 1);
	WINCONSOLE.save_region.Right = (SHORT)(WINCONSOLE.SBI.dwSize.X - 1);

	if (read_screen_data())
	{
		result = TRUE;
	}
	else
	{

		WINCONSOLE.save_region.Top = WINCONSOLE.SBI.srWindow.Top;
		WINCONSOLE.save_region.Left = WINCONSOLE.SBI.srWindow.Left;
		WINCONSOLE.save_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
		WINCONSOLE.save_region.Right = WINCONSOLE.SBI.srWindow.Right;

		WINCONSOLE.window_only = TRUE;

		if (read_screen_data())
		{
			result = TRUE;
		}
	}
	T(("... save original screen contents %s", result ? "ok" : "err"));
	return result;
}

static bool
restore_original_screen(void)
{
	COORD bufferCoord;
	bool result = FALSE;
	SMALL_RECT save_region = WINCONSOLE.save_region;

	T(("... restoring %s",
	   WINCONSOLE.window_only ? "window" : "entire buffer"));

	bufferCoord.X = (SHORT)(WINCONSOLE.window_only ? WINCONSOLE.SBI.srWindow.Left : 0);
	bufferCoord.Y = (SHORT)(WINCONSOLE.window_only ? WINCONSOLE.SBI.srWindow.Top : 0);

	if (write_screen(WINCONSOLE.hdl,
			 WINCONSOLE.save_screen,
			 WINCONSOLE.save_size,
			 bufferCoord,
			 &save_region))
	{
		result = TRUE;
		SetConsoleCursorPosition(WINCONSOLE.hdl, WINCONSOLE.save_SBI.dwCursorPosition);
		T(("... restore original screen contents ok %dx%d (%d,%d - %d,%d)",
		   WINCONSOLE.save_size.Y,
		   WINCONSOLE.save_size.X,
		   save_region.Top,
		   save_region.Left,
		   save_region.Bottom,
		   save_region.Right));
	}
	else
	{
		T(("... restore original screen contents err"));
	}
	return result;
}

NCURSES_EXPORT(bool)
_nc_console_checkinit()
{
	bool res = FALSE;

	T((T_CALLED("lib_win32conpty::_nc_console_checkinit()")));

	/* initialize once, or not at all */
	if (!console_initialized)
	{
		int i;
		DWORD num_buttons;
		WORD a;
		BOOL b;

		START_TRACE();

		if (GetNumberOfConsoleMouseButtons(&num_buttons))
		{
			WINCONSOLE.numButtons = (int)num_buttons;
		}
		else
		{
			WINCONSOLE.numButtons = 1;
		}

		a = _nc_console_MapColor(true, COLOR_WHITE) |
		    _nc_console_MapColor(false, COLOR_BLACK);
		for (i = 0; i < CON_NUMPAIRS; i++)
			WINCONSOLE.pairs[i] = a;

#define SaveConsoleMode(handle, value) \
	GetConsoleMode(WINCONSOLE.handle, &WINCONSOLE.originalMode.value)

		WINCONSOLE.inp = GetStdHandle(STD_INPUT_HANDLE);
		WINCONSOLE.out = GetStdHandle(STD_OUTPUT_HANDLE);
		WINCONSOLE.hdl = WINCONSOLE.out;

		SaveConsoleMode(inp, dwFlagIn);
		SaveConsoleMode(out, dwFlagOut);

		/* We set binary I/O even when using the console
		   driver to cover the situation, that the
		   TERM variable is set to #win32con, but actually
		   Windows supports virtual terminal processing.
		   So if terminfo functions are used in this setup,
		   they actually may work.
		 */
		/*_nc_setmode(fileno(stdin), true, false);
		_nc_setmode(fileno(stdout), false, false);
		_nc_setmode(fileno(stderr), false, false);*/
		if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE)
		{
			_nc_console_get_SBI();
			WINCONSOLE.save_SBI = WINCONSOLE.SBI;
			save_original_screen();
			_nc_console_set_scrollback(FALSE, &WINCONSOLE.SBI);

			GetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
			T(("... initial cursor is %svisible, %d%%",
			   (WINCONSOLE.save_CI.bVisible ? "" : "not-"),
			   (int)WINCONSOLE.save_CI.dwSize));
		}

		WINCONSOLE.initialized = TRUE;
		console_initialized = TRUE;
	}
	res = (WINCONSOLE.hdl != INVALID_HANDLE_VALUE);
	BOOL check = _nc_stdout_is_conpty();
	T(("... console initialized=%d, isConpty=%d",
	   console_initialized,
	   check));
	returnBool(res);
}

#define REQUIRED_MAX_V (DWORD)10
#define REQUIRED_MIN_V (DWORD)0
#define REQUIRED_BUILD (DWORD)17763
/*
  This function returns 0 if the Windows version has no support for
  the modern Console interface, otherwise it returns 1
 */
NCURSES_EXPORT(int)
_nc_console_vt_supported(void)
{
	OSVERSIONINFO osvi;
	int res = 0;
	
	T((T_CALLED("lib_win32con::_nc_console_vt_supported")));
	ZeroMemory(&osvi, sizeof(OSVERSIONINFO));
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	GetVersionEx(&osvi);
	T(("GetVersionEx returnedMajor=%lu, Minor=%lu, Build=%lu",
	   (unsigned long)osvi.dwMajorVersion,
	   (unsigned long)osvi.dwMinorVersion,
	   (unsigned long)osvi.dwBuildNumber));
	if (osvi.dwMajorVersion >= REQUIRED_MAX_V)
	{
		if (osvi.dwMajorVersion == REQUIRED_MAX_V)
		{
			if (((osvi.dwMinorVersion == REQUIRED_MIN_V) &&
			     (osvi.dwBuildNumber >= REQUIRED_BUILD)) ||
			    ((osvi.dwMinorVersion > REQUIRED_MIN_V)))
				res = 1;
		}
		else
			res = 1;
	}
	returnCode(res);
}

/*   Our replacement for the systems _isatty to include also
	 a test for mintty. This is called from the NC_ISATTY macro
	 defined in curses.priv.h

	 Return codes:
	 - 0 : Not a TTY
	 - 1 : A Windows character device detected by _isatty
	 - 2 : A future implementation may return 2 for mintty
 */
NCURSES_EXPORT(int)
_nc_console_isatty(int fd)
{
	int result = 0;

	T((T_CALLED("lib_win32conpty::_nc_console_isatty(%d"), fd));

	if (isatty(fd))
		result = 1;
	returnCode(result);
}
int xxx = 0;

NCURSES_EXPORT(HANDLE)
_nc_console_fd2handle(int fd)
{
	HANDLE hdl = _nc_console_handle(fd);

	if (hdl == WINCONSOLE.inp)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.inp", fd));
	}
	else if (hdl == WINCONSOLE.hdl)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.hdl", fd));
	}
	else if (hdl == WINCONSOLE.out)
	{
		T(("lib_win32con:validateHandle %d -> WINCONSOLE.out", fd));
	}
	else if (hdl == GetStdHandle(STD_INPUT_HANDLE))
	{
		T(("lib_win32con:validateHandle %d -> STD_INPUT_HANDLE", fd));
	}
	else
	{
		T(("lib_win32con:validateHandle %d maps to unknown HANDLE", fd));
		hdl = INVALID_HANDLE_VALUE;
	}
	return hdl;
}

#define OutHandle() ((WINCONSOLE.progMode) ? WINCONSOLE.hdl : WINCONSOLE.out)

NCURSES_EXPORT(int)
_nc_console_setmode(int fd, const TTY *arg)
{
	HANDLE hdl = _nc_console_fd2handle(fd);
	DWORD dwFlag = 0;
	int code = ERR;
	HANDLE alt;

	if (arg)
	{
#ifdef TRACE
		TTY TRCTTY;
#define TRCTTYOUT(flag) TRCTTY.dwFlagOut = flag
#define TRCTTYIN(flag) TRCTTY.dwFlagIn = flag
#else
#define TRCTTYOUT(flag)
#define TRCTTYIN(flag)
#endif
		T(("lib_win32con:_nc_console_setmode %s", _nc_trace_ttymode(arg)));
		if (hdl == WINCONSOLE.inp)
		{
			dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT | VT_FLAG_IN;
			TRCTTYIN(dwFlag);
			SetConsoleMode(hdl, dwFlag);

			alt = OutHandle();
			dwFlag = arg->dwFlagOut;
			TRCTTYOUT(dwFlag);
			SetConsoleMode(alt, dwFlag);
		}
		else
		{
			dwFlag = arg->dwFlagOut;
			TRCTTYOUT(dwFlag);
			SetConsoleMode(hdl, dwFlag);

			alt = WINCONSOLE.inp;
			dwFlag = arg->dwFlagIn | ENABLE_MOUSE_INPUT;
			TRCTTYIN(dwFlag);
			SetConsoleMode(alt, dwFlag);
			T(("effective mode set %s", _nc_trace_ttymode(&TRCTTY)));
		}
		code = OK;
		assert(_nc_stdout_is_conpty());
	}
	return (code);
}

NCURSES_EXPORT(int)
_nc_console_getmode(HANDLE hdl, TTY *arg)
{
	int code = ERR;
	
	if (arg)
	{
		DWORD dwFlag = 0;
		HANDLE alt;

		if (hdl == WINCONSOLE.inp)
		{
			if (GetConsoleMode(hdl, &dwFlag))
			{
				arg->dwFlagIn = dwFlag;
				alt = OutHandle();
				if (GetConsoleMode(alt, &dwFlag))
				{
					arg->dwFlagOut = dwFlag;
					code = OK;
				}
			}
		}
		else
		{
			if (GetConsoleMode(hdl, &dwFlag))
			{
				arg->dwFlagOut = dwFlag;
				alt = WINCONSOLE.inp;
				if (GetConsoleMode(alt, &dwFlag))
				{
					arg->dwFlagIn = dwFlag;
					code = OK;
				}
			}
		}
	}
	T(("lib_win32con:_nc_console_getmode %s", _nc_trace_ttymode(arg)));
	return (code);
}

/* Convert a file descriptor into a HANDLE
   That's not necessarily a console HANDLE
*/
NCURSES_EXPORT(HANDLE)
_nc_console_handle(int fd)
{
	intptr_t value = _get_osfhandle(fd);
	return (HANDLE)value;
}

/* Validate that a HANDLE is actually a
   console HANDLE
*/
static BOOL
IsConsoleHandle(HANDLE hdl)
{
	DWORD dwFlag = 0;
	BOOL result = FALSE;

	T((T_CALLED("lib_win32con::IsConsoleHandle(HANDLE=%p"), hdl));

	EnsureInit();

	if (!GetConsoleMode(hdl, &dwFlag))
	{
		T(("GetConsoleMode failed"));
	}
	else
	{
		result = TRUE;
	}

	returnBool(result);
}

NCURSES_EXPORT(void)
_nc_console_size(int *Lines, int *Cols)
{
	EnsureInit();
	if (Lines != NULL && Cols != NULL)
	{
		*Lines = (int)(WINCONSOLE.SBI.srWindow.Bottom + 1 -
			       WINCONSOLE.SBI.srWindow.Top);
		*Cols = (int)(WINCONSOLE.SBI.srWindow.Right + 1 -
			      WINCONSOLE.SBI.srWindow.Left);
	}
}

NCURSES_EXPORT(WORD)
_nc_console_MapColor(bool fore, int color)
{
	static const int _cmap[] =
	    {0, 4, 2, 6, 1, 5, 3, 7};
	int a;
	if (color < 0 || color > 7)
		a = fore ? 7 : 0;
	else
		a = _cmap[color];
	if (!fore)
		a = a << 4;
	return (WORD)a;
}

/*
 * Reliably validate that stdout is in ConPTY mode and not in console mode.
 *
 * Returns:
 *   true  - stdout is in ConPTY mode (virtual terminal processing enabled)
 *   false - stdout is in legacy console mode or not a console at all
 */
NCURSES_EXPORT(bool)
_nc_stdout_is_conpty(void)
{
	HANDLE stdout_handle;
	DWORD console_mode = 0;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	bool result = false;

	T((T_CALLED("lib_win32conpty::_nc_stdout_is_conpty()")));

	// Get stdout handle
	stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
	if (stdout_handle == INVALID_HANDLE_VALUE)
	{
		T(("GetStdHandle(STD_OUTPUT_HANDLE) failed"));
		returnCode(false);
	}

	// First check if we can get console mode at all
	if (!GetConsoleMode(stdout_handle, &console_mode))
	{
		T(("GetConsoleMode failed - not a console"));
		returnCode(false);
	}

	// Check if virtual terminal processing is enabled (ConPTY mode)
	if (console_mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	{
		// Additional validation: try to get console screen buffer info
		// In ConPTY mode, this should succeed but behavior may differ
		if (GetConsoleScreenBufferInfo(stdout_handle, &csbi))
		{
			// ConPTY typically has different characteristics
			// We can be confident this is ConPTY mode if VT processing is enabled
			result = true;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING - ConPTY detected"));
		}
		else
		{
			// Even if GetConsoleScreenBufferInfo fails, VT processing flag indicates ConPTY
			result = true;
			T(("Console mode has ENABLE_VIRTUAL_TERMINAL_PROCESSING (no CSBI) - ConPTY detected"));
		}
	}
	else
	{
		T(("Console mode lacks ENABLE_VIRTUAL_TERMINAL_PROCESSING - legacy console mode"));
	}

	returnCode(result);
}

NCURSES_EXPORT(int)
_nc_console_flush(void *hdl)
{
	int code = OK;

	T((T_CALLED("lib_win32conpty::_nc_console_flush(hdl=%p"), hdl));

	if (hdl != INVALID_HANDLE_VALUE)
	{
		if (hdl == WINCONSOLE.hdl ||
			hdl == WINCONSOLE.inp ||
			hdl == WINCONSOLE.out)
		{
			if (!FlushConsoleInputBuffer(WINCONSOLE.inp))
				code = ERR;
		}
		else
		{
			code = ERR;
			T(("_nc_console_flush not requesting a handle owned by console."));
		}
	}
	assert(_nc_stdout_is_conpty());
	returnCode(code);
}

#define MIN_WIDE 80
#define MIN_HIGH 24

/*
 * In "normal" mode, reset the buffer- and window-sizes back to their original values.
 */
NCURSES_EXPORT(void)
_nc_console_set_scrollback(bool normal, CONSOLE_SCREEN_BUFFER_INFO *info)
{
	SMALL_RECT rect;
	COORD coord;
	bool changed = FALSE;

	T((T_CALLED("lib_win32con::_nc_console_set_scrollback(%s)"),
	   (normal
			? "normal"
			: "application")));

	T(("... SBI.srWindow %d,%d .. %d,%d",
	   info->srWindow.Top,
	   info->srWindow.Left,
	   info->srWindow.Bottom,
	   info->srWindow.Right));
	T(("... SBI.dwSize %dx%d",
	   info->dwSize.Y,
	   info->dwSize.X));

	if (normal)
	{
		rect = info->srWindow;
		coord = info->dwSize;
		if (memcmp(info, &WINCONSOLE.SBI, sizeof(*info)) != 0)
		{
			changed = TRUE;
			WINCONSOLE.SBI = *info;
		}
	}
	else
	{
		int high = info->srWindow.Bottom - info->srWindow.Top + 1;
		int wide = info->srWindow.Right - info->srWindow.Left + 1;

		if (high < MIN_HIGH)
		{
			T(("... height %d < %d", high, MIN_HIGH));
			high = MIN_HIGH;
			changed = TRUE;
		}
		if (wide < MIN_WIDE)
		{
			T(("... width %d < %d", wide, MIN_WIDE));
			wide = MIN_WIDE;
			changed = TRUE;
		}

		rect.Left =
			rect.Top = 0;
		rect.Right = (SHORT)(wide - 1);
		rect.Bottom = (SHORT)(high - 1);

		coord.X = (SHORT)wide;
		coord.Y = (SHORT)high;

		if (info->dwSize.Y != high ||
			info->dwSize.X != wide ||
			info->srWindow.Top != 0 ||
			info->srWindow.Left != 0)
		{
			changed = TRUE;
		}
	}

	if (changed)
	{
		T(("... coord %d,%d", coord.Y, coord.X));
		T(("... rect %d,%d - %d,%d",
		   rect.Top, rect.Left,
		   rect.Bottom, rect.Right));
		SetConsoleScreenBufferSize(WINCONSOLE.hdl, coord); /* dwSize */
		SetConsoleWindowInfo(WINCONSOLE.hdl, TRUE, &rect); /* srWindow */
		_nc_console_get_SBI();
	}
	returnVoid;
}

NCURSES_EXPORT(bool)
_nc_console_get_SBI(void)
{
	bool rc = FALSE;
	if (GetConsoleScreenBufferInfo(WINCONSOLE.hdl, &(WINCONSOLE.SBI)))
	{
		T(("GetConsoleScreenBufferInfo"));
		T(("... buffer(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwSize.X,
		   WINCONSOLE.SBI.dwSize.Y));
		T(("... window(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwMaximumWindowSize.X,
		   WINCONSOLE.SBI.dwMaximumWindowSize.Y));
		T(("... cursor(X:%d Y:%d)",
		   WINCONSOLE.SBI.dwCursorPosition.X,
		   WINCONSOLE.SBI.dwCursorPosition.Y));
		T(("... display(Top:%d Bottom:%d Left:%d Right:%d)",
		   WINCONSOLE.SBI.srWindow.Top,
		   WINCONSOLE.SBI.srWindow.Bottom,
		   WINCONSOLE.SBI.srWindow.Left,
		   WINCONSOLE.SBI.srWindow.Right));
		WINCONSOLE.origin.X = WINCONSOLE.SBI.srWindow.Left;
		WINCONSOLE.origin.Y = WINCONSOLE.SBI.srWindow.Top;
		rc = TRUE;
	}
	else
	{
		T(("GetConsoleScreenBufferInfo ERR"));
	}
	return rc;
}

NCURSES_EXPORT(int)
_nc_console_read(SCREEN *sp, HANDLE hdl, int *buf)
{
	int rc = -1;
	INPUT_RECORD inp_rec;
	BOOL b;
	DWORD nRead;
	WORD vk;

	assert(sp);
	assert(buf);

	memset(&inp_rec, 0, sizeof(inp_rec));

	T((T_CALLED("lib_win32con::_nc_console_read(%p)"), sp));

	while ((b = read_keycode(hdl, &inp_rec, 1, &nRead)))
	{
		if (b && nRead > 0)
		{
			if (rc < 0)
				rc = 0;
			rc = rc + (int)nRead;
			if (inp_rec.EventType == KEY_EVENT)
			{
				if (!inp_rec.Event.KeyEvent.bKeyDown)
					continue;
#if USE_WIDEC_SUPPORT
				*buf = (int)inp_rec.Event.KeyEventChar;
#else
				*buf = (int)(unsigned char)(inp_rec.Event.KeyEventChar);
#endif
				vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
				/*
				 * There are 24 virtual function-keys, and typically
				 * 12 function-keys on a keyboard.  Use the shift-modifier
				 * to provide the remaining 12 keys.
				 */
				if (vk >= VK_F1 && vk <= VK_F12)
				{
					if (inp_rec.Event.KeyEvent.dwControlKeyState &
					    SHIFT_PRESSED)
					{
						vk = (WORD)(vk + 12);
					}
				}
				if (vk == VK_BACK)
				{
					if (!(inp_rec.Event.KeyEvent.dwControlKeyState & (SHIFT_PRESSED | CONTROL_PRESSED)))
					{
						*buf = KEY_BACKSPACE;
					}
				}
				else if (vk == VK_TAB)
				{
					if ((inp_rec.Event.KeyEvent.dwControlKeyState & (SHIFT_PRESSED | CONTROL_PRESSED)))
					{
						*buf = KEY_BTAB;
					}
				}
				break;
			}
			else if (inp_rec.EventType == MOUSE_EVENT)
			{
				if (handle_mouse(sp,
						 inp_rec.Event.MouseEvent))
				{
					*buf = KEY_MOUSE;
					break;
				}
			}
			else if (inp_rec.EventType == WINDOW_BUFFER_SIZE_EVENT)
			{
				T(("_nc_console_read: WINDOW_BUFFER_SIZE_EVENT"));
				/* Console window was resized - handle like Unix SIGWINCH */
				{
					int old_lines, old_cols;
					int new_lines, new_cols;

					old_lines = sp ? screen_lines(sp) : 0;
					old_cols = sp ? screen_columns(sp) : 0;
					_nc_console_get_SBI();
					_nc_console_size(&new_lines, &new_cols);

					if (sp && ((new_lines != old_lines) || (new_cols != old_cols)))
					{
						T(("Console resized from %dx%d to %dx%d", old_lines, old_cols, new_lines, new_cols));
						NCURSES_SP_NAME(resizeterm)(sp, new_lines, new_cols);
						*buf = KEY_RESIZE;
						break;
					}
				}
			}
			continue;
		}
	}
	returnCode(rc);
}

NCURSES_EXPORT(bool)
_nc_console_restore(void)
{
	bool res = FALSE;

	T((T_CALLED("lib_win32con::_nc_console_restore")));
	if (WINCONSOLE.hdl != INVALID_HANDLE_VALUE)
	{
		res = TRUE;
		_nc_console_set_scrollback(TRUE, &WINCONSOLE.save_SBI);
		if (!restore_original_screen())
		res = FALSE;
		SetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
	}
	returnBool(res);
}

/*NCURSES_EXPORT(void)
_nc_console_selectActiveHandle(void)
{
	if (WINCONSOLE.lastOut != WINCONSOLE.hdl)
	{
		WINCONSOLE.lastOut = WINCONSOLE.hdl;
		// JPF SetConsoleActiveScreenBuffer(WINCONSOLE.lastOut);
	}
}*/

NCURSES_EXPORT(int)
_nc_console_test(int fd)
{
	int code = 0;
	HANDLE hdl = INVALID_HANDLE_VALUE;
	T((T_CALLED("lib_win32con::_nc_console_test(%d)"), fd));
	hdl = _nc_console_handle(fd);
	code = (int)IsConsoleHandle(hdl);
	returnCode(code);
}

NCURSES_EXPORT(int)
_nc_console_testmouse(
	const SCREEN *sp,
	HANDLE hdl,
	int delay
		EVENTLIST_2nd(_nc_eventlist *evl))
{
	int rc = 0;

	assert(sp);

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail)
	{
		rc = TW_MOUSE;
	}
	else
	{
		rc = _nc_console_twait(sp,
							   hdl,
							   TWAIT_MASK,
							   delay,
							   (int *)0 EVENTLIST_2nd(evl));
	}
	return rc;
}

NCURSES_EXPORT(int)
_nc_console_twait(
	const SCREEN *sp,
	HANDLE hdl,
	int mode,
	int milliseconds,
	int *timeleft
		EVENTLIST_2nd(_nc_eventlist *evl))
{
	INPUT_RECORD inp_rec;
	BOOL b;
	DWORD nRead = 0, rc = WAIT_FAILED;
	int code = 0;
	FILETIME fstart;
	FILETIME fend;
	int diff;
	bool isNoDelay = (milliseconds == 0);

#ifdef NCURSES_WGETCH_EVENTS
	(void)evl; /* TODO: implement wgetch-events */
#endif

#define IGNORE_CTRL_KEYS (SHIFT_PRESSED |                        \
						  LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED | \
						  LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)
#define CONSUME() read_keycode(hdl, &inp_rec, 1, &nRead)

	assert(sp);

	TR(TRACE_IEVENT, ("start twait: hdl=%p, %d milliseconds, mode: %d",
					  hdl, milliseconds, mode));

	if (milliseconds < 0)
		milliseconds = NC_INFINITY;

	memset(&inp_rec, 0, sizeof(inp_rec));

	while (true)
	{
		if (!isNoDelay)
		{
			GetSystemTimeAsFileTime(&fstart);
			rc = WaitForSingleObject(hdl, (DWORD)milliseconds);
			GetSystemTimeAsFileTime(&fend);
			diff = (int)tdiff(fstart, fend);
			milliseconds = Adjust(milliseconds, diff);
			if (milliseconds < 0)
				break;
		}

		if (isNoDelay || (rc == WAIT_OBJECT_0))
		{
			if (mode)
			{
				nRead = 0;
				b = GetNumberOfConsoleInputEvents(hdl, &nRead);
				if (!b)
				{
					T(("twait:err GetNumberOfConsoleInputEvents"));
				}
				if (isNoDelay && b)
				{
					T(("twait: Events Available: %lu", (unsigned long)nRead));
					if (nRead == 0)
					{
						code = 0;
						goto end;
					}
					else
					{
						DWORD n = 0;
						MakeArray(pInpRec, INPUT_RECORD, nRead);
						if (pInpRec != NULL)
						{
							DWORD i;
							BOOL f;
							memset(pInpRec, 0, sizeof(INPUT_RECORD) * nRead);
							f = PeekConsoleInput(hdl, pInpRec, nRead, &n);
							if (f)
							{
								for (i = 0; i < n; i++)
								{
									if (pInpRec[i].EventType == KEY_EVENT)
									{
										if (pInpRec[i].Event.KeyEvent.bKeyDown)
										{
											DWORD ctrlMask =
												(pInpRec[i].Event.KeyEvent.dwControlKeyState &
												 IGNORE_CTRL_KEYS);
											if (!ctrlMask)
											{
												code = TW_INPUT;
												goto end;
											}
										}
									}
								}
							}
							else
							{
								T(("twait:err PeekConsoleInput"));
							}
							code = 0;
							goto end;
						}
						else
						{
							T(("twait:err could not alloca input records"));
						}
					}
				}
				if (b && nRead > 0)
				{
					b = PeekConsoleInput(hdl, &inp_rec, 1, &nRead);
					if (!b)
					{
						T(("twait:err PeekConsoleInput"));
					}
					if (b && nRead > 0)
					{
						switch (inp_rec.EventType)
						{
						case KEY_EVENT:
							if (mode & TW_INPUT)
							{
								WORD vk = inp_rec.Event.KeyEvent.wVirtualKeyCode;
								WORD ch = inp_rec.Event.KeyEventChar;

								T(("twait:event KEY_EVENT"));
								T(("twait vk=%d, ch=%d, keydown=%d",
								   vk, ch, inp_rec.Event.KeyEvent.bKeyDown));

								if (inp_rec.Event.KeyEvent.bKeyDown)
								{
									T(("twait:event KeyDown"));
									code = TW_INPUT;
									goto end;
								}
								else
								{
									CONSUME();
								}
							}
							continue;
						case MOUSE_EVENT:
							T(("twait:event MOUSE_EVENT"));
							if (decode_mouse(sp,
											 (inp_rec.Event.MouseEvent.dwButtonState & BUTTON_MASK)) == 0)
							{
								CONSUME();
							}
							else if (mode & TW_MOUSE)
							{
								code = TW_MOUSE;
								goto end;
							}
							continue;
						case WINDOW_BUFFER_SIZE_EVENT:
							T(("twait:event WINDOW_BUFFER_SIZE_EVENT"));
							/* Console window was resized - handle like Unix SIGWINCH */
							{
								int old_lines, old_cols;
								int new_lines, new_cols;
								
								old_lines = sp ? screen_lines(sp) : 0;
								old_cols = sp ? screen_columns(sp) : 0;								
								_nc_console_get_SBI();								
								_nc_console_size(&new_lines, &new_cols);
								
								if (sp && ((new_lines != old_lines) || (new_cols != old_cols))) {
									code = TW_INPUT;
									goto end;	
								}
							}
							CONSUME();
							continue;
						default:
							T(("twait:event Type %d", inp_rec.EventType));
							CONSUME();
							// JPF remove? _nc_console_selectActiveHandle();
							continue;
						}
					}
				}
			}
			continue;
		}
		else
		{
			if (rc != WAIT_TIMEOUT)
			{
				code = -1;
				break;
			}
			else
			{
				code = 0;
				break;
			}
		}
	}
end:

	TR(TRACE_IEVENT, ("end twait: returned %d (%lu), remaining time %d msec",
					  code, (unsigned long)GetLastError(), milliseconds));

	if (timeleft)
		*timeleft = milliseconds;

	return code;
}

static int
locale_is_utf8(const char *loc)
{
	if (!loc)
		return 0;
	return strstr(loc, "UTF-8") ||
	       strstr(loc, "utf8") ||
	       strstr(loc, "utf-8");
}

static int
locale_compatible_with_ncurses(const char *loc)
{
#if USE_WIDEC_SUPPORT
	return locale_is_utf8(loc);
#else
	return !locale_is_utf8(loc);
#endif
}

static int
codepage_compatible_with_ncurses(UINT cp)
{
#if USE_WIDEC_SUPPORT
	return cp == 65001; /* only UTF-8 */
#else
	return cp != 65001; /* all but UTF-8 */
#endif
}

/* Check Codepage exists */
static int valid_codepage(UINT cp)
{
	CPINFOEX info;
	return GetCPInfoEx(cp, 0, &info) != 0;
}

/* Check Windows Locale is valid */
static int valid_locale(const char *loc)
{
	if (!loc || !*loc)
		return 0;

	if (!setlocale(LC_CTYPE, loc))
		return 0;

	/* Reset to previous value */
	setlocale(LC_CTYPE, "");
	return 1;
}

/* Encoding setup for Windows */
NCURSES_EXPORT(void)
_nc_win32_encoding_init(void)
{
#if USE_WIDEC_SUPPORT
	UINT default_cp = CP_UTF8;
	const char *default_ctype = "C.UTF-8";
#else
	UINT default_cp = 1252;
	const char *default_ctype = "English_United States.1252";
#endif

	const char *env_cp = getenv("NC_WINCP");
	const char *env_ctype = getenv("NC_WIN_CTYPE");

	UINT cp = default_cp;
	const char *ctype = default_ctype;
	UINT tmp;
	UINT cur_in;
	UINT cur_out;
	const char *cur_loc;

	T(("lib_win32conpty:_nc_win32_encoding_init()"));

	EnsureInit();

	if (env_cp && *env_cp)
	{
		tmp = (UINT)atoi(env_cp);
		if (valid_codepage(tmp) && codepage_compatible_with_ncurses(tmp))
			cp = tmp;
	}

	if (env_ctype && *env_ctype)
	{
		if (valid_locale(env_ctype) && locale_compatible_with_ncurses(env_ctype))
			ctype = env_ctype;
	}

	cur_in = GetConsoleCP();
	cur_out = GetConsoleOutputCP();

	if (!valid_codepage(cur_in) ||
	    !valid_codepage(cur_out) ||
	    !codepage_compatible_with_ncurses(cur_in) ||
	    !codepage_compatible_with_ncurses(cur_out))
	{
		cur_in = cur_out = default_cp;
	}

	if (!env_cp && valid_codepage(cur_out) && codepage_compatible_with_ncurses(cur_out))
		cp = cur_out;

	cur_loc = setlocale(LC_CTYPE, NULL);
	if (!env_ctype && cur_loc && valid_locale(cur_loc) &&
	    locale_compatible_with_ncurses(cur_loc))
		ctype = cur_loc;

	if (valid_codepage(cp) && codepage_compatible_with_ncurses(cp))
	{
		SetConsoleCP(cp);
		SetConsoleOutputCP(cp);
	}
	else
	{
		SetConsoleCP(default_cp);
		SetConsoleOutputCP(default_cp);
	}

	if (!setlocale(LC_CTYPE, ctype))
	{
		/* Fallback - try alternative UTF-8 locale names for Windows */
#if USE_WIDEC_SUPPORT
		if (!setlocale(LC_CTYPE, ".UTF8") &&
		    !setlocale(LC_CTYPE, ".utf8") &&
		    !setlocale(LC_CTYPE, "en_US.UTF-8") &&
		    !setlocale(LC_CTYPE, "English_United States.65001"))
		{
			/* Final fallback */
			setlocale(LC_CTYPE, default_ctype);
		}
#else
		setlocale(LC_CTYPE, default_ctype);
#endif
	}

	_nc_setmode(_fileno(stdin), true, false);
	_nc_setmode(_fileno(stdout), false, false);
	_nc_setmode(_fileno(stderr), false, false);
}

#endif /* defined(USE_WIN32_CONPTY)) */
