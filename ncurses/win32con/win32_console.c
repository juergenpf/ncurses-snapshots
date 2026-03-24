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

#if USE_LEGACY_CONSOLE || !JPF

#define DispatchMethod(name) legacy_##name
#define Dispatch(name) .name = DispatchMethod(name)
#define NoDispatch(name) .name = NULL
#define METHOD(name, type) static type DispatchMethod(name)

METHOD(init, BOOL)(int fdOut, int fdIn);
METHOD(size, void)(int *Lines, int *Cols);
METHOD(size_changed, BOOL)(void);
METHOD(getmode, int)(int fd GCC_UNUSED, TTY *arg);
METHOD(setmode, int)(int fd GCC_UNUSED, const TTY *arg);
METHOD(defmode, int)(TTY *arg, short kind);
METHOD(AdjustSize,BOOL)(void);
METHOD(napms, int)(int ms);
METHOD(termattrs, chtype)(void);
METHOD(keypad, int)(BOOL flag);
METHOD(beeporflash, int)(BOOL beep);
METHOD(keyok, int)(int keycode,int flag);
METHOD(has_key, int)(int keycode);
METHOD(init_acs, void)(chtype *acs);
METHOD(reset_color_pair, BOOL)(void);
METHOD(init_pair, int)(int pair, int fg, int bg);
METHOD(setcolor, void)(BOOL fg, int color);

static LegacyConsoleInterface legacyCONSOLE =
	{
		.core =
			{
				Dispatch(init),
				Dispatch(size),
				Dispatch(size_changed),
				Dispatch(setmode),
				Dispatch(getmode),
				Dispatch(defmode)
			},
		.hShellMode = INVALID_HANDLE_VALUE,
		.hProgMode = INVALID_HANDLE_VALUE,
		.numButtons = 0,
		.ansi_map = NULL,
		.map = NULL,
		.rmap = NULL,
		.pairs = {0},
		.SBI = {},
		.save_CI = {0},

		Dispatch(AdjustSize),
		Dispatch(napms),
		Dispatch(termattrs),
		Dispatch(keypad),
		Dispatch(beeporflash),
		Dispatch(keyok),
		Dispatch(has_key),
		Dispatch(init_acs),
		Dispatch(reset_color_pair),
		Dispatch(init_pair),
		Dispatch(setcolor)
	};
NCURSES_EXPORT_VAR(LegacyConsoleInterface *)
_nc_LEGACYCONSOLE = &legacyCONSOLE;

#define RevAttr(attr) (WORD)(((attr) & 0xff00) |       \
							 ((((attr) & 0x07) << 4) | \
							  (((attr) & 0x70) >> 4)))

#define GenMap(vKey, key) MAKELONG(key, vKey)

static const LONG keylist[] =
	{
		GenMap(VK_PRIOR, KEY_PPAGE),
		GenMap(VK_NEXT, KEY_NPAGE),
		GenMap(VK_END, KEY_END),
		GenMap(VK_HOME, KEY_HOME),
		GenMap(VK_LEFT, KEY_LEFT),
		GenMap(VK_UP, KEY_UP),
		GenMap(VK_RIGHT, KEY_RIGHT),
		GenMap(VK_DOWN, KEY_DOWN),
		GenMap(VK_DELETE, KEY_DC),
		GenMap(VK_INSERT, KEY_IC)};

static const LONG ansi_keys[] =
	{
		GenMap(VK_PRIOR, 'I'),
		GenMap(VK_NEXT, 'Q'),
		GenMap(VK_END, 'O'),
		GenMap(VK_HOME, 'H'),
		GenMap(VK_LEFT, 'K'),
		GenMap(VK_UP, 'H'),
		GenMap(VK_RIGHT, 'M'),
		GenMap(VK_DOWN, 'P'),
		GenMap(VK_DELETE, 'S'),
		GenMap(VK_INSERT, 'R')};

/* *INDENT-ON* */
#define array_length(a) (sizeof(a) / sizeof(a[0]))
#define N_INI ((int)array_length(keylist))
#define FKEYS 24
#define MAPSIZE (FKEYS + N_INI)

static WORD
console_MapColor(BOOL fore, int color)
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

static int
rkeycompare(const void *el1, const void *el2)
{
	WORD key1 = (LOWORD((*((const LONG *)el1)))) & 0x7fff;
	WORD key2 = (LOWORD((*((const LONG *)el2)))) & 0x7fff;

	return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static int
keycompare(const void *el1, const void *el2)
{
	WORD key1 = HIWORD((*((const LONG *)el1)));
	WORD key2 = HIWORD((*((const LONG *)el2)));

	return ((key1 < key2) ? -1 : ((key1 == key2) ? 0 : 1));
}

static BOOL
get_SBI(void)
{
	bool rc = FALSE;
	if (CORECONSOLE.getSBI(&(LEGACYCONSOLE.SBI)))
	{
		T(("GetConsoleScreenBufferInfo"));
		T(("... buffer(X:%d Y:%d)",
		   LEGACYCONSOLE.SBI.dwSize.X,
		   LEGACYCONSOLE.SBI.dwSize.Y));
		T(("... window(X:%d Y:%d)",
		   LEGACYCONSOLE.SBI.dwMaximumWindowSize.X,
		   LEGACYCONSOLE.SBI.dwMaximumWindowSize.Y));
		T(("... cursor(X:%d Y:%d)",
		   LEGACYCONSOLE.SBI.dwCursorPosition.X,
		   LEGACYCONSOLE.SBI.dwCursorPosition.Y));
		T(("... display(Top:%d Bottom:%d Left:%d Right:%d)",
		   LEGACYCONSOLE.SBI.srWindow.Top,
		   LEGACYCONSOLE.SBI.srWindow.Bottom,
		   LEGACYCONSOLE.SBI.srWindow.Left,
		   LEGACYCONSOLE.SBI.srWindow.Right));
		rc = TRUE;
	}
	else
	{
		T(("GetConsoleScreenBufferInfo ERR"));
	}
	return rc;
}

NCURSES_EXPORT(void)
_nc_legacy_console_init(void)
{
	WORD a;
	int i;
	DWORD num_buttons;

	LEGACYCONSOLE.map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.rmap = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);
	LEGACYCONSOLE.ansi_map = (LPDWORD)malloc(sizeof(DWORD) * MAPSIZE);

	for (i = 0; i < (N_INI + FKEYS); i++)
	{
		if (i < N_INI)
		{
			LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
			    (DWORD)keylist[i];
			LEGACYCONSOLE.ansi_map[i] = (DWORD)ansi_keys[i];
		}
		else
		{
			LEGACYCONSOLE.rmap[i] = LEGACYCONSOLE.map[i] =
			    (DWORD)GenMap((VK_F1 + (i - N_INI)),
					  (KEY_F(1) + (i - N_INI)));
			LEGACYCONSOLE.ansi_map[i] =
			    (DWORD)GenMap((VK_F1 + (i - N_INI)),
					  (';' + (i - N_INI)));
		}
	}
	qsort(LEGACYCONSOLE.ansi_map,
	      (size_t)(MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(LEGACYCONSOLE.map,
	      (size_t)(MAPSIZE),
	      sizeof(keylist[0]),
	      keycompare);
	qsort(LEGACYCONSOLE.rmap,
	      (size_t)(MAPSIZE),
	      sizeof(keylist[0]),
	      rkeycompare);

	if (GetNumberOfConsoleMouseButtons(&num_buttons))
		LEGACYCONSOLE.numButtons = (int)num_buttons;
	else
		LEGACYCONSOLE.numButtons = 1;

	a = console_MapColor(TRUE, COLOR_WHITE) |
	    console_MapColor(FALSE, COLOR_BLACK);
	for (i = 0; i < CON_NUMPAIRS; i++)
		LEGACYCONSOLE.pairs[i] = a;

	get_SBI();
	GetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
	T(("... initial cursor is %svisible, %d%%",
	   (LEGACYCONSOLE.save_CI.bVisible ? "" : "not-"),
	   (int)LEGACYCONSOLE.save_CI.dwSize));
       
	LEGACYCONSOLE.info.initcolor = TRUE;
	LEGACYCONSOLE.info.canchange = FALSE;
	LEGACYCONSOLE.info.hascolor = TRUE;
	LEGACYCONSOLE.info.caninit = TRUE;
	LEGACYCONSOLE.info.maxpairs = CON_NUMPAIRS;
	LEGACYCONSOLE.info.maxcolors = 8;
	LEGACYCONSOLE.info.numlabels = 0;
	LEGACYCONSOLE.info.labelwidth = 0;
	LEGACYCONSOLE.info.labelheight = 0;
	LEGACYCONSOLE.info.nocolorvideo = 1;
	LEGACYCONSOLE.info.tabsize = 8;
	LEGACYCONSOLE.info.numbuttons = LEGACYCONSOLE.numButtons;
	LEGACYCONSOLE.info.defaultPalette = _nc_cga_palette;
}

METHOD(AdjustSize, BOOL)(void)
{
    BOOL res = FALSE;
    COORD newSize;
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    
    T((T_CALLED("win32_console::legacy_AdjustSize()")));

    if (HasConsoleResizeLimitations()) {
	T(("Console has resize limitations, skipping AdjustSize"));
	returnBool(res);
    }

    /*
     * This piece is just for Windows 10 before the introduction of the new console:.
     * In older Versions of Windows (before Windows 10), the conhost behaves differently 
     * when resizing the console window. 
     */
    if (!GetConsoleScreenBufferInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &csbi)) {
        T(("GetConsoleScreenBufferInfo failed"));
        returnBool(res);
    }

    newSize.X = (short)(csbi.srWindow.Right - csbi.srWindow.Left + 1);
    newSize.Y = (short)(csbi.srWindow.Bottom - csbi.srWindow.Top + 1);

    T(("New console size: %d x %d", newSize.X, newSize.Y));
    SetConsoleScreenBufferSize(LEGACYCONSOLE.core.ConsoleHandleOut, newSize);

    LEGACYCONSOLE.core.getSBI(&(LEGACYCONSOLE.SBI));
    LEGACYCONSOLE.core.sbi_lines = newSize.Y;
    LEGACYCONSOLE.core.sbi_cols = newSize.X;
    res = TRUE;

    returnBool(res);
}

METHOD(napms,int)(int ms)
{
	T((T_CALLED("win32_console::legacy_napms(%d)"), ms));
	Sleep((DWORD)ms);
	returnCode(OK);
}

METHOD(termattrs, chtype)(void)
{
	chtype res = A_NORMAL;
	res |= (A_BOLD | A_DIM | A_REVERSE | A_STANDOUT | A_COLOR);
	return res;
}


METHOD(keypad, int)(BOOL flag)
{
	int code = ERR;
	SCREEN *sp = ConsoleScreen();

	T((T_CALLED("win32_console::legacy_keypad(%d)"), flag));

	if (sp)
	{
		sp->_keypad_on = flag;
		code = OK;
	}
	returnCode(code);
}

METHOD(beeporflash, int)(BOOL beepFlag)
{
	int res = ERR;

	int high = (LEGACYCONSOLE.SBI.srWindow.Bottom -
				LEGACYCONSOLE.SBI.srWindow.Top + 1);
	int wide = (LEGACYCONSOLE.SBI.srWindow.Right -
				LEGACYCONSOLE.SBI.srWindow.Left + 1);
	int max_cells = (high * wide);
	int i;

	MakeArray(this_screen, CHAR_INFO, max_cells);
	MakeArray(that_screen, CHAR_INFO, max_cells);
	COORD this_size;
	SMALL_RECT this_region;
	COORD bufferCoord;

	this_region.Top = LEGACYCONSOLE.SBI.srWindow.Top;
	this_region.Left = LEGACYCONSOLE.SBI.srWindow.Left;
	this_region.Bottom = LEGACYCONSOLE.SBI.srWindow.Bottom;
	this_region.Right = LEGACYCONSOLE.SBI.srWindow.Right;

	this_size.X = (SHORT)wide;
	this_size.Y = (SHORT)high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
		read_screen(LEGACYCONSOLE.core.ConsoleHandleOut,
					this_screen,
					this_size,
					bufferCoord,
					&this_region))
	{

		memcpy(that_screen,
			   this_screen,
			   sizeof(CHAR_INFO) * (size_t)max_cells);

		for (i = 0; i < max_cells; i++)
		{
			that_screen[i].Attributes =
				RevAttr(that_screen[i].Attributes);
		}

		write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, that_screen, this_size,
					 bufferCoord, &this_region);
		Sleep(200);
		write_screen(LEGACYCONSOLE.core.ConsoleHandleOut, this_screen, this_size,
					 bufferCoord, &this_region);
	}
	else
	{
		MessageBeep(MB_ICONWARNING); /* MB_OK might be better */
	}
	res = OK;
	return res;
}

METHOD(keyok, int)(int keycode, int flag)
{
	int code = ERR;
	WORD nKey;
	WORD vKey;
	void *res;
	LONG key = GenMap(0, (WORD)keycode);

	T((T_CALLED("win32_console::legacy_keyok(%d, %d)"), keycode, flag));

	res = bsearch(&key,
				  LEGACYCONSOLE.rmap,
				  (size_t)(N_INI + FKEYS),
				  sizeof(keylist[0]),
				  rkeycompare);
	if (res)
	{
		key = *((LONG *)res);
		vKey = HIWORD(key);
		nKey = (LOWORD(key)) & 0x7fff;
		if (!flag)
			nKey |= 0x8000;
		*(LONG *)res = GenMap(vKey, nKey);
	}
	returnCode(code);
}

METHOD(has_key, int)(int keycode)
{
	WORD nKey;
	void *res;
	bool found = FALSE;
	LONG key = GenMap(0, (WORD)keycode);

	T((T_CALLED("win32_console::legacy_has_key(%d)"), keycode));
	res = bsearch(&key,
				  LEGACYCONSOLE.rmap,
				  (size_t)(N_INI + FKEYS),
				  sizeof(keylist[0]),
				  rkeycompare);
	if (res)
	{
		key = *((LONG *)res);
		nKey = LOWORD(key);
		if (!(nKey & 0x8000))
			found = TRUE;
	}
	returnCode(found ? OK : ERR);
}

METHOD(init_acs, void)(chtype *real_map)
{
#define DATA(a, b) \
	{              \
		a, b       \
	}
	static struct
	{
		int acs_code;
		int use_code;
	} table[] = {
		DATA('a', 0xb1), /* ACS_CKBOARD  */
		DATA('f', 0xf8), /* ACS_DEGREE   */
		DATA('g', 0xf1), /* ACS_PLMINUS  */
		DATA('j', 0xd9), /* ACS_LRCORNER */
		DATA('l', 0xda), /* ACS_ULCORNER */
		DATA('k', 0xbf), /* ACS_URCORNER */
		DATA('m', 0xc0), /* ACS_LLCORNER */
		DATA('n', 0xc5), /* ACS_PLUS     */
		DATA('q', 0xc4), /* ACS_HLINE    */
		DATA('t', 0xc3), /* ACS_LTEE     */
		DATA('u', 0xb4), /* ACS_RTEE     */
		DATA('v', 0xc1), /* ACS_BTEE     */
		DATA('w', 0xc2), /* ACS_TTEE     */
		DATA('x', 0xb3), /* ACS_VLINE    */
		DATA('y', 0xf3), /* ACS_LEQUAL   */
		DATA('z', 0xf2), /* ACS_GEQUAL   */
		DATA('0', 0xdb), /* ACS_BLOCK    */
		DATA('{', 0xe3), /* ACS_PI       */
		DATA('}', 0x9c), /* ACS_STERLING */
		DATA(',', 0xae), /* ACS_LARROW   */
		DATA('+', 0xaf), /* ACS_RARROW   */
		DATA('~', 0xf9), /* ACS_BULLET   */
	};
#undef DATA
	unsigned n;
	SCREEN *sp = ConsoleScreen();

	for (n = 0; n < SIZEOF(table); ++n)
	{
		real_map[table[n].acs_code] =
			(chtype)table[n].use_code | A_ALTCHARSET;
		if (sp != NULL)
			sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
}

METHOD(reset_color_pair, BOOL)(void)
{
	BOOL res = FALSE;
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
	res = TRUE;
	return res;
}

METHOD(init_pair, int)(int pair, int f, int b)
{
	int code = ERR;
	int num_colors = LEGACYCONSOLE.info.maxcolors;
	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < num_colors) && (b >= 0) && (b < num_colors))
	{
		LEGACYCONSOLE.pairs[pair] =
			console_MapColor(TRUE, f) |
			console_MapColor(FALSE, b);
		T(("... legacy_init_pair: pair %d: fg=%d, bg=%d", pair, f, b));
		code = OK;
	}
	return code;
}

METHOD(setcolor, void)(BOOL fore, int color)
{
	WORD a = console_MapColor(fore, color);
	a |= (WORD)((LEGACYCONSOLE.SBI.wAttributes) & ((BOOL)fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(LEGACYCONSOLE.core.ConsoleHandleOut, a);
	get_SBI();
}

/* This function sets the console mode for the input and output handles. It is called by the main thread
 * when it wants to change the console mode. The function takes a TTY structure that contains the desired
 * mode flags, and it returns OK on success or ERR on failure.
 * It is also responsible for detecting switches between shell mode and program mode, and starting or
 * stopping the input subsystem accordingly. */
METHOD(setmode, int)(int fd GCC_UNUSED, const TTY *arg)
{
	HANDLE input_target = LEGACYCONSOLE.core.ConsoleHandleIn;
	HANDLE output_target = LEGACYCONSOLE.core.ConsoleHandleOut;
	BOOL input_ok = FALSE;
	BOOL output_ok = FALSE;
	SCREEN *sp = ConsoleScreen();

	T((T_CALLED("win32_driver::legacy_setmode(fd=%d, TTY*=%p)"), fd, arg));

	if (!arg)
		returnCode(ERR);

	if (input_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = arg->dwFlagIn;
		if (arg->kind == TTY_MODE_SHELL)
		{
			/* In shell mode, we want to disable VT input and enable the basic line input, processed
			 * input and echo input modes, to provide a more traditional console input experience.
			 * This allows the user to interact with the console in a way that is consistent with
			 * what they would expect from a typical command prompt or terminal window, with
			 * features like line editing and input processing enabled. */
			mode |= (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT);
			//mode &= ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT);
		}
		else if (arg->kind == TTY_MODE_PROGRAM)
		{
			/* In program mode, we want to enable VT input. */
			mode |= ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT;
		}

		/* Sanitize: ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
		if ((mode & ENABLE_ECHO_INPUT) && !(mode & ENABLE_LINE_INPUT))
		{
			mode &= ~ENABLE_ECHO_INPUT;
		}

		input_ok = SetConsoleMode(input_target, mode);
		if (input_ok)
		{
			/* Make sure the cached value reflects the real value we set, as the
			 * caller may not have provided all necessary flags (e.g.
			 * ENABLE_PROCESSED_INPUT when VT is requested) */
			DWORD realMode;
			if (GetConsoleMode(input_target, &realMode))
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagIn = realMode;
			}
			else
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagIn = mode;
			}
		}
		else
		{
			T(("Invalid input file descriptor"));
		}
	}

	if (output_target != INVALID_HANDLE_VALUE)
	{
		DWORD mode = arg->dwFlagOut;
		output_ok = SetConsoleMode(output_target, mode);
		if (output_ok)
		{
			/* Make sure the cached value reflects the real value we set,
			 * as the caller may not have provided all necessary flags
			 * (e.g. VT output is required for the Windows Console backend) */
			DWORD realMode;
			if (GetConsoleMode(output_target, &realMode))
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagOut = realMode;
			}
			else
			{
				LEGACYCONSOLE.core.ttyflags.dwFlagOut = mode;
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
		if (IsConsoleProgMode()) {
			ClearConsoleProgMode();
			if (sp)
			{
				_nc_keypad(sp, FALSE);
				NCURSES_SP_NAME(_nc_flush)(sp);
			}
		
			if (LEGACYCONSOLE.hShellMode != INVALID_HANDLE_VALUE)
			{
				T(("... LEGACYCONSOLE: switching to shell mode buffer"));
				LEGACYCONSOLE.core.ConsoleHandleOut = LEGACYCONSOLE.hShellMode;
				SetConsoleActiveScreenBuffer(LEGACYCONSOLE.core.ConsoleHandleOut);
				SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
			} else {
				T(("... LEGACYCONSOLE: no valid shell mode buffer"));
			}
		} else {
			T(("... LEGACYCONSOLE: Already in shell mode"));
		}
	}
	else if (arg->kind == TTY_MODE_PROGRAM)
	{
		T(("Program mode set"));
		if (!IsConsoleProgMode())
		{
			SetConsoleProgMode();
			if (sp)
			{
				if (sp->_keypad_on)
					_nc_keypad(sp, TRUE);
			}
				
			if (LEGACYCONSOLE.hProgMode != INVALID_HANDLE_VALUE)
			{
				T(("... LEGACYCONSOLE: switching to program mode buffer"));
				LEGACYCONSOLE.core.ConsoleHandleOut = LEGACYCONSOLE.hProgMode;
				SetConsoleActiveScreenBuffer(LEGACYCONSOLE.core.ConsoleHandleOut);
				SetConsoleCursorInfo(LEGACYCONSOLE.core.ConsoleHandleOut, &LEGACYCONSOLE.save_CI);
			} else {
				T(("... LEGACYCONSOLE: no valid program mode buffer"));
			}
		} else {
			T(("... LEGACYCONSOLE: Already in program mode"));
		}
	}

	// Handle errors
	if (!input_ok || !output_ok)
	{
		returnCode(ERR);
	}

	returnCode(OK);
}

/* getmode always sets the kind field to TTY_MODE_UNSPECIFIED. The trick is, that
 * def_shell_mode, def_prog_mode and savetty will call above method defmode to
 * set the field right after getting it.
 * So only calls to reset_shell_mode, reset_prog_mode and resetty will have the kind
 * field in the TTY structure set to a specific mode, which means that the setmode
 * function will know that it should apply the necessary changes to the input subsystem
 * when restoring that TTY. All other calls to setmode will have the kind field in the
 * TTY structure set to TTY_MODE_UNSPECIFIED, which means that the setmode function
 * will know that it should not change the status of the input subsystem when restoring
 * that TTY. */
METHOD(getmode, int)(int fd GCC_UNUSED, TTY *arg)
{
	T((T_CALLED("win32_driver::legacy_getmode(fd=%d, TTY*=%p)"), fd, arg));

	if (NULL == arg)
		returnCode(ERR);

	*arg = LEGACYCONSOLE.core.ttyflags;
	arg->kind = TTY_MODE_UNSPECIFIED;
	returnCode(OK);
}

/* The defmode function is only called from def_shell_mode, def_prog_mode, and savetty.
 * It's only purpose is to set the kind field in the TTY structure and to set the
 * REQUIRED console mode flags for shell mode and program mode.
 * The design idea is this: the three mentioned calls are the only ones used to get the
 * TTY structure in order to store it and later on use it to restore the console to the
 * desired state. TTY changing calls like raw() or cbreak() don't do that. The implementation
 * of the getmode function will always set the kind field to TTY_MODE_UNSPECIFIED, which means
 * that if that TTY is later used in a setmode call, the setmode function will know that it
 * should not change the status of the input subsystem. Only the def_shell_mode, def_prog_mode,
 * and savetty functions will set the kind field to a specific mode, which means that the setmode
 * function will know that it should apply the necessary changes to the input subsystem when
 * restoring that TTY. */
METHOD(defmode, int)(TTY *arg, short kind)
{
	short realMode = kind;

	T((T_CALLED("win32_driver::legacy_defmode(TTY*=%p, kind=%d)"), arg, kind));

	if (NULL == arg)
		returnCode(ERR);

	if (realMode == TTY_MODE_AUTO)
	{
		realMode = IsConsoleProgMode() ? TTY_MODE_PROGRAM : TTY_MODE_SHELL;
	}

	arg->kind = realMode;
	returnCode(OK);
}

METHOD(size, void)(int *Lines, int *Cols)
{
	if (Lines != NULL && Cols != NULL)
	{
		*Lines = (int)(LEGACYCONSOLE.SBI.srWindow.Bottom + 1 -
					   LEGACYCONSOLE.SBI.srWindow.Top);
		*Cols = (int)(LEGACYCONSOLE.SBI.srWindow.Right + 1 -
					  LEGACYCONSOLE.SBI.srWindow.Left);
		T(("win32_driver::legacy_size() returns %d lines, %d cols", *Lines, *Cols));
	}
}

METHOD(size_changed, BOOL)(void)
{
	BOOL resized = FALSE;
	T((T_CALLED("win32_console::legacy_size_changed()")));

	if (HasConsolePendingResize())
	{
		T(("Resize event pending, returning TRUE"));
		resized = TRUE;
		ClearConsoleResizeLimitations();
		_nc_globals.have_sigwinch = 1;
	}
	returnBool(resized);
}

/* This initializaton function can be called multiple time, and actually it is called from within
 * setupterm() the first time and potentially if we enter ncurses from newterm() the next time.
 * The main purpose is to initialize the defaultCONPTY structure when called the first time. The
 * first call will alway have fdIn set to -1, as setupterm() only cares about output. Please note
 * that setupterm() already handles redirection of stdout and assigns stderr for output if stdout
 * is not a tty.
 *
 * The other purpose of this routine is to manage the assignment of pseudo-console handles. If the
 * assigned filedescriptors are NOT valid pseudo-console handles, the call will return FALSE.
 *
 * The function will also return FALSE, if the Windows version we run on does not support ConPTY,
 * which is a requirement for the Windows Console backend of ncurses. This is because without
 * ConPTY, the Windows Console does not provide the necessary capabilities for ncurses and
 * especially the terminfo layer to function properly. */
METHOD(init, BOOL)(int fdOut, int fdIn)
{
	BOOL result = FALSE;

	T((T_CALLED("win32_driver::legacy_init(fdOut=%d, fdIn=%d)"), fdOut, fdIn));

	/* initialize once, or not at all */
	if (!IsConsoleInitialized())
	{
		/*
		 * We set the console mode flags to the most basic ones that are required for ConPTY
		 * to function properly. */
		DWORD dwFlagIn = (ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT | ENABLE_EXTENDED_FLAGS);
		DWORD dwFlagOut = (ENABLE_PROCESSED_OUTPUT | DISABLE_NEWLINE_AUTO_RETURN | ENABLE_WRAP_AT_EOL_OUTPUT);
		DWORD dwFlag;

		HANDLE stdin_handle  = INVALID_HANDLE_VALUE;
		HANDLE stdout_handle = INVALID_HANDLE_VALUE;

		if (fdIn != -1)
		{
			T(("In the first call fdIn is expected to be -1."));
			returnBool(FALSE);
		}

		stdin_handle = CreateFileA(
			"CONIN$", 
			GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_READ, 
			NULL, OPEN_EXISTING, 0, NULL);

		stdout_handle = CreateFileA(
			"CONOUT$", 
			GENERIC_READ | GENERIC_WRITE, 
			FILE_SHARE_WRITE, 
			NULL, OPEN_EXISTING, 0, NULL);

		LEGACYCONSOLE.hShellMode = stdout_handle;

		/* Especially with UCRT and wide mode, make sure we use an UTF-8 capable locale.
		 * At least we set the codepage to a proper value that's either compatible with
		 * ASCII or UTF-8, to ensure that the console can display characters properly.
		 * The actual locale setting is not that important, as long as the code page is set
		 * correctly, because we handle UTF-8 encoding and decoding ourselves and we don't
		 * rely on the C runtime for that. */
		// encoding_init();

		if (stdout_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdout_handle,
																	&dwFlag) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ConsoleHandleOut = stdout_handle;

		if (stdin_handle == INVALID_HANDLE_VALUE || GetConsoleMode(stdin_handle,
																   &dwFlag) == 0)
		{
			T(("StdIn handle is not a console"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ConsoleHandleIn = stdin_handle;

		SetConsoleMode(stdout_handle, dwFlagOut);
		/* We immediately read the console mode back to reflect any changes the
		 * runtime may have added, so the saved value reflects the actual mode
		 * of the console. */
		if (GetConsoleMode(stdout_handle, &dwFlagOut) == 0)
		{
			T(("GetConsoleMode() failed for stdout"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ttyflags.dwFlagOut = dwFlagOut;

		dwFlagIn &= ~(ENABLE_QUICK_EDIT_MODE);
		SetConsoleMode(stdin_handle, dwFlagIn);
		/* We immediately read the console mode back to reflect any changes the
		 * runtime may have added, so the saved value reflects the actual mode
		 * of the console. */
		if (GetConsoleMode(stdin_handle, &dwFlagIn) == 0)
		{
			T(("GetConsoleMode() failed for stdin"));
			returnBool(FALSE);
		}
		LEGACYCONSOLE.core.ttyflags.dwFlagIn = dwFlagIn;

		_nc_legacy_console_init();

		LEGACYCONSOLE.core.ConsoleHandleIn = stdin_handle;
		LEGACYCONSOLE.core.ConsoleHandleOut = stdout_handle;
		MarkConsoleInitialized();
		result = TRUE;
	}
	else
	{
		/* This branch is called from newterm() when fdIn is provided, so we need to validate
		 * that the provided fdIn and fdOut are valid pseudo-console handles, and if so we
		 * update the defaultCONPTY structure to use the new handles. */
		DWORD dwFlagOut;

		if (LEGACYCONSOLE.hProgMode == INVALID_HANDLE_VALUE) {
			T(("... creating console buffer"));
			/* Save the original active screen buffer handle so we can switch
			 * back to it when endwin() / reset_shell_mode is called. */
			LEGACYCONSOLE.hProgMode =
				CreateConsoleScreenBuffer(GENERIC_READ | GENERIC_WRITE,
									  FILE_SHARE_READ | FILE_SHARE_WRITE,
									  NULL,
									  CONSOLE_TEXTMODE_BUFFER,
									  NULL);
		}		
		if (LEGACYCONSOLE.hProgMode==INVALID_HANDLE_VALUE || GetConsoleMode(LEGACYCONSOLE.hProgMode, &dwFlagOut) == 0)
		{
			T(("Output handle is not a console"));
			returnBool(FALSE);
		}
		result = TRUE;
	}
	returnBool(result);
}
#endif /* USE_LEGACY_CONSOLE */
