/****************************************************************************
 * Copyright 2018-2024,2025 Thomas E. Dickey                                *
 * Copyright 2008-2010,2017 Free Software Foundation, Inc.                  *
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
 * Author: Thomas Dickey, 2008-on                                           *
 ****************************************************************************/

/* $Id: nc_win32.h.in,v 1.19 2025/12/26 23:32:43 tom Exp $ */

#ifndef NC_WIN32_H
#define NC_WIN32_H 1

#include <ncurses_cfg.h>

#if defined(_WIN32) || defined(_WIN64) || defined(USE_WIN32_CONPTY)

#ifndef _NC_WINDOWS_NATIVE
#define _NC_WINDOWS_NATIVE
#endif

#ifdef TERMIOS
#error TERMIOS must not be defined on Windows
#endif

/*
   Minimum requirement for named pipes is Windows Vista or Server2008,
   aka Windows NT 6.0
*/
#ifdef WINVER
#  if WINVER < 0x0600
#    error WINVER must at least be 0x0600
#  endif
#else
#  define WINVER 0x0600
#endif

#undef _NC_CHECK_MINTTY
#if WINVER >= 0x0600
#define _NC_CHECK_MINTTY
#endif

#include <windows.h>

#if HAVE_SYS_TIME_H
#include <sys/time.h>		/* for struct timeval */
#endif

#ifdef _MSC_VER
#include <winsock2.h>		/* for struct timeval */
#endif

#include <stdint.h>		/* for uint32_t */

/*
 * Allow for build-override, e.g., MinGW used "cygwin".
 */
#ifndef DEFAULT_TERM_ENV
#define DEFAULT_TERM_ENV "ms-terminal"
#endif

#undef VALID_TERM_ENV
#define VALID_TERM_ENV(term_env, no_terminal) \
	(term_env = (NonEmpty(term_env) \
		      ? term_env \
		      : (_nc_console_vt_supported() \
		         ? DEFAULT_TERM_ENV \
		         : no_terminal)), \
	 NonEmpty(term_env))

  /*
   * Various Console mode definitions
   */

  /* Default flags for input/output modes */
#define CONMODE_IN_DEFAULT (ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_LINE_INPUT | ENABLE_PROCESSED_INPUT | ENABLE_ECHO_INPUT | ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT)
#define CONMODE_OUT_DEFAULT (ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_PROCESSED_OUTPUT)

#include <ncurses_dll.h>

#ifdef __cplusplus
extern "C" {
#endif

#if defined(CURSES_PRIV_H) || defined(TEST_PRIV_H)

#if !HAVE_CLOCK_GETTIME && !HAVE_GETTIMEOFDAY
extern NCURSES_EXPORT(int) _nc_gettimeofday(struct timeval *, void *);
#undef HAVE_GETTIMEOFDAY
#define HAVE_GETTIMEOFDAY 2
#define gettimeofday(tv,tz) _nc_gettimeofday(tv,tz)
#endif

#endif /* defined(CURSES_PRIV_H) || defined(TEST_PRIV_H) */

#if !HAVE_WCWIDTH
#undef wcwidth
#define wcwidth(ucs) _nc_wcwidth((wchar_t)(ucs))
extern NCURSES_EXPORT(int) _nc_wcwidth(uint32_t);
#endif

/* Terminal input mode flags (c_lflag equivalents) */
#define ISIG    0x01    /* Enable signal character processing */
#define ICANON  0x02    /* Canonical input processing */  
#define ECHO    0x04    /* Echo input characters */
#define ONLCR   0x08    /* Map NL to CR-NL on output (newline processing) */

/* Terminal control mode flags (custom for Windows) */
#define CBREAK  0x10    /* Single character vs line mode */
#define RAW     0x20    /* Raw vs cooked mode */

struct win32_termio {
    unsigned int c_lflag;     /* Local mode flags (like termios) */
    // Special characters equivalent to termios c_cc[]
    unsigned char intr_char;  // interrupt char (Ctrl+C)
    unsigned char quit_char;  // quit char (Ctrl+\)
    unsigned char erase_char; // backspace char
    unsigned char kill_char;  // kill line char
    unsigned char eof_char;   // EOF char (Ctrl+D)
};

#define CON_NUMPAIRS 64
typedef struct {
    BOOL initialized;
    BOOL window_only;
    BOOL progMode;
    BOOL isMinTTY;
    HANDLE out;
    HANDLE inp;
    HANDLE hdl;
    // JPF remove HANDLE lastOut;
    int numButtons;
    WORD pairs[CON_NUMPAIRS];
    COORD origin;
    CHAR_INFO *save_screen;
    COORD save_size;
    SMALL_RECT save_region;
    CONSOLE_SCREEN_BUFFER_INFO SBI;
    CONSOLE_SCREEN_BUFFER_INFO save_SBI;
    CONSOLE_CURSOR_INFO save_CI;
    struct win32_termio ttyflags;
} ConsoleInfo;

extern NCURSES_EXPORT_VAR(ConsoleInfo) _nc_CONSOLE;
#define WINCONSOLE _nc_CONSOLE

#ifdef __cplusplus
}
#endif

#ifdef CURSES_PRIV_H	/* test.priv.h just needs the preceding */
#include <term.h>
#endif

#if USE_DOS_PATHS
NCURSES_EXPORT(const char *) _nc_to_dospath(const char *, char *);
#define FixupPathname(path) \
	char fixed_pathname[PATH_MAX]; \
	path = _nc_to_dospath(path, fixed_pathname)
#define FixupPathname2(path,buffer) \
	path = _nc_to_dospath(path, buffer)
#endif

#undef  ttyname
#define ttyname(fd) NULL

#endif /* _WIN32 || _WIN64 */

#endif /* NC_WIN32_H */
