/****************************************************************************
 * Copyright 2018-2024,2025 Thomas E. Dickey                                *
 * Copyright 2008-2016,2017 Free Software Foundation, Inc.                  *
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
#define CUR TerminalType((TERMINAL*)TCB).
#include <tic.h>
#include <termcap.h>		/* ospeed */

#if HAVE_NANOSLEEP
#include <time.h>
#if HAVE_SYS_TIME_H
#include <sys/time.h>		/* needed for MacOS X DP3 */
#endif
#endif

#if HAVE_SIZECHANGE
# if !defined(sun) || !TERMIOS
#  if HAVE_SYS_IOCTL_H
#   include <sys/ioctl.h>
#  endif
# endif
#endif

MODULE_ID("$Id: tinfo_driver.c,v 1.88 2025/12/27 12:33:34 tom Exp $")

/*
 * SCO defines TIOCGSIZE and the corresponding struct.  Other systems (SunOS,
 * Solaris, IRIX) define TIOCGWINSZ and struct winsize.
 */
#ifdef TIOCGSIZE
# define IOCTL_WINSIZE TIOCGSIZE
# define STRUCT_WINSIZE struct ttysize
# define WINSIZE_ROWS(n) (int)n.ts_lines
# define WINSIZE_COLS(n) (int)n.ts_cols
#else
# ifdef TIOCGWINSZ
#  define IOCTL_WINSIZE TIOCGWINSZ
#  define STRUCT_WINSIZE struct winsize
#  define WINSIZE_ROWS(n) (int)n.ws_row
#  define WINSIZE_COLS(n) (int)n.ws_col
# endif
#endif

/*
 * These should be screen structure members.  They need to be globals for
 * historical reasons.  So we assign them in start_color() and also in
 * set_term()'s screen-switching logic.
 */
#if USE_REENTRANT
NCURSES_EXPORT(int)
NCURSES_PUBLIC_VAR(COLOR_PAIRS) (void)
{
    return CURRENT_SCREEN ? CURRENT_SCREEN->_pair_count : -1;
}
NCURSES_EXPORT(int)
NCURSES_PUBLIC_VAR(COLORS) (void)
{
    return CURRENT_SCREEN ? CURRENT_SCREEN->_color_count : -1;
}
#else
NCURSES_EXPORT_VAR(int) COLOR_PAIRS = 0;
NCURSES_EXPORT_VAR(int) COLORS = 0;
#endif

#define TCBMAGIC NCDRV_MAGIC(NCDRV_TINFO)
#define AssertTCB() assert(TCB != NULL && TCB->magic == TCBMAGIC)
#define SetSP() assert(TCB->csp != NULL); sp = TCB->csp; (void) sp

/*
 * This routine needs to do all the work to make curscr look
 * like newscr.
 */
static int
drv_doupdate(TERMINAL_CONTROL_BLOCK * TCB)
{
    AssertTCB();
    return TINFO_DOUPDATE(TCB->csp);
}

static const char *
drv_Name(TERMINAL_CONTROL_BLOCK * TCB)
{
    (void) TCB;
    return "tinfo";
}

static void
get_baudrate(TERMINAL *termp)
{
    int my_ospeed;
    int result;
    if (GET_TTY(termp->Filedes, &termp->Nttyb) == OK) {
#ifdef TERMIOS
	termp->Nttyb.c_oflag &= (unsigned) (~OFLAGS_TABS);
#elif defined(USE_CONSOLE_API)
	/* noop */
#else
	termp->Nttyb.sg_flags &= (unsigned) (~XTABS);
#endif
    }
#ifdef USE_OLD_TTY
    result = (int) cfgetospeed(&(termp->Nttyb));
    my_ospeed = (NCURSES_OSPEED) _nc_ospeed(result);
#else /* !USE_OLD_TTY */
#ifdef TERMIOS
    my_ospeed = (NCURSES_OSPEED) cfgetospeed(&(termp->Nttyb));
#elif defined(USE_CONSOLE_API)
    /* noop */
    my_ospeed = 0;
#else
    my_ospeed = (NCURSES_OSPEED) termp->Nttyb.sg_ospeed;
#endif
    result = _nc_baudrate(my_ospeed);
#endif
    termp->_baudrate = result;
    ospeed = (NCURSES_OSPEED) my_ospeed;
}

#undef SETUP_FAIL
#define SETUP_FAIL FALSE

#define NO_COPY {}

static bool
drv_CanHandle(TERMINAL_CONTROL_BLOCK * TCB, const char *tname, int *errret)
{
    bool result = FALSE;
    int status;
    TERMINAL *termp;
    SCREEN *sp;

    START_TRACE();
    T((T_CALLED("tinfo::drv_CanHandle(%p,%s,%p)"),
       (void *) TCB, NonNull(tname), (void *) errret));

    assert(TCB != NULL && tname != NULL);
    termp = (TERMINAL *) TCB;
    sp = TCB->csp;
    TCB->magic = TCBMAGIC;

#if (NCURSES_USE_DATABASE || NCURSES_USE_TERMCAP)
    status = _nc_setup_tinfo(tname, &TerminalType(termp));
    T(("_nc_setup_tinfo returns %d", status));
#else
    T(("no database available"));
    status = TGETENT_NO;
#endif

    /* try fallback list if entry on disk */
    if (status != TGETENT_YES) {
	const TERMTYPE2 *fallback = _nc_fallback2(tname);

	if (fallback) {
	    T(("found fallback entry"));
	    TerminalType(termp) = *fallback;
	    status = TGETENT_YES;
	}
    }

    if (status != TGETENT_YES) {
	NCURSES_SP_NAME(del_curterm) (NCURSES_SP_ARGx termp);
	if (status == TGETENT_ERR) {
	    ret_error0(status, "terminals database is inaccessible\n");
	} else if (status == TGETENT_NO) {
	    ret_error1(status, "unknown terminal type.\n",
		       tname, NO_COPY);
	} else {
	    ret_error0(status, "unexpected return-code\n");
	}
    }
    result = TRUE;
#if NCURSES_EXT_NUMBERS
    _nc_export_termtype2(&termp->type, &TerminalType(termp));
#endif
#if !USE_REENTRANT
    save_ttytype(termp);
#endif

    if (VALID_STRING(command_character))
	_nc_tinfo_cmdch(termp, UChar(*command_character));

    /*
     * If an application calls setupterm() rather than initscr() or
     * newterm(), we will not have the def_prog_mode() call in
     * _nc_setupscreen().  Do it now anyway, so we can initialize the
     * baudrate.
     */
    if (sp == NULL && NC_ISATTY(termp->Filedes)) {
	get_baudrate(termp);
    }
#if NCURSES_EXT_NUMBERS
#define cleanup_termtype() \
    _nc_free_termtype2(&TerminalType(termp)); \
    _nc_free_termtype(&termp->type)
#else
#define cleanup_termtype() \
    _nc_free_termtype2(&TerminalType(termp))
#endif

    if (generic_type) {
	/*
	 * BSD 4.3's termcap contains mis-typed "gn" for wy99.  Do a sanity
	 * check before giving up.
	 */
	if ((VALID_STRING(cursor_address)
	     || (VALID_STRING(cursor_down) && VALID_STRING(cursor_home)))
	    && VALID_STRING(clear_screen)) {
	    cleanup_termtype();
	    ret_error1(TGETENT_YES, "terminal is not really generic.\n",
		       tname, NO_COPY);
	} else {
	    cleanup_termtype();
	    ret_error1(TGETENT_NO, "I need something more specific.\n",
		       tname, NO_COPY);
	}
    }
    if (hard_copy) {
	cleanup_termtype();
	ret_error1(TGETENT_YES, "I can't handle hardcopy terminals.\n",
		   tname, NO_COPY);
    }

    returnBool(result);
}


static void
drv_init(TERMINAL_CONTROL_BLOCK * TCB)
{
    TERMINAL *trm;

    AssertTCB();

    trm = (TERMINAL *) TCB;

    TCB->info.initcolor = VALID_STRING(initialize_color);
    TCB->info.canchange = can_change;
    TCB->info.hascolor = ((VALID_NUMERIC(max_colors) && VALID_NUMERIC(max_pairs)
			   && (((set_foreground != NULL)
				&& (set_background != NULL))
			       || ((set_a_foreground != NULL)
				   && (set_a_background != NULL))
			       || set_color_pair)) ? TRUE : FALSE);

    TCB->info.caninit = !(exit_ca_mode && non_rev_rmcup);

    TCB->info.maxpairs = VALID_NUMERIC(max_pairs) ? max_pairs : 0;
    TCB->info.maxcolors = VALID_NUMERIC(max_colors) ? max_colors : 0;
    TCB->info.numlabels = VALID_NUMERIC(num_labels) ? num_labels : 0;
    TCB->info.labelwidth = VALID_NUMERIC(label_width) ? label_width : 0;
    TCB->info.labelheight = VALID_NUMERIC(label_height) ? label_height : 0;
    TCB->info.nocolorvideo = VALID_NUMERIC(no_color_video) ? no_color_video
	: 0;
    TCB->info.tabsize = VALID_NUMERIC(init_tabs) ? (int) init_tabs : 8;

    TCB->info.defaultPalette = hue_lightness_saturation ? _nc_hls_palette : _nc_cga_palette;

    /*
     * If an application calls setupterm() rather than initscr() or
     * newterm(), we will not have the def_prog_mode() call in
     * _nc_setupscreen().  Do it now anyway, so we can initialize the
     * baudrate.
     */
    if (NC_ISATTY(trm->Filedes)) {
	NCURSES_SP_NAME(def_prog_mode) (TCB->csp);	
    }
}

#define MAX_PALETTE	8
#define InPalette(n)	((n) >= 0 && (n) < MAX_PALETTE)


static int
drv_mvcur(TERMINAL_CONTROL_BLOCK * TCB, int yold, int xold, int ynew, int xnew)
{
    SCREEN *sp = TCB->csp;
    AssertTCB();
    return NCURSES_SP_NAME(_nc_mvcur) (sp, yold, xold, ynew, xnew);
}


static void
drv_setfilter(TERMINAL_CONTROL_BLOCK * TCB)
{
    AssertTCB();

    /* *INDENT-EQLS* */
    clear_screen     = ABSENT_STRING;
    cursor_address   = ABSENT_STRING;
    cursor_down      = ABSENT_STRING;
    cursor_up        = ABSENT_STRING;
    parm_down_cursor = ABSENT_STRING;
    parm_up_cursor   = ABSENT_STRING;
    row_address      = ABSENT_STRING;
    cursor_home      = carriage_return;

    if (back_color_erase)
	clr_eos = ABSENT_STRING;
}


#define ENSURE_TINFO(sp) (TCBOf(sp)->drv->isTerminfo)

NCURSES_EXPORT(void)
_nc_cookie_init(SCREEN *sp)
{
    bool support_cookies = USE_XMC_SUPPORT;
    TERMINAL_CONTROL_BLOCK *TCB = (TERMINAL_CONTROL_BLOCK *) (sp->_term);

    if (sp == NULL || !ENSURE_TINFO(sp))
	return;

#if USE_XMC_SUPPORT
    /*
     * If we have no magic-cookie support compiled-in, or if it is suppressed
     * in the environment, reset the support-flag.
     */
    if (magic_cookie_glitch >= 0) {
	if (getenv("NCURSES_NO_MAGIC_COOKIE") != NULL) {
	    support_cookies = FALSE;
	}
    }
#endif

    if (!support_cookies && magic_cookie_glitch >= 0) {
	T(("will disable attributes to work w/o magic cookies"));
    }

    if (magic_cookie_glitch > 0) {	/* tvi, wyse */

	sp->_xmc_triggers = sp->_ok_attributes & XMC_CONFLICT;
#if 0
	/*
	 * We "should" treat colors as an attribute.  The wyse350 (and its
	 * clones) appear to be the only ones that have both colors and magic
	 * cookies.
	 */
	if (has_colors()) {
	    sp->_xmc_triggers |= A_COLOR;
	}
#endif
	sp->_xmc_suppress = sp->_xmc_triggers & (chtype) ~(A_BOLD);

	T(("magic cookie attributes %s", _traceattr(sp->_xmc_suppress)));
	/*
	 * Supporting line-drawing may be possible.  But make the regular
	 * video attributes work first.
	 */
	acs_chars = ABSENT_STRING;
	ena_acs = ABSENT_STRING;
	enter_alt_charset_mode = ABSENT_STRING;
	exit_alt_charset_mode = ABSENT_STRING;
#if USE_XMC_SUPPORT
	/*
	 * To keep the cookie support simple, suppress all of the optimization
	 * hooks except for clear_screen and the cursor addressing.
	 */
	if (support_cookies) {
	    clr_eol = ABSENT_STRING;
	    clr_eos = ABSENT_STRING;
	    set_attributes = ABSENT_STRING;
	}
#endif
    } else if (magic_cookie_glitch == 0) {	/* hpterm */
    }

    /*
     * If magic cookies are not supported, cancel the strings that set
     * video attributes.
     */
    if (!support_cookies && magic_cookie_glitch >= 0) {
	magic_cookie_glitch = ABSENT_NUMERIC;
	set_attributes = ABSENT_STRING;
	enter_blink_mode = ABSENT_STRING;
	enter_bold_mode = ABSENT_STRING;
	enter_dim_mode = ABSENT_STRING;
	enter_reverse_mode = ABSENT_STRING;
	enter_standout_mode = ABSENT_STRING;
	enter_underline_mode = ABSENT_STRING;
    }

    /* initialize normal acs before wide, since we use mapping in the latter */
#if !USE_WIDEC_SUPPORT
    if (_nc_unicode_locale() && _nc_locale_breaks_acs(sp->_term)) {
	acs_chars = NULL;
	ena_acs = NULL;
	enter_alt_charset_mode = NULL;
	exit_alt_charset_mode = NULL;
	set_attributes = NULL;
    }
#endif
}


NCURSES_EXPORT_VAR (TERM_DRIVER) _nc_TINFO_DRIVER = {
    TRUE,
	drv_Name,		/* Name */
	drv_CanHandle,		/* CanHandle */
	drv_init,		/* init */
	drv_mvcur,		/* hwcur */
	drv_setfilter,		/* setfilter */
	drv_doupdate		/* update */
};

#if USE_TERM_DRIVER
/*
 * The terminfo driver is mandatory and must always be present.
 * So this is the natural place for the driver initialisation
 * logic.
 */

typedef struct DriverEntry {
    const char *name;
    TERM_DRIVER *driver;
} DRIVER_ENTRY;

static DRIVER_ENTRY DriverTable[] =
{
#if USE_LEGACY_CONSOLE
    {"win32console", &_nc_WIN_DRIVER},
#endif
    {"tinfo", &_nc_TINFO_DRIVER}	/* must be last */
};

NCURSES_EXPORT(int)
_nc_get_driver(TERMINAL_CONTROL_BLOCK * TCB, const char *name, int *errret)
{
    int code = ERR;
    size_t i;
    TERM_DRIVER *res = (TERM_DRIVER *) 0;
    TERM_DRIVER *use = NULL;

    T((T_CALLED("_nc_get_driver(%p, %s, %p)"),
       (void *) TCB, NonNull(name), (void *) errret));

    assert(TCB != NULL);

    for (i = 0; i < SIZEOF(DriverTable); i++) {
	res = DriverTable[i].driver;
#if USE_LEGACY_CONSOLE
	if ((i + 1) == SIZEOF(DriverTable)) {
	    /* For Windows >= 10.0.17763 Windows Console interface implements
	       virtual Terminal functionality.
	       If on Windows td_CanHandle returned FALSE although the terminal
	       name is empty, we default to ms-terminal as tinfo TERM type.
	     */
	    if (name == NULL || *name == 0 || (strcmp(name, "unknown") == 0)) {
		name = DEFAULT_TERM_ENV;
		T(("Set TERM=%s", name));
	    }
	}
#endif
	if (strcmp(DriverTable[i].name, res->td_name(TCB)) == 0) {
	    if (res->td_CanHandle(TCB, name, errret)) {
		T(("matched driver %s with TERM=%s", DriverTable[i].name, name));
		use = res;
		break;
	    }
	}
    }
    if (use != 0) {
	TCB->drv = use;
	code = OK;
    }
    returnCode(code);
}
#endif /* USE_TERM_DRIVER */
