
/***************************************************************************
*                            COPYRIGHT NOTICE                              *
****************************************************************************
*                ncurses is copyright (C) 1992-1995                        *
*                          Zeyd M. Ben-Halim                               *
*                          zmbenhal@netcom.com                             *
*                          Eric S. Raymond                                 *
*                          esr@snark.thyrsus.com                           *
*                                                                          *
*        Permission is hereby granted to reproduce and distribute ncurses  *
*        by any means and for any fee, whether alone or as part of a       *
*        larger distribution, in source or in binary form, PROVIDED        *
*        this notice is included with any such distribution, and is not    *
*        removed from any of its header files. Mention of ncurses in any   *
*        applications linked with it is highly appreciated.                *
*                                                                          *
*        ncurses comes AS IS with no warranty, implied or expressed.       *
*                                                                          *
***************************************************************************/


/*
 *	raw.c
 *
 *	Routines:
 *		raw()
 *		echo()
 *		nl()
 *		cbreak()
 *		noraw()
 *		noecho()
 *		nonl()
 *		nocbreak()
 *		qiflush()
 *		noqiflush()
 *		intrflush()
 *
 */

#include <curses.priv.h>
#include <term.h>	/* cur_term */

MODULE_ID("$Id: lib_raw.c,v 1.20 1997/09/02 22:41:54 tom Exp $")

#ifdef SVR4_TERMIO
#define _POSIX_SOURCE
#endif

#if HAVE_SYS_TERMIO_H
#include <sys/termio.h>	/* needed for ISC */
#endif

/* may be undefined if we're using termio.h */
#ifndef TOSTOP
#define TOSTOP 0
#endif
#ifndef IEXTEN
#define IEXTEN 0
#endif

#define COOKED_INPUT	(IXON|BRKINT|PARMRK)

#ifdef TRACE

typedef struct {unsigned int val; const char *name;} BITNAMES;

static void lookup_bits(char *buf, const BITNAMES *table, const char *label, unsigned int val)
{
	const BITNAMES *sp;

	(void) strcat(buf, label);
	(void) strcat(buf, ": {");
	for (sp = table; sp->name; sp++)
		if (sp->val != 0
		&& (val & sp->val) == sp->val)
		{
			(void) strcat(buf, sp->name);
			(void) strcat(buf, ", ");
		}
	if (buf[strlen(buf) - 2] == ',')
		buf[strlen(buf) - 2] = '\0';
	(void) strcat(buf,"} ");
}

char *_tracebits(void)
/* describe the state of the terminal control bits exactly */
{
char	*buf;
static const	BITNAMES

#ifdef TERMIOS
iflags[] =
    {
	{BRKINT,	"BRKINT"},
	{IGNBRK,	"IGNBRK"},
	{IGNPAR,	"IGNPAR"},
	{PARMRK,	"PARMRK"},
	{INPCK, 	"INPCK"},
	{ISTRIP,	"ISTRIP"},
	{INLCR, 	"INLCR"},
	{IGNCR, 	"IGNC"},
	{ICRNL, 	"ICRNL"},
	{IXON,  	"IXON"},
	{IXOFF, 	"IXOFF"},
	{0,		NULL}
#define ALLIN	(BRKINT|IGNBRK|IGNPAR|PARMRK|INPCK|ISTRIP|INLCR|IGNCR|ICRNL|IXON|IXOFF)
    },
oflags[] =
    {
	{OPOST, 	"OPOST"},
	{0,		NULL}
#define ALLOUT	(OPOST)
    },
cflags[] =
    {
	{CLOCAL,	"CLOCAL"},
	{CREAD, 	"CREAD"},
	{CSTOPB,	"CSTOPB"},
#if !defined(CS5) || !defined(CS8)
	{CSIZE, 	"CSIZE"},
#endif
	{HUPCL, 	"HUPCL"},
	{PARENB,	"PARENB"},
	{PARODD|PARENB,	"PARODD"},	/* concession to readability */
	{0,		NULL}
#define ALLCTRL	(CLOCAL|CREAD|CSIZE|CSTOPB|HUPCL|PARENB|PARODD)
    },
lflags[] =
    {
	{ECHO,  	"ECHO"},
	{ECHOE|ECHO, 	"ECHOE"},	/* concession to readability */
	{ECHOK|ECHO, 	"ECHOK"},	/* concession to readability */
	{ECHONL,	"ECHONL"},
	{ICANON,	"ICANON"},
	{ISIG,  	"ISIG"},
	{NOFLSH,	"NOFLSH"},
	{TOSTOP,	"TOSTOP"},
	{IEXTEN,	"IEXTEN"},
	{0,		NULL}
#define ALLLOCAL	(ECHO|ECHONL|ICANON|ISIG|NOFLSH|TOSTOP|IEXTEN)
    };


    buf = _nc_trace_buf(0,
    	8 + sizeof(iflags) +
    	8 + sizeof(oflags) +
    	8 + sizeof(cflags) +
    	8 + sizeof(lflags) +
	8);

    if (cur_term->Nttyb.c_iflag & ALLIN)
	lookup_bits(buf, iflags, "iflags", cur_term->Nttyb.c_iflag);

    if (cur_term->Nttyb.c_oflag & ALLOUT)
	lookup_bits(buf, oflags, "oflags", cur_term->Nttyb.c_oflag);

    if (cur_term->Nttyb.c_cflag & ALLCTRL)
	lookup_bits(buf, cflags, "cflags", cur_term->Nttyb.c_cflag);

#if defined(CS5) && defined(CS8)
    switch (cur_term->Nttyb.c_cflag & CSIZE) {
    case CS5:	strcat(buf, "CS5 ");	break;
    case CS6:	strcat(buf, "CS6 ");	break;
    case CS7:	strcat(buf, "CS7 ");	break;
    case CS8:	strcat(buf, "CS8 ");	break;
    default:	strcat(buf, "CSIZE? ");	break;
    }
#endif

    if (cur_term->Nttyb.c_lflag & ALLLOCAL)
	lookup_bits(buf, lflags, "lflags", cur_term->Nttyb.c_lflag);

#else
    /* reference: ttcompat(4M) on SunOS 4.1 */
#ifndef EVENP
#define EVENP 0
#endif
#ifndef LCASE
#define LCASE 0
#endif
#ifndef LLITOUT
#define LLITOUT 0
#endif
#ifndef ODDP
#define ODDP 0
#endif
#ifndef TANDEM
#define TANDEM 0
#endif

cflags[] =
    {
	{CBREAK,	"CBREAK"},
	{CRMOD,		"CRMOD"},
	{ECHO,		"ECHO"},
	{EVENP,		"EVENP"},
	{LCASE,		"LCASE"},
	{LLITOUT,	"LLITOUT"},
	{ODDP,		"ODDP"},
	{RAW,		"RAW"},
	{TANDEM,	"TANDEM"},
	{XTABS,		"XTABS"},
	{0,		NULL}
#define ALLCTRL	(CBREAK|CRMOD|ECHO|EVENP|LCASE|LLITOUT|ODDP|RAW|TANDEM|XTABS)
    };

    buf = _nc_trace_buf(0,
    	8 + sizeof(cflags));

    if (cur_term->Nttyb.sg_flags & ALLCTRL)
    {
	lookup_bits(buf, cflags, "cflags", cur_term->Nttyb.sg_flags);
    }

#endif
    return(buf);
}

#define BEFORE(N)	if (_nc_tracing&TRACE_BITS) _tracef("%s before bits: %s", N, _tracebits())
#define AFTER(N)	if (_nc_tracing&TRACE_BITS) _tracef("%s after bits: %s", N, _tracebits())
#else
#define BEFORE(s)
#define AFTER(s)
#endif /* TRACE */

int raw(void)
{
	T((T_CALLED("raw()")));

	SP->_raw = TRUE;
	SP->_cbreak = TRUE;

#ifdef TERMIOS
	BEFORE("raw");
	cur_term->Nttyb.c_lflag &= ~(ICANON|ISIG);
	cur_term->Nttyb.c_iflag &= ~(COOKED_INPUT);
	cur_term->Nttyb.c_cc[VMIN] = 1;
	cur_term->Nttyb.c_cc[VTIME] = 0;
	AFTER("raw");
#else
	cur_term->Nttyb.sg_flags |= RAW;
#endif
	returnCode(_nc_set_curterm(&cur_term->Nttyb));
}

int cbreak(void)
{
	T((T_CALLED("cbreak()")));

	SP->_cbreak = TRUE;

#ifdef TERMIOS
	BEFORE("cbreak");
	cur_term->Nttyb.c_lflag &= ~ICANON;
	cur_term->Nttyb.c_iflag &= ~ICRNL;
	cur_term->Nttyb.c_lflag |= ISIG;
	cur_term->Nttyb.c_cc[VMIN] = 1;
	cur_term->Nttyb.c_cc[VTIME] = 0;
	AFTER("cbreak");
#else
	cur_term->Nttyb.sg_flags |= CBREAK;
#endif
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
}

int echo(void)
{
	T((T_CALLED("echo()")));

	SP->_echo = TRUE;

	returnCode(OK);
}


int nl(void)
{
	T((T_CALLED("nl()")));

	SP->_nl = TRUE;

	returnCode(OK);
}


int qiflush(void)
{
	T((T_CALLED("qiflush()")));

	/*
	 * Note: this implementation may be wrong.  See the comment under
	 * intrflush().
	 */

#ifdef TERMIOS
	BEFORE("qiflush");
	cur_term->Nttyb.c_lflag &= ~(NOFLSH);
	AFTER("qiflush");
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
#else
	returnCode(ERR);
#endif
}


int noraw(void)
{
	T((T_CALLED("noraw()")));

	SP->_raw = FALSE;
	SP->_cbreak = FALSE;

#ifdef TERMIOS
	BEFORE("noraw");
	cur_term->Nttyb.c_lflag |= ISIG|ICANON;
	cur_term->Nttyb.c_iflag |= COOKED_INPUT;
	AFTER("noraw");
#else
	cur_term->Nttyb.sg_flags &= ~(RAW|CBREAK);
#endif
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
}


int nocbreak(void)
{
	T((T_CALLED("nocbreak()")));

	SP->_cbreak = 0;

#ifdef TERMIOS
	BEFORE("nocbreak");
	cur_term->Nttyb.c_lflag |= ICANON;
	cur_term->Nttyb.c_iflag |= ICRNL;
	AFTER("nocbreak");
#else
	cur_term->Nttyb.sg_flags &= ~CBREAK;
#endif
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
}

int noecho(void)
{
	T((T_CALLED("noecho()")));
	SP->_echo = FALSE;
	returnCode(OK);
}


int nonl(void)
{
	T((T_CALLED("nonl()")));

	SP->_nl = FALSE;

	returnCode(OK);
}

int noqiflush(void)
{
	T((T_CALLED("noqiflush()")));

	/*
	 * Note: this implementation may be wrong.  See the comment under
	 * intrflush().
	 */

#ifdef TERMIOS
	BEFORE("noqiflush");
	cur_term->Nttyb.c_lflag |= NOFLSH;
	AFTER("noqiflush");
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
#else
	returnCode(ERR);
#endif
}

int intrflush(WINDOW *win GCC_UNUSED, bool flag)
{
	T((T_CALLED("intrflush(%d)"), flag));

	/*
	 * This call does the same thing as the qiflush()/noqiflush()
	 * pair.  We know for certain that SVr3 intrflush() tweaks the
	 * NOFLSH bit; on the other hand, the match (in the SVr4 man
	 * pages) between the language describing NOFLSH in termio(7)
	 * and the language describing qiflush()/noqiflush() in
	 * curs_inopts(3x) is too exact to be coincidence.
	 */

#ifdef TERMIOS
	BEFORE("intrflush");
	if (flag)
		cur_term->Nttyb.c_lflag &= ~(NOFLSH);
	else
		cur_term->Nttyb.c_lflag |= (NOFLSH);
	AFTER("intrflush");
	returnCode(_nc_set_curterm( &cur_term->Nttyb));
#else
	returnCode(ERR);
#endif
}