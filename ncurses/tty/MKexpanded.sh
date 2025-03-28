#! /bin/sh
##############################################################################
# Copyright 2019-2021,2024 Thomas E. Dickey                                  #
# Copyright 1998-2015,2017 Free Software Foundation, Inc.                    #
#                                                                            #
# Permission is hereby granted, free of charge, to any person obtaining a    #
# copy of this software and associated documentation files (the "Software"), #
# to deal in the Software without restriction, including without limitation  #
# the rights to use, copy, modify, merge, publish, distribute, distribute    #
# with modifications, sublicense, and/or sell copies of the Software, and to #
# permit persons to whom the Software is furnished to do so, subject to the  #
# following conditions:                                                      #
#                                                                            #
# The above copyright notice and this permission notice shall be included in #
# all copies or substantial portions of the Software.                        #
#                                                                            #
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR #
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   #
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    #
# THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER      #
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    #
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        #
# DEALINGS IN THE SOFTWARE.                                                  #
#                                                                            #
# Except as contained in this notice, the name(s) of the above copyright     #
# holders shall not be used in advertising or otherwise to promote the sale, #
# use or other dealings in this Software without prior written               #
# authorization.                                                             #
##############################################################################
#
# Author: Thomas E. Dickey, 1997-on
#
# $Id: MKexpanded.sh,v 1.25 2024/12/07 20:48:46 tom Exp $
#
# Script to generate 'expanded.c', a dummy source that contains functions
# corresponding to complex macros used in this library.  By making functions,
# we simplify analysis and debugging.

if test $# != 0; then
preprocessor="$1"
else
preprocessor="cc -E"
fi
shift
if test $# != 0 ; then
	preprocessor="$preprocessor $*"
else
	preprocessor="$preprocessor -DHAVE_CONFIG_H -I. -I../include"
fi

TMP=gen$$.c
trap "rm -f $TMP; exit 1" 1 2 3 15
trap "rm -f $TMP" 0

cat <<EOF
/* generated by MKexpanded.sh */
#define NEED_NCURSES_CH_T 1
#include <curses.priv.h>

#ifndef CUR
#define CUR SP_TERMTYPE
#endif

#if NCURSES_EXPANDED
EOF

cat >$TMP <<EOF
#include <ncurses_cfg.h>
#undef NCURSES_EXPANDED /* this probably is set in ncurses_cfg.h */
#include <curses.priv.h>
/* these are names we'd like to see */
#undef ALL_BUT_COLOR
#undef PAIR_NUMBER
#undef TRUE
#undef FALSE
/* this is a marker */
IGNORE
NCURSES_EXPORT(void)
_nc_toggle_attr_on (attr_t *S, attr_t at)
{
	toggle_attr_on(*S,at);
}

NCURSES_EXPORT(void)
_nc_toggle_attr_off (attr_t *S, attr_t at)
{
	toggle_attr_off(*S,at);
}

NCURSES_EXPORT(int)
NCURSES_SP_NAME(_nc_DelCharCost) (NCURSES_SP_DCLx int count)
{
	return DelCharCost(SP_PARM, count);
}

NCURSES_EXPORT(int)
NCURSES_SP_NAME(_nc_InsCharCost) (NCURSES_SP_DCLx int count)
{
	return InsCharCost(SP_PARM, count);
}

NCURSES_EXPORT(void)
NCURSES_SP_NAME(_nc_UpdateAttrs) (NCURSES_SP_DCLx CARG_CH_T c)
{
	UpdateAttrs(SP_PARM, CHDEREF(c));
}

@if_NCURSES_SP_FUNCS
NCURSES_EXPORT(int)
_nc_DelCharCost (int count)
{
	return NCURSES_SP_NAME(_nc_DelCharCost) (CURRENT_SCREEN, count);
}

NCURSES_EXPORT(int)
_nc_InsCharCost (int count)
{
	return NCURSES_SP_NAME(_nc_InsCharCost)(CURRENT_SCREEN, count);
}

NCURSES_EXPORT(void)
_nc_UpdateAttrs (CARG_CH_T c)
{
	NCURSES_SP_NAME(_nc_UpdateAttrs)(CURRENT_SCREEN,c);
}
@endif
EOF

$preprocessor $TMP 2>/dev/null | \
	sed -e '1,/^IGNORE$/d' \
		-e 's/^@/#/' \
		-e 's/^#[ 	]*if_/#if /' \
		-e "s,$TMP,expanded.c," \
		-e 's/[ 	][ 	]*$//' \
		-e '/^#[ 	]*[0-9]/d'

cat <<EOF
#else /* ! NCURSES_EXPANDED */
NCURSES_EXPORT(void) _nc_expanded (void) { }
#endif /* NCURSES_EXPANDED */
EOF
