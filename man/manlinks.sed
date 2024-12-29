# $Id: manlinks.sed,v 1.23 2024/12/28 21:36:50 Branden.Robinson Exp $
##############################################################################
# Copyright 2020-2023,2024 Thomas E. Dickey                                  #
# Copyright 2000-2003,2008 Free Software Foundation, Inc.                    #
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
# Given a manpage (nroff) as input, writes a list of the names that are
# listed in the "NAME" section, i.e., the names that we would like to use
# as aliases for the manpage -T.Dickey
#
# eliminate formatting controls that get in the way
/^'\\"/d
/\.\\"/d
/^\.br/d
/^\.sp/d
/typedef/d
s/^\.IX//
s/\\f.//g
s/\\%//g
s/[:,]/ /g
#
# ignore C-style comments
s%/\*.*\*/%%
#
# Eliminate unnecessary whitespace, convert multiple blanks to single space.
s/^[ 	][ 	]*//
s/[ 	][ 	]*$//
s/[ 	][ 	]*/ /g
/^$/d
/^[<>]/d
#
# convert ".SH" into a more manageable form
s/\.SH[ 	][ 	]*/.SH_(/
#
# in ".SH NAME"
# Convert a list of names separated from their description by " \-" to a list
# of names on separate lines.  Normally the list is also comma-separated, but
# we ignore that detail here.  The description is on a separate line to make
# the nroff source more pleasing to some eyes.
/^\.SH_(NAME/,/ \\-$/{
s/\\-/-/g
s/  / /g
/ -$/{
s/ -$//
n
d
}
s/ /\
/g
}
#
# delete remainder of document
/^\.SH_([^N]/,${
d
}
#
# delete any remaining directives
/^\./d
