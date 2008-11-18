#!/bin/sh
# $Id: mk-dlls.sh,v 0.1 2008/11/16 00:19:59 juergen Exp $
##############################################################################
# Copyright (c) 2008 Free Software Foundation, Inc.                          #
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
# Author: Juergen Pfeifer
#
# Build DLLs on MinGW
#
gcc -v 2>&1 | grep specs | grep mingw
if [ $? -eq 1 ]; then
  echo "$0 requires a mingw environment" >&2
else
  if [ -d lib ]; then
    cf="-shared"
    lf="--enable-auto-import"
    pushd lib 2>&1 >/dev/null
      for t in "" "t"
      do
        for m in "" "_g"
        do  
	  echo check libncurses${t}${m}.a
          if [ -f libncurses${t}${m}.a ]; then
            f=libncurses${t}${m}.a
            g=`basename $f .a | cut -c 4-`
	    gi=libw${g}.a
	    ar x $f `ar t $f`
            gcc $cf -o w${g}.dll -Wl,--out-implib,${gi} -Wl,--output-def,w${g}.def -Wl,$lf `ar t $f`
            lib /machine:i386 /def:w${g}.def
            rm -f `ar t $f`
	    echo Built w${g}.dll, w${g}.def, w${g}.lib and libw${g}.a

            for l in panel menu form
            do
              for f in lib${l}${t}${m}.a
              do
                g=`basename $f .a | cut -c 4-`
                echo $g
	        ar x $f `ar t $f`
                gcc $cf -o w${g}.dll -Wl,--out-implib,libw${g}.a -Wl,--output-def,w${g}.def -Wl,$lf `ar t $f` ${gi}
                lib /machine:i386 /def:w${g}.def
                rm -f `ar t $f`
	        echo Built w${g}.dll, w${g}.def, w${g}.lib and libw${g}.a
              done
            done
          fi
        done
      done
    popd
  else
    echo lib has not been build >&2
  fi
fi