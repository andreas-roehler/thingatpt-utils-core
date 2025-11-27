#!/bin/sh

# Author: Andreas Roehler <andreas.roehler@online.de>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# Commentary:

# This script tests functions from ar-mode.el.

# Code:

if [ $1 == en ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif [ $1 == e25 ]; then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e26 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e27 ];then
    #  export EMACS="$HOME/emacs-20220306/src/emacs -Q"
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e28 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e29 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
elif
    [ $1 == e30 ];then
    export EMACS=$(echo $(alias $1) | sed "s,alias [^~]*.\([^ ]*\).*,$HOME\1,g")
else
    export EMACS=emacs
fi

echo "before shift \$EMACS: $EMACS"
shift

WERKSTATT=${WERKSTATT:=1}
echo "\$WERKSTATT: $WERKSTATT"

IFLOCAL=${IFLOCAL:=1}
echo "\$IFLOCAL: $IFLOCAL"

echo "\$*: $*"
PDIR=$PWD
echo "\$PWD: $PWD"

TESTDIR=test/
export TESTDIR
echo "\$TESTDIR: $TESTDIR"

UTILSDIR=$HOME/werkstatt/thing-at-point-utils/

FILE1=ar-subr.el
FILE2=ar-beg-end.el
FILE3=ar-emacs-generics-start-Zf98zM.el
FILE4=ar-thingatpt-basic-definitions.el
FILE5=ar-thingatpt-utils-core.el
FILE6=ar-navigate.el
FILE7=${UTILSDIR}ar-comment-lor.el

SETUP=${TESTDIR}ar-thingatpt-setup-tests.el
TEST1=${TESTDIR}ar-thingatpt-utils-core-elisp-tests.el
TEST2=${TESTDIR}ar-thingatpt-utils-python-mode-tests.el
TEST3=${TESTDIR}ar-thingatpt-core-tests.el
TEST4=${TESTDIR}ar-thingatpt-core-haskell-tests.el
TEST5=${TESTDIR}ar-thingatpt-utils-interactive-tests.el

# if [ -s emacs24 ]; then
#     EMACS=emacs24
# else
#     EMACS=emacs
# fi

echo "\$EMACS: $EMACS"

h1() { 
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq py-install-dir \"$PDIR\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
-l $TEST1 \
-f ert-run-tests-batch-and-exit
}

h2() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
-l $TEST2 \
-f ert-run-tests-batch-and-exit
}

h3() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
-l $TEST3 \
-f ert-run-tests-batch-and-exit
}

h4() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
-l $TEST4 \
-f ert-run-tests-batch-and-exit
}

h4() {
    date; time -p $EMACS -Q -L . --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
-l $TEST5 \
-f ert-run-tests-batch-and-exit
}

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(setq ar-debug-p nil)" \
--eval "(require 'ert)" \
--eval "(setq py-install-dir \"$PDIR\")" \
--eval "(add-to-list 'load-path \"$PDIR/\")" \
--eval "(add-to-list 'load-path \"$TESTDIR/\")" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
-load $SETUP \
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
-load $FILE6 \
-load $FILE7 \
\
-load $TEST1 \
-f ert-run-tests-batch-and-exit
}

WGE=$HOME/werkstatt/emacs-generics

cp -p ${WGE}/ar-beg-end.el .
cp -p ${WGE}/ar-subr.el .
cp -p ${WGE}/ar-navigate.el .
cp -p ${WGE}/ar-subr.el .
cp -p ${WGE}/ar-emacs-generics-start-Zf98zM.el .


if [ $IFLOCAL -eq 0 ]; then

    while getopts 123456789abcdefghijklmnpqrstuvx option
    do
        case $option in
	    1) echo "h1: Lade \$TEST1: \"$TEST1\"";h1;;
	    2) echo "h2: Lade \$TEST2: \"$TEST2\"";h2;;
	    3) echo "h3: Lade \$TEST3: \"$TEST3\"";h3;;
	    4) echo "h4: Lade \$TEST4: \"$TEST4\"";h4;;
	    # 5) echo "h5: Lade \$TEST5: \"$TEST5\"";h5;;
	    # 6) echo "h6: Lade \$TEST6: \"$TEST6\"";h6;;
	    # 7) echo "h7: Lade \$TEST7: \"$TEST7\"";h7;;
	    # 8) echo "h8: Lade \$TEST8: \"$TEST8\"";h8;;
	    # 9) echo "h9: Lade \$TEST9: \"$TEST9\"";h9;;
	    # a) echo "h10: Lade \$TEST10: \"$TEST10\"";h10;;
	    # b) echo "h11: Lade \$TEST11: \"$TEST11\"";h11;;
	    # c) echo "h12: Lade \$TEST12: \"$TEST12\"";h12;;
	    # d) echo "h13: Lade \$TEST13: \"$TEST13\"";h13;;
	    # e) echo "h14: Lade \$TEST14: \"$TEST14\"";h14;;
	    # f) echo "h15: Lade \$TEST15: \"$TEST15\"";h15;;
	    # g) echo "h16: Lade \$TEST16: \"$TEST16\"";h16;;
            # h) echo "h17: Running python-tests.el";h17;;
	    # i) ;;
	    # j) echo "h19: Lade \$TEST19: \"$TEST19\"";h19;;
	    # k) echo "h20: Lade \$TEST20: \"$TEST20\"";h20;;
	    # l) echo "hier: Lade Testumgebung ‘hier’";hier;;
	    # m) echo "h20: Lade \$TEST20: \"$TEST20\"";h20;;
            n) echo "hier: Lade Testumgebung ‘hier’";hier;;

	esac
	shift
	echo "\$*: $*"
	EMACS=$1
	
    done

    # hier1
    # echo "Lade testumgebung \"HIER1\""
    # hier2
    # echo "Lade testumgebung \"HIER1\""

else
    echo "entfernt"
    echo "\$WERKSTATT: $WERKSTATT"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi

# if [ $ORT -eq 0 ]; then
#     hier
#     echo "Lade testumgebung \"$HOSTNAME\""
# else
#     echo "entfernt"
#     echo "Lade testumgebung \"ENTFERNT\""
#     entfernt
# fi

# -load $FILE3 \
