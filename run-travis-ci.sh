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

WERKSTATT=$HOME/werkstatt

DIR1=thingatpt-utils-core

TESTDIR1=$WERKSTATT/$DIR1/test

FILE1=$WERKSTATT/$DIR1/beg-end.el
FILE2=$WERKSTATT/$DIR1/ar-subr.el
FILE3=$WERKSTATT/$DIR1/thingatpt-utils-base.el

TEST1=$TESTDIR1/ar-setup-ert-tests.el
TEST2=$TESTDIR1/ar-subr-ert-tests.el
# TEST3=$TESTDIR1/ar-ert-tests-1.el
TEST3=$TESTDIR1/ar-core-tests-1.el

if [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"DIR1\"))" \
--eval "(add-to-list 'load-path (getenv \"TESTDIR1\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
--eval "(add-to-list 'load-path (getenv \"WERKSTATT/\"))" \
--eval "(add-to-list 'load-path (getenv \"DIR1\"))" \
--eval "(add-to-list 'load-path (getenv \"TESTDIR1\"))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
\
-load $TEST1 \
-load $TEST2 \
-load $TEST3 \
-f ert-run-tests-batch-and-exit
}


if [ $ORT -eq 0 ]; then
    hier
    echo "Lade \$DIR6 und \$DIR7"
else
    echo "entfernt"
    echo "Lade testumgebung \"ENTFERNT\""
fi

# -load $FILE3 \
