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

# This var is set in my bashrc to 0
ORT=${ORT:-1}

echo "\$ORT: $ORT"

TESTDIR=test/
export TESTDIR

FILE1=beg-end.el
FILE2=ar-subr.el
FILE3=thingatpt-utils-map.el
FILE4=thingatpt-utils-core.el

TEST1=${TESTDIR}ar-thing-at-point-utils-setup-tests.el
TEST2=${TESTDIR}thingatpt-utils-core-tests.el


if [ -s emacs24 ]; then
    EMACS=emacs24
else
    EMACS=emacs
fi

echo "\$EMACS: $EMACS"

hier () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
\
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

entfernt () {
    $EMACS -Q --batch \
--eval "(message (emacs-version))" \
-load $FILE1 \
-load $FILE2 \
-load $FILE3 \
-load $FILE4 \
\
-load $TEST1 \
-load $TEST2 \
-f ert-run-tests-batch-and-exit
}

if [ $ORT -eq 0 ]; then
    hier
    echo "Lade testumgebung \"$HOSTNAME\""
else
    echo "entfernt"
    echo "Lade testumgebung \"ENTFERNT\""
    entfernt
fi

# -load $FILE3 \
