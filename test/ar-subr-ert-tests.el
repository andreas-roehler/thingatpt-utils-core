;;; ar-subr-ert-tests.el --- -*- lexical-binding: t; -*- 


;; Copyright (C) 2013-2016 Free Software Foundation, Inc.
;; Copyright (C) 2014-2016 Andreas Röhler, <andreas.roehler@online.de>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



(defvar ar-subr-test-string1 "
#!/bin/bash
 # --

function virtualenvwrapper_derive_workon_home {
    typeset workon_home_dir=\"\$WORKON_HOME\"

    # Make sure there is a default value for WORKON_HOME.
    # You can override this setting in your .bashrc.
    if [ \"\$workon_home_dir\" = \"\" ]
    then
        workon_home_dir=\"\$HOME/.virtualenvs\"
    fi

    # If the path is relative, prefix it with \$HOME
    # (note: for compatibility)
    if echo \"\$workon_home_dir\" | (unset GREP_OPTIONS; command \\grep '^[^/~]' > /dev/null)
    then
        workon_home_dir=\"\$HOME/\$WORKON_HOME\"
    fi

    # Only call on Python to fix the path if it looks like the
    # path might contain stuff to expand.
    # (it might be possible to do this in shell, but I don't know a
    # cross-shell-safe way of doing it -wolever)
    if echo \"\$workon_home_dir\" | (unset GREP_OPTIONS; command \\egrep '([\\\$~]|//)' >/dev/null)
    then
        # This will normalize the path by:
        # - Removing extra slashes (e.g., when TMPDIR ends in a slash)
        # - Expanding variables (e.g., \$foo)
        # - Converting ~s to complete paths (e.g., ~/ to /home/brian/ and ~arthur to /home/arthur)
        workon_home_dir=\"\$(virtualenvwrapper_expandpath \"\$workon_home_dir\")\"
    fi

    echo \"\$workon_home_dir\"
    return 0
}")

(defvar ar-subr-test-string2 "#!/bin/bash
 # --

if [ \$# == 0 ]; then
    # some comment (note: for compatibility)
    set \"\" `find .  -maxdepth 1 -type f -name \"\*.txt\" | sed 's/..\\(.\*\\)/\\1/'`

    for i in \$\*; do
        # some comment (note: for compatibility)
	pass
    done

fi

vorhanden() {
    for i in \"/usr/bin/lynx\" \"/usr/bin/pdftotext\" \"/usr/bin/ps2ascii\" \"/usr/bin/abiword\";
    do
        # some comment (note: for compatibility)
	if [ ! -x \$i ]; then
	    echo \"Achtung! \$i nicht vorhanden oder nicht ausfuehrbar\\nWeitermachen?\"

	    a=nu
	    read a
	    case \$a in
                # some comment (note: for compatibility)
		y) echo \"Fortsetzung!\";;

		\*) exit 0
            # some comment (note: for compatibility)
	    esac
	else
            # some comment (note: for compatibility)
	    echo \"\$i vorhanden!\"

	fi
    # some comment (note: for compatibility)
    done
}
")



(ert-deftest ar-forward-comment-elisp-test ()
  (ar-test-with-elisp-buffer
      "(defun foo1 (&optional beg end)
  \" \"
  (interactive \"\*\")
;;   (let ((beg (cond (beg)
;;                    ((use-region-p)"
      (forward-line -2)
    (end-of-line)
    (ar-forward-comment)
    (should (eobp))))

(ert-deftest in-comment-p-elisp-test ()
  (ar-test-with-elisp-buffer
      "
\"
;;;\""
    (should (not (ar-in-comment-p)))))

(provide 'ar-subr-ert-tests)
;;; ar-subr-ert-tests.el ends here
