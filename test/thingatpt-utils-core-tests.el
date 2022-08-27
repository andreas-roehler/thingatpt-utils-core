;; ar-core-tests-1.el --- Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2018 Andreas Röhler, <andreas.roehler@online.de>

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

;; (require 'ert)

(require 'ar-subr)

;; tests are expected to run from directory test

(ert-deftest ar-ert-raise-numbers-3 ()
  (ar-test-with-elisp-buffer-point-min
      "117"
    ;; (should (eq 118 (1+ (car (read-from-string (number-at-point))))
    (should (eq 118 (1+ (number-at-point))))))

(ert-deftest ar-ert-symbol-atpt-1 ()
  (ar-test-with-elisp-buffer-point-min
      "o\\\""
      (let ((erg (ar-th-bounds 'symbol)))
	(should (eq 1 (car erg)))
	(should (eq 4 (cdr erg))))))

(ert-deftest ar-ert-symbol-atpt-2 ()
  (ar-test-with-elisp-buffer-point-min
      "(defun w ()"
      (goto-char (point-min))
    (search-forward "w")
    (let ((erg (ar-th-bounds 'symbol)))
      (should (eq 8 (car erg)))
      (should (eq 9 (cdr erg))))))

(ert-deftest ar-ert-forward-defun-test ()
  (ar-test-with-elisp-buffer-point-min
      "(defun foo ()
  \"This docstring contains a line starting with \\\"(\\\"
(asdf)\")"
  (ar-forward-defun)
  (should (eq (char-before) ?\)))))

(ert-deftest ar-ert-docstring-starting-with-paren-indent-test ()
  (ar-test-with-elisp-buffer
      "(defun foo ()
  \"This docstring contains a line starting with \\\"(\\\"
\(asdf)\")"
    (ar-beginning-of-defun)
  (should (bobp))))

(ert-deftest ar-ert-forward-sexp-test-LGnrk9 ()
  (ar-test-with-elisp-buffer-point-min
      ";;;\\\;; (beg)
(defun foo1 (&optional beg end))"
      (goto-char (point-min))
    (forward-char 9)
    (ar-forward-sexp)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-ert-backward-toplevel-test-WRXXPd ()
  (ar-test-with-elisp-buffer
      "(defun foo1 (&optional beg end)
  \" \")

(defun foo2 (&optional beg end)
  \" \")"
    (goto-char (point-max))
    (beginning-of-line)
    (ar-backward-toplevel)
    (should (eq (point) 41))))

(ert-deftest ar-backward-defun-test-1977mjB ()
  (with-temp-buffer
    (insert "(defun foo1 ()
  \"[Some docu]

\(defun other-foo \"
  (interactive)
  \;\; defining another function inside
  (defun foo2 ()
    (interactive)
    (message \"%s\"   \"Foo2\")
\;\; another syntactically correct function just badly indented
\(defun foo3 ()
  (interactive)
  (message \"%s\" \"Foo3\"))
      )
  (message \"%s\" \"Foo1\"))")
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))
    (back-to-indentation)
    (if (functionp 'ar-backward-defun)
	(ar-backward-defun)
      (beginning-of-defun))
    (should (looking-at "(defun foo1"))))

(ert-deftest ar-backward-defun-test-1977ztH ()
  (with-temp-buffer
    (insert "(defun foo1 ()
  \"[Some docu]

\(defun other-foo \"
  (interactive)
  \;\; defining another function inside
  (defun foo2 ()
    (interactive)
    (message \"%s\"   \"Foo2\")
\;\; another syntactically correct function just badly indented
\(defun foo3 ()
  (interactive)
  (message \"%s\" \"Foo3\"))
)
  (message \"%s\" \"Foo1\"))")
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))
    (search-backward "foo2")
    (if (functionp 'ar-backward-defun)
	(ar-backward-defun)
      (beginning-of-defun))
    (should (looking-at "(defun foo2"))))

(ert-deftest ar-backward-defun-test-1977A4N ()
  (with-temp-buffer
    (insert "(defun foo1 ()
  \"[Some docu]

\(defun other-foo \"
  (interactive)
  \;\; defining another function inside
  (defun foo2 ()
    (interactive)
    (message \"%s\"   \"Foo2\")
\;\; another syntactically correct function just badly indented
\(defun foo3 ()
  (interactive)
  (message \"%s\" \"Foo3\"))
)
  (message \"%s\" \"Foo1\"))")
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))
    (search-backward "Foo3")
    (forward-line 1)
    (if (functionp 'ar-backward-defun)
	(ar-backward-defun)
      (beginning-of-defun))
    (should (looking-at "(defun foo2"))))


(provide 'ar-core-tests-1)
;;; ar-core-tests-1.el ends here
