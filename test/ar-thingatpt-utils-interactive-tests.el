;; ar-thingatpt-utils-interactive-tests.el --- Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2023 Andreas Röhler, <andreas.roehler@online.de>

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

;;; Commentary: Test known to succeed when called interactively only

;;; Code:

(require 'ar-subr)
(require 'ar-thingatpt-setup-tests)

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

(provide 'ar-thingatpt-utils-interactive-tests)
;;; ar-thingatpt-utils-interactive-tests.el ends here
