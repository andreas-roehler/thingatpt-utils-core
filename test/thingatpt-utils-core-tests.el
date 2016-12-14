;; ar-core-tests-1.el --- Tests -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.
;; Copyright (C) 2014-2015 Andreas Röhler, <andreas.roehler@online.de>

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

;; tests are expected to run from directory test


(ert-deftest ar-ert-raise-numbers-1 ()
  (ar-test-with-elisp-buffer
      "#x75"
    (forward-char -1) 
    ;; (should (eq 118 (1+ (car (read-from-string (number-at-point))))
    (should (eq 118 (1+ (number-at-point))))))

(ert-deftest ar-ert-raise-numbers-2 ()
  (ar-test-with-elisp-buffer
      "#o165"
    (forward-char -1) 
    ;; (should (eq 118 (1+ (car (read-from-string (number-at-point))))
    (should (eq 118 (1+ (number-at-point))))))

(ert-deftest ar-ert-raise-numbers-3 ()
  (ar-test-with-elisp-buffer
      "117"
    (forward-char -1) 
    ;; (should (eq 118 (1+ (car (read-from-string (number-at-point))))
    (should (eq 118 (1+ (number-at-point))))))

(ert-deftest ar-ert-symbol-atpt-1 ()
  (ar-test-with-elisp-buffer
      "o\\\""
    (forward-char -1)
    (let ((erg (ar-th-bounds 'symbol)))
    (should (eq 1 (car erg)))
    (should (eq 4 (cdr erg))))))

(ert-deftest ar-ert-symbol-atpt-2 ()
  (ar-test-with-elisp-buffer
      "(defun w ()"
    (search-backward "w")
    (let ((erg (ar-th-bounds 'symbol)))
    (should (eq 8 (car erg)))
    (should (eq 9 (cdr erg))))))


(provide 'ar-core-tests-1)
;;; ar-core-tests-1.el ends here