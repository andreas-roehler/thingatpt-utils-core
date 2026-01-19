;;; ar-navigate.el --- Provide generic navigation -*- lexical-binding: t; -*-

;; URL: https://github.com/andreas-roehler/emacs-generics

;; Copyright (C) 2015-2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Keywords: lisp, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ar-subr)
(require 'ar-subr-x)
(require 'ar-beg-end)

(defvar ar-block-closing-keywords-re ""
  "Closing keywords.")

(defvar ar-string-delim-re "\\(\"\"\"\\|'''\\|\"\\|'\\)"
  "When looking at beginning of string.")

(defvar ar-smart-indentation nil)
(defvar ar-block-re "")
(make-variable-buffer-local 'ar-block-re)

(defvar ar-clause-re "")
(make-variable-buffer-local 'ar-clause-re)

(defvar ar-extended-block-or-clause-re "")
(make-variable-buffer-local 'ar-extended-block-or-clause-re)

(defvar ar-labelled-re "[ \\t]*:[[:graph:]]+"
  "When looking at label.")
;; (setq ar-labelled-re "[ \\t]*:[[:graph:]]+")

(defvar ar-expression-skip-regexp "[^ (=:#\t\r\n\f]"
  "Possible not composing an expression.")
(make-variable-buffer-local 'ar-expression-skip-regexp)

(defvar ar-expression-skip-chars "^ (:=#\t\r\n\f"
  "Chars indicated not composing an expression.")
(make-variable-buffer-local 'ar-expression-skip-chars)

(defvar ar-expression-re "[^ =#\t\r\n\f]+"
  "Chars indicated possible composing an expression, when ‘looking-at’ or -back.")
(make-variable-buffer-local 'ar-expression-re)

(defvar ar-not-expression-regexp "[ .=#\t\r\n\f)]+"
  "Expression assumes chars indicated probably will not compose an expression.")
(make-variable-buffer-local 'ar-not-expression-regexp)

(defvar ar-not-expression-chars " #\t\r\n\f"
  "Ar-expression assumes chars indicated probably will not compose an expression.")
(make-variable-buffer-local 'ar-not-expression-chars)

(defvar ar-partial-expression-backward-chars "^ =,\"'()[]{}:#\t\r\n\f"
  "Chars indicated possible composing a partial-expression.")
(make-variable-buffer-local 'ar-partial-expression-backward-chars)

(defvar ar-partial-expression-forward-chars "^ \"')}]:#\t\r\n\f")
(make-variable-buffer-local 'ar-partial-expression-forward-chars)

(defvar ar-operator-regexp "[ \t]*\\(\\.\\|+\\|-\\|*\\|//\\|//\\|&\\|%\\||\\|\\^\\|>>\\|<<\\|<\\|<=\\|>\\|>=\\|==\\|!=\\)[ \t]*"
  "Matches most of operators inclusive whitespaces around.

See also `ar-assignment-regexp'")

(defvar ar-assignment-regexp "[ \t]*=[^=]"
  "Matches assignment operator inclusive whitespaces around.

See also `ar-operator-regexp'")
(make-variable-buffer-local 'ar-operator-regexp)

(defvar ar-delimiter-regexp "\\(\\.[[:alnum:]]\\|,\\|;\\|:\\)[ \t\n]"
  "Delimiting elements of lists or other programming constructs.")
(make-variable-buffer-local 'ar-delimiter-regexp)

(defvar ar-line-number-offset 0
  "Store the line number where the region started.")
(make-variable-buffer-local 'ar-line-number-offset)

(defvar ar-match-paren-no-use-syntax-pps nil)
(make-variable-buffer-local 'ar-match-paren-no-use-syntax-pps)

(defvar ar-traceback-line-re
  "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "Regular expression that describes tracebacks.")

(defvar ar-block-or-clause-re ""
  "Describe beginning of block-or-clause")

(defvar ar-class-re ""
  "Describe beginning of class")

(defvar ar-def-re ""
  "Describe beginning of def")

(defvar ar-def-or-class-re ""
  "Describe beginning of def-or-class")

(defvar ar-minor-block-re ""
  "Describe beginning of minor-block")

(defvar ar-clause-re ""
  "Describe beginning of clause")

(defvar ar-else-re ""
  "Describe beginning of else")

(defvar ar-try-re ""
  "Describe beginning of try")

(defvar ar-elif-re ""
  "Describe beginning of elif")

(defvar ar-except-re ""
  "Describe beginning of except")

(defvar ar-if-re ""
  "Describe beginning of if")

(defvar ar-finally-re ""
  "Describe beginning of finally")

(defvar ar-decorator-re "@\\w+"
  "Describe beginning of decorator")

(defvar ar-section-re ""
  "Describe beginning of section")

(defvar ar-XXX-tag-face 'ar-XXX-tag-face)

(defvar ar-pseudo-keyword-face 'ar-pseudo-keyword-face)
(make-variable-buffer-local 'ar-XXX-tag-face)

(defvar ar-variable-name-face 'ar-variable-name-face)

(defvar ar-number-face 'ar-number-face)
(make-variable-buffer-local 'ar-variable-name-face)

(defvar ar-decorators-face 'ar-decorators-face)

(defvar ar-object-reference-face 'ar-object-reference-face)
(make-variable-buffer-local 'ar-decorators-face)

(defvar ar-builtins-face 'ar-builtins-face)

(defvar ar-class-name-face 'ar-class-name-face)
(make-variable-buffer-local 'ar-builtins-face)

(defvar ar-exception-name-face 'ar-exception-name-face)

(defvar ar-import-from-face 'ar-import-from-face)
(make-variable-buffer-local 'ar-exception-name-face)

(defvar ar-def-class-face 'ar-def-class-face)

(defvar ar-try-if-face 'ar-try-if-face)
(make-variable-buffer-local 'ar-def-class-face)

(defvar ar-vertical-move-start-column nil)

(defvar ar-honor-decorators nil)
(make-variable-buffer-local 'ar-honor-decorators)

(defcustom ar-indent-offset 4
  "Amount of offset per level of indentation."
  :type 'integer
  :tag "ar-indent-offset")
(make-variable-buffer-local 'ar-indent-offset)

(defun ar-previous-line (arg)
  "Moving ARG upwards.

Keep the column if possible."
  (interactive "p")
  (unless (bobp)
    (let ((col
	   (if
	       (eq last-command 'ar-previous-line)
	       ar-vertical-move-start-column
	     (setq ar-vertical-move-start-column (current-column)))))
      (forward-line (- arg))
      (if (eq major-mode 'dired-mode)
	  (dired-move-to-filename)
	(when (< col (save-excursion (end-of-line) (current-column)))
	  (move-to-column col))))))

(defun ar-next-line (arg)
  "Moving ARG upwards.

Keep the column if possible."
  (interactive "p")
  (unless (eobp)
    (let ((col
	   (if
	       (eq last-command 'ar-next-line)
	       ar-vertical-move-start-column
	     (setq ar-vertical-move-start-column (current-column)))))
      (forward-line arg)
      (if (eq major-mode 'dired-mode)
	  (dired-move-to-filename)
      (when (< col (save-excursion (end-of-line) (current-column)))
	(move-to-column col))))))

(defun ar-toggle-indent-tabs-mode ()
  "Toggle `indent-tabs-mode'.

Returns value of `indent-tabs-mode' switched to."
  (interactive)
  (when
      (setq indent-tabs-mode (not indent-tabs-mode))
    (setq tab-width ar-indent-offset))
  (when (and ar-verbose-p (called-interactively-p 'any)) (message "indent-tabs-mode %s  ar-indent-offset %s" indent-tabs-mode ar-indent-offset))
  indent-tabs-mode)

(defun ar-indent-tabs-mode (arg &optional iact)
  "With positive ARG switch `indent-tabs-mode' on.

With negative ARG switch `indent-tabs-mode' off.
Returns value of `indent-tabs-mode' switched to.
Optional argument IACT Interactively called."
  (interactive "p")
  (if (< 0 arg)
      (progn
        (setq indent-tabs-mode t)
        (setq tab-width ar-indent-offset))
    (setq indent-tabs-mode nil))
  (when (and ar-verbose-p (or iact (called-interactively-p 'any))) (message "indent-tabs-mode %s   ar-indent-offset %s" indent-tabs-mode ar-indent-offset))
  indent-tabs-mode)

(defun ar-indent-tabs-mode-on (arg)
  "Switch `indent-tabs-mode' on.
Argument ARG sent by p."
  (interactive "p")
  (ar-indent-tabs-mode (abs arg)(called-interactively-p 'any)))

(defun ar-indent-tabs-mode-off (arg)
  "Switch `indent-tabs-mode' off.
Argument ARG sent by p."
  (interactive "p")
  (ar-indent-tabs-mode (- (abs arg))(called-interactively-p 'any)))

;;  Guess indent offset
(defun ar-guessed-sanity-check (guessed)
  (and (>= guessed 2)(<= guessed 8)(eq 0 (% guessed 2))))

(defun ar--guess-indent-final (indents)
  "Calculate and do sanity-check.
Argument INDENTS Previous indents.
Argument ORIG Position."
  (let* ((first (car indents))
         (second (cadr indents))
         (erg (if (and first second)
                  (if (< second first)
                      ;; (< (point) orig)
                      (- first second)
                    (- second first))
                (default-value 'ar-indent-offset))))
    (setq erg (and (ar-guessed-sanity-check erg) erg))
    erg))

(defun ar-beginning-of-statement-p ()
  "Return position, if cursor is at the beginning of a `statement', nil otherwise."
  (when (eq (current-column) (current-indentation))
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (unless (or (nth 3 pps) (nth 4 pps)(empty-line-p))
	(point)))))

;; (defun ar-beginning-of-statement-p (&optional pps)
;;   "Return position, if cursor is at the beginning of a ‘statement’, nil otherwise."
;;   (interactive)
;;   (save-excursion
;;     (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
;;       (and (not (or (nth 8 pps) (nth 1 pps)))
;;            (looking-at ar-statement-re)
;;            (looking-back "[^ \t]*" (line-beginning-position))
;;            (eq (current-column) (current-indentation))
;; 	   (eq (point) (progn (ar-forward-statement) (ar-backward-statement)))
;;            (point)))))

(defun ar--guess-indent-forward ()
  "Called when moving to end of a form and `ar-smart-indentation' is on."
  (let* ((first (if
                    (ar-beginning-of-statement-p)
                    (current-indentation)
                  (progn
                    (ar-forward-statement)
                    (ar-backward-statement)
                    (current-indentation))))
         (second (if (or (looking-at ar-extended-block-or-clause-re)(eq 0 first))
                     (progn
                       (ar-forward-statement)
                       (ar-forward-statement)
                       (ar-backward-statement)
                       (current-indentation))
                   ;; when not starting from block, look above
                   (while (and (re-search-backward ar-extended-block-or-clause-re nil 'movet 1)
                               (or (>= (current-indentation) first)
                                   (nth 8 (parse-partial-sexp (point-min) (point))))))
                   (current-indentation))))
    (list first second)))

(defun ar--guess-indent-backward ()
  "Called when moving to beginning of a form and `ar-smart-indentation' is on."
  (let* ((cui (current-indentation))
         (indent (if (< 0 cui) cui 999))
         (pos (progn (while (and (re-search-backward ar-extended-block-or-clause-re nil 'move 1)
                                 (or (>= (current-indentation) indent)
                                     (nth 8 (parse-partial-sexp (point-min) (point))))))
                     (unless (bobp) (point))))
         (first (and pos (current-indentation)))
         (second (and pos (ar-forward-statement) (ar-forward-statement) (ar-backward-statement)(current-indentation))))
    (list first second)))

(defun ar-guess-indent-offset (&optional direction)
  "Guess `ar-indent-offset'.

Set local value of `ar-indent-offset', return it

Might change local value of `ar-indent-offset' only when called
downwards from beginning of block followed by a statement.
Otherwise ‘default-value’ is returned.
Optional argument DIRECTION where to move."
  (interactive)
  (save-excursion
    (let* (;;(orig (point))
           (indents
            (cond (direction
                   (if (eq 'forward direction)
                       (ar--guess-indent-forward)
                     (ar--guess-indent-backward)))
                  ;; guess some usable indent is above current position
                  ((eq 0 (current-indentation))
                   (ar--guess-indent-forward))
                  (t (ar--guess-indent-backward))))
           (erg (ar--guess-indent-final indents)))
      (if erg (setq ar-indent-offset erg)
        (setq ar-indent-offset
              (default-value 'ar-indent-offset)))
      (when (called-interactively-p 'any) (message "%s" ar-indent-offset))
      ar-indent-offset)))

(defun ar-up-statement ()
  "Go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise."
  (interactive)
  (let (;;(orig (point))
        erg)
    (if (ar-beginning-of-statement-p)
	(setq erg (ar-backward-statement))
      (setq erg (and (ar-backward-statement) (ar-backward-statement))))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))
(make-variable-buffer-local 'ar-beginning-of-defun-re)

(defun ar-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Return position if statement found, nil otherwise."
  (interactive)
  (let* ((orig (point))
	  (erg
	   (cond ((ar--end-of-statement-p)
		  (and (ar-forward-statement) (ar-backward-statement)))
		 ((< orig (progn (ar-forward-statement) (ar-backward-statement)))
		  (point))
		 (t (and (ar-forward-statement) (ar-forward-statement)(ar-backward-statement))))))
	   (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	   erg))

(defun ar-down-base (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise.
Argument REGEXP determined by form."
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* (;;(orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (parse-partial-sexp (point-min) (point)))))
        (back-to-indentation)
        (when (looking-at regexp) (setq erg (point)))
        ;; (when ar-verbose-p (message "%s" erg))
        erg))))

(defun ar-down-base-bol (regexp)
  "Go to the beginning of next form below in buffer.

Return position if form found, nil otherwise.
Argument REGEXP determined by form"
  (unless (eobp)
    (forward-line 1)
    (beginning-of-line)
    (let* (;;(orig (point))
           erg)
      (if (eobp)
          (setq erg nil)
        (while (and (re-search-forward regexp nil t 1)
                    (nth 8 (parse-partial-sexp (point-min) (point)))))
        (beginning-of-line)
        (when (looking-at regexp) (setq erg (point)))
        ;; (when ar-verbose-p (message "%s" erg))
        erg))))

(defun ar-up-block ()
  "Go to the beginning of next block upwards in buffer.

Return position if block found, nil otherwise."
  (interactive)
  (ar-up-base ar-block-re))

(defun ar-up-block-or-clause ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Return position if block-or-clause found, nil otherwise."
  (interactive)
  (ar-up-base ar-block-or-clause-re))

(defun ar-up-class ()
  "Go to the beginning of next class upwards in buffer.

Return position if class found, nil otherwise."
  (interactive)
  (ar-up-base ar-class-re))

(defun ar-up-clause ()
  "Go to the beginning of next clause upwards in buffer.

Return position if clause found, nil otherwise."
  (interactive)
  (ar-up-base ar-clause-re))

(defun ar-up-def ()
  "Go to the beginning of next def upwards in buffer.

Return position if def found, nil otherwise."
  (interactive)
  (ar-up-base ar-def-re))

(defun ar-up-def-or-class ()
  "Go to the beginning of next def-or-class upwards in buffer.

Return position if def-or-class found, nil otherwise."
  (interactive)
  (ar-up-base ar-def-or-class-re))

(defun ar-up-minor-block ()
  "Go to the beginning of next minor-block upwards in buffer.

Return position if minor-block found, nil otherwise."
  (interactive)
  (ar-up-base ar-minor-block-re))

(defun ar-up-section ()
  "Go to the beginning of next section upwards in buffer.

Return position if section found, nil otherwise."
  (interactive)
  (ar-up-base ar-section-re))

(defun ar-down-block ()
  "Go to the beginning of next block below in buffer.

Return position if block found, nil otherwise."
  (interactive)
  (ar-down-base ar-block-re))

(defun ar-down-block-or-clause ()
  "Go to the beginning of next block-or-clause below in buffer.

Return position if block-or-clause found, nil otherwise."
  (interactive)
  (ar-down-base ar-block-or-clause-re))

(defun ar-down-class ()
  "Go to the beginning of next class below in buffer.

Return position if class found, nil otherwise."
  (interactive)
  (ar-down-base ar-class-re))

(defun ar-down-clause ()
  "Go to the beginning of next clause below in buffer.

Return position if clause found, nil otherwise."
  (interactive)
  (ar-down-base ar-clause-re))

(defun ar-down-def ()
  "Go to the beginning of next def below in buffer.

Return position if def found, nil otherwise."
  (interactive)
  (ar-down-base ar-def-re))

(defun ar-down-def-or-class ()
  "Go to the beginning of next def-or-class below in buffer.

Return position if def-or-class found, nil otherwise."
  (interactive)
  (ar-down-base ar-def-or-class-re))

(defun ar-down-minor-block ()
  "Go to the beginning of next minor-block below in buffer.

Return position if minor-block found, nil otherwise."
  (interactive)
  (ar-down-base ar-minor-block-re))

(defun ar-down-section ()
  "Go to the beginning of next section below in buffer.

Return position if section found, nil otherwise."
  (interactive)
  (ar-down-base ar-section-re))

(defun ar-up-block-bol ()
  "Go to the beginning of next block upwards in buffer.

Go to beginning of line.
Return position if block found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-block-re))

(defun ar-up-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause upwards in buffer.

Go to beginning of line.
Return position if block-or-clause found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-block-or-clause-re))

(defun ar-up-class-bol ()
  "Go to the beginning of next class upwards in buffer.

Go to beginning of line.
Return position if class found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-class-re))

(defun ar-up-clause-bol ()
  "Go to the beginning of next clause upwards in buffer.

Go to beginning of line.
Return position if clause found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-clause-re))

(defun ar-up-def-bol ()
  "Go to the beginning of next def upwards in buffer.

Go to beginning of line.
Return position if def found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-def-re))

(defun ar-up-def-or-class-bol ()
  "Go to the beginning of next def-or-class upwards in buffer.

Go to beginning of line.
Return position if def-or-class found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-def-or-class-re))

(defun ar-up-minor-block-bol ()
  "Go to the beginning of next minor-block upwards in buffer.

Go to beginning of line.
Return position if minor-block found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-minor-block-re))

(defun ar-up-section-bol ()
  "Go to the beginning of next section upwards in buffer.

Go to beginning of line.
Return position if section found, nil otherwise."
  (interactive)
  (ar-up-base-bol ar-section-re))

(defun ar-down-block-bol ()
  "Go to the beginning of next block below in buffer.

Go to beginning of line
Return position if block found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-block-re))

(defun ar-down-block-or-clause-bol ()
  "Go to the beginning of next block-or-clause below in buffer.

Go to beginning of line
Return position if block-or-clause found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-block-or-clause-re))

(defun ar-down-class-bol ()
  "Go to the beginning of next class below in buffer.

Go to beginning of line
Return position if class found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-class-re))

(defun ar-down-clause-bol ()
  "Go to the beginning of next clause below in buffer.

Go to beginning of line
Return position if clause found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-clause-re))

(defun ar-down-def-bol ()
  "Go to the beginning of next def below in buffer.

Go to beginning of line
Return position if def found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-def-re))

(defun ar-down-def-or-class-bol ()
  "Go to the beginning of next def-or-class below in buffer.

Go to beginning of line
Return position if def-or-class found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-def-or-class-re))

(defun ar-down-minor-block-bol ()
  "Go to the beginning of next minor-block below in buffer.

Go to beginning of line
Return position if minor-block found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-minor-block-re))

(defun ar-down-section-bol ()
  "Go to the beginning of next section below in buffer.

Go to beginning of line
Return position if section found, nil otherwise"
  (interactive)
  (ar-down-base-bol ar-section-re))

(defun ar-forward-comment-or-defun ()
  "Not implemented yet.

If inside a comment, go to end of commented section.

If inside a function def, go to its end"
  )

(defun ar-end-of-string (&optional beginning-of-string-position)
  "Go to end of string at point if any, if successful return position.
Optional argument BEGINNING-OF-STRING-POSITION Position."
  (interactive)
  ;; (when ar-debug-p (message "(current-buffer): %s" (current-buffer)))
  ;; (when ar-debug-p (message "major-mode): %s" major-mode))
  (let ((orig (point))
	(beginning-of-string-position (or beginning-of-string-position (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
                                          (and (looking-at "\"\"\"\\|'''\\|\"\\|\'")(match-beginning 0))))
        erg)
    (if beginning-of-string-position
        (progn
          (goto-char beginning-of-string-position)
	  (when
	      ;; work around parse-partial-sexp error
	      (and (nth 3 (parse-partial-sexp 1 (point)))(nth 8 (parse-partial-sexp 1 (point))))
	    (goto-char (nth 3 (parse-partial-sexp 1 (point)))))
          (if (ignore-errors (setq erg (scan-sexps (point) 1)))
			      (goto-char erg)
	    (goto-char orig)))

      (error (concat "ar-end-of-string: don't see end-of-string at " (buffer-name (current-buffer)) "at pos " (point))))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

;; Expression
(defun ar-backward-expression ()
  "Go to the beginning of a compound expression.

A a compound expression might be concatenated by \".\" operator,
thus composed by minor expressions.

If already at the beginning or before a expression,
go to next expression in buffer upwards"
  (interactive)
  (let (erg)
    (setq erg (ar--beginning-of-expression-intern))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun ar--beginning-of-expression-intern (&optional orig)
  (unless (bobp)
    (let ((orig (or orig (point)))
          (pps (syntax-ppss))
          erg)
      (cond
       ( ;; (empty-line-p)
        (eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
             (eq 9 (char-after))(not (bobp)))
          (forward-line -1)
          (end-of-line))
        (ar--beginning-of-expression-intern orig))
       ;; lists
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (skip-chars-backward ar-expression-skip-chars))
       ((and (nth 3 pps)(nth 8 pps)
             (goto-char (nth 8 pps)))
        (cond (;; consider expression a string starting at BOL
               (bolp))
              ((looking-back ar-assignment-regexp (line-beginning-position)))
              ((looking-back ar-operator-regexp (line-beginning-position))
               (when (nth 2 pps)
                 (goto-char (nth 2 pps))))
              (t (ar--beginning-of-expression-intern orig))))
       ;; comments left
       ((nth 8 pps)
        (goto-char (nth 8 pps))
        (unless (bobp)
          (ar--beginning-of-expression-intern orig)))
       ;; concatenated strings
       ((looking-back (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re) (line-beginning-position))
        (goto-char (match-beginning 0))
        (while (looking-back (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp) (line-beginning-position) t)
          (goto-char (match-beginning 0)))
        (skip-chars-backward ar-expression-skip-chars))
       ;; before comment
       ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
        (forward-line -1)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (ar--beginning-of-expression-intern orig)))
       ((and (< (point) orig)(looking-at (concat ar-expression-re ar-delimiter-regexp))))
       ((looking-back (concat "[^ \t\n\r\f]+" ar-delimiter-regexp) (line-beginning-position))
        (goto-char (match-beginning 0))
	(skip-chars-backward ar-expression-skip-chars)
        (unless (or (looking-back ar-assignment-regexp (line-beginning-position)) (looking-back "^[ \t]*" (line-beginning-position)))
          (ar--beginning-of-expression-intern orig)))
       ;; before assignment
       ((looking-back ar-assignment-regexp (line-beginning-position))
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (ar--beginning-of-expression-intern orig))
       ((looking-back ar-operator-regexp (line-beginning-position))
        (goto-char (1- (match-beginning 0)))
        (forward-char -1)
        (unless (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "\"\\|'" (line-beginning-position))
        (forward-char -1)
        (skip-chars-backward "\"'")
        (unless (looking-back ar-assignment-regexp (line-beginning-position))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "(\\|\\[" (line-beginning-position))
        (forward-char -1)
        (unless (looking-back ar-assignment-regexp (line-beginning-position))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back "[\])}]" (line-beginning-position))
        (forward-char -1)
        (unless (looking-back ar-assignment-regexp (line-beginning-position))
          (ar--beginning-of-expression-intern orig)))
       ;; inside expression
       ((looking-back ar-expression-re (line-beginning-position))
        (skip-chars-backward ar-expression-skip-chars)
        (unless (or (looking-back "^[ \t]*" (line-beginning-position)) (looking-back ar-assignment-regexp (line-beginning-position)))
          (ar--beginning-of-expression-intern orig)))
       ((looking-back (concat "[ \t]*" "[[:alnum:]_]*" ar-operator-regexp "[[:alnum:]_]*") (line-beginning-position) t)
        (goto-char (match-beginning 0))
        (unless (looking-back "^[ \t]*" (line-beginning-position))
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (looking-back "[ \t\r\n\f]" (line-beginning-position)))
        (skip-chars-backward " \t\r\n\f")
        (unless (bobp)
          (forward-char -1)
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig) (not (bobp)) (looking-back ar-expression-re (line-beginning-position)))
        (forward-char -1)
        (when (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((and (looking-at ar-expression-re) (not (looking-back "[ \t\r\n\f]" (line-beginning-position))))
        (unless (< 0 (abs (skip-chars-backward ar-expression-skip-chars)))
          (ar--beginning-of-expression-intern orig)))
       ((and (eq (point) orig)(looking-back "[ \t]*=" (line-beginning-position)))
        (goto-char (match-beginning 0))
        (skip-chars-backward " \t\r\n\f")
        (ar--beginning-of-expression-intern orig)))
      (unless (or (eq (point) orig)(looking-at "[ \t]*#"))
        (setq erg (point)))
      erg)))

(defun ar-forward-of-expression (&optional arg)
  "Go to the end of a compound expression.

With numeric ARG do it that many times.

A a compound expression might be concatenated by \".\" operator,
thus composed by minor expressions."
  (interactive "p")
  (or arg (setq arg 1))
  (let (erg)
    (if (< 0 arg)
        (save-restriction
          (widen)
          (while (< 0 arg)
            (setq erg (ar--end-of-expression-intern))
            (setq arg (1- arg))))
      (setq arg (abs arg))
      (setq erg (ar-backward-expression)))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun ar--end-of-expression-intern (&optional orig)
  (unless (eobp)
    (let* ((orig (or orig (point)))
           (pps (syntax-ppss))
           erg
           ;; use by scan-lists
           parse-sexp-ignore-comments)
      (cond
       ((nth 1 pps)
        (goto-char (nth 1 pps))
        (let ((parse-sexp-ignore-comments t))
          (forward-list))
        (unless (or (looking-at "[ \t]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ;; in comment
       ((nth 4 pps)
        (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
        (ar--end-of-expression-intern orig))
       ( ;; (empty-line-p)
	(eq 9 (char-after))
        (while
            (and  ;; (empty-line-p)
	     (eq 9 (char-after))(not (eobp)))
          (forward-line 1))
        (ar--end-of-expression-intern orig))
       ((looking-at (concat ar-string-delim-re ar-expression-re ar-string-delim-re ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re))
        (goto-char (match-end 0))
        (while (looking-at (concat ar-operator-regexp ar-string-delim-re ar-expression-re ar-string-delim-re))
          (goto-char (match-end 0))))
       ;; inside string
       ((ar-in-string-p)
        (when (looking-at "\"\"\"\\|'''\\|\"\\|'")
          (goto-char (match-end 0)))
        (while
            (nth 3 (syntax-ppss))
          (forward-char 1))
        (unless (looking-at "[ \t]*$")
          (ar--end-of-expression-intern orig)))
       ((looking-at "[(\[]")
        (forward-list)
        (unless (looking-at "[ \t]*$")
          (ar--end-of-expression-intern orig)))
       ((and (looking-at "[ \t]*#")(looking-back "^[ \t]*" (line-beginning-position)))
        (while (and (looking-at "[ \t]*#") (not (eobp)))
          (forward-line 1))
        (ar--end-of-expression-intern orig))
       ((and (eq orig (point)) (looking-at ar-assignment-regexp))
        (goto-char (match-end 0))
        (if (looking-at "[(\[]")
            (forward-list 1)
          (ar--end-of-expression-intern orig)))
       ((looking-at (concat "[^ \t\n\r\f]*" ar-delimiter-regexp))
        (goto-char (match-end 0))
        (while (looking-at (concat "[^ \t\n\r\f]*" ar-delimiter-regexp))
          (goto-char (match-end 0)))
        (forward-char -1)
        (unless (looking-at (concat ar-assignment-regexp "\\|[ \t]*$\\|" ar-delimiter-regexp))
          (ar--end-of-expression-intern orig)))
       ((looking-at (concat "\\([[:alnum:] ]+ \\)" ar-assignment-regexp))
	(goto-char (match-end 1))
	(skip-chars-backward " \t\r\n\f"))
       ((and (eq orig (point)) (looking-at (concat "[ \t]*" "[^(\t\n\r\f]+" ar-operator-regexp)))
	(skip-chars-forward " \t\r\n\f")
	(when (< 0 (skip-chars-forward ar-expression-skip-chars))
	  (ar--end-of-expression-intern orig)))
       ((and (eq orig (point)) (looking-at ar-not-expression-regexp))
        (skip-chars-forward ar-not-expression-chars)
        (unless (or (looking-at "[ \t]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ((looking-at ar-expression-skip-regexp)
        (skip-chars-forward ar-expression-skip-chars)
        (unless (or (looking-at "[ \n\t\r\f]*$")(looking-at ar-assignment-regexp))
          (ar--end-of-expression-intern orig)))
       ((and (eq (point) orig)
	     (skip-chars-forward " \t\r\n\f")
	     (< 0 (skip-chars-forward ar-expression-skip-chars)))
	(ar--end-of-expression-intern orig)))

      (unless (or (eq (point) orig)(and (eobp)(bolp)))
        (setq erg (point)))
      erg)))

(defun ar-backward-partial-expression ()
  "Go to the beginning of a partial expression.
Optional argument ORIG Position."
  (interactive)
  (let ((orig (point))
	erg)
    (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))(not (bobp))(forward-char -1))
    (when (ar-in-comment-p)
      (ar-backward-comment)
      (skip-chars-backward " \t\r\n\f"))
    ;; part of ar-partial-expression-forward-chars
    (when (member (char-after) (list ?\ ?\" ?' ?\) ?} ?\] ?: ?#))
      (forward-char -1))
    (skip-chars-backward ar-partial-expression-forward-chars)
    (when (< 0 (abs (skip-chars-backward ar-partial-expression-backward-chars)))
      (while (and (not (bobp)) (ar-in-comment-p)(< 0 (abs (skip-chars-backward ar-partial-expression-backward-chars))))))
    (when (< (point) orig)
      (unless
	  (and (bobp) (member (char-after) (list ?\ ?\t ?\r ?\n ?\f)))
	(setq erg (point))))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-forward-of-partial-expression ()
  "Go to the end of a partial expression.
Optional argument ORIG Position."
  (interactive)
  (let (erg)
    (skip-chars-forward ar-partial-expression-backward-chars)
    ;; group arg
    (and
     (looking-at "[\[{(]")
     (goto-char (scan-sexps (point) 1)))
    (setq erg (point))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-forward-expression (&optional orig done repeat pps)
  "Go to the end of a compound expression.

Operators are ignored.
Optional argument ORIG Position.
Optional argument DONE status.
Optional argument REPEAT counter.
Optional argument PPS result of ‘parse-partial-sexp’."
  (interactive)
  (unless done (skip-chars-forward " \t\r\n\f"))
  (unless (eobp)
    (let ((comment-start (ar-fix-comment-start))
	  (repeat (or (and repeat (1+ repeat)) 0))
	  (pps (or pps (parse-partial-sexp (point-min) (point))))
          (orig (or orig (point)))
          erg)
      (if (< ar-max-specpdl-size repeat)
	  (error "`ar-forward-expression' reached loops max")
	(cond
	 ;; in comment
	 ((nth 4 pps)
	  (or (< (point) (progn (forward-comment 1)(point)))(forward-line 1))
	  (ar-forward-expression orig done repeat))
	 ;; empty before comment
	 ((and comment-start (looking-at (concat "[ \t]*" comment-start))(looking-back "^[ \t]*" (line-beginning-position)))
	  (while (and (looking-at (concat "[ \t]*" comment-start)) (not (eobp)))
	    (forward-line 1))
	  (ar-forward-expression orig done repeat))
	 ;; inside string
	 ((nth 3 pps)
	  (goto-char (nth 8 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((looking-at "\"\"\"\\|'''\\|\"\\|'")
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((nth 1 pps)
	  (goto-char (nth 1 pps))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ;; looking at opening delimiter
	 ((eq 4 (car-safe (syntax-after (point))))
	  (goto-char (scan-sexps (point) 1))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ((and (eq orig (point)) (looking-at ar-operator-regexp))
	  (goto-char (match-end 0))
	  (ar-forward-expression orig done repeat))
	 ((and (not done)
	       (< 0 (skip-chars-forward ar-expression-skip-chars)))
	  (setq done t)
	  (ar-forward-expression orig done repeat))
	 ;; at colon following arglist
	 ((looking-at ":[ \t]*$")
	  (forward-char 1)))
	(unless (or (eq (point) orig)(and (eobp)(bolp)))
	  (setq erg (point)))
	(when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
	erg))))

(defun ar--beginning-of-line-form ()
  "Internal use: Go to beginning of line following end of form.

Return position."
  (if (eobp)
      (point)
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun ar-forward-expression-bol ()
  "Go to the ‘beginning-of-line’ following current expression."
  (interactive)
  (let ((erg (ar-forward-expression)))
    (setq erg (ar--beginning-of-line-form))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun ar-down-expression ()
  "Go to the beginning of next expression downwards in buffer.

Return position if expression found, nil otherwise."
  (interactive)
  (let* ((orig (point))
         (erg
          (cond ((ar--end-of-expression-p)
                 (and (ar-forward-expression) (ar-backward-expression)))
                ((ignore-errors (< orig (progn (ar-forward-expression) (ar-backward-expression))))
                 (point))
                (t (goto-char orig) (and (ar-forward-expression) (ar-forward-expression)(ar-backward-expression))))))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defun ar--backward-top-level-function (function &optional regexp)
  "Optional REGEXP in ‘sh-mode’ would match \"fi\" for example.

Argument FUNCTION function."
  (let (erg)
    (while (and (not (bobp))
		(setq erg (funcall function regexp))
		(or
		 (< 0 (current-column))
		 (looking-at regexp)
		 (ignore-errors (looking-at comment-start)))))
    erg))

(defun ar-mode-specific-tuning (mode)
  (pcase mode
    (`scala-mode
     (and (looking-at "class")
          (looking-back "case *" (line-beginning-position))
          (goto-char (match-beginning 0))))))

(defun ar--beginning-of-form-intern (regexp &optional iact indent orig lc)
  "Go to beginning of FORM.

With INDENT, go to beginning one level above.
Whit IACT, print result in message buffer.

Returns beginning of FORM if successful, nil otherwise
Argument REGEXP determined by form."
  (interactive "P")
  (skip-chars-backward " \t\r\n\f")
  (unless (bobp)
    (let* ((orig (or orig (point)))
           (indent (or indent (progn
                                (back-to-indentation)
                                (or (ar-beginning-of-statement-p)
                                    (ar-backward-statement))
                                (current-indentation))))
           (erg (cond ((and (< (point) orig) (looking-at regexp))
                       (point))
                      ((and (eq 0 (current-column)) (numberp indent) (< 0 indent))
                       (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
                         (ar-backward-statement)
                         (if (looking-at regexp)
			     (progn
			       (goto-char (match-beginning 1))
			       (point))
                           (cdr (ar--go-to-keyword regexp (current-indentation))))))
                      ((numberp indent)
		       (cdr (ar--go-to-keyword regexp indent)))
                      (t (ignore-errors
                           (cdr (ar--go-to-keyword regexp
                                                   (- (progn (if (ar-beginning-of-statement-p) (current-indentation) (save-excursion (ar-backward-statement) (current-indentation)))) ar-indent-offset))))))))
      (when lc (beginning-of-line) (setq erg (point)))
      (when (and ar-verbose-p iact) (message "%s" erg))
      erg)))

(defun ar--beginning-of-prepare (indent final-re &optional inter-re iact lc)
  "final-re should be some backward-regexp
inter-re expected some clause-regexp"
  (let ((orig (point))
        (indent
         (or indent
             (progn (back-to-indentation)
                    (or (ar-beginning-of-statement-p)
                        (ar-backward-statement))
                    (cond ((eq 0 (current-indentation))
                           (current-indentation))
                          ;; ((looking-at (symbol-value inter-re))
                          ((looking-at inter-re)
			   (current-indentation))
                          (t
                           (if (<= ar-indent-offset (current-indentation))
                               (- (current-indentation) (if ar-smart-indentation (ar-guess-indent-offset) ar-indent-offset))
                             ar-indent-offset))))))
        erg)
    ;; (if (and (< (point) orig) (looking-at (symbol-value final-re)))
    (if (and (< (point) orig) (looking-at final-re))
	(progn
          (and lc (beginning-of-line))
          (setq erg (point))
          (when (and ar-verbose-p iact) (message "%s" erg))
          erg)
      (ar--beginning-of-form-intern final-re iact indent orig lc))))

(defun ar-forward-block ()
  "Go to end of block.

Returns end of block if successful, nil otherwise
Optional argument INDENT indent."
  (interactive "P")
  (let* ((orig (point))
         (erg (ar--end-base 'ar-block-re orig)))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" erg))
    erg))

(defalias 'ar-down-block-lc 'ar-forward-block-bol)
(defun ar-forward-block-bol ()
  "Goto beginning of line following end of block.
Returns position reached, if successful, nil otherwise.

See also `ar-down-block'."
  (interactive)
  (let ((erg (ar-forward-block)))
    (setq erg (ar--beginning-of-line-form))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-backward-block (&optional indent block-re clause-re)
  "Go to beginning block, skip whitespace at BOL.

Returns beginning of block if successful, nil otherwise
Optional argument INDENT indent."
  (interactive)
  (let ((block-re (or block-re ar-block-re))
	(clause-re (or clause-re ar-clause-re)))
    ;; haskell block-re
    ;; ".*\\_<\\(\\(?:anyclass\\|c\\(?:ase\\|lass\\)\\|d\\(?:ata\\|o\\)\\|i\\(?:mport\\|n\\(?:fix[lr]?\\|stance\\)\\|[fn]\\)\\|let\\|m\\(?:ain\\|do\\|odule\\)\\|newtype\\|p\\(?:attern\\|roc\\)\\|rec\\|s\\(?:ignature\\|tock\\)\\|\\(?:typ\\|wher\\)e\\)\\)\\_>.*"
    (ar--beginning-of-prepare indent block-re clause-re (called-interactively-p 'any))))

(defun ar--travel-this-indent-backward (&optional indent)
  "Travel current INDENT backward.

With optional INDENT travel bigger or equal indentation"
  (let ((indent (or indent (current-indentation)))
	last)
    (while (and (not (bobp))
		(ar-backward-statement)
		(<= indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    last))

(defun ar-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (ar--travel-this-indent-backward))
      (when erg (goto-char erg))
      erg)))

(defun ar--travel-this-indent-backward-bol (indent)
  "Internal use.

Travel this INDENT backward until bol"
  (let (erg)
    (while (and (ar-backward-statement-bol)
		(or indent (setq indent (current-indentation)))
		(eq indent (current-indentation))(setq erg (point)) (not (bobp))))
    (when erg (goto-char erg))))

(defun ar-backward-statement-bol ()
  "Goto beginning of line where statement start.
Returns position reached, if successful, nil otherwise.

See also ‘ar-up-statement’"
  (interactive)
  (let* ((orig (point))
         erg)
    (unless (bobp)
      (cond ((bolp)
	     (and (ar-backward-statement orig)
		  (progn (beginning-of-line)
			 (setq erg (point)))))
	    (t (setq erg
		     (and
		      (ar-backward-statement)
		      (progn (beginning-of-line) (point)))))))
    erg))

(defun ar-backward-indent-bol ()
  "Go to the beginning of line of a section of equal indent.

If already at the beginning or before an indent,
go to next indent in buffer upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let ((indent (when (eq (current-indentation) (current-column)) (current-column)))
	  erg)
      (setq erg (ar--travel-this-indent-backward-bol indent))
      erg)))

(defun ar--travel-this-indent-forward (indent)
  "Internal use.

Travel this INDENT forward"
  (let (last erg)
    (while (and (ar-down-statement)
		(eq indent (current-indentation))
		(setq last (point))))
    (when last (goto-char last))
    (setq erg (ar-forward-statement))
    erg))

(defun ar-forward-indent ()
  "Go to the end of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when moved, nil otherwise"
  (interactive)
  (let (done
	(orig (line-beginning-position))
	(indent (current-indentation))
	(last (progn (back-to-indentation) (point))))
    (while (and (not (eobp)) (not done)
		(progn (forward-line 1) (back-to-indentation) (or (ar-empty-line-p) (and (<= indent (current-indentation))(< last (point))))))
      (unless (ar-empty-line-p) (skip-chars-forward " \t\r\n\f")(setq last (point)))
      (and (not (ar-empty-line-p))(< (current-indentation) indent)(setq done t)))
    (goto-char last)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (and (< orig (point))(point))))

(defun ar-forward-indent-bol ()
  "Go to beginning of line following of a section of equal indentation.

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (eobp)
    (when (ar-forward-indent)
      (unless (eobp) (progn (forward-line 1) (beginning-of-line) (point))))))

(defun ar-mark-indent-level ()
  "Mark lines around point where indentation is equal or deeper."
  (interactive)
  (let ((orig (point)))
    (ar-backward-indent)
    (push-mark (point) t)
    (goto-char orig)
    (ar-forward-indent)))

(defun mark-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-new (buffer-substring-no-properties (mark) (point))))

(defun kill-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-region (mark) (point)))

(defun beginning-of-form (&optional bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
 Don't use this from a program, use `beginning-of-form-base' instead. "
  (interactive)
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (beginning-of-form-base begstr endstr bound noerror count permit-comment))
  (when ar-verbose-p  (message "%s" (point))) (point))

(defun end-of-form (&optional iact bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
Set comment to ‘t’ if forms inside comments should match
- also for processing comments itself."
  (interactive "p")
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (end-of-form-base begstr endstr bound noerror count permit-comment))
  (when iact (message "%s" (point))) (point))

(defun ar-leave-begstr-backward (begstr unquoted-beg)
  (let* ((stringcount (length unquoted-beg))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-beg)))
    (while (and indx (not (ignore-errors (looking-at begstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-beg))
      (setq stringcount (1- stringcount)))))

(defun ar-leave-endstr-backward (endstr unquoted-end)
  (let* ((stringcount (length unquoted-end))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-end)))
    (while (and indx (not (ignore-errors (looking-at endstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-end))
      (setq stringcount (1- stringcount)))))

;; (defalias 'ar-backward-def-or-class 'ar-up-def-or-class-bol)
;; (defalias 'ar-forward-def-or-class 'ar-down-def-or-class-bol)

(defalias 'ar-end-of-def-or-class 'ar-forward-def-or-class)
(defalias 'ar-beginning-of-def-or-class 'ar-backward-def-or-class)

(defun ar-backward-class ()
 "Go to beginning of ‘class’.

If already at beginning, go one ‘class’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let (erg)
    (setq erg (cdr-safe (ar--go-to-keyword ar-class-re)))
    (when ar-honor-decorators (and (ar-backward-decorator)
                                                 (setq erg (point))))
    erg))

(defun ar-backward-def ()
  "Go to beginning of ‘def’.

If already at beginning, go one ‘def’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let ((erg (cdr-safe (ar--go-to-keyword ar-def-re))))
    (or (and ar-honor-decorators (ar-backward-decorator)) erg)))

(defun ar-backward-def-or-class ()
 "Go to beginning of ‘def-or-class’.

If already at beginning, go one ‘def-or-class’ backward.
Return beginning of form if successful, nil otherwise"
  (interactive)
  (let ((erg (cdr-safe (ar--go-to-keyword ar-def-or-class-re))))
    (or (and ar-honor-decorators (ar-backward-decorator)) erg)))

(defun ar-backward-block-bol ()
  "Go to beginning of ‘block’, go to BOL.
If already at beginning, go one ‘block’ backward.
Return beginning of ‘block’ if successful, nil otherwise"
  (interactive)
  (and (ar-backward-block)
       (progn (beginning-of-line)(point))))

(defun ar-backward-class-bol ()
  "Go to beginning of ‘class’, go to BOL.
If already at beginning, go one ‘class’ backward.
Return beginning of ‘class’ if successful, nil otherwise"
  (interactive)
  (and (ar-backward-class)
       (progn (beginning-of-line)(point))))

(defun ar-backward-def-bol ()
  "Go to beginning of ‘def’, go to BOL.
If already at beginning, go one ‘def’ backward.
Return beginning of ‘def’ if successful, nil otherwise"
  (interactive)
  (and (ar-backward-def)
       (progn (beginning-of-line)(point))))

(defun ar-backward-def-or-class-bol ()
  "Go to beginning of ‘def-or-class’, go to BOL.
If already at beginning, go one ‘def-or-class’ backward.
Return beginning of ‘def-or-class’ if successful, nil otherwise"
  (interactive)
  (and (ar-backward-def-or-class)
       (progn (beginning-of-line)(point))))

(defun ar--forward-regexp (re)
  (interactive)
  (re-search-forward re nil t 1))

(defun ar--down-end-form ()
  "Return position."
  (progn (ar--backward-empty-lines-or-comment)
	 (point)))

(defun ar--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	  (ar-backward-comment))))

(defun ar--forward-string-maybe (&optional start)
  "Go to the end of string.

Expects START position of string
Return position of moved, nil otherwise."
  (let ((orig (point)))
    (when start (goto-char start)
	  (when (looking-at "\"\"\"\\|'''")
	    (goto-char (1- (match-end 0)))
	    (forward-sexp))
	  ;; maybe at the inner fence
	  (when (looking-at "\"\"\\|''")
	    (goto-char (match-end 0)))
	  (and (< orig (point)) (point)))))

(defun ar-forward-clause-intern (indent)
  (end-of-line)
  (let (last)
    (while
        (and
         (ar-forward-statement)
         (save-excursion (ar-backward-statement) (< indent (current-indentation)))
         (setq last (point))
         ))
    (when last (goto-char last))))

(defun ar--end-of-statement-p ()
  "Return position, if cursor is at the end of a statement, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (ar-backward-statement)
      (ar-forward-statement)
      (when (eq orig (point))
        orig))))

(defun ar--check-scan-from-current-line()
  (let (pps last)
    (save-excursion
      (end-of-line)
      (when (progn (setq pps (parse-partial-sexp (point-min) (point))) (nth 4 pps)) (goto-char (nth 8 pps)))
      (skip-chars-backward " \t\r\n\f")
      (skip-chars-backward "^{" (line-beginning-position))
      (setq last (point))
      (if (eq (car-safe (syntax-after (1- (point)))) 4)
          (list last (char-before) pps)))))

(defun in-braces-from-start (pps orig)
  (let (erg)
    (when (setq erg (ar--check-scan-from-current-line))
      (setq pps (nth 2 erg))
      (goto-char (car erg))
      (when (nth 1 pps) (goto-char (nth 1 pps))
            (forward-sexp))
      (< orig (point)))))

(defun ar--end-base-intern (regexp &optional orig bol repeat)
  ""
  (ar-navigate-update-vars major-mode)
  (let ((pps (parse-partial-sexp (point-min) (point)))
        (use-regexp (member regexp (list 'ar-def-re 'ar-class-re 'ar-def-or-class-re)))
        (orig (or orig (point)))
        (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
        last def-scan-from-current-line-p erg)
    ;; Scala
    ;;     object LargestTree { |
    ;; def largestTree(a: Seq[List[Int]]): Seq[List[Int]] = {
    (unless
        (in-braces-from-start pps orig)
      (skip-chars-forward " \t\r\n\f")
      (unless (eobp)
        (when (or (nth 4 pps) (looking-at comment-start-skip)) (ar-forward-comment))
	(unless (ar-beginning-of-statement-p)
	  (ar-backward-statement))
        (when (looking-at regexpvalue)
          (setq erg (ar--check-scan-from-current-line))
          (setq last (car-safe erg)
                def-scan-from-current-line-p (cdr-safe erg))))
      (if def-scan-from-current-line-p
          (progn (goto-char last) (forward-char -1) (ar-forward-sexp))
        (let (
	      (repeat (if repeat (1+ repeat) 0))
	      (indent (if
			  (looking-at regexpvalue)
			  (if (bolp) 0
			    (abs
			     (- (current-indentation) ar-indent-offset)))
			(current-indentation)))
	      ;; when at block-start, be specific
	      ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	      (res
	       (cond
		((and (ar-beginning-of-statement-p)
		      ;; (eq 0 (current-column))
		      (or (looking-at regexpvalue)
			  (and (member regexp (list 'ar-def-re 'ar-def-or-class-re 'ar-class-re))
                               ar-honor-decorators
			       (looking-at ar-decorator-re)
			       (ar-down-def-or-class))
			  (and (member regexp (list 'ar-minor-block-re 'ar-if-re 'ar-for-re 'ar-try-re))
			       (looking-at ar-clause-re))))
		 (list (current-indentation) (point) (ar--end-base-determine-secondvalue regexp)))
		((looking-at regexpvalue)
		 (list (current-indentation) (point) (ar--end-base-determine-secondvalue regexp)))
		;; ((eq 0 (current-indentation))
		;;  (ar--down-according-to-indent regexp nil 0 use-regexp))
		;; look upward
		(t (ar--go-to-keyword regexpvalue))))
	      (secondvalue (ignore-errors (nth 2 res)))
	      erg)
	  ;; (ar-for-block-p (looking-at ar-for-re))
	  (setq indent (or (and res (car-safe res)) indent))
          (when (looking-at regexpvalue)
            (setq erg (ar--check-scan-from-current-line))
            (setq last (car-safe erg)
                  def-scan-from-current-line-p (cdr-safe erg)))
          ;; (when (looking-at regexpvalue)
          ;; (setq def-scan-from-current-line-p (ar--check-scan-from-current-line)))
	  (cond
	   (res (setq erg
		      (and
		       (ar--down-according-to-indent regexp secondvalue indent use-regexp def-scan-from-current-line-p)
		       ;; (if (>= indent (current-indentation))
		       (ar--down-end-form)
		       ;; (ar--end-base regexp orig bol repeat)
		       ;;)
                       )))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (while
                  (and
                   (re-search-forward (symbol-value regexp) nil 'move)
                   (nth 8 (parse-partial-sexp (point-min) (point)))))
	      (beginning-of-line)
	      (setq erg
                    ;; Now at beginning of form of interest, re-calculate ‘def-scan-from-current-line-p’
                    (progn
                      (when (looking-at regexpvalue)
                        (setq erg (ar--check-scan-from-current-line))
                        (setq last (car-safe erg)
                              def-scan-from-current-line-p (cdr-safe erg)))
                      ;; (setq def-scan-from-current-line-p (and (looking-at regexpvalue) (count-lines (point-min) (point))))
		      (ar--down-according-to-indent regexp secondvalue (current-indentation) t def-scan-from-current-line-p)
		      (ar--down-end-form)))))
	  (cond ((< orig (point))
		 (setq erg (point))
		 (progn
		   (and erg bol (setq erg (ar--beginning-of-line-form)))
		   (and erg (cons (current-indentation) erg))))
		((eq (point) orig)
		 (unless (eobp)
		   (cond
		    ((and (< repeat 1)
			  (or
			   ;; looking next indent as part of body
			   (ar--down-according-to-indent regexp secondvalue
							 indent
							 ;; if expected indent is 0,
							 ;; search for new start,
							 ;; search for regexp only
							 (eq 0 indent)
                                                         def-scan-from-current-line-p)
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent ar-indent-offset)) indent))
			    (ar--down-according-to-indent regexp secondvalue
							  indent t def-scan-from-current-line-p))))
		     (ar--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (ar--down-according-to-indent regexp secondvalue nil t def-scan-from-current-line-p)
		   (ar--end-base regexp (point) bol (1+ repeat))))))))))

(defun ar-forward-def (&optional orig bol)
  "Go to end of def.

Return end of ‘def’ if successful, nil otherwise
Optional ORIG: start position
Optional BOL: go to beginning of line following end-position"
  (interactive)
  (cdr-safe (ar--end-base 'ar-def-re orig bol)))

;;  Decorator
(defun ar-backward-decorator ()
  "Go to the beginning of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)))
    (unless (bobp) (forward-line -1)
	    (back-to-indentation)
	    (while (and (progn (looking-at ar-decorator-re)(not (looking-at "\\w+")))
			(not
			 ;; (ar-empty-line-p)
			 (member (char-after) (list 9 10)))
			(not (bobp))(forward-line -1))
	      (back-to-indentation))
	    (or (and (looking-at ar-decorator-re) (match-beginning 0))
		(goto-char orig)))))

(defun ar-forward-decorator ()
  "Go to the end of a decorator.

Returns position if succesful"
  (interactive)
  (let ((orig (point)) erg)
    (unless (looking-at ar-decorator-re)
      (setq erg (ar-backward-decorator)))
    (when erg
      (if
          (re-search-forward ar-def-or-class-re nil t)
          (progn
            (back-to-indentation)
            (skip-chars-backward " \t\r\n\f")
            ;; (ar-leave-comment-or-string-backward)
            (skip-chars-backward " \t\r\n\f")
            (setq erg (point)))
        (goto-char orig)
        (end-of-line)
        (skip-chars-backward " \t\r\n\f")
        (when (ignore-errors (goto-char (ar-list-beginning-position-atpt)))
          (forward-list))
        (when (< orig (point))
          (setq erg (point))))
      erg)))

(defun ar-nav-last-prompt ()
  (interactive)
  (goto-char (pos-bol))
  (when
      (re-search-backward comint-prompt-regexp nil t 1)
    (comint-skip-prompt)))

(defconst ar-beginning-of-defun-re
  (concat
   "[ \t]*\\("
   (mapconcat 'identity
              (list
	       "(defgroup"
	       "(defconst"
	       "(defcustom"
	       "(defface"
	       "(define-.+-mode"
	       "(defmacro"
	       "(defsubst"
	       "(deftheme"
	       "(defun"
	       "(defvar"
	       "(ert-deftest"
	       )

              "\\|")
   "\\)")
  "Regular expression matching beginning of defun.")


(defalias 'ar-beginning-of-defun 'ar-backward-defun)
(defun ar-backward-defun (&optional outmost pps)
  "Move to the beginning of a function definition.

With OUTMOST don't stop at a nested inner function.
When `beginning-of-defun-function' is set, call with optional ARG

If no function found inside a list, go to list-start.
Otherwise reach next list upward in buffer
Optional argument PPS result of `parse-partial-sexp'."
  (interactive "P")
  (unless (bobp)
  (let* ((outmost (or outmost (eq 4 (prefix-numeric-value outmost))))
	 (pps (or pps (parse-partial-sexp (point-min) (point))))
	 (liststart (or (and (bobp) (point))(nth 1 pps)))
	 )
    (cond
     ((and (not liststart)(looking-at ar-beginning-of-defun-re))
      (unless (bobp) (skip-chars-backward " \t\r\n\f")
	      (ar-backward-defun)))
     (liststart
      (goto-char liststart)
      (while (and (not (looking-at ar-beginning-of-defun-re))(setq liststart (nth 1 (parse-partial-sexp (point-min) (point)))))
	(goto-char liststart))
      (and outmost (nth 1 (setq pps (parse-partial-sexp (point-min) (point)))) (ar-backward-defun outmost pps)))
     ((nth 4 pps) (ar-backward-comment)
      (ar-backward-defun outmost))
     ((or (eq ?\)(char-before)) (< 0 (abs (skip-chars-backward "^)"))))
	  (unless (bobp) (forward-char -1))
	  (ar-beginning-of-defun outmost)))
    liststart)))

(defalias 'ar-end-of-defun 'ar-forward-defun)
(defun ar-forward-defun ()
  "Move to the end of a function definition.

Return position if successful, nil otherwise
When `end-of-defun-function' is set, call it with optional ARG"
  (interactive)
  (unless (eobp)
    (skip-chars-forward " \t\r\n\f")
    (let* ((pps (parse-partial-sexp (point-min)(point)))
	   (nesting (nth 0 pps))
	   (in-comment (or (nth 4 pps)(looking-at comment-start)))
	   (orig (point)))
      (cond
       ((nth 1 pps)
	;; (goto-char (nth 1 pps))
	(ar-backward-defun)
	(forward-sexp))
       (in-comment
	(end-of-line)
	(ar-forward-comment)
	(ar-forward-defun))
       ((looking-at ar-beginning-of-defun-re)
	(forward-sexp))
       ((< 0 nesting)
	(ar-beginning-of-defun)
	(ar-end-of-defun))
       ((eq (char-after) ?\()
	(forward-sexp)))
       (when (< orig (point))
	(point)))))

(defalias 'defun-beginning-position 'function-beginning-position)
(defun function-beginning-position ()
  "Return the position where the current functions definition starts"
  (interactive)
  (save-excursion
    (let* ((orig (point)))
      (ar-beginning-of-defun)
      (when (< (point) orig)
        (when ar-verbose-p (message "%s" (point)))
        (point)
        ))))

(defalias 'defun-end-position 'function-end-position)
(defun function-end-position ()
  "Print the position where the current functions definition ends"
  (interactive)
  (save-excursion
    (let ((orig (point)))
      (ar-end-of-defun)
      (when (< orig (point))
        (when ar-verbose-p  (message "%s" (point)))
        (point)))))

(provide 'ar-navigate)
;;; ar-navigate.el ends here
