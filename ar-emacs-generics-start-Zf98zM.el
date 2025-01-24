;;; ar-emacs-generics-start-Zf98zM.el -- Searching downwards in buffer -*- lexical-binding: t; -*-

;; URL: https://github.com/andreas-roehler/emacs-generics
;; Keywords: languages

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

;;; Commentary: ‘ar--go-to-keyword’ and related fundamental stuff

;;; Code:

(defcustom ar-mark-decorators nil
  "If decorators should be marked too.

Default is nil.

Also used by navigation"
  :type 'boolean
  :tag "ar-mark-decorators")

(defun ar-escaped-p (&optional pos)
    "Return t if char at POS is preceded by an odd number of backslashes. "
    (save-excursion
      (when pos (goto-char pos))
      (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro ar-current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (ar-escaped-p))))

(defun ar--skip-to-comment-or-semicolon ()
  "Returns position if point was moved."
  (let ((orig (point)))
    (cond ((while (and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
                       ;; (sit-for 1)
                       (and (nth 8 (parse-partial-sexp (point-min) (point))) (skip-chars-forward "#;" (line-end-position)))))))
    (and (< orig (point))(point))))

(defun ar--end-of-comment-intern (pos)
  (while (and (not (eobp))
              (forward-comment 99999)))
  ;; forward-comment fails sometimes
  (and (eq pos (point)) (prog1 (forward-line 1) (back-to-indentation))
       (while (member (char-after) (list  (string-to-char comment-start) 10))(forward-line 1)(back-to-indentation))))

(defun ar-forward-statement (&optional orig done repeat)
  "Go to the last char of current statement.

ORIG - consider original position or point.
DONE - transaktional argument
REPEAT - count and consider repeats"
  (interactive)
  (ar-navigate-update-vars major-mode)
  (unless (eobp)
    (let ((repeat (or (and repeat (1+ repeat)) 0))
	  (orig (or orig (point)))
	  erg last
	  ;; use by scan-lists
	  forward-sexp-function pps err)
      (setq pps (parse-partial-sexp (point-min) (point)))
      ;; (origline (or origline (ar-count-lines)))
      (cond
       ;; which-function-mode, lp:1235375
       ;; (re-search-forward "'ar-\\([[:alpha:]-]+" nil t 1)
       ((< ar-max-specpdl-size repeat)
	(error "forward-statement reached loops max. If no error, customize ‘max-specpdl-size’"))
       ((looking-at (symbol-value (quote ar-def-or-class-re)))
        (end-of-line)
        (skip-chars-backward " \t\r\n\f"))
       ;; list
       ((nth 1 pps)
	(if (<= orig (point))
	    (progn
	      (setq orig (point))
	      ;; do not go back at a possible unclosed list
	      (goto-char (nth 1 pps))
	      (if
		  (ignore-errors (forward-list))
		  (progn
		    (when (looking-at ":[ \t]*$")
		      (forward-char 1))
		    (setq done t)
		    (skip-chars-forward "^#" (line-end-position))
		    (skip-chars-backward " \t\r\n\f" (line-beginning-position))
		    (ar-forward-statement orig done repeat))
		(setq err (ar--record-list-error pps))
		(goto-char orig)))))
       ;; in comment
       ((and comment-start (looking-at (concat " *" comment-start)))
        (ar--end-of-comment-intern (point)))
       ;; (goto-char (match-end 0))
       ;; (ar-forward-statement orig done repeat))
       ((nth 4 pps)
	(ar--end-of-comment-intern (point))
	(ar--skip-to-comment-or-semicolon)
	(while (and (eq (char-before (point)) ?\\)
		    (ar-escaped-p) (setq last (point)))
	  (forward-line 1) (end-of-line))
	(and last (goto-char last)
	     (forward-line 1)
	     (back-to-indentation))
	;; ar-forward-statement-test-3JzvVW
	(unless (or (looking-at (concat " *" comment-start))(eolp))
	  (ar-forward-statement orig done repeat)))
       ;; string
       ((looking-at ar-string-delim-re)
	(goto-char (match-end 0))
	(ar-forward-statement orig done repeat))
       ((nth 3 pps)
	(when (ar-end-of-string)
	  (end-of-line)
	  (skip-chars-forward " \t\r\n\f")
	  (setq pps (parse-partial-sexp (point-min) (point)))
	  (unless (and done (not (or (nth 1 pps) (nth 8 pps))) (eolp)) (ar-forward-statement orig done repeat))))
       ((ar-current-line-backslashed-p)
	(end-of-line)
	(skip-chars-backward " \t\r\n\f" (line-beginning-position))
	(while (and (eq (char-before (point)) ?\\)
		    (ar-escaped-p))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" (line-beginning-position)))
	(unless (eobp)
	  (ar-forward-statement orig done repeat)))
       ((eq orig (point))
	(if (eolp)
	    (skip-chars-forward " \t\r\n\f#'\"")
	  (end-of-line)
	  (skip-chars-backward " \t\r\n\f" orig))
	;; point at orig due to a trailing whitespace
	(and (eq (point) orig) (skip-chars-forward " \t\r\n\f"))
	;; (setq done t)
	(ar-forward-statement orig done repeat))
       ((eq (current-indentation) (current-column))
	(ar--skip-to-comment-or-semicolon)
	(setq pps (parse-partial-sexp orig (point)))
	(if (nth 1 pps)
	    (ar-forward-statement orig done repeat)
	  (unless done
	    (ar-forward-statement orig done repeat))))
       ((and (looking-at "[[:print:]]+$") (not done) (ar--skip-to-comment-or-semicolon))
	(ar-forward-statement orig done repeat)))
      (unless
	  (or
	   (eq (point) orig)
	   (member (char-before) (list 10 32 9 ?#)))
	(setq erg (point)))
      (if (and ar-verbose-p err)
	  (ar--message-error err))
      erg)))

(defun ar-backward-statement (&optional orig done limit ignore-in-string-p repeat maxindent)
  "Go to the initial line of a simple statement.

For beginning of compound statement use ‘ar-backward-block’.
For beginning of clause ‘ar-backward-clause’.

‘ignore-in-string-p’ allows moves inside a docstring, used when
computing indents
ORIG - consider original position or point.
DONE - transaktional argument
LIMIT - honor limit
IGNORE-IN-STRING-P - also much inside a string
REPEAT - count and consider repeats
Optional MAXINDENT: don't stop if indentation is larger"
  (interactive)
  (save-restriction
    (unless (bobp)
      (let* ((repeat (or (and repeat (1+ repeat)) 0))
	     (orig (or orig (point)))
             (pps (parse-partial-sexp (or limit (point-min))(point)))
             (done done)
             erg)
	;; lp:1382788
	(unless done
	  (and (< 0 (abs (skip-chars-backward " \t\r\n\f")))
 	       (setq pps (parse-partial-sexp (or limit (point-min))(point)))))
        (cond
	 ((< ar-max-specpdl-size repeat)
	  (error "ar-forward-statement reached loops max. If no error, customize ‘ar-max-specpdl-size’"))
         ((and (bolp) (eolp))
          (skip-chars-backward " \t\r\n\f")
          (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; inside string
         ((and (nth 3 pps) (not ignore-in-string-p))
	  (setq done t)
	  (goto-char (nth 8 pps))
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 4 pps)
	  (while (ignore-errors (goto-char (nth 8 pps)))
	    (skip-chars-backward " \t\r\n\f")
	    (setq pps (parse-partial-sexp (line-beginning-position) (point))))
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((nth 1 pps)
          (goto-char (1- (nth 1 pps)))
	  (when (ar--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
          (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((ar-preceding-line-backslashed-p)
          (forward-line -1)
          (back-to-indentation)
          (setq done t)
          (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
         ((looking-at comment-start-skip)
	  (setq done t)
	  (forward-char -1)
          (skip-chars-backward " \t\r\n\f")
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at raw-string
	 ;; (and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	 ((and (looking-at "\"\"\"\\|'''") (member (char-before) (list ?u ?U ?r ?R)))
	  (forward-char -1)
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; BOL or at space before comment
         ((and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
          (forward-comment -1)
          (while (and (not (bobp)) (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
            (forward-comment -1))
          (unless (bobp)
            (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	 ;; at inline comment
         ((looking-at "[ \t]*#")
	  (when (ar--skip-to-semicolon-backward (save-excursion (back-to-indentation) (point)))
	    (setq done t))
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at beginning of string
	 ((looking-at ar-string-delim-re)
	  (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
	    (setq done t))
	  (back-to-indentation)
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; after end of statement
	 ((and (not done) (eq (char-before) ?\;))
	  (skip-chars-backward ";")
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; travel until indentation or semicolon
	 ((and (not done) (ar--skip-to-semicolon-backward))
	  (unless (and maxindent (< maxindent (current-indentation)))
	    (setq done t))
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ;; at current indent
	 ((and (not done) (not (eq 0 (skip-chars-backward " \t\r\n\f"))))
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent))
	 ((and maxindent (< maxindent (current-indentation)))
	  (forward-line -1)
	  (ar-backward-statement orig done limit ignore-in-string-p repeat maxindent)))
	;; return nil when before comment
	(unless (and (looking-at "[ \t]*#") (looking-back "^[ \t]*" (line-beginning-position)))
	  (when (< (point) orig)(setq erg (point))))
	erg))))

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

(defun ar-forward-statement-bol ()
  "Go to the ‘beginning-of-line’ following current statement."
  (interactive)
  (ar-forward-statement)
  (ar--beginning-of-line-form))

(defun ar-beginning-of-statement-p ()
  (interactive)
  (save-restriction
    (eq (point)
    (save-excursion
      (ar-forward-statement)
      (ar-backward-statement)))))

(defun ar-up-statement ()
  "go to the beginning of next statement upwards in buffer.

Return position if statement found, nil otherwise."
  (interactive)
  (if (ar--beginning-of-statement-p)
      (ar-backward-statement)
    (progn (and (ar-backward-statement) (ar-backward-statement)))))

(defun ar--end-of-statement-p ()
  "Return position, if cursor is at the end of a statement, nil otherwise."
  (let ((orig (point)))
    (save-excursion
      (ar-backward-statement)
      (ar-forward-statement)
      (when (eq orig (point))
        orig))))

(defun ar-down-statement ()
  "Go to the beginning of next statement downwards in buffer.

Corresponds to backward-up-list in Elisp
Return position if statement found, nil otherwise."
  (interactive)
  (let* ((orig (point)))
    (cond ((ar--end-of-statement-p)
	   (progn
	     (and
	      (ar-forward-statement)
	      (ar-backward-statement)
	      (< orig (point))
	      (point))))
	  ((ignore-errors (< orig (and (ar-forward-statement) (ar-backward-statement))))
	   (point))
	  ((ignore-errors (< orig (and (ar-forward-statement) (ar-forward-statement)(ar-backward-statement))))
	     (point)))))

(defun ar--fetch-indent-statement-above (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (back-to-indentation)
    (if (or (looking-at comment-start)(ar-beginning-of-statement-p))
        (current-indentation)
      (ar-backward-statement)
      (current-indentation))))

(defun ar--end-base-determine-secondvalue (regexp)
  "Expects being at block-opener.

REGEXP: a symbol"
  (cond
   ((eq regexp (quote ar-minor-block-re))
    (cond ((looking-at ar-else-re)
	   nil)
	  ((or (looking-at (concat ar-try-re)))
	   (concat ar-elif-re "\\|" ar-else-re "\\|" ar-except-re))
	  ((or (looking-at (concat ar-except-re "\\|" ar-elif-re "\\|" ar-if-re)))
	   (concat ar-elif-re "\\|" ar-else-re))))
   ((member regexp
	    (list
	     (quote ar-block-re)
	     (quote ar-block-or-clause-re)
	     (quote ar-clause-re)
	     (quote ar-if-re)
	     ))
    (cond ((looking-at ar-if-re)
	   (concat ar-elif-re "\\|" ar-else-re))
	  ((looking-at ar-elif-re)
	   (concat ar-elif-re "\\|" ar-else-re))
	  ((looking-at ar-else-re))
	  ((looking-at ar-try-re)
	   (concat ar-except-re "\\|" ar-else-re "\\|" ar-finally-re))
	  ((looking-at ar-except-re)
	   (concat ar-else-re "\\|" ar-finally-re))
	  ((looking-at ar-finally-re)
	   nil)))
   ((eq regexp (quote ar-for-re)) nil)
   ((eq regexp (quote ar-try-re))
    (cond
     ;; ((looking-at ar-try-re)
     ;;  (concat ar-except-re "\\|" ar-else-re "\\|" ar-finally-re))
     ((looking-at ar-except-re)
      (concat ar-else-re "\\|" ar-finally-re))
     ((looking-at ar-finally-re))
     (t
      (concat ar-except-re "\\|" ar-else-re "\\|" ar-finally-re))))))

(defun ar--backward-regexp (regexp &optional indent condition orig regexpvalue)
  "Search backward next regexp not in string or comment.

Return position if successful"
  (unless (bobp)
    (save-match-data
      (unless (ar-beginning-of-statement-p) (skip-chars-backward " \t\r\n\f")
	      (ar-backward-comment (point)))
      (let* (pps
	     (regexpvalue (or regexpvalue (symbol-value regexp)))
	     (indent (or indent (current-indentation)))
             (condition (or condition '<=))
	     (orig (or orig (point)))
             (allvalue (if (member regexp (list (quote ar-block-re) (quote ar-clause-re) (quote ar-def-or-class-re) (quote ar-def-re) (quote ar-class-re))) regexpvalue (symbol-value (quote ar-block-or-clause-re)))))
        (if (eq (current-indentation) (current-column))
	    (while (and (not (bobp))
		        (re-search-backward allvalue nil 'move 1)
		        (not (and (looking-back "async *" (line-beginning-position))
			          (goto-char (match-beginning 0))))
		        (or (and
                             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
                             (goto-char pps))
                            (not (looking-at regexpvalue))
                            (and (not (eq (current-column) 0))
                                 (looking-at regexpvalue)
                                 indent
                                 ;; def foo(): pass
                                 ;; def bar(): pass_|_
                                 ;; need '<
		      	         (funcall condition indent (current-indentation))))
                        (prog1 t
                          (when (< (current-indentation) indent)

                            ;; update required indent if a form should not match
                            ;; as ‘def bar()’, when ‘ar-backward-def’ from end below

                            ;; def foo():
                            ;;     if True:
                            ;;         def bar():
                            ;;             pass
                            ;;     elif False:
                            ;;         def baz():
                            ;;             pass
                            ;;     else:
                            ;;         try:
                            ;;             1 == 1
                            ;;         except:
                            ;;             pass

                            (setq indent (current-indentation))))))
	  (unless (bobp)
            (back-to-indentation)
	    (and
             (setq pps (nth 8 (parse-partial-sexp (point-min) (point))))
             (goto-char pps))
	    (unless (and (< (point) orig) (looking-at regexpvalue))
	      (ar--backward-regexp regexp (current-indentation) condition orig))
            (unless (or (eq (point) orig)(bobp)) (back-to-indentation))))
        (and (looking-at regexpvalue) (not (nth 8 (parse-partial-sexp (point-min) (point))))(point))))))

(defun ar--go-to-keyword (regexp &optional condition maxindent ignoreindent)
  "Expects being called from beginning of a statement.

Argument REGEXP: a symbol.

Return a list if found, whose car holds indentation, cdr position in buffer.

Keyword detected from REGEXP
Honor MAXINDENT if provided
Optional IGNOREINDENT: find next keyword at any indentation"
  (unless (bobp)
    ;;    (when (ar-empty-line-p) (skip-chars-backward " \t\r\n\f"))
    (let* ((orig (point))
	   (regexp (if (eq regexp (quote ar-clause-re)) (quote ar-extended-block-or-clause-re) regexp))
	   (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
	   (maxindent
	    (if ignoreindent
		;; just a big value
		9999
	      (or maxindent
                  (if (ar-empty-line-p) (current-column) (current-indentation)))))
           (allvalue (symbol-value (quote ar-block-or-clause-re)))
           erg)
      (unless (ar-beginning-of-statement-p)
	(ar-backward-statement))
      (when (and (not (string= "" ar-block-closing-keywords-re))(looking-at ar-block-closing-keywords-re))
        (setq maxindent (min maxindent (- (current-indentation) ar-indent-offset))))
      (cond
       ((and (looking-at regexpvalue)(< (point) orig))
        (setq erg (point)))
        (t (while
              (not (or (bobp) (and (looking-at regexpvalue)(< (point) orig) (not (nth 8 (parse-partial-sexp (point-min) (point)))))))
             ;; search backward and reduce maxindent, if non-matching forms suggest it
              (setq erg (ar--backward-regexp regexp maxindent
                                         (or condition '<=)
                                         orig regexpvalue)))))
      erg)))

(defun ar-up-base (regexp &optional indent)
  "Expects a symbol as REGEXP like `(quote ar-clause-re)'

Return position if successful"
  (unless (ar-beginning-of-statement-p) (ar-backward-statement))
  (unless (looking-at (symbol-value regexp))
    (ar--go-to-keyword regexp '< (or indent (current-indentation))))
  ;; now from beginning-of-block go one indent level upwards
  (when
      (looking-at (symbol-value regexp))
    (ar--go-to-keyword regexp '< (- (or indent (current-indentation)) ar-indent-offset))))

(defun ar-up-base-bol (regexp)
  "Go to the beginning of next form upwards in buffer.

Return position if form found, nil otherwise.
Argument REGEXP determined by form"
  (let* (;;(orig (point))
         erg)
    (if (bobp)
        (setq erg nil)
      (while (and (re-search-backward regexp nil t 1)
                  (nth 8 (parse-partial-sexp (point-min) (point)))))
      (beginning-of-line)
      (when (looking-at regexp) (setq erg (point)))
      ;; (when ar-verbose-p (message "%s" erg))
      erg)))

(defun ar--down-according-to-indent (regexp secondvalue &optional indent use-regexp)
  "Return position if moved, nil otherwise.

Optional ENFORCE-REGEXP: search for regexp only."
  (unless (eobp)
    (let* ((orig (point))
	   (indent (or indent 0))
	   done
	   (regexpvalue (if (member regexp (list (quote ar-def-re) (quote ar-def-or-class-re) (quote ar-class-re)))
			    (concat (symbol-value regexp) "\\|" (symbol-value (quote ar-decorator-re)))
			  (symbol-value regexp)))
	   (lastvalue (and secondvalue
			   (pcase regexp
			     (`ar-try-re (concat ar-finally-re "\\|" ar-except-re "\\|" ar-else-re))
			     (`ar-if-re ar-else-re))))
           last)
      (if (eq regexp (quote ar-clause-re))
          (ar-forward-clause-intern indent)
        (while
	    (and
	     (not done)
	     (progn (end-of-line)
		    (cond (use-regexp
			   ;; using regexpvalue might stop behind global settings, missing the end of form
			   (re-search-forward (concat "^ \\{0,"(format "%s" indent) "\\}"regexpvalue) nil 'move 1))
			  (t (re-search-forward (concat "^ \\{"(format "0,%s" indent) "\\}[[:alnum:]_@]+") nil 'move 1))))
	     (or (nth 8 (parse-partial-sexp (point-min) (point)))
	         (progn (back-to-indentation) (ar--forward-string-maybe (nth 8 (parse-partial-sexp orig (point)))))
	         (and secondvalue (looking-at secondvalue) (setq last (point)))
	         (and lastvalue (looking-at lastvalue)(setq last (point)))
	         (and (looking-at regexpvalue) (setq done t) (setq last (point)))
	         ;; ar-forward-def-or-class-test-3JzvVW
	         ;; (setq done t)
                 ))))
      (when last (goto-char last))
      (and (< orig (point)) (point)))))

(defun ar--end-base (regexp &optional orig bol repeat)
  "Used internal by functions going to the end FORM.

Returns the indentation of FORM-start
Arg REGEXP, a symbol"
  (unless (eobp)
    (let (;; not looking for an assignment
	  (use-regexp (member regexp (list (quote ar-def-re) (quote ar-class-re) (quote ar-def-or-class-re))))
	  (orig (or orig (point))))
      (unless (eobp)
	(unless (ar-beginning-of-statement-p)
	  (ar-backward-statement))
	(let* (;; when at block-start, be specific
	       ;; (regexp (ar--refine-regexp-maybe regexp))
               (regexpvalue (if (symbolp regexp)(symbol-value regexp) regexp))
               ;; (regexp (or regexp (symbol-value (quote ar-extended-block-or-clause-re))))
	       (repeat (if repeat (1+ repeat) 0))
	       (indent (if
			   (looking-at regexpvalue)
                           (current-indentation)
			   ;; (if (bolp) 0
			   ;;   (abs
			   ;;    (- (current-indentation) ar-indent-offset)))
			 (current-indentation)))
               (secondvalue (ar--end-base-determine-secondvalue regexp))
	       ;; when at block-start, be specific
	       ;; return current-indentation, position and possibly needed clause-regexps (secondvalue)
	       (res
		(cond
		 ((and (ar-beginning-of-statement-p)
		       ;; (eq 0 (current-column))
		       (or (looking-at regexpvalue)
			   (and (member regexp (list (quote ar-def-re) (quote ar-def-or-class-re) (quote ar-class-re)))
				(looking-at ar-decorator-re)
				(ar-down-def-or-class (current-indentation)))
			   (and (member regexp (list (quote ar-minor-block-re) (quote ar-if-re) (quote ar-for-re) (quote ar-try-re)))
				(looking-at ar-minor-clause-re))))
		  (list (current-indentation) (point) secondvalue))
		 ((looking-at regexpvalue)
		  (list (current-indentation) (point) secondvalue))
		 ((eq 0 (current-indentation))
		  (ar--down-according-to-indent regexp nil 0 use-regexp))
		 ;; look upward
		 (t (ar--go-to-keyword regexp (if (member regexp (list (quote ar-def-re) (quote ar-class-re) (quote ar-def-or-class-re))) '< '<=)
                                       ;; (if (and (member regexp (list (quote ar-block-re) (quote ar-clause-re) (quote ar-def-or-class-re) (quote ar-def-re) (quote ar-class-re))) (looking-at (symbol-value regexp))) '< '<=)
                                       )))))
	  ;; (ar-for-block-p (looking-at ar-for-re))
	  ;; (setq indent (current-indentation))
	  (cond
	   (res
	    (and
	     (ar--down-according-to-indent regexp secondvalue (current-indentation))
             (progn
               (when (and secondvalue (looking-at secondvalue))
                 ;; (when (looking-at ar-else-re)
                 (ar--down-according-to-indent regexp secondvalue (current-indentation)))
	       ;; (if (>= indent (current-indentation))
	       (ar--down-end-form))
	     ;; (ar--end-base regexp orig bol repeat)
	     ;; )
	     ))
	   (t (unless (< 0 repeat) (goto-char orig))
	      (ar--forward-regexp (symbol-value regexp))
	      (beginning-of-line)
	      (and
	       (ar--down-according-to-indent regexp secondvalue (current-indentation) t)
	       (ar--down-end-form))))
	  (cond ((< orig (point))
		 (if bol
                     (ar--beginning-of-line-form)
		   (point)))
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
							 (eq 0 indent))
			   (and
			    ;; next block-start downwards, reduce expected indent maybe
			    (setq indent (or (and (< 0 indent) (- indent ar-indent-offset)) indent))
			    (ar--down-according-to-indent regexp secondvalue
							  indent t))))
		     (ar--end-base regexp orig bol (1+ repeat))))))
		((< (point) orig)
		 (goto-char orig)
		 (when (ar--down-according-to-indent regexp secondvalue nil t)
		   (ar--end-base regexp (point) bol (1+ repeat))))))))))

;; ar-emacs-generics-start-Zf98zM.el ends here
(provide 'ar-emacs-generics-start-Zf98zM)
