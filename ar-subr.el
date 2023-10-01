;;; ar-subr.el --- A reliable beginning-of-defun and other helper functions  -*- lexical-binding: t; -*-

;; Author: Andreas Röhler <andreas.roehler@online.de>, unless indicated
;; otherwise

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

;;; Commentary:

;; Provide a reliable jump to start and end of a defun - and some more
;; subroutines.
;;

;;; Code:

(defvar ar-verbose-p nil
  "Internal use only.")

;; (setq ar-verbose-p t)

(defun ar-toggle-verbose-p ()
  "Toggle `ar-verbose-p'. "
  (interactive)
  (setq ar-verbose-p (not ar-verbose-p))
  (message "ar-verbose-p: %s"  ar-verbose-p))

(defvar ar-debug-p nil
  "Switch into test-buffer.")

(defun ar-toggle-debug-p ()
  "Toggle `ar-debug-p'. "
  (interactive)
  (setq ar-debug-p (not ar-debug-p))
  (message "ar-debug-p: %s"  ar-debug-p))

(defvar ar-keyboard-operator-chars "<>~!@#$^&*_-+=|:;\"',.?"
  "(Insert known operator chars existing on a qwerty keybord")

(defun ar-insert-keyboard-operator-chars ()
  (interactive "*")
  (insert ar-keyboard-operator-chars))

(defun ar-insert-keyboard-operator-char-listing ()
  (interactive "*")
  (let* ((chars ar-keyboard-operator-chars)
         (laenge (length chars)))
    (insert "(list")
    (dotimes (i laenge)
      (insert (concat " ?" (char-to-string (aref chars i)))))
    (insert ")")
    (save-excursion
      (search-backward ";")
      (insert "\\"))
    (save-excursion
      (search-backward "\"")
      (insert "\\"))))

(defvar ar-line-move-forward t)

(defvar ar-max-specpdl-size max-specpdl-size
  "Protect against eternal loop.")

(defvar ar-literal-delim-re "\""
  "When looking at beginning of string.")

(defalias 'ar-escaped 'ar-escaped-p)
;; (defmacro ar-escaped-p ()
;;   "Return t if char is preceded by an odd number of backslashes."
;;   `(save-excursion
;;      (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defun ar-escaped-p (&optional pos)
    "Return t if char at POS is preceded by an odd number of backslashes. "
    (save-excursion
      (when pos (goto-char pos))
      (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defun ar-backslash-escaped-p (&optional pos)
  "Return t if backslash before point is escaped. "
  (save-excursion
    (when pos (goto-char pos))
    (< 0 (% (abs (skip-chars-backward "\\\\")) 1))))

(defun ar--skip-to-comment-or-semicolon ()
  "Returns position if point was moved."
  (let ((orig (point)))
    (cond ((while (and (< 0 (abs (skip-chars-forward "^#;" (line-end-position))))
                       ;; (sit-for 1)
                       (and (nth 8 (parse-partial-sexp (point-min) (point))) (skip-chars-forward "#;" (line-end-position)))))))
    (and (< orig (point))(point))))

(defmacro ar--preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
           (ar-escaped-p))))

(defun ar-nth1-syntax ()
  "Goto beginning of list at point."
  )

(defvar ar-strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING -
which is defined by `string-chars-preserve'.")

(defvar ar-strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING -
which is defined by `string-chars-preserve'.")

(defun ar-fix-comment-start ()
  "Comment at point might not have a padding."
  (if (and comment-start (string-match "[ \t]$" comment-start))
      (concat comment-start "*")
    comment-start))

(defun ar-string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages."
  (let* (end
	 (s-c-b (or chars-before
		    ar-strip-chars-before))
	 (s-c-a (or chars-after
		    ar-strip-chars-after))
	 (erg (with-temp-buffer
		(switch-to-buffer (current-buffer))
		(insert str)
		(skip-chars-backward s-c-a)
		(setq end (point))
		(goto-char (point-min))
		(skip-chars-forward s-c-b)
		(buffer-substring-no-properties (point) end))))
    erg))

(defun ar--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of expression after a semicolon.

Returns position reached if point was moved.
Optional argument LIMIT limit."
  (let ((orig (point)))
    (and (< 0 (abs
               (skip-chars-backward "^;" (or limit (line-beginning-position)))))
         (skip-chars-forward " \t" (line-end-position))
         (and (< (point) orig) (point)))))

(defmacro ar--current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line."
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (ar-escaped-p))))

;; Comment
(defun ar--skip-to-comment-or-comma ()
  "Return position if comment or semicolon found."
  (let ((orig (point)))
    (cond ;; ((and done (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
	  ;; 	(member (char-after) (list ?# ?\,)))
	  ;;  (when (eq ?\, (char-after))
	  ;;    (skip-chars-forward "," (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward "," (line-end-position))))
	  (t
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (< orig (point))))

(defun ar--skip-to-comma-backward (&optional limit)
  "Fetch the beginning of expression after a comma.

Returns position reached if point was moved.
Optional argument LIMIT limit."
  (let ((orig (point)))
    (and (< 0 (abs (skip-chars-backward "^," (or limit (line-beginning-position)))))
	 (skip-chars-forward " \t" (line-end-position))
	 (and (< (point) orig) (point)))))

(defconst ar-eq-assignment-re
  (concat
   "\\([^=\n:]+\\):?\\([^=\n:]*\\)\\(=\\)\\([^:]*\\)$")
  "Regular expression matching assigment.")

(defconst ar-vertical-line-re
  (concat
   ".*|")
  "Regular expression matching a guard.")

;;; string-strip stuff ends here
(defcustom empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :group 'convenience)

(defcustom ar-paired-openers (list ?‘ ?` ?< ?\( ?\[ ?{ ?\〈 ?\⦑ ?\⦓ ?\【 ?\⦗ ?\⸤ ?\「 ?\《 ?\⦕ ?\⸨ ?\⧚ ?\｛ ?\（ ?\［ ?\｟ ?\｢ ?\❰ ?\❮ ?\“ ?\‘ ?\❲ ?\⟨ ?\⟪ ?\⟮ ?\⟦ ?\⟬ ?\❴ ?\❪ ?\❨ ?\❬ ?\᚛ ?\〈 ?\⧼ ?\⟅ ?\⸦ ?\﹛ ?\﹙ ?\﹝ ?\⁅ ?\⦏ ?\⦍ ?\⦋ ?\₍ ?\⁽ ?\༼ ?\༺ ?\⸢ ?\〔 ?\『 ?\⦃ ?\〖 ?\⦅ ?\〚 ?\〘 ?\⧘ ?\⦉ ?\⦇)
  "Specify the paired delimiter opening char."
  :type '(repeat character)
  :group 'sytactic-close)

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'ar-empty-line-p))
(defun ar-empty-line-p (&optional iact)
  "Return t if cursor is at an empty line, nil otherwise.
Optional argument IACT saying interactively called."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when iact
      (message "%s" (looking-at empty-line-p-chars)))
    (looking-at empty-line-p-chars)))

(defun ar-previous-line-empty-or-BOB-p ()
  (save-excursion
    (beginning-of-line)
    (or
     (bobp)
     (when (forward-line -1)
       (ar-empty-line-p)))))

(defun ar-previous-line-or-comment-empty-BOB-p ()
  (save-excursion
    (beginning-of-line)
    (or
     (bobp)
     (when (forward-line -1)
       (or (ar-in-comment-p)
	   (ar-empty-line-p))))))

(defun guess-what--after-typedef-maybe (regexp)
  (save-excursion
    (beginning-of-line)
    (or
     (bobp)
     (progn
       (forward-line -1)
       (when
	   (looking-at regexp)
	 (insert (match-string 1)))))))

(defun ar-forward-comment (&optional pos char)
  "Go to end of (next) commented section following point.

Optional args position and `comment-start' character
Travel empty lines
Optional argument POS orig.
Optional argument CHAR comment start."
  (interactive)
  (let ((orig (or pos (point)))
	(char (or char (string-to-char comment-start)))
        (pps (parse-partial-sexp (point-min) (point)))
	last)
    (cond ((nth 8 pps)
           (goto-char (nth 8 pps))
           (forward-comment 99999)
           (unless (eq (point) orig)
             (point)))
          (t (search-forward comment-start nil t 1)))
    ))

(defun ar-in-comment-p (&optional start)
  "Return the beginning of current line's comment, if inside.
Optional argument START start."
  (interactive)
  (let* ((pps (parse-partial-sexp (or start (point-min)) (point)))
	 (erg (and (nth 4 pps) (nth 8 pps))))
    (unless (or erg (nth 3 pps))
      (when (or (eq (car (syntax-after (point))) 11)
		(ignore-errors (looking-at comment-start)))
	(setq erg (point))))
  erg))

(defun ar-backward-comment (&optional pos)
  "Got to beginning of a commented section.
Optional argument POS start."
  (interactive)
  (let ((erg pos)
	last)
    (when erg (goto-char erg))
    (while (and (not (bobp)) (setq erg (ar-in-comment-p)))
      (when (< erg (point))
	(goto-char erg)
	(setq last (point)))
      (skip-chars-backward " \t\r\n\f"))
    (when last (goto-char last))
    last))

;; String
(defun ar-in-string-p ()
  "Return start position, if inside or at opening delimiter.

Otherwise return nil."
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
	 (erg (and (nth 3 pps) (nth 8 pps)))
	 (la (unless (or erg (eobp))
	       (and (eq (char-syntax (char-after)) 34)
		    ;; look for closing char
		    (save-excursion
		      (forward-char 1)
		      (nth 3 (parse-partial-sexp (point-min) (point))))
		    (point)))))
    (or erg la)))

(defun ar-in-string-p-fast ()
  "Return delimiting character if inside, nil otherwise."
  (nth 3 (parse-partial-sexp (point-min) (point))))

(defun ar-empty-string-p (strg)
    (string= "" strg))

(defun ar-forward-string (&optional start)
  "Go to the end of string, if inside.
Optional argument START start."
  (let ((pos (or start (ar-in-string-p))))
    (goto-char pos)
    (forward-sexp)))

;; Navigate
(defun ar-skip-blanks-and-comments (&optional arg pps orig)
  "Go forward over empty lines and comments alike.

Stop at first non-empty char.
With negative arg go backward. "
  (interactive)
  (let ((arg (or arg 1))
	(pos (point))
	(orig (or orig (point)))
	(pps (or pps (parse-partial-sexp (point-min) (point)))))
    (if (< 0 arg)
        (progn
          (skip-chars-forward " \t\r\n")
          (when (or (and pps (nth 4 pps))(ar-in-comment-p))
	    (end-of-line)
	    (skip-chars-forward " \t\r\n\f"))
          (when (empty-line-p)
            (forward-line arg))
          (when (> (point) pos)
            (ar-skip-blanks-and-comments arg nil orig))
	  (< orig (point)))
      (skip-chars-backward " \t\r\n")
      (when (or (and pps (nth 4 pps))(ar-in-comment-p))
        (goto-char (or (and pps (nth 4 pps))(ar-comment-beginning-position-atpt))))
      (> orig (point)))))

(defun ar-backward-line ()
  "Go to indentation of current source-code line.

If already at beginning of line, go one line above.
Skip comments, empty lines and strings"
  (interactive)
  (unless (bobp)
    (let ((orig (point))
	  cmm)
      (back-to-indentation)
      (while (and (eolp) (not (bobp)))
	(forward-line -1))
      (unless (bobp)
	(and (not (setq cmm (ar-in-comment-p)))(eq (point) orig)
	     (forward-line -1))
	(cond (cmm
	       (ar-backward-comment)
	       (ar-backward-line))
	      ((nth 3 (parse-partial-sexp (point-min) (point)))
	       ;; beginning of string
	       (goto-char (nth 8 (parse-partial-sexp (point-min) (point)))))))
      (back-to-indentation)
      (when (or (eq (car (syntax-after (point))) 11)(eolp))(ar-backward-line)))))

(defvar ar-move-line-this-column nil)

(defun ar-move-line-keep-column-intern (arg)
  (unless (eq last-command this-command)
    (setq ar-move-line-this-column (current-column)))
  (forward-line arg)
  (while (and (not (eolp)) (< (current-column) ar-move-line-this-column))
    (forward-char 1)))

(defun ar-forward-line-keep-column (&optional arg)
  "Go to current column of next line.

If line is shorter, go to end of line"
  (interactive "p")
  (ar-move-line-keep-column-intern (or arg 1)))

(defun ar-backward-line-keep-column (&optional arg)
  "Go to current column of line above.

If line is shorter, go to end of line"
  (interactive "p")
  (ar-move-line-keep-column-intern (- (or arg 1))))

(defun ar-forward-line ()
  "Go to indentation of next source-code line.

Skip comments, empty lines and strings"
  (interactive)
  (unless (eobp)
    (let ((orig (point))
	  strg)
      (forward-line 1)
      (back-to-indentation)
      (while (and (eolp) (not (eobp)))
	(forward-line 1))
      (when (ar-in-comment-p)
	(ar-forward-comment))
      (when (setq strg (ar-in-string-p))
	(ar-forward-string strg))
      (back-to-indentation)
      (unless (or (eobp) (< orig (point)))
	(forward-line 1))
      (back-to-indentation))
    (when (ar-in-comment-p) (ar-forward-line))))

(defun ar-forward-line-eol ()
  "Down one line at EOL."
  (interactive)
  (forward-line 1)
  (end-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun ar-move-line-indent (&optional arg)
  "Move line forward or upward travelling indentation.

\\[universal-argument] toggles direction
Optional argument ARG toggles direction."
  (interactive "P")
  (when (eq 4 (prefix-numeric-value arg))
    (setq ar-line-move-forward (not ar-line-move-forward)))
  (if ar-line-move-forward
      (ar-forward-line)
    (ar-backward-line)))

(defun ar-match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal ARG \\[universal-argument] insert a `%'."
  (interactive "P")
  (if arg
      (self-insert-command (if (numberp arg) arg 1))
    (cond ((eq 4 (car (syntax-after (point))))
	   (forward-sexp)
	   (forward-char -1))
	  ((eq 5 (car (syntax-after (point))))
	   (forward-char 1)
	   (backward-sexp))
	  (t (self-insert-command 1)))))

(defun ar--skip-to-comment ()
  "Return position if comment."
  (let ((comment-start (ar-string-strip comment-start))
	erg)
    (when (and (< 0 (abs (skip-chars-forward (concat "^" comment-start) (line-end-position))))
		(looking-at comment-start)
		(setq erg (point)))
    erg)))

(defun ar-trim-string-left (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string
Argument STRG start."
  (setq arg (or arg 1))
  (substring strg arg))

(defun ar-trim-string-right (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string
Argument STRG end."
  (setq arg (or arg 1))
  (let ((laenge (length strg)))
    (substring strg 0 (- laenge arg))))

(defun ar-trim-string (strg &optional left right)
  "Remove ARG characters from beginning and end of STRING.

With no arguments remove just one character
Return the shortened string
Argument STRG strg.
Optional argument LEFT border.
Optional argument RIGHT border."
  (let ((left (or left 1))
	(right (or right 1))
	(laenge (length strg)))
    (setq right (- laenge right))
    (substring strg left right)))

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

(defun ar--backward-regexp (regexp &optional indent condition)
  "Search backward next regexp not in string or comment.

Return and move to match-beginning if successful"
  (while (and
	  (re-search-backward regexp nil 'move 1)
	  ;; (setq last (point))
	  (or (nth 8 (parse-partial-sexp (point-min) (point)))
	      (and indent
		   (not (funcall condition (current-indentation) indent))))))
  (unless (bobp)
    (back-to-indentation)
    (point)))

(defun ar-check-parens ()
  "Like `check-parens' but avoid error.

Just return t if parentheses in the current buffer are balanced.
Return nil if not."
  (interactive)
  (scan-sexps (point-min) (point-max)))

(defun ar-backward-defun-DWIM (&optional outmost pps)
  "A fault-tolerant backward-function command.

In case of invalid source-code at point, try some heuristics"
  (interactive)
  (if (ar-check-parens)
      (ar-backward-defun outmost pps)
    (ar--backward-regexp ar-beginning-of-defun-re)))

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

(defun ar-count-lines (&optional beg end)
  "Count lines in accessible part of buffer.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115
Optional argument BEG start.
Optional argument END end."
  (interactive)
  (let ((beg (or beg (point-min)))
	(end (or end (point)))
	erg)
    (if (bolp)
	(setq erg (1+ (count-lines beg end)))
      (setq erg (count-lines beg end)))
    (when (called-interactively-p 'any) (message "%s" erg))
    erg))

(defun ar-list-indents ()
  "Return a list of indentations up to start of toplevel."
  (save-excursion
    (let ((beg (save-excursion (ar-backward-toplevel)))
	  (ilist nil))
      (save-restriction
	(narrow-to-region beg (point))
	(while (ar-backward-statement)
	  (unless (member (current-column) ilist)
	    (cons (current-column) ilist)))))))

(defun ar-reverse-char (&optional char)
  "Reverse reciproke chars as \"[\" to \"]\".

Change doublequotes into singlequotes et vice versa
Likewise switch uppercase/downcase

Edit at point if challed without optional CHAR
otherwise return complement char"
  (interactive "*")
  (let* ((ret char)
	 (cf (or ret (char-after)))
	 (erg (cond
	       ((and (< 96 cf)(< cf 123))
	       	(upcase cf))
	       ((and (< 64 cf)(< cf 91))
	       	(downcase cf))
	       (t (ar--return-complement-char-maybe (char-after))))))
    (when erg
      (if ret
	  erg
	(delete-char 1)
	(insert (char-to-string erg))))))

(defun ar--return-complement-char-maybe (char)
  "Reverse reciproke CHARs as \"[\" to \"]\"."
  (pcase char
    ;; ‘M-x append-to-register <RET> R’ can use ‘C-x r +’
    (?+ ?-)
    (?- ?+)
    (?» ?«)
    ;; (92 47)
    ;; (47 92)
    ;; (?' ?\")
    ;; (?\" ?')
    (?‘ ?’)
    ;; backticked is needed
    (?` ?`)
    ;; (?´ ?`)
    (?< ?>)
    (?> ?<)
    (?\( ?\))
    (?\) ?\()
    (?\] ?\[)
    (?\[ ?\])
    (?} ?{)
    (?{ ?})
    (?\〈 ?\〉)
    (?\⦑ ?\⦒)
    (?\⦓ ?\⦔)
    (?\【 ?\】)
    (?\⦗ ?\⦘)
    (?\⸤ ?\⸥)
    (?\「 ?\」)
    (?\《 ?\》)
    (?\⦕ ?\⦖)
    (?\⸨ ?\⸩)
    (?\⧚ ?\⧛)
    (?\｛ ?\｝)
    (?\（ ?\）)
    (?\［ ?\］)
    (?\｟ ?\｠)
    (?\｢ ?\｣)
    (?\❰ ?\❱)
    (?\❮ ?\❯)
    (?\“ ?\”)
    (?\❲ ?\❳)
    (?\⟨ ?\⟩)
    (?\⟪ ?\⟫)
    (?\⟮ ?\⟯)
    (?\⟦ ?\⟧)
    (?\⟬ ?\⟭)
    (?\❴ ?\❵)
    (?\❪ ?\❫)
    (?\❨ ?\❩)
    (?\❬ ?\❭)
    (?\᚛ ?\᚜)
    (?\〈 ?\〉)
    (?\⧼ ?\⧽)
    (?\⟅ ?\⟆)
    (?\⸦ ?\⸧)
    (?\﹛ ?\﹜)
    (?\﹙ ?\﹚)
    (?\﹝ ?\﹞)
    (?\⁅ ?\⁆)
    (?\⦏ ?\⦎)
    (?\⦍ ?\⦐)
    (?\⦋ ?\⦌)
    (?\₍ ?\₎)
    (?\⁽ ?\⁾)
    (?\༼ ?\༽)
    (?\༺ ?\༻)
    (?\⸢ ?\⸣)
    (?\〔 ?\〕)
    (?\『 ?\』)
    (?\⦃ ?\⦄)
    (?\〖 ?\〗)
    (?\⦅ ?\⦆)
    (?\〚 ?\〛)
    (?\〘 ?\〙)
    (?\⧘ ?\⧙)
    (?\⦉ ?\⦊)
    (?\⦇ ?\⦈)
    (?\〉 ?\〈)
    (?\⦒ ?\⦑)
    (?\⦔ ?\⦓)
    (?\】 ?\【)
    (?\⦘ ?\⦗)
    (?\⸥ ?\⸤)
    (?\」 ?\「)
    (?\》 ?\《)
    (?\⦖ ?\⦕)
    (?\⸩ ?\⸨)
    (?\⧛ ?\⧚)
    (?\｝ ?\｛)
    (?\） ?\（)
    (?\］ ?\［)
    (?\｠ ?\｟)
    (?\｣ ?\｢)
    (?\❱ ?\❰)
    (?\❯ ?\❮)
    (?\” ?\“)
    (?\’ ?\‘)
    (?\❳ ?\❲)
    (?\⟩ ?\⟨)
    (?\⟫ ?\⟪)
    (?\⟯ ?\⟮)
    (?\⟧ ?\⟦)
    (?\⟭ ?\⟬)
    (?\❵ ?\❴)
    (?\❫ ?\❪)
    (?\❩ ?\❨)
    (?\❭ ?\❬)
    (?\᚜ ?\᚛)
    (?\〉 ?\〈)
    (?\⧽ ?\⧼)
    (?\⟆ ?\⟅)
    (?\⸧ ?\⸦)
    (?\﹜ ?\﹛)
    (?\﹚ ?\﹙)
    (?\﹞ ?\﹝)
    (?\⁆ ?\⁅)
    (?\⦎ ?\⦏)
    (?\⦐ ?\⦍)
    (?\⦌ ?\⦋)
    (?\₎ ?\₍)
    (?\⁾ ?\⁽)
    (?\༽ ?\༼)
    (?\༻ ?\༺)
    (?\⸣ ?\⸢)
    (?\〕 ?\〔)
    (?\』 ?\『)
    (?\⦄ ?\⦃)
    (?\〗 ?\〖)
    (?\⦆ ?\⦅)
    (?\〛 ?\〚)
    (?\〙 ?\〘)
    (?\⧙ ?\⧘)
    (?\⦊ ?\⦉)
    (?\⦈ ?\⦇)
    (_ char)))

(defvar ar-closing-chars (list ?’  ?´  ?\]  ?}  ?\〉  ?\⦒  ?\⦔  ?\】  ?\⦘  ?\⸥  ?\」  ?\》  ?\⦖  ?\⸩  ?\⧛  ?\｝  ?\）  ?\］  ?\｠  ?\｣  ?\❱  ?\❯  ?\”  ?\❳  ?\⟩  ?\⟫  ?\⟯  ?\⟧  ?\⟭  ?\❵  ?\❫  ?\❩  ?\❭  ?\᚜  ?\〉  ?\⧽  ?\⟆  ?\⸧  ?\﹜  ?\﹚  ?\﹞  ?\⁆  ?\⦎  ?\⦐  ?\⦌  ?\₎  ?\⁾  ?\༽  ?\༻  ?\⸣  ?\〕  ?\』  ?\⦄  ?\〗  ?\⦆  ?\〛  ?\〙  ?\⧙  ?\⦊  ?\⦈  )
  "List of closing delimiters.")

(defun ar-align-with-previous-line-maybe ()
  (when (and (eq this-command 'self-insert-command)
	     (eq (char-before) ?=))
    (ar-align-with-previous-line (current-column))))

(defun ar--do-align-intern (col)
  (while (< (current-column) col)
    (insert 32)))

(defun ar--align-with-previous-line (uppercol downcol orig upperpos)
  (if (< uppercol downcol)
      (let ((col downcol))
	(goto-char upperpos)
	(ar--do-align-intern col)
	;; upper-line extended, maybe line above needs that too?
	(ar-align-with-previous-line))
    (let ((col uppercol))
      (goto-char orig)
      (forward-char -1)
      (ar--do-align-intern col))))

(defun ar-align-with-previous-line (&optional regexp)
  "Align with previous line.

Defaults aligning to equal and vertical bar sign
Optional argument REGEXP regexp."
  (interactive "*")
  (save-excursion
    (let* ((orig (copy-marker (point)))
	   (regexp (or regexp (concat ar-eq-assignment-re "\\|" ar-vertical-line-re)))
	   (downcol (progn (beginning-of-line)
			   (when (looking-at regexp)
			     (ignore-errors (goto-char (match-beginning 3)))
			     (current-column))))
	   upperpos
	   (uppercol (progn
		       (beginning-of-line)
		       (unless (bobp)
			 (forward-line -1)
			 (unless (ar-empty-line-p)
			   (beginning-of-line)
			   (when (looking-at regexp)
			     (save-excursion
			       (goto-char (match-beginning 3))
			       (setq upperpos (point))
			       (current-column))))))))
      (when (and downcol uppercol)
	(ar--align-with-previous-line uppercol downcol orig upperpos)))))

(defun ar-align (beg end &optional regexp)
  (interactive "r*")
  (save-excursion
    (goto-char beg)
    (ar-align-with-previous-line regexp)
    (while (not
	    (or
	     (eobp)
	     (progn
	       (forward-line 1)
	       (ar-align-with-previous-line)
	       (<= end (line-end-position))))))))

(defun ar--fetch-previous-indent (orig)
  "Report the preceding indent.
Argument ORIG start."
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(defun ar-backward-toplevel (&optional arg)
  "Go to beginning of a toplevel form.

Returns position if successful, nil otherwise
Optional argument ARG times"
  (interactive "p")
  (unless (bobp)
    ;; (forward-line -1)
    ;; (beginning-of-line)
    (let* ((arg (or arg 1))
	   (orig (point))
	   (pps (parse-partial-sexp (point-min) (point)))
	   ;; set ppss start point
	   (limit (or (nth 8 pps) (point-min)))
	   (comment-start (ar-fix-comment-start))
	   erg this)
      ;; (unless (bobp)
      (while (and
	      (prog1 (re-search-backward "^[^ \t\n\f\r]" nil 'move arg)
		(beginning-of-line))
	      (or (ignore-errors (looking-at comment-start))(ignore-errors (looking-at comment-start-skip))
		  (and (setq this (save-excursion (ignore-errors (nth 8 (parse-partial-sexp limit (point))))))
		       (setq limit this))
		  (member (char-after) toplevel-nostart-chars)))
	(forward-line -1)
	(beginning-of-line))
      (when (< (point) orig)
	(setq erg (point))
	(when ar-verbose-p (message "%s" erg)))
      erg)))

(defun ar--forward-toplevel-intern (orig pps)
  (let (last)
    (unless (ar--beginning-of-expression-p orig pps)
      (ar-backward-expression))
    (unless (eq 0 (current-column))
      (ar-backward-toplevel))
    (unless (< orig (point))
      (while (and
	      (not (eobp))
	      (save-excursion
		(ar-forward-expression orig nil nil pps)
		(setq last (point)))
	      (ar-down-expression)(< 0 (current-indentation)))))
    ;; (if (looking-at (ar-rx builtin-declaration))
    ;; (ar-forward-toplevel)
    (and last (goto-char last))
    ;; (ar-forward-expression)
    ;;)
))

(defvar toplevel-nostart-chars (list ?-))

(defun ar-forward-toplevel (&optional arg)
  "Go to end of a toplevel form.

Returns position if successful, nil otherwise
Optional argument ARG times."
  (interactive "p")
  (unless (eobp)
    (let* ((arg (or arg 1))
	   (orig (point))
	   (pps (parse-partial-sexp (point-min) (point)))
	   ;; set ppss start point
	   (limit (or (nth 8 pps) (point-min)))
	   (comment-start (ar-fix-comment-start))
	   erg this)
      (ar-skip-blanks-and-comments)
      (while
	  (and
	   (progn (end-of-line)
		  (setq erg (re-search-forward "^[^ \t\n\f\r]" nil 'move arg)))
	   (or
	    (progn
	      (beginning-of-line)
	      (nth 8 (parse-partial-sexp (point-min) (point))))
	    (ignore-errors (when
			       (looking-at comment-start)
			     (forward-line 1)
			     t))
	    (ignore-errors (when (looking-at comment-start-skip)
			     (forward-line 1)
			     t))
	    (and (setq this (ignore-errors (nth 8 (parse-partial-sexp limit (point)))))
		 (setq limit this)))))
      (when erg
	(beginning-of-line)
	(skip-chars-backward " \t\r\n\f")
	(forward-line 1) (beginning-of-line))
      ;; (unless (eobp) (forward-line 1) (beginning-of-line))

      ;; (if (and (ar--forward-toplevel-intern orig pps)
      ;; 	       (< orig (point)))
      ;; 	  (setq erg (point))
      ;; 	(ar-down-expression)
      ;; 	(ar-forward-toplevel)))
      (when (< orig (point))
	(setq erg (point))
	(when ar-verbose-p (message "%s" erg)))
      erg)))

(defun ar-forward-toplevel-bol ()
  "Go to beginning of line after end of a toplevel form.

Returns position if successful, nil otherwise"
  (interactive)
  (let ((orig (point))
        erg)
    (unless (eobp)
      (when (ar--forward-toplevel-intern orig (parse-partial-sexp (point-min) (point)))
	(if (eobp)
	    (newline 1)
	  (forward-line 1)
	  (beginning-of-line)))
      (when (< orig (point))
	(setq erg (point))))
    (when ar-verbose-p (message "%s" erg))
    erg))

(defun ar-reverse-at-point (&optional beg end)
  "Replace a string or region at point by result of `reverse'.

Works at any string detected at position, unless
optional BEG as start and
optional END as end are given as arguments or
an active region is set deliberately"
  (interactive "*")
  (let* ((pps (parse-partial-sexp (point-min) (point)))
	 ;; (save-excursion (cadr (ar-beginning-of-string-atpt)))
	 (beg (cond (beg)
		    ((use-region-p)
		     (region-beginning))
		    ((and (nth 8 pps)(nth 3 pps))
		     (goto-char (nth 8 pps))
		     (point))))
	 ;; (copy-marker (cdr (ar-end-of-string-atpt)))
	 (end (cond (end)
		    ((use-region-p)
		     (copy-marker (region-end)))
		    ((and (nth 8 pps)(nth 3 pps))
		     (forward-sexp)
		     (copy-marker (point)))))
	 (erg (buffer-substring beg end)))
    (when (and beg end)
      (delete-region beg end)
      (insert (reverse erg)))))

(defun ar-replace--in-list (pred elem replacement list)
  "Expects a LIST whose type must fit to arg PRED.

PRED: a function to select elem
ELEM: element to replace by arg REPLACEMENT"
  (let (newlist)
    (dolist (ele list)
      (push (if (funcall pred ele elem)
		replacement ele)
	    newlist))
    (nreverse newlist)))

(defun ar-default-directory-into-load-path ()
  "Push the current directory into Emacs load-path

unless not already there"
  (interactive)
  (unless (member (substring default-directory 0 -1) load-path)
    (push (substring default-directory 0 -1) load-path)))

(defun ar-edit-in-comment ()
  "Edit commented region as in major-mode."
  (let* ((orig (point))
	 (bounds (ar-bounds-of-comment-atpt))
	 (beg (caar bounds))
	 (end (cdar (cdr bounds)))
	 (strg (buffer-substring beg end))
	 (mode major-mode)
	 ;; position from beginning of region
	 (relpos (progn (goto-char orig) (- (point) beg))))
    (delete-region beg end)
    (insert (with-temp-buffer
	      (switch-to-buffer (current-buffer))
	      (insert strg)
	      (funcall mode)
	      (goto-char (point-min))
	      (forward-char relpos)
	      (save-excursion
		(uncomment-region (point-min) (point-max)))
	      (ar-raise-symbolic-expression)
	      (comment-region (point-min) (point-max))
	      (buffer-string)))))

;; Basics lifted from paredit.el
(defun ar-raise-symbolic-expression ()
  "Raise the following S-expression in a tree, deleting its siblings. "  (interactive "*")
  (save-excursion
    (cond ((ar-in-string-p)
           (goto-char (ar-in-string-p))
	   (backward-prefix-chars)
	   (ar-raise-symbolic-expression))
          ((ar-in-comment-p)
	   (ar-edit-in-comment))
	  (t (let* ((bound (scan-sexps (point) 1))
		    (expr
      		     (buffer-substring (save-excursion (forward-sexp) (backward-sexp) (point)) bound)))
	       ;; Move up to the list we're raising those S-expressions out of and
	       ;; delete it.
	       (backward-up-list)
	       (delete-region (point) (scan-sexps (point) 1))
	       (insert expr))))))

(defun ar-align-equal-sign ()
  (interactive "*")
  (when (eq this-command 'self-insert-command)
    (save-excursion
      (let ((end (copy-marker (line-end-position)))
            (secondcolumn (and (looking-back "[^=]+\\(=\\) *" (line-beginning-position))
                               ;; (member (char-after) (list 32 ?\n ?\t ?\f))
                               ;;  haskell
                               (not (looking-back "main +\\(=+\\) *" (line-beginning-position)))
                               (goto-char (match-beginning 1))
                               (current-column)))
            (maxcolumn 0)
            orig)
        ;; an equal-sign at current line
        (when secondcolumn
          ;; (put 'secondcolumn 'pos (point))
          (setq maxcolumn secondcolumn)
          (setq orig (point))
          (while
              (and (progn (beginning-of-line)
                          (not (bobp)))
                   (progn (forward-line -1)
                          (not (looking-at comment-start)))
                   (looking-at ".+ +\\(=\\)[[:blank:]]+.*"))
            (when (match-beginning 1)(goto-char (match-beginning 1)))
            (when (< maxcolumn (current-column))
              (setq maxcolumn (current-column))))
          ;; before going downward, reach the last match
          (when (match-beginning 1)(goto-char (match-beginning 1)))
          (while (< (line-end-position) end)
            (when (looking-at ".+ +\\(=\\)[[:blank:]]+.*")
              (goto-char (match-beginning 1)))
            (when (< (current-column) maxcolumn)
              (insert (make-string (- maxcolumn (current-column)) 32)))
            (forward-line 1)
            )
          (goto-char orig)
          (when (looking-at " *\\(=\\)[[:blank:]]*")
            (when (< (current-column) maxcolumn)
              (insert (make-string (- maxcolumn (current-column)) 32))))
          )))))

(defun ar-align-in-current-buffer ()
  (interactive)
  (add-hook 'post-command-hook #'ar-align-equal-sign nil t))

(defun ar-in-string-or-comment-p ()
  "If inside or at beginning of a string or comment."
  (interactive)
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (ignore-errors (eq 7 (car-safe (syntax-after (point)))))
      (ignore-errors (looking-at comment-start))
      (ignore-errors (looking-at comment-start-skip))))

(defun ar-in-string-comment-or-escaped (&optional pps permit-comment permit-string)
  "Check for matches inside a string, a comment or at escaped char.

Optional PERMIT-COMMENT: match inside a comment.
Optional PERMIT-STRING: match inside a string."
  (let ((pps (or pps (parse-partial-sexp (point-min) (point)))))
    (or (and (nth 4 pps) (not permit-comment))
        (and (nth 3 pps) (not permit-string))
        (ar-escaped-p))))


(defun ar-narrow-to-comment-atpt ()
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (when (nth 4 pps)
      (ar-backward-comment)
      (set-mark (point))
      (ar-forward-comment)
      (narrow-to-region (mark) (point)))))

(defun ar-narrow-to-string-atpt ()
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (set-mark (goto-char (nth 8 pps)))
    (forward-sexp)
    (narrow-to-region (mark) (point))))


(provide 'ar-subr)
;;; ar-subr.el ends here
