;;; ar-subr.el --- A reliable beginning-of-defun and other helper functions  -*- lexical-binding: t; -*-

;; Author: Andreas Röhler <andreas.roehler@online.de>
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

(defvar ar-verbose-p nil)

(defvar ar-line-move-forward t)

(defvar ar-max-specpdl-size max-specpdl-size
  "Protect against eternal loop")

(defvar ar-literal-delim-re "\""
  "When looking at beginning of string. ")

(defmacro ar--escaped ()
  "Return t if char is preceded by an odd number of backslashes. "
  `(save-excursion
     (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

(defmacro ar--preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line. "
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (ar-escaped))))

(defvar ar-strip-chars-before  "\\`[ \t\r\n]*"
  "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'.")

(defvar ar-strip-chars-after  "[ \t\r\n]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'.")

(defun ar-fix-comment-start ()
  "Comment at point might not have a padding. "
  (if (and comment-start (string-match "[ \t]$" comment-start))
      (concat comment-start "*")
    comment-start))

(defun ar-string-strip (str &optional chars-before chars-after)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages. "
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

Returns position reached if point was moved. "
  (let ((orig (point)))
    (and (< 0 (abs
               (skip-chars-backward "^;" (or limit (line-beginning-position)))))
         (skip-chars-forward " \t" (line-end-position))
         (and (< (point) orig) (point)))))

(defmacro ar--current-line-backslashed-p ()
  "Return t if current line is a backslashed continuation line. "
  `(save-excursion
     (end-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (ar-escaped))))

;; Comment
(defun ar--skip-to-comment-or-comma ()
  "Returns position if comment or semicolon found. "
  (let ((orig (point))
	done)
    (cond ((and done (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward "," (line-end-position))))
	  ((and (< 0 (abs (skip-chars-forward "^#," (line-end-position))))
		(member (char-after) (list ?# ?\,)))
	   (when (eq ?\, (char-after))
	     (skip-chars-forward "," (line-end-position))))
	  ((not done)
	   (end-of-line)))
    (skip-chars-backward " \t" (line-beginning-position))
    (and (< orig (point))(setq done t)
	 done)))

(defun ar--skip-to-comma-backward (&optional limit)
  "Fetch the beginning of expression after a comma.

Returns position reached if point was moved. "
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

(unless (functionp 'empty-line-p)
  (defalias 'empty-line-p 'ar-empty-line-p))
(defun ar-empty-line-p (&optional iact)
  "Returns t if cursor is at an empty line, nil otherwise."
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

Optional args position and comment-start character
Travel empty lines "
  (interactive)
  (let ((orig (or pos (point)))
	(char (or char (string-to-char comment-start)))
	last)
    (while (and (not (eobp))
		(forward-comment 99999)))
    (when (eq (point) orig)
      ;; forward-comment fails sometimes
      (while
	  (and (not (eobp)) (or (ar-in-comment-p)(eq (point) orig)))
	(setq last (line-end-position))
	(forward-line 1)
	(end-of-line)
	;; (setq orig (point))
))
    (and (eq orig (point)) (prog1 (forward-line 1) (back-to-indentation))
	 (while (member (char-after) (list char 10))(forward-line 1)(back-to-indentation)))
    ;; go
    (when last
      (goto-char last)
      (skip-chars-forward " \t\r\n\f")
      (back-to-indentation))))

(defun ar-in-comment-p (&optional start)
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let* ((pps (parse-partial-sexp (or start (point-min)) (point)))
	 (erg (and (nth 4 pps) (nth 8 pps))))
    (unless (or erg (nth 3 pps))
      (when (or (eq (car (syntax-after (point))) 11)
		(ignore-errors (looking-at comment-start)))
	(setq erg (point))))
  erg))

(defun ar-backward-comment (&optional pos)
  "Got to beginning of a commented section. "
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

Otherwise return nil. "
  (interactive)
  (let* ((pps (parse-partial-sexp (point-min) (point)))
	 (erg (and (nth 3 pps) (nth 8 pps)))
	 (la (unless (or erg (eobp)) (when (eq (char-syntax (char-after)) 34)
			   (point)))))
    (setq erg (or erg la))
    (when (interactive-p) (message "%s" erg))
    erg))

(defun ar-in-string-p-fast ()
  "Returns delimiting character if inside, nil otherwise. "
  (nth 3 (parse-partial-sexp (point-min) (point))))

(defun ar-empty-string-p (strg)
    (string= "" strg))

(defun ar-forward-string (&optional start)
  "Go to the end of string, if inside. "
  (let ((pos (or start (ar-in-string-p))))
    (goto-char pos)
    (forward-sexp)))

;; Navigate

(defun ar-forward-sexp ()
  "Like forward-sexp, diffs below.

From inside string go to end-of-string.
From inside comment, go to end-of-comment.

At the end of sexp-same-level, go up if possible.
Otherwise return nil. "
  (interactive)
  (let ((orig (point))
	(pps (parse-partial-sexp (point-min) (point)))
	erg)
    (cond ((nth 3 pps)
	   (goto-char (nth 8 pps))
	   (forward-sexp))
	  ((or (nth 4 pps)(eq (car (syntax-after (point))) 11))
	   (ar-skip-blanks-and-comments-lor nil pps))
	  (t (or (progn (ignore-errors (forward-sexp))
			(< orig (point)))
		 (ignore-errors (up-list)))))
    (when (< orig (point)) (setq erg (point)))
    ;; (when (interactive-p) (message "%s" erg))
    erg))

;; (defun ar-forward-literal ()
;;   "Go to end of string at point if any, if successful return position. "
;;   (interactive)
;;   (let ((orig (point))
;; 	(start (ar-in-literal-p)))
;;     (when start
;;       (goto-char start)
;;       (forward-sexp)
;;       (and (< orig (point)) (setq erg (point))))
;;     (when (and ar-verbose-p (interactive-p)) (message "%s" erg))
;;     erg))

(defun ar-backward-line ()
  "Go to indentation of current source-code line.

If already at beginning of line, go one line above.
Skip comments, empty lines and strings "
  (interactive)
  (unless (bobp)
    (let ((orig (point))
	  cmm pps)
      (back-to-indentation)
      (while (and (eolp) (not (bobp)))
	(forward-line -1))
      (unless (bobp)
	(and (not (setq cmm (ar-in-comment-p)))(eq (point) orig)
	     (forward-line -1))
	(cond (cmm
	       (ar-backward-comment)
	       (ar-backward-line))
	      ((nth 3 pps)
	       ;; beginning of string
	       (goto-char (nth 8 pps)))))
      (back-to-indentation)
      (when (or (eq (car (syntax-after (point))) 11)(eolp))(ar-backward-line)))))

(defun ar-forward-line ()
  "Go to indentation of next source-code line.

Skip comments, empty lines and strings "
  (interactive)
  (unless (eobp)
    (let ((orig (point))
	  cmm strg)
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
  "Down one line at EOL. "
  (interactive)
  (forward-line 1)
  (end-of-line)
  (skip-chars-backward " \t\r\n\f"))

(defun ar-move-line-indent (&optional arg)
  "Move line forward or upward travelling indentation.

\\[universal-argument] toggles direction"
  (interactive "P")
  (when (eq 4 (prefix-numeric-value arg))
    (setq ar-line-move-forward (not ar-line-move-forward)))
  (if ar-line-move-forward
      (ar-forward-line)
    (ar-backward-line)))

(defun ar-match-paren (&optional arg)
  "Go to the matching brace, bracket or parenthesis if on its counterpart.

Otherwise insert the character, the key is assigned to, here `%'.
With universal arg \C-u insert a `%'. "
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
  "Returns position if comment. "
  (let ((comment-start (ar-string-strip comment-start))
	erg)
    (when (and (< 0 (abs (skip-chars-forward (concat "^" comment-start) (line-end-position))))
		(looking-at comment-start)
		(setq erg (point)))
    erg)))

(defun ar-trim-string-left (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string"
  (setq arg (or arg 1))
  (substring strg arg))

(defun ar-trim-string-right (strg &optional arg)
  "Remove ARG characters from beginning and end of STRING.

Return the shortened string"
  (setq arg (or arg 1))
  (let ((laenge (length strg)))
    (substring strg 0 (- laenge arg))))

(defun ar-trim-string (strg &optional left right)
  "Remove ARG characters from beginning and end of STRING.

With no arguments remove just one character
Return the shortened string"
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
  "Regular expression matching beginning of defun. ")

(defalias 'ar-beginning-of-defun 'ar-backward-defun)
(defun ar-backward-defun (&optional outmost pps)
  "Move to the beginning of a function definition.

With OUTMOST don't stop at a nested inner function.
When `beginning-of-defun-function' is set, call with optional ARG

If no function found inside a list, go to list-start.
Otherwise reach next list upward in buffer "
  (interactive "P")
  (let* ((outmost (or outmost (eq 4 (prefix-numeric-value outmost))))
	 (pps (or pps (parse-partial-sexp (point-min) (point))))
	 (liststart (nth 1 pps)))
    (unless (bobp)
      (cond (liststart
	     (goto-char liststart)
	     (while (and (not (looking-at ar-beginning-of-defun-re))(setq liststart (nth 1 (parse-partial-sexp (point-min) (point)))))
	       (goto-char liststart))
	     (and outmost (nth 1 (setq pps (parse-partial-sexp (point-min) (point)))) (ar-backward-defun outmost pps)))
	    ((nth 4 pps) (ar-backward-comment)
	     (ar-backward-defun outmost))
	    (t (when (or (eq ?\)(char-before)) (< 0 (abs (skip-chars-backward "^)"))))
		 (forward-char -1)
		 (ar-beginning-of-defun outmost)))))
    liststart))

(defalias 'ar-end-of-defun 'ar-forward-defun)
(defun ar-forward-defun ()
  "Move to the end of a function definition.

Return position if successful, nil otherwise
When `end-of-defun-function' is set, call it with optional ARG "
  (interactive)
  (unless (eobp)
    (skip-chars-forward " \t\r\n\f")
    (let* ((pps (parse-partial-sexp (point-min) (point)))
	   (nesting (nth 0 pps))
	   (in-comment (or (nth 4 pps) (looking-at comment-start)))
	   (orig (point))
	   erg)
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
	(setq erg (forward-sexp))
	(setq erg (point)))
       ((< 0 nesting)
	(ar-beginning-of-defun)
	(setq erg (ar-end-of-defun)))
       ((eq (char-after) ?\()
	(forward-sexp)
	(setq erg (point))))
      (when (< orig (point))
	erg))))

(defun ar-count-lines (&optional beg end)
  "Count lines in accessible part of buffer.

See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=7115"
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
  "Return a list of indentations up to start of top-level. "
  (save-excursion
    (let ((beg (save-excursion (ar-backward-top-level)))
	  ilist)
      (save-restriction
	(narrow-to-region beg (point))
	(while (ar-backward-statement)
	  (unless (member (current-column) ilist)
	    (cons (current-column) ilist)))))))

(defun reverse-char (&optional char)
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
  "Reverse reciproke chars as \"[\" to \"]\"."
         (pcase char
	       (?' ?")
	       (?" ?')
	       (?‘ ?’)
	       (?` ?')
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
	       (?\‘ ?\’)
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
	       (?‘ ?’)
	       (?' ?`)
	       (?> ?<)
	       (?< ?>)
	       (?\) ?\()
	       (?\( ?\))
	       (?\[ ?\])
	       (?\] ?\[)
	       (?{ ?})
	       (?} ?{)
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

(defun ar-align-with-previous-line-maybe ()
  (when (and (eq this-command self-insert-command)
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

Defaults aligning to equal and vertical bar sign"
  (interactive "*")
  (save-excursion
    (let* ((orig (copy-marker (point)))
	   (end (line-end-position))
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
  (let (done)
    (save-excursion
      (goto-char beg)
      (ar-align-with-previous-line regexp)
      (while (not (or done (eobp)))
	(forward-line 1)
	(ar-align-with-previous-line)
	(when (<= end (line-end-position))
	  (setq done t))))))

(defun ar--fetch-previous-indent (orig)
  "Report the preceding indent. "
  (save-excursion
    (goto-char orig)
    (forward-line -1)
    (end-of-line)
    (skip-chars-backward " \t\r\n\f")
    (current-indentation)))

(provide 'ar-subr)
;;; ar-subr.el ends here
