;;; ar-subr.el --- filling -*- lexical-binding: t; -*-

(defcustom ar-empty-line-p-chars "^[ \t\r]*$"
  "Empty-line-p-chars."
  :type 'regexp
  :tag "ar-empty-line-p-chars"
  :group 'ar-mode)

(defun ar-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (save-match-data (looking-at ar-empty-line-p-chars))))

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


(defun ar--beginning-of-buffer-p ()
  "Returns position, if cursor is at the beginning of buffer.
Return nil otherwise. "
  (when (bobp)(point)))

(defun ar-toggle-ar-debug-p ()
  "Toggle value of ‘ar-debug-p’."
  (interactive)
  (setq ar-debug-p (not ar-debug-p))
  (when (called-interactively-p 'interactive) (message "ar-debug-p: %s" ar-debug-p)))

(defmacro ar-preceding-line-backslashed-p ()
  "Return t if preceding line is a backslashed continuation line."
  `(save-excursion
     (beginning-of-line)
     (skip-chars-backward " \t\r\n\f")
     (and (eq (char-before (point)) ?\\ )
          (ar-escaped-p))))

(defun ar-in-comment-p ()
  "Return the beginning of current line's comment, if inside. "
  (interactive)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    (and (nth 4 pps) (nth 8 pps))))

(defun ar-in-string-or-comment-p ()
  "Returns beginning position if inside a string or comment, nil otherwise. "
  (or (nth 8 (parse-partial-sexp (point-min) (point)))
      (when (or (looking-at "\"") (looking-at "[ \t]*#[ \t]*"))
        (point))))

(defun ar--skip-to-semicolon-backward (&optional limit)
  "Fetch the beginning of statement after a semicolon at the same line.

Returns ‘t’ if point was moved"
  (let ((orig (point)))
    (skip-chars-backward "^;" (or limit (line-beginning-position)))
    (skip-chars-forward " \t" (line-end-position))
    (< (point) orig)))

(defun ar-forward-comment ()
  "Go to the end of commented section at point."
  (interactive)
  (let (last)
    (while
        (and (not (eobp))
             (or
              (and comment-start (looking-at comment-start))
              (and comment-start-skip (looking-at comment-start-skip))
              (nth 4 (parse-partial-sexp (point-min) (point)))))
      (setq last (line-end-position))
      (forward-line 1)
      (skip-chars-forward " \t\r\n\f")
      (unless (or (eobp) (eq (point) last))
        (back-to-indentation)))
    (when last (goto-char last))))

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

(defun ar--in-comment-p ()
  "Return the beginning of current line's comment, if inside or at comment-start. "
  (save-restriction
    (widen)
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 4 pps) (nth 8 pps))))
      (unless erg
        (when (ignore-errors (looking-at (concat "[ \t]*" comment-start)))
          (setq erg (point))))
      erg)))

(defun ar-list-beginning-position (&optional start)
  "Return lists beginning position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (nth 1 (parse-partial-sexp (or start (point-min)) (point))))

(defun ar-end-of-list-position (&optional arg)
  "Return end position, nil if not inside.

Optional ARG indicates a start-position for ‘parse-partial-sexp’."
  (interactive)
  (let* ((ppstart (or arg (point-min)))
         (erg (parse-partial-sexp ppstart (point)))
         (beg (nth 1 erg))
         end)
    (when beg
      (save-excursion
        (goto-char beg)
        (forward-list 1)
        (setq end (point))))
    (when (and ar-verbose-p (called-interactively-p 'any)) (message "%s" end))
    end))


(defun ar-backward-comment ()
  "Got to beginning of a commented section.

Start from POS if specified"
  (interactive)
  (let ((last (point))
        (orig (point)))
    (while (and (not (bobp))
                (ignore-errors (< (ignore-errors (goto-char (ar-in-comment-p))) last)))
      (setq last (point))
      (skip-chars-backward " \t\r\n\f"))
    (and (< (point) orig) (< (point)  last) (goto-char last))))

(defun ar-go-to-beginning-of-comment ()
  "Go to the beginning of current line's comment, if any.

From a programm use macro ‘ar-backward-comment’ instead"
  (interactive)
  (let ((erg (ar-backward-comment)))
    (when (and ar-verbose-p (called-interactively-p 'any))
      (message "%s" erg))))

(defun ar--backward-empty-lines-or-comment ()
  "Travel backward"
  (while
      (or (< 0 (abs (skip-chars-backward " \t\r\n\f")))
          (ar-backward-comment))))

(provide 'ar-subr)
;;; ar-subr.el ends here
