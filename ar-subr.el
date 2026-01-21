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

(provide 'ar-subr)
;;; ar-subr.el ends here
