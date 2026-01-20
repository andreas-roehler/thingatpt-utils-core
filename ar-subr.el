;;; ar-subr.el --- filling -*- lexical-binding: t; -*-

(defun ar-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (save-match-data (looking-at ar-empty-line-p-chars))))

(defsubst ar--string-trim-left (strg &optional regexp trim-left)
  "Trim STRING of leading string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (and trim-left (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") strg))
      (replace-match "" t t strg)
    strg))

(defsubst ar--string-trim-right (strg &optional regexp trim-right)
  "Trim STRING of trailing string matching REGEXP.

REGEXP defaults to \"[ \\t\\n\\r]+\"."
  (if (and trim-right (string-match (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'") strg))
      (replace-match "" t t strg)
    strg))

(defsubst ar--string-trim (strg &optional regexp trim-left trim-right)
  "Trim STRING of leading and trailing strings matching TRIM-LEFT and TRIM-RIGHT.

TRIM-LEFT and TRIM-RIGHT default to \"[ \\t\\n\\r]+\"."
  (ar--string-trim-left (ar--string-trim-right strg regexp trim-right) regexp trim-left))

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
