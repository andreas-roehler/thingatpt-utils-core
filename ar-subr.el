;;; ar-subr.el --- filling -*- lexical-binding: t; -*-


(defun ar-empty-line-p ()
  "Return t if cursor is at an empty line, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (save-match-data (looking-at ar-empty-line-p-chars))))

(provide 'ar-subr)
;;; ar-subr.el ends here
