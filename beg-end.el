;;; beg-end.el --- Detecting nested forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2023  Andreas Röhler

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

;; Routine detecting nested forms - for all kind of
;; things which have a start- and a end-string.
;; End-string might be the empty string.  Jump to the
;; beginning or end of a form.  Repeated execution
;; forward or backward.  With argument as many times.

;; FixMe: `ar-in-comment-p-atpt' uses
;; thingatpt-utils-core while required from

;;; Code:

(require 'ar-subr)

(defgroup werkstatt nil
  "Return, mover over or manipulate a given THING."
  :prefix "ar-"
  )

(defun beg-end-regexp-quote-maybe (str)
  "Some chars need ‘regexp-quote’ ."
  (cond ((or (string= str (char-to-string ?\[))(string= str (char-to-string ?$)) (string= str (char-to-string ?\\)))
         (regexp-quote str))
        (t str)))

(defun beginning-of-form-core (begstr endstr regexp nesting in-comment in-string condition searchform bound noerror)
  (let ((form (if regexp 're-search-backward 'search-backward))
        (nesting nesting)
        (first nil))
    (if (looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
        (progn
          (setq nesting (1+ nesting))
          (forward-char -1))
      (when (looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position))
        (setq nesting (1- nesting))
        (setq first t)
        (unless (bobp) (forward-char -1))))
    (when (or (< 0 nesting) (not first))
      (setq first t)
      (cond ((and in-comment in-string)
             (while (and (funcall form searchform bound noerror) (ar-escaped-p))))
            (in-comment
             (while (and (funcall form searchform bound noerror) (or (nth 3 (parse-partial-sexp (point-min) (point))) (ar-escaped-p)))))
            (in-string
             (while (and (funcall form searchform bound noerror) (or (nth 4 (parse-partial-sexp (point-min) (point)))(ar-escaped-p)))))
            (t
             (while (and (funcall form searchform bound noerror) (or (nth 8 (parse-partial-sexp (point-min) (point))) (ar-escaped-p)))))))
    (if (looking-at (beg-end-regexp-quote-maybe begstr))
        (setq nesting (1- nesting))
      (when (looking-at (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
        (setq nesting (1+ nesting))))
    nesting))

(defun beginning-of-form-intern (begstr endstr bound noerror nesting in-comment regexp condition in-string backward)
  (let ((searchform (if (stringp begstr)
			(cond ((and (string= begstr endstr))
                               (setq regexp nil)
			       begstr)
			      ((and begstr endstr)
			       (progn
				 (setq regexp t)
				 (concat (beg-end-regexp-quote-maybe begstr) "\\|" endstr)))
			      (t begstr))
		      begstr))
        (nesting (or nesting 0))
        (orig (point))
        first done)
    (cond
     ((and regexp (looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position)))
      (goto-char (match-beginning 0))
      ;; (if (string= begstr endstr)
      ;;     (progn
      ;;       (setq nesting (1- nesting))
      (setq first t)
     (setq nesting (1+ nesting)))
        ;; )
      ;; )
     ((and regexp (not first) (looking-at (beg-end-regexp-quote-maybe endstr)))
      (goto-char (match-beginning 0))
      (if (string= begstr endstr)
          (progn
            (setq nesting (1- nesting))
            (setq first t))
        (setq nesting (1+ nesting)))))
    ;; (when (and (< 1 (length endstr))(looking-back (beg-end-regexp-quote-maybe searchform) (line-beginning-position)))
      ;; (goto-char (match-beginning 0)))
    (while
        (and
         (or (not first) (< 0 nesting)) (not (bobp)))
      (setq first t)
      (setq nesting (beginning-of-form-core begstr endstr regexp nesting in-comment in-string condition searchform bound noerror)))
    (if (< (point) orig)
        (progn

          (when (and (looking-at (beg-end-regexp-quote-maybe begstr))
                     (not (ar-in-string-comment-or-escaped (parse-partial-sexp (point-min) (point)) in-comment in-string)))
            (list (match-beginning 0) (match-end 0))))
      (if backward
          (progn
            (setq nesting 0)
            (while (and (< 0 (abs (skip-chars-backward (concat "^" endstr) bound)))
                        (ar-in-string-comment-or-escaped (parse-partial-sexp (point-min) (point)) in-comment in-string)))
            (cond ((or (string= endstr (char-to-string ?$))(string= endstr (char-to-string ?\\)))
                   (when
                       (looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
                     (list (match-beginning 0) (match-end 0))))
                  (t (when (looking-back endstr (line-beginning-position))
                       (list (match-beginning 0) (match-end 0))))))))))

(defun beginning-of-form-base (begstr &optional endstr bound noerror nesting in-comment regexp condition in-string backward)
  "Assume being inside delimiters, go to start.

Bound limits search.
NOERROR: no error message is raised if not successful.
NESTING: counts nested forms.
PERMIT-COMMENT: match inside comments.
REGEXP: if beg/end-str are regular expressions.
CONDITION takes a function as argument perfoming the match.
PERMIT-STRING: forms inside string match.
"
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (in-comment (unless (eq in-comment 'ignore)(or in-comment (nth 4 pps))))
         (in-string (unless (eq in-string 'ignore)(or in-string (nth 3 pps)))))
        (cond (in-string
               (if
                   (and (not (string-match begstr ar-delimiters-atpt))(looking-at (beg-end-regexp-quote-maybe begstr)))
                   (list (match-beginning 0) (match-end 0))
                 (unless bound (setq bound (when (nth 8 pps)(nth 8 pps))))
                 (beginning-of-form-intern begstr endstr (when (numberp bound) bound) noerror nesting in-comment regexp condition in-string backward)))
          (in-comment
           (if
               (looking-at (beg-end-regexp-quote-maybe begstr))
               (list (match-beginning 0) (match-end 0))
             (unless bound (setq bound (when (nth 8 pps) (nth 8 pps))))
             (beginning-of-form-intern begstr endstr (when (numberp bound) bound) noerror nesting in-comment regexp condition in-string backward)))
          (t (if
                 (looking-at (beg-end-regexp-quote-maybe begstr))
                 (list (match-beginning 0) (match-end 0))
               (beginning-of-form-intern begstr endstr bound noerror nesting in-comment regexp condition in-string backward))))))

(defun end-of-form-core (begstr endstr regexp nesting in-comment in-string condition searchform bound noerror)
  (let ((form (if regexp 're-search-forward 'search-forward))
        (nesting nesting))
    (while (and (not (eobp)) (or (not bound)(< (point) bound))(< 0 nesting))
      (cond ((and in-comment in-string)
             (while (and (funcall form searchform bound noerror) (ar-escaped-p (1- (point)))))
             )
            (in-comment
             (while (and (funcall form searchform bound noerror)
                         (ar-escaped-p (1- (point)))
                         )))
            (in-string
             (while (and (funcall form searchform bound noerror) (or (ar-escaped-p (1- (point)))(nth 4 (parse-partial-sexp (point-min) (point)))))))
            (t
             (while (and (funcall form searchform bound noerror)
                         (or (nth 8 (parse-partial-sexp (point-min) (point))) (if (string= (beg-end-regexp-quote-maybe searchform) (regexp-quote "\\")) (ar-backslash-escaped-p) (ar-escaped-p (1- (point)))))))))
      (cond ((looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position))
             (if (string= begstr endstr)
                 (setq nesting (1- nesting))
               (setq nesting (1+ nesting))))
            ((looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
             (setq nesting (1- nesting)))))
    nesting))

(defun end-of-form-base-intern (begstr endstr &optional bound noerror nesting in-comment regexp condition in-string forward)
  "Goto closing of a programming structure in this level.

Returns a list if a match found after start, nil otherwise.

As it stops one char after form, go one char back onto the last char of form.

Set comment to ‘t’ if forms inside comments should match -
also for processing comments itself.
If SHOW, display nesting and point in message buffer.
Set 7th argument REGEXP t, if beg/end-strings are regular expressions.
Optional CONDITION expects a function whose return value - ‘t’ or a number -
is checked for success, otherwise search continues.
If IN-STRING is non-nil, forms inside string match.
"
  (let* ((searchform (cond ((string= begstr endstr)
                            endstr)
                           ((and begstr endstr)
                            (progn
                              (setq regexp t)
                              (concat (beg-end-regexp-quote-maybe begstr) "\\|" (beg-end-regexp-quote-maybe endstr))))
                           (t endstr)))
         (nesting (or nesting 0))
         (orig (point))
         first)
    (when (looking-at (beg-end-regexp-quote-maybe begstr))
      (goto-char (match-end 0))
      (setq nesting (1+ nesting)))
    (when (and (< 1 (length endstr))(looking-at (beg-end-regexp-quote-maybe searchform)))
      (goto-char (match-end 0)))
    (while
        (and
         (or (not first) (< 0 nesting)) (or (not bound)(< (point) bound))(not (eobp)))
      (setq first t)
      (cond ((eq (car (syntax-after (1- (point)))) 7)
             ;; in-string set to t
             (setq nesting (end-of-form-core begstr endstr regexp nesting in-comment t condition searchform bound noerror)))
            ((eq (syntax-after (1- (point))) 11)
             ;; in-comment set to t
             (setq nesting (end-of-form-core begstr endstr regexp nesting t in-string condition searchform bound noerror)))
            (t (setq nesting (end-of-form-core begstr endstr regexp nesting in-comment in-string condition searchform bound noerror)))))
    (if (< orig (point))
        (progn
	  (when forward (goto-char (match-end 0)))
          (when (looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
            (list (match-beginning 0) (match-end 0))))
      (if forward
          (progn
            (setq nesting 0)
            (while (and (< 0 (abs (skip-chars-forward (concat "^" begstr) bound)))
                        (ar-in-string-comment-or-escaped (parse-partial-sexp (point-min) (point)) in-comment in-string)))
            (cond ((or (string= begstr (char-to-string ?$))(string= begstr (char-to-string ?\\)))
                   (when
                       (looking-at (beg-end-regexp-quote-maybe begstr))
                     (list (match-beginning 0) (match-end 0))))
                  (t (when (looking-at begstr)
                       (list (match-beginning 0) (match-end 0))))))))))

(defun end-of-form-base (begstr endstr &optional bound noerror nesting in-comment regexp condition in-string forward)
  "Assume being inside delimiters, go to start.

Bound limits search.
NOERROR: no error message is raised if not successful.
NESTING: counts nested forms.
PERMIT-COMMENT: match inside comments.
REGEXP: if beg/end-str are regular expressions.
CONDITION takes a function as argument perfoming the match.
PERMIT-STRING: forms inside string match.
"
  (let* ((pps (parse-partial-sexp (point-min) (point)))
         (in-comment (unless (eq in-comment 'ignore)(or in-comment (nth 4 pps))))
         (in-string (unless (eq in-string 'ignore)(or in-string (nth 3 pps))))
         )
    (cond (in-string
           (unless bound (setq bound (save-excursion (when (nth 8 pps) (goto-char (nth 8 pps))) (forward-sexp) (point))))
           (end-of-form-base-intern begstr endstr (when (numberp bound) bound) noerror nesting in-comment regexp condition in-string forward))
          (in-comment
           (unless bound (setq bound (save-excursion (when (nth 8 pps) (ar-forward-comment)))))
           (end-of-form-base-intern begstr endstr (when (numberp bound) bound) noerror nesting in-comment regexp condition in-string forward))
          (t (end-of-form-base-intern begstr endstr bound noerror nesting in-comment regexp condition in-string forward)))))

(provide 'beg-end)
;;; beg-end.el ends here
