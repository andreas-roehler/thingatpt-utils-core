;;; ar-beg-end.el --- Detecting nested forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Version: 0.2

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

(defcustom ar-ignore-escaped-chars t
  "If it escaped characters should be ignored.
Default is t, escaped characters don't match."

  :type 'boolean
  :tag "ar-ignore-escaped-chars"
  :group 'werkstatt
  :safe 'booleanp)

(defvar-local ar-match-in-string-or-comment nil
  "Internally used only.")

;; (defun ar-in-delimiter-intern (count orig beglist match-in-comment match-in-string &optional first regexp)
;;   (let (done this pps)
;;     (while (and (not done)(< (point) orig))
;;       (while (and (re-search-forward regexp orig t 1)
;; 		  (if (save-excursion
;; 			(goto-char (match-beginning 0))
;; 			(ar-escaped))
;; 		      (progn
;; 			(while
;; 			    (and
;; 			     (re-search-forward regexp orig t 1)
;; 			     (save-excursion
;; 			       (goto-char (match-beginning 0))
;; 			       (ar-escaped))))
;; 			t)
;; 		    t)
;; 		  (setq first (point))
;;                   (not (beg-end--related-exceptions match-in-comment match-in-string (point)))
;; 		  ;; (not match-in-comment)
;; 		  (save-match-data
;; 		    ;; (ar-in-comment-p-atpt)
;; 		    (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
;;       (if first
;;           (progn
;;             (setq count (1+ count))
;;             (setq beglist (list (match-beginning 0) (match-end 0)))
;;             (when (eq 1 (% count 2))
;;               (goto-char (match-beginning 0))
;;               (setq delimiter (concat "\\([^\\]\\)" (match-string-no-properties 0) "\\|\\(\\\\\\\\\\)" (match-string-no-properties 0)))
;;               (setq old delimiter)
;;               (while (and (setq this (re-search-forward delimiter orig t 1))
;;                           (not ar-thing-inside-comment)
;;                           (save-match-data
;;                             ;; (ar-in-comment-p-atpt)
;;                             (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
;;               (if this (setq count (1+ count))
;;                 (setq done t)))
;;             (setq first nil))
;;         (setq done t)))
;;     (setq erg (and (< 0 count)(eq 1 (% count 2))))
;;     (when (and (< 0 count) erg)
;;       beglist)))

;; (defun ar-in-delimiter-base (regexp &optional condition guess-delimiter)
;;   "REGEXP expected of an unary delimiter, for example
;; \"\\\\\\\"\\\\\\\"\\\\\\\"\\\\|'''\\\\|\\\\\\\"\\\\|'\" indicating string delimiters in Python.
;; Optional second arg --a number, nil or ‘t’-- if interactively called. "
;;   (let* ((orig (point))
;;          (pps (parse-partial-sexp (point-min) (point)))
;;          (match-in-comment (nth 4 pps))
;;          (match-in-string (nth 3 pps))
;;         (count 0)
;;         tell beglist)
;;     (save-excursion
;;       ;; (save-restriction
;;       ;; (widen)
;;       (goto-char (point-min))
;;       (let* ((delimiter (when
;; 			    (looking-at regexp)
;; 			  (setq count (1+ count))
;; 			  (match-string-no-properties 0)))
;; 	     (delimiter-p (stringp delimiter)))
;; 	(if delimiter-p
;; 	    (progn
;; 	      (setq delimiter (concat "\\([^\\]\\)" (replace-regexp-in-string "\"" "\\\\\""  delimiter)))
;; 	      (setq beglist (list (match-beginning 0) (match-end 0)))
;; 	      (goto-char (match-end 0))
;; 	      (ar-in-delimiter-intern count orig beglist match-in-comment match-in-string delimiter-p delimiter))
;; 	  (setq regxep (concat "[^\\]" regexp))
;; 	  (ar-in-delimiter-intern count orig beglist match-in-comment match-in-string nil regexp ))))))

(defun beg-end-regexp-quote-maybe (str)
  "Some chars need ‘regexp-quote’ ."
  (cond ((or (string= str (char-to-string ?\[))(string= str (char-to-string ?$)) (string= str (char-to-string ?\\)))
         (regexp-quote str))
        (t str)))

(defun beg-end--related-exceptions (match-in-comment match-in-string pos flag)
  "Check for contradiction, return t if it fails.

  FLAG: if moving forward or backward"
  (unless (or (bobp)
              ;; (ignore-errors (eq (car-safe (syntax-after (point))) 7))
              )
    (let* ((pps (save-excursion (parse-partial-sexp (point-min) pos
                                                    ;; (if (eq flag 'beg) (1+ pos) pos)
                                                    ))))
      (or
       ;; (and (ar-escaped-p (1- (point))) ar-ignore-escaped-chars)
       (and ar-ignore-escaped-chars (ar-escaped-p (if (eq flag 'beg) (point) (1- (point)))))

       ;; \s-*[[|<({"]"].["]"]>")"}"]"] ar-forward-sexp fails
       ;; ar-forward-sexp-test-tEV2zp
       ;; (and (not match-in-string)
       ;;      (eq 7 (car-safe (syntax-after pos))))

       ;; (and match-in-string
       ;;      (not (eq 7 (car-safe (syntax-after (if (eq flag 'beg) (point) (1- (point))))))))
       (and (not (or (and comment-start-skip (looking-at comment-start-skip))(nth 4 pps)))
            match-in-comment
            )
       (and (nth 4 pps)
            (not match-in-comment)
            )
       (and (not (nth 3 pps))
            match-in-string
            (not (eq (car (syntax-after (point))) 7))  
            )
       (and (nth 3 pps)
            (not match-in-string)
            )))))

(defun beginning-of-form-core (begstr endstr regexp nesting condition searchform bound noerror match-in-comment match-in-string orig)
  (let* ((form (if regexp 're-search-backward 'search-backward))
         (nesting (or nesting 0))
         first)
    (while
        (and (not (bobp)) (or (< 0 nesting) (not first))
             (funcall form searchform bound noerror))
      (setq last (point))
      (unless
          (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg)
        ;; ";;;" "Write 'etc. "
        ;; doublequoted starts either right behind closer or must travel some distanz
        (cond ((string= begstr endstr)
               (if (< (point) orig) ;; (not first)
                   (setq nesting (1- nesting))
                 (setq nesting (1+ nesting)))
               (setq first t))
              ((looking-at (beg-end-regexp-quote-maybe begstr))
               (setq nesting (1- nesting))
               (setq first t))
              ((looking-at (beg-end-regexp-quote-maybe endstr))
               (setq nesting (1+ nesting))
               (setq first t)))))
    nesting))

(defun beginning-of-form-base (begstr endstr &optional bound noerror nesting regexp condition match-in-comment match-in-string)
  "Optional argument CONDITION: may signal 'behind
when starting from behind unary delimiters like single-quotes"
  (let* ((searchform
          (if (stringp begstr)
	      (cond ((and (string= begstr endstr))
                     (setq regexp nil)
		     begstr)
		    ((and begstr endstr)
		     (progn
		       (setq regexp t)
		       (concat (beg-end-regexp-quote-maybe begstr) "\\|" endstr)))
		    (t begstr))
	    begstr))
         (orig (point))
         (pps (parse-partial-sexp (or bound (point-min)) (point)))
         (match-in-comment (or match-in-comment (nth 4 pps)))
         (match-in-string (or match-in-string (nth 3 pps)
                              ;; looking for opening doublequotes
                              ;; (not (eq 7 (car-safe (syntax-after (point)))))
                              ))
         first erg)
    (if (and
         (not first) (< nesting 1)
         ;; ";;;" "Write 'etc. "
         (looking-at (beg-end-regexp-quote-maybe begstr))
         (or (not (string= begstr endstr))(bobp)(and (setq erg (ar-buffer-narrowed-p))(eq (point) (car-safe erg))))
         (or (and (ar-escaped-p (point)) ar-ignore-escaped-chars)
             (not (ar-escaped-p (point))))
         ;; Check for contradiction, return t if it fails.
         (not (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg)))
        (list (match-beginning 0) (match-end 0))
      ;; (when (and
      ;;        (not first)
      ;;        ;; ((asdf))|
      ;;        (or (looking-at (beg-end-regexp-quote-maybe endstr))
      ;;            (not (string= begstr endstr)))
      ;;        (not (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg)))
      ;;   (setq nesting (1+ nesting)))
      (while
          (and
           (or (not first) (< 0 nesting)))
        (if (eq condition 'behind)
            (progn
              (beginning-of-form-core begstr endstr regexp nesting condition searchform bound noerror match-in-comment match-in-string orig)
              (setq condition nil))
          (setq first t)
        (setq nesting (beginning-of-form-core begstr endstr regexp nesting condition searchform bound noerror match-in-comment match-in-string orig))))
      (cond
       ((<= (point) orig)
        (when (looking-at (beg-end-regexp-quote-maybe begstr))

          ;; (unless
          ;; (beg-end--related-exceptions match-in-comment match-in-string (1+ (point)) 'beg)
          (list (match-beginning 0) (match-end 0))))
       ((eq (point) orig)
        (cond ((eq (length begstr) 1)
               ;; (< 0 (abs (skip-chars-backward (concat "^" begstr))))
               (skip-chars-backward (concat "^" endstr))
               (when (looking-at endstr)
                 (unless
                     (or (and (ar-escaped-p (1- (point))) ar-ignore-escaped-chars)
                         (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg))
                   (list (match-beginning 0) (match-end 0)))))
              ((characterp begstr)
               (and (< 0 (abs (skip-chars-backward (concat "^" begstr))))
                    (looking-at begstr))
               (unless
                   (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg)
                 (list (match-beginning 0) (match-end 0))))

              (t (when (and (re-search-backward endstr nil t)
                            (looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position)))
                   (unless
                       (beg-end--related-exceptions match-in-comment match-in-string (point) 'beg)
                     (goto-char (match-beginning 0))
                     (list (match-beginning 0) (match-end 0)))))))))))

(defun end-of-form-core (begstr endstr regexp nesting condition searchform bound noerror pps forward match-in-comment match-in-string)
  (let* ((form (if regexp 're-search-forward 'search-forward))
         ;; (start (or start (point-min)))
         ;; (pps (parse-partial-sexp start (point)))
         ;; (match-in-comment (or match-in-comment (nth 4 pps)))
         ;; (match-in-string (or match-in-string (nth 3 pps)))
         first)
    (while (and (not (eobp))(or (not first) (< 0 nesting)) (or (not bound) (< (point) bound))
                (funcall form
                         searchform
                         ;; (if (ignore-errors
                         ;;            (eq condition 'backslashed))
                         ;;          (regexp-quote searchform)
                         ;;        searchform)
                         bound noerror)
                (setq last (point))
                (setq first t))
      (cond ((looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position))
             (if (string= begstr endstr)
                 (unless
                     (beg-end--related-exceptions match-in-comment match-in-string (1- (point)) 'end)
                   (setq nesting (1- nesting)))
               (unless
                   (or (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
                       (and forward (eq nesting 0)))
                 (setq nesting (1+ nesting)))))
            ((looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
             (unless (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
               (setq nesting (1- nesting))))))
    ;; (setq nesting 0)
    ;; in case of no match, make it stop
    (setq nesting 0)
    nesting))

(defun end-of-form-base (begstr endstr &optional bound noerror nesting regexp condition forward match-in-comment match-in-string)
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
  (unless (eobp)
    (let* ((searchform (cond ((string= begstr endstr)
                              endstr)
                             ((and begstr endstr)
                              (progn
                                (setq regexp t)
                                (concat (beg-end-regexp-quote-maybe begstr) "\\|"(beg-end-regexp-quote-maybe endstr))))
                             (t endstr)))
           ;; (nesting (or nesting (ar-in-delimited-p begstr)))
           (orig (point))
           (bound (or bound (point-max)))
           (pps (unless (or match-in-comment match-in-string)
                  (if (or (eq 7 (car-safe (syntax-after (point)))) (bobp))
                    (save-excursion (forward-char 1) (parse-partial-sexp (point-min) (point)))
                  (parse-partial-sexp (point-min) (point)))))
           (match-in-comment (or match-in-comment (nth 4 pps)))
           (match-in-string (or match-in-string (nth 3 pps)))
           first)
      (while (looking-at (beg-end-regexp-quote-maybe begstr))
        (goto-char (match-end 0))
        (unless (or match-in-comment match-in-string)
          (setq pps (parse-partial-sexp (point-min) (point))))
        (unless
             (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
          (setq nesting (1+ nesting))))
      (while
          (and
           (or (not first) (< 0 nesting)) (or (not bound) (< (point) bound))(not (eobp)))
          (setq nesting (end-of-form-core begstr endstr regexp nesting condition searchform bound noerror pps forward match-in-comment match-in-string))
        (setq first t)
)
      (cond
       ((< orig (point))
        (if forward
            (cond
             ((looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position))
              ;; (unless
                  ;; (or
                   ;; (string= begstr endstr)
                   ;; (beg-end--related-exceptions match-in-comment match-in-string (1- (point)) 'end))
                ;; (forward-char -1)
                (list (match-beginning 0) (match-end 0)))
             ((looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
              (unless
                  (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
                (list (match-beginning 0) (match-end 0))))
)
          (when
              (and (looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
                   (< orig (match-beginning 0)))
            ;; (unless
                ;; (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
              (list (match-beginning 0) (match-end 0))))
        )
       ((eq orig (point))
        (if forward
            (cond ((eq (length begstr) 1)
                   ;; (< 0 (abs (skip-chars-forward (concat "^" begstr))))
                   (skip-chars-forward (concat "^" begstr))
                   (when (looking-at begstr)
                     (unless
                         (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
                       (list (match-beginning 0) (match-end 0)))))
                  ((characterp begstr)
                   (and (< 0 (abs (skip-chars-forward (concat "^" begstr))))
                        (looking-at begstr))
                   (unless (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
                     (list (match-beginning 0) (match-end 0))))

                  (t (when (and (re-search-forward begstr nil t)
                                (looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position)))
                       (unless (beg-end--related-exceptions match-in-comment match-in-string (point) 'end)
                         (goto-char (match-beginning 0))
                         (list (match-beginning 0) (match-end 0)))
)))))))
    ))

(provide 'ar-beg-end)
;;; ar-beg-end.el ends here
