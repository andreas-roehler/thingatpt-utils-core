;;; beg-end.el --- Detecting nested forms  -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2016  Andreas Röhler

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

(defun ar-syntax (&optional arg)
  "Return t if char meant as syntax-symbol. "
  (interactive "p")
  (let ((orig (point))
	erg)
    (goto-char (match-beginning 0))
    (setq erg (looking-back "\\\\s" (line-beginning-position)))
    (goto-char orig)
    (when arg (message "%s" erg))
    erg))

(defun beg-end-regexp-quote-maybe (str)
  "Some chars need ‘regexp-quote’ ."
  (cond ((or (string= str (char-to-string ?\[))(string= str (char-to-string ?$)) (string= str (char-to-string ?\\)))
         (regexp-quote str))
        (t str)))

(defun beginning-of-form-core (begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)
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
      (cond ((and permit-comment permit-string)
             (while (and (funcall form searchform bound noerror) (ar-escaped-p))))
            (permit-comment
             (while (and (funcall form searchform bound noerror) (or (nth 3 (parse-partial-sexp (point-min) (point))) (ar-escaped-p)))))
            (permit-string
             (while (and (funcall form searchform bound noerror) (or (nth 4 (parse-partial-sexp (point-min) (point)))(ar-escaped-p)))))
            (t
             (while (and (funcall form searchform bound noerror) (or (nth 8 (parse-partial-sexp (point-min) (point))) (ar-escaped-p)))))))
    (if (looking-at (beg-end-regexp-quote-maybe begstr))
        (setq nesting (1- nesting))
      (when (looking-at (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
        (setq nesting (1+ nesting))))
    nesting))

(defun beginning-of-form-base (begstr &optional endstr bound noerror nesting permit-comment regexp condition permit-string backward)
  "Assume being inside delimiters, go to start.

Bound limits search.
NOERROR: no error message is raised if not successful.
NESTING: counts nested forms.
PERMIT-COMMENT: match inside comments.
REGEXP: if beg/end-str are regular expressions.
CONDITION takes a function as argument perfoming the match.
PERMIT-STRING: forms inside string match.
"
  (let* ((searchform (if (stringp begstr)
			 (cond ((and (string= begstr endstr))
				begstr)
			       ((and begstr endstr)
				(progn
				  (setq regexp t)
				  (concat (beg-end-regexp-quote-maybe begstr) "\\|" endstr)))
			       (t begstr))
		       begstr))
         (nesting (or nesting 0))
         (orig (point))
         (permit-comment (or permit-comment (nth 4 (parse-partial-sexp (point-min) (point)))))
         (permit-string (nth 3 (parse-partial-sexp (point-min) (point))))
         (pps (parse-partial-sexp (point-min) (point)))
         beg-pos-delimiter end-pos-delimiter first)
    (if
        (and (looking-at (beg-end-regexp-quote-maybe begstr))
             (not (ar-in-string-comment-or-escaped pps permit-comment permit-string)))
        (list (match-beginning 0) (match-end 0))
      (cond
       ((looking-back (beg-end-regexp-quote-maybe endstr) (line-beginning-position))
        (goto-char (match-beginning 0))
        (if (string= begstr endstr)
            (progn
              (setq nesting (1- nesting))
              (setq first t))
          (setq nesting (1+ nesting))))
       ((looking-at (beg-end-regexp-quote-maybe endstr))
        (goto-char (match-beginning 0))
        ;; (setq nesting (1- nesting))
        ;; (setq first t)
        (if (string= begstr endstr)
            (progn
              (setq nesting (1- nesting))
              (setq first t))
          (setq nesting (1+ nesting)))

        ))
      (when (and (< 1 (length endstr))(looking-back (beg-end-regexp-quote-maybe searchform) (line-beginning-position)))
        (goto-char (match-beginning 0)))
      (while
          (and
           (or (not first) (< 0 nesting)) (not (bobp)))
        (setq first t)
        (setq nesting (beginning-of-form-core begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)))
      (when (and (looking-at (beg-end-regexp-quote-maybe begstr))
                 (not (ar-in-string-comment-or-escaped pps permit-comment permit-string)))
        (list (match-beginning 0) (match-end 0))))))

(defun end-of-form-base-update-nesting (nesting endstr permit-comment)
  (save-match-data
    (let (erg)
      (if (string-match endstr (match-string-no-properties 0))
	  (progn
	    (if permit-comment
		(setq erg (1- nesting))
	      (unless (ar-in-comment-p)
		(setq erg (1- nesting)))))
        ;; got another beginning while moving down
	(if permit-comment
	    (setq erg (1+ nesting))
	  (unless (ar-in-comment-p)
	    (setq erg (1+ nesting)))))
      erg)))

(defun end-of-form-base-intern (nesting begstr endstr permit-comment permit-string &optional condition regexp)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string
    (if (and (not permit-string) (nth 3 pps)(nth 8 pps))
        (progn
          (forward-char 1)
          (while (and (setq pps (parse-partial-sexp (point-min) (point)))(nth 3 pps)(nth 8 pps))
	    (goto-char (nth 8 pps))
            (forward-sexp)))
      (unless (save-match-data
                (and condition (funcall condition)))
	(setq nesting (end-of-form-base-update-nesting nesting endstr permit-comment))))
    nesting))

(defun end-of-form-core-update-nesting (begstr endstr)
  (if (beg-end-regexp-quote-maybe begstr)
      ;; (looking-at (beg-end-regexp-quote-maybe begstr))
      (progn
        (setq nesting (1+ nesting))
        (forward-char 1))
    (when (looking-at (beg-end-regexp-quote-maybe endstr))
      (setq nesting (1- nesting))
      (forward-char 1)
      )))

(defun end-of-form-core (begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)
  (let ((form (if regexp 're-search-forward 'search-forward))
        (nesting nesting))
    (while (and (not (eobp)) (< 0 nesting))
      (cond ((and permit-comment permit-string)
             (while (and (funcall form searchform bound noerror)(ar-escaped-p)))
             ;; (end-of-form-core-update-nesting begstr endstr)
             )
            (permit-comment
             (while (and (funcall form searchform bound noerror) (or (nth 3 (parse-partial-sexp (point-min) (point))) (ar-escaped-p)))))
            (permit-string
             (while (and (funcall form searchform bound noerror) (or (nth 4 (parse-partial-sexp (point-min) (point)))(ar-in-comment-p)))))
            (t
             (while (and (funcall form
                                  ;; (beg-end-regexp-quote-maybe searchform)
                                  searchform
                                  bound noerror)
                         (or (nth 8 (parse-partial-sexp (point-min) (point))) (if (string= (beg-end-regexp-quote-maybe searchform) (regexp-quote "\\")) (ar-backslash-escaped-p)(ar-escaped-p))))))
            )
      (cond ((looking-back (beg-end-regexp-quote-maybe begstr) (line-beginning-position))
             (if (string= begstr endstr)
                 (setq nesting (1- nesting))
               (setq nesting (1+ nesting))))
            ((looking-back (beg-end-regexp-quote-maybe endstr))
             (setq nesting (1- nesting)))))
    nesting))

(defun end-of-form-base (begstr endstr &optional bound noerror nesting permit-comment regexp condition permit-string forward)
  "Goto closing of a programming structure in this level.

Returns a list (match-beginning 0) (match-end 0) if a match find after start, nil otherwise.

As it stops one char after form, go one char back onto the last char of form.

Set comment to ‘t’ if forms inside comments should match - also for processing comments itself.
If SHOW, display nesting and point in message buffer.
Set 7th argument REGEXP t, if beg/end-strings are regular expressions.
Optional arg CONDITION expects a function whose return value - ‘t’ or a number - is checked for success, otherwise search continues.
If IN-STRING is non-nil, forms inside string match.
"
  (let* ((searchform (cond ((string= begstr endstr)
                            endstr)
                           ((and begstr endstr)
                            (progn
                              (setq regexp t)
                              (concat begstr "\\|" endstr)))
                           (t endstr)))
         (nesting (or nesting 0))
         (orig (point))
         beg-pos-delimiter end-pos-delimiter erg first pps)
    (when (looking-at (beg-end-regexp-quote-maybe begstr))
      (goto-char (match-end 0))
      (setq nesting (1+ nesting)))
    ;; (cond ((or (string= begstr (char-to-string ?$))(string= begstr (char-to-string ?\\)))
    ;;        (when (looking-at (beg-end-regexp-quote-maybe begstr))
    ;;          (goto-char (match-end 0))
    ;;          (setq nesting (1+ nesting))))
    ;;       (t (when (looking-at begstr)
    ;;            (goto-char (match-end 0))
    ;;            (setq nesting (1+ nesting)))))
    (when (and (< 1 (length endstr))(looking-at (beg-end-regexp-quote-maybe searchform)))
      (goto-char (match-end 0)))
    (while
        (and
         (or (not first) (< 0 nesting)) (not (eobp)))
      (setq first t)
      (setq nesting (end-of-form-core begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)))
    (if (< orig (point))
        (progn
	  (when forward (goto-char (match-end 0)))
          (when (looking-back (beg-end-regexp-quote-maybe endstr))
            (list (match-beginning 0) (match-end 0))))
      (if forward
          (progn
            (setq nesting 0)
            (while (and (< 0 (abs (skip-chars-forward (concat "^" begstr))))
                        (ar-in-string-comment-or-escaped (parse-partial-sexp (point-min) (point)) permit-comment permit-string)))
            (cond ((or (string= begstr (char-to-string ?$))(string= begstr (char-to-string ?\\)))
                   (when
                       (looking-at (beg-end-regexp-quote-maybe begstr))
                       ;; (looking-at (beg-end-regexp-quote-maybe begstr))
                     (list (match-beginning 0) (match-end 0))))
                  (t (when (looking-at begstr)
                       (list (match-beginning 0) (match-end 0))))))))))

(defvar match-paren-key-char "%")

(defvar be-match-paren-mode nil)

(defun be-match-paren-mode (&optional iact)
  "Toggle be-match-paren-mode.
If on, inserting of `be-match-paren-char', default is \"%\", moves to the matching opening/closing.
With arg, insert the charakter the key is on
Key per default is \"%\" as with elisp's `match-paren'. "
  (interactive "p")
  (if be-match-paren-mode
      (progn
        (setq be-match-paren-mode nil)
        ;;        (define-key be-mode-map "%" 'self-insert-command)
        (when iact (message "be-match-paren-mode: %s" be-match-paren-mode)))
    (setq be-match-paren-mode t)
    ;;    (define-key be-mode-map "%" 'be-match-paren)
    (when iact (message "be-match-paren-mode: %s" be-match-paren-mode))))

(defun beg-end-match-paren (begstr endstr &optional ins)
  "Go to the matching opening/closing.
First to opening, unless cursor is already there.
With arg, insert the charakter of `sh-match-paren-char'.
Key per default is \"%\" as with elisp's `match-paren'. "
  (if ins
      (insert match-paren-key-char)
    (cond ((ignore-errors (looking-at begstr))
           (end-of-form-base begstr endstr bound noerror count))
          ((ignore-errors (looking-at "\\s("))
           (match-paren arg))
          ((ignore-errors (looking-at "\\s)"))
           (match-paren arg))
          (t (beginning-of-form-base begstr endstr bound noerror count permit-comment)))))

(defun ar--char-delimiters-beginning-whitespaced-form (char)
  (cond ((eq (char-after) ?\ )
	 (point))
	((eq (char-before) ?\ )
	 (1- (point)))
	(t (and (< 0 (abs (skip-chars-backward (concat "^" (char-to-string char)))))
		(eq (char-before) ?\ )
		(1- (point))))))

(defun ar--char-delimiters--skip-chars-backward-form (char)
  (< 0 (abs (skip-chars-backward
	     (concat "^"
		     (if (stringp char)
			 char
		       (regexp-quote (char-to-string char))))))))

(defun ar--char-delimiters--success-form (char)
  "Returns position if successful. "
  (when (eq (char-before) char)
       (1- (point))))

(defun ar--char-delimiters-beginning-no-check-forms (char orig &optional escaped comment)
  "Expects starting point after opening delimiter."
  (let (erg)
    (if (eq char 32)
	;; optional args not implemented yet
	(setq erg (ar--char-delimiters-beginning-whitespaced-form char))
      (cond ((and escaped comment)
	     (when (ar--char-delimiters--skip-chars-backward-form char))
	     (setq erg (ar--char-delimiters--success-form char)))
	    (escaped
	     (when (and (ar--char-delimiters--skip-chars-backward-form char) (ar-in-comment-p)))
	     (setq erg (ar--char-delimiters--success-form char)))
	    (t (when (and (ar--char-delimiters--skip-chars-backward-form char) (ar-escaped) (ar-in-comment-p)))
	       (setq erg (ar--char-delimiters--success-form char)))))
    erg))

(defun ar--char-delimiters-forward (char &optional orig)
  (unless (eobp)
    (when (char-equal char (char-after))
      (forward-char 1))
    (< 0 (abs (skip-chars-forward (concat "^" (regexp-quote (char-to-string char))) orig)))))

(defun ar--delimiters-beginning-nesting-intern (char orig escaped comment counter)
  (let ((this counter)
	erg last)
    (cond ((and escaped comment)
	   (while
	       (ar--char-delimiters-forward char orig)
	     (when (eq (char-after) char)
	       (unless
	       	   ;; called from closing delimiter
		   (<=  orig (point))
 		 (setq counter (1+ counter))
		 (setq this (point))))))
	  (escaped
	   (while
	       (ar--char-delimiters-forward char orig)
	     (when (eq (char-after) char)
	       (setq pps (parse-partial-sexp (point-min) (point)))
	       (unless (or (nth 4 pps)
			   ;; called from closing delimiter
			   (<= orig (point)))
		 (setq counter (1+ counter))
		 (setq this (point))))))
	  (comment
	   (while
	       (ar--char-delimiters-forward char orig)
	     (when (eq (char-after) char)
	       (unless (or (ar-escaped)
			   ;; called from closing delimiter
			   (<= orig (point)))
		 (setq counter (1+ counter))
		 (setq this (point))))))
	  (t (while
		 (ar--char-delimiters-forward char orig)
	       (when (eq (char-after) char)
		 (setq pps (parse-partial-sexp (point-min) (point)))
		 (unless (or (nth 4 pps) (ar-escaped)
			     ;; called from closing delimiter
			     (<= orig (point)))
 		   (setq counter (1+ counter))
		   (setq this (point)))))))
    (when (eq 1 (% counter 2))
      this)))
	;; (when (and (eq 0 (% counter 2))(eq (char-after) char))
	;; last))))

(defun ar--char-delimiters-beginning-check-forms (char orig escaped comment)
  (let ((counter 0))
    (goto-char (point-min))
    (when (char-equal char (char-after))
      (setq counter (1+ counter))
      (setq last (point))
      (unless (eobp) (forward-char 1)))
    (ar--delimiters-beginning-nesting-intern char orig escaped comment counter)))

(defun ar--char-delimiters-beginning-intern (char orig &optional escaped comment whole-buffer)
  "With NO-CHECK assume being after or at an opening delimiter or at closing. "
  (if (eq char ?\ )
      (ignore-errors (goto-char (ar--char-delimiters-beginning-whitespaced-form char)))
    (let (pps last erg)
      (if whole-buffer
	  (setq erg (ar--char-delimiters-beginning-check-forms char orig escaped comment))
	(setq erg (ar--char-delimiters-beginning-no-check-forms char orig escaped comment))
	)
      (or (ignore-errors (goto-char erg)) (ignore-errors (goto-char last))))))

(defun ar-char-delimiters-beginning (char &optional escaped comment whole-buffer)
  "Determine beginning of forms delimited by a single character.

WRT forms delimited by a string or regexp use ‘beginning-of-form-base’.

ESCAPED: match also chars which are backslashed.
COMMENT: match also in comments
NO-CHECK: don't consider nesting"
  (let* ((orig (point))
    	 (erg (ar--char-delimiters-beginning-intern char orig escaped comment whole-buffer)))
    (unless erg
      (goto-char orig))
    erg))

(defun ar--delimiters-end-check-forms-intern (char orig escaped comment counter)
  (let ((this counter)
	erg last)
    (cond ((and escaped comment)
	   (while
	       (ar--char-delimiters-forward char orig)
	     (setq counter (1+ counter)))
	   ;; try another one beyond limit set by orig
	   (ar--char-delimiters-forward char)
	     ;; don't raise counter, as it must remain uneven
	   (when (eq (char-after) char)
	     (setq last (1+ (point)))))
	  (escaped
	   (while
	       (setq erg (ar--char-delimiters-forward char orig))
	     (setq counter erg)
	     (forward-char 1)
	     (setq last (point))
	     (setq pps (parse-partial-sexp (point-min) (point)))
	     (when (nth 4 pps)
	       ;; in comment, remove from counter again
	       (setq counter (1- counter))))
	   ;; try another one beyond limit set by orig
	   (when
	       (setq erg (ar--char-delimiters-forward char))
	     (forward-char 1)
	     (setq counter erg)
	     (setq last (point))
	     (setq pps (parse-partial-sexp (point-min) (point)))
	     (when (nth 4 pps)
	       ;; in comment, remove from counter again
	       (setq counter (1- counter)))))
	  (comment
	   (while
	       (setq erg (ar--char-delimiters-forward char orig))
	     (setq counter erg)
	     (forward-char 1)
	     (setq last (point))
	     (when
		 (ar-escaped)
	       ;; escaped, remove from counter again
	       (setq counter (1- counter))))
	   ;; try another one beyond limit set by orig
	   (when
	       (setq erg (ar--char-delimiters-forward char))
	     (setq counter erg)
	     (forward-char 1)
	     (setq last (point))
	     (when
		 (ar-escaped)
	       ;; escaped, remove from counter again
	       (setq counter (1- counter)))))
	  (t (while (ar--char-delimiters-forward char orig)
	       (unless
		   (or (ar-escaped) (ar-in-comment-p))
		 (setq counter (1+ counter))))
	     (while (and
		     (ar--char-delimiters-forward char))
	       (or (ar-escaped) (ar-in-comment-p)))
	     (when (eq (char-after) char)
	       (setq last (1+ (point))))))
    (when (eq 1 (% counter 2))
      last)))

(defun ar--char-delimiters-end-check-forms (char orig escaped comment)
  (goto-char (point-min))
  (let ((counter 0))
    (when (char-equal char (char-after))
      (setq counter (1+ counter))
      (setq last (point))
      (unless (eobp) (forward-char 1)))
    (ar--delimiters-end-check-forms-intern char orig escaped comment counter)))

(defun ar--char-delimiters-end-no-check-forms (char &optional escaped comment)
  (let (erg)
    (cond ((and escaped comment)
	   (skip-chars-forward (concat "^" (regexp-quote (char-to-string char))))
	   (when (eq (char-after) char)
	     (setq erg (point))))
	  (comment
	   (while (and (< 0 (skip-chars-forward (concat "^" (regexp-quote (char-to-string char)))))
		       (setq erg (point))
		       (ar--escaped)
		       (setq erg nil))))
	  (t (while (and (not (eobp))
			 (< 0 (skip-chars-forward (concat "^" (regexp-quote (char-to-string char)))))
			 (setq pps (parse-partial-sexp (point-min) (point)))
			 (when (or (ar--escaped) (nth 4 pps))
			   (forward-char 1)
			   t)))
	     (when (eq (char-after) char)(forward-char 1) (setq erg (point)))))
    erg))

(defun ar--char-delimiters-end-intern (char orig &optional escaped comment whole-buffer)
  "optional WHOLE-BUFFER: check from point-min
otherwise assume being after or at an opening delimiter or at closing. "
  (if whole-buffer
      (ar--char-delimiters-end-check-forms char orig escaped comment)
    (ar--char-delimiters-end-no-check-forms char escaped comment)))

(defun ar-char-delimiters-end (char &optional escaped comment whole-buffer)
  "Determine end of forms delimited by a single character.

WRT forms delimited by a string or regexp use ‘end-of-form-base’.

ESCAPED: match also chars which are backslashed.
COMMENT: match also in comments
NO-CHECK: don't consider nesting"
  (let* ((orig (point))
    	 (erg (ar--char-delimiters-end-intern char orig escaped comment whole-buffer)))
    (unless erg
      (goto-char orig))
    erg))

;; (defun ar--delimiters-forward-check-forms-intern (char escaped comment counter)
;;   (let ((this counter)
;; 	erg last)
;;     (cond ((and escaped comment)
;; 	   (when
;; 	       (setq erg (ar--char-delimiters-forward char))
;; 	     (forward-char 1)
;; 	     (setq last (point))
;; 	     (setq counter erg)))
;; 	  (escaped
;; 	   (when
;; 	       (setq erg (ar--char-delimiters-forward char))
;; 	     (forward-char 1)
;; 	     (setq counter erg)
;; 	     (setq last (point))
;; 	     (setq pps (parse-partial-sexp (point-min) (point)))
;; 	     (when (nth 4 pps)
;; 	       ;; in comment, remove from counter again
;; 	       (setq counter (1- counter)))))
;; 	  (comment
;; 	   (when
;; 	       (setq erg (ar--char-delimiters-forward char))
;; 	     (setq counter erg)
;; 	     (forward-char 1)
;; 	     (setq last (point))
;; 	     (when
;; 		 (ar-escaped)
;; 	       ;; escaped, remove from counter again
;; 	       (setq counter (1- counter)))))
;; 	  (t (when
;; 		 (setq erg (ar--char-delimiters-forward char))
;; 	       (setq counter erg)
;; 	       (if
;; 		   (or (ar-escaped) (ar-in-comment-p))
;; 		   ;; escaped or in comment, remove from counter again
;; 		   (setq counter (1- counter))
;; 		 ;; (forward-char 1)
;; 		 (setq this (point))
;; 		 (setq last this)))))
;;     ;; (if (eq 1 (% counter 2))
;;     ;; 	last
;;     ;;   (when (and (eq 0 (% counter 2))(eq (char-after) char))
;;     ;; 	last))
;;     last))

;; (defun ar--delimiters-forward-intern (char escaped comment orig)
;;   "Expects being called from end of a delimited form."
;;   (let (last)
;;     (cond ((and escaped comment)
;; 	   (when
;; 	       (and (ar--char-delimiters-forward char) (eq (char-after) char))
;; 	     (forward-char 1)
;; 	     (setq last (point))))
;; 	  (escaped
;; 	   (when
;; 	       (and (ar--char-delimiters-forward char)(eq (char-after) char))
;; 	     (setq pps (parse-partial-sexp (point-min) (point)))
;; 	     (unless
;; 		 (nth 4 pps)
;; 	       (setq last (point)))))
;; 	  (comment
;; 	   (when
;; 	       (and (ar--char-delimiters-forward char)(eq (char-after) char))
;; 	     (unless
;; 		 (ar-escaped)
;; 	       (setq last (point)))))
;; 	  (t (when
;; 		 (and (ar--char-delimiters-forward char)(eq (char-after) char))
;; 	       (unless (or (ar-escaped) (ar-in-comment-p))
;; 		 (setq last (point))))))
;;     last))

(defun ar-in-unary-delimited-p (char)
  "Stop and return position if CHAR is delimiting at point

Or if at opening delimiter"
  (let ((orig (point))
	(counter 0)
	erg last)
    (goto-char (point-min))
    (while (and (search-forward (char-to-string char) orig t)
		(not (ar-escaped))
		(setq last (point)))
      (setq counter (1+ counter)))
    ;; (setq erg
    (or (and (eq 1 (% counter 2))
	     (1- last))
	(and (eq (char-after) char)(point))
	(goto-char orig))))

;; (defun ar-end-of-unary-delimited-atpt (char &optional start)
;;   "Go to end of unary-delimited form at point.

;; Whith optional argument START assume being at beginning"
;;   (interactive)
;;   (let (last erg)
;;     (unless start
;;       (when (setq erg (ar-in-unary-delimited-p char))
;; 	(when (eq (point) erg)(forward-char 1))
;; 	(while (and (search-forward (char-to-string char) nil t)
;; 		    (setq last (point))
;; 		    (ar-escaped)))))
;;     (when last (backward-char) (cons (point) last))))

(provide 'beg-end)
;;; beg-end.el ends here
