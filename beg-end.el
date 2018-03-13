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

;; (require 'ar-subr)

(defgroup werkstatt nil
  "Return, mover over or manipulate a given THING."
  :prefix "ar-"
  )

(defcustom ar-thing-inside-comments nil
  "If text inside comments matches when determining the border of a THING. "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-thing-escaped nil
  "If a backslashed character matches when determining the border of a THING. "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-scan-whole-buffer nil
  "Unary delimiters require the scan from beginning-of-buffer

in order to detect if inside a delimited form.
As this might be slow with large buffers, switching it off is an option.
If off, results are only reliable when called from inside a delimited form, resp. from before or after if forward- or backward- form is called."
  :type 'boolean
  :group 'werkstatt)

(defun ar-toggle-thing-inside-comments ()
  "If thing-at-point forms should match inside comments.

Toggles value of `ar-thing-inside-comments'. Default is nil "
  (interactive)
  (setq ar-thing-inside-comments (not ar-thing-inside-comments)))

(defun mark-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-new (buffer-substring-no-properties (mark) (point))))

(defun kill-form (begstr endstr &optional bound noerror count permit-comment)
  (beginning-of-form-base begstr endstr bound noerror count permit-comment)
  (push-mark (point) t t)
  (end-of-form-base begstr endstr bound noerror count permit-comment)
  (kill-region (mark) (point)))

(defun beg-end-match-paren-base (begstr endstr &optional iact)
  "Switch between opening and closing programming constructs.
If not at the beginning or the end, to opening first. "
  (cond ((ignore-errors (looking-at begstr))
	 (end-of-form))
	((progn (backward-word)
		(ignore-errors (looking-at endstr)))
	 (beginning-of-form)
	 (when iact (message "%s" (point))) (point))
	(t (beginning-of-form)
	   (when iact (message "%s" (point))) (point))))

(defun beginning-of-form (&optional bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
 Don't use this from a program, use `beginning-of-form-base' instead. "
  (interactive)
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (beginning-of-form-base begstr endstr bound noerror count permit-comment))
  (when (interactive-p) (message "%s" (point))) (point))

(defun end-of-form (&optional iact bound noerror count permit-comment)
  "Goto opening of a programming structure in this level.
Reads strings as arguments from minibuffer.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
 Don't use this from a program, use `end-of-form-base' instead. "
  (interactive "p")
  (let ((begstr (read-from-minibuffer "Insert start: "))
	(endstr (read-from-minibuffer "Insert endstr: ")))
    (end-of-form-base begstr endstr bound noerror count permit-comment))
  (when iact (message "%s" (point))) (point))

(defun ar-leave-begstr-backward (begstr unquoted-beg)
  (let* ((stringcount (length unquoted-beg))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-beg)))
    (while (and indx (not (ignore-errors (looking-at begstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-beg))
      (setq stringcount (1- stringcount)))))

(defun ar-leave-endstr-backward (endstr unquoted-end)
  (let* ((stringcount (length unquoted-end))
         (collected (char-to-string (char-after)))
         (indx (string-match collected unquoted-end)))
    (while (and indx (not (ignore-errors (looking-at endstr)))(< 1 stringcount))
      (forward-char -1)
      (setq collected (concat (char-to-string (char-after)) collected))
      (setq indx (string-match collected unquoted-end))
      (setq stringcount (1- stringcount)))))

(defun ar-escaped (&optional pos)
  "Return t if char at POS is preceded by an odd number of backslashes. "
  (save-excursion
    (when pos (goto-char pos))
    (< 0 (% (abs (skip-chars-backward "\\\\")) 2))))

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

(defun beginning-of-form-base-intern (nesting begstr endstr permit-comment permit-string condition)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string not permitted, leave it
    (if (and (not permit-string) (nth 3 pps)(nth 8 pps))
        (goto-char (nth 8 pps))
      (unless (save-match-data (and condition (funcall condition)))
        (save-match-data
          (if
              (ignore-errors (string-match begstr (match-string-no-properties 0)))
              (if permit-comment
		  (progn
		    (setq first nil)
		    (setq nesting (1- nesting)))
                (unless (ar-in-comment-p)
		  (setq first nil)
                  (setq nesting (1- nesting))))
            (when
                (ignore-errors (string-match endstr (match-string-no-properties 0))))
            (if permit-comment
                ;; non-nesting comments don't matter
                (unless (or (string= "" comment-end)(eq 10 comment-end))
		  (if first
		      (setq first nil)
		    (setq nesting (1+ nesting))))
              (unless (ar-in-comment-p)
		(if first
		    (setq first nil)
		  (setq nesting (1+ nesting))))))))))
  nesting)

(defun beginning-of-form-core (begstr endstr regexp searchform bound noerror nesting permit-comment permit-string condition)
  (when
      (or
       ;; (looking-back searchform (line-beginning-position))
       (and (or regexp (and begstr endstr))
	    (re-search-backward searchform bound noerror))
       (and (not regexp) (not (and begstr endstr))
	    (search-backward searchform bound noerror)))
    (setq nesting (beginning-of-form-base-intern nesting begstr endstr permit-comment permit-string condition))))

;; should `parse-sexp-ignore-comments' be usedq?
(defun beginning-of-form-base (begstr &optional endstr bound noerror nesting permit-comment regexp condition permit-string)
  "Assume being inside ary delimiters, go to start.

Bound limits search.
If NOERROR is non-nil, no error message is raised if not successful.
NESTING, a number, enables match of nested forms.
Set IN-COMMENT to `t' if forms inside comments should match - also for processing comments itself.
Set 7th argument REGEXP t, if beg/end-str are regular expressions.
CONDITION takes a function as argument perfoming the match.
If IN-STRING is non-nil, forms inside string match.
"
  (let* ((searchform (if (stringp begstr)
			 (cond ((and (string= begstr endstr))
				begstr)
			       ((and begstr endstr)
				(progn
				  (setq regexp t)
				  (concat begstr "\\|" endstr)))
			       (t begstr))
		       begstr))
         (nesting (or nesting 0))
         (orig (point))
         (permit-comment (or permit-comment ar-thing-inside-comments))
         beg-pos-delimiter end-pos-delimiter)
    (when
	(setq nesting (beginning-of-form-core begstr endstr regexp searchform bound noerror nesting permit-comment permit-string condition))
      (when (< nesting 1)
	(setq beg-pos-delimiter (match-beginning 0))
	(setq end-pos-delimiter (match-end 0))))
    (while
        (and
         (or (< 0 nesting)) (not (bobp)))
      (setq nesting (beginning-of-form-core begstr endstr regexp searchform bound noerror nesting permit-comment permit-string condition)))
    (when (eq nesting 0)
	(setq beg-pos-delimiter (match-beginning 0))
	(setq end-pos-delimiter (match-end 0)))
    (when (and beg-pos-delimiter end-pos-delimiter)
      (goto-char beg-pos-delimiter)
      (list beg-pos-delimiter end-pos-delimiter))))

(defun end-of-form-base-intern (nesting begstr endstr permit-comment permit-string &optional condition regexp)
  (let ((pps (parse-partial-sexp (point-min) (point))))
    ;; in string
    (if (and (not permit-string) (nth 3 pps)(nth 8 pps))
        (progn
          (forward-char 1)
          (while (and (setq pps (parse-partial-sexp (point-min) (point)))(nth 3 pps)(nth 8 pps))
            (forward-char 1)))
      (unless (save-match-data
                (and condition (funcall condition)))
        (save-match-data
          (if (string-match endstr (match-string-no-properties 0))
	      (progn
		(if permit-comment
		    (setq nesting (1- nesting))
		  (unless (ar-in-comment-p)
		    (setq nesting (1- nesting)))))
            ;; got another beginning while moving down
	    (if permit-comment
		(setq nesting (1+ nesting))
	      (unless (ar-in-comment-p)
		(setq nesting (1+ nesting))))))))
    nesting))

(defun end-of-form-core (begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)
  (when
      (or (and regexp (re-search-forward searchform bound noerror))
	  (search-forward searchform bound noerror))
    (setq nesting (end-of-form-base-intern nesting begstr endstr permit-comment permit-string condition))
    nesting))

(defun end-of-form-base (begstr endstr &optional bound noerror nesting permit-comment regexp condition permit-string)
  "Goto closing of a programming structure in this level.
As it stops one char after form, go one char back onto the last char of form.
Set comment to `t' if forms inside comments should match - also for processing comments itself.
If SHOW, display nesting and point in message buffer.
Set 7th argument REGEXP t, if beg/end-strings are regular expressions.
Optional arg CONDITION expects a function whose return value - `t' or a number - is checked for success, otherwise search continues.
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
         (permit-comment (or permit-comment ar-thing-inside-comments))
         beg-pos-delimiter end-pos-delimiter erg done)
    (when (looking-at begstr)
      (goto-char (match-end 0)))
    (when (and (< 1 (length endstr))(looking-at searchform))
      (goto-char (match-end 0)))
    ;; (and (string= (prin1-to-string (char-after)) endstr)
    ;; (forward-char 1)))
    (setq nesting (end-of-form-core begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror))
    ;; (when (< nesting 0)
    ;;   (setq beg-pos-delimiter (match-beginning 0))
    ;;   (setq end-pos-delimiter (match-end 0)))
    (while
        (and
         (< -1 nesting) (not (eobp)))
      (setq done t)
      (setq nesting (end-of-form-core begstr endstr regexp nesting permit-comment permit-string condition searchform bound noerror)))
    (if (ignore-errors (and (match-beginning 0) (match-end 0)))
	(progn
	  (goto-char (1- (match-end 0)))
	  (list (match-beginning 0) (match-end 0)))
      (goto-char orig))))

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

(defun ar-in-delimiter-base (regexp &optional condition guess-delimiter)
  "REGEXP expected of an unary delimiter, for example
\"\\\\\\\"\\\\\\\"\\\\\\\"\\\\|'''\\\\|\\\\\\\"\\\\|'\" indicating string delimiters in Python.
Optional second arg --a number, nil or `t'-- if interactively called. "
  (let ((orig (point))
        (count 0)
        tell beglist)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let* ((delimiter (when
                              (looking-at regexp)
                            (setq count (1+ count))
                            (match-string-no-properties 0)))
               (delimiter-p (stringp delimiter)))
          (if delimiter-p
              (progn
                (setq delimiter (concat "\\([^\\]\\)" (replace-regexp-in-string "\"" "\\\\\""  delimiter)))
                (setq beglist (list (match-beginning 0) (match-end 0)))
                (goto-char (match-end 0))
                (ar-in-delimiter-intern count orig beglist delimiter-p delimiter))
            (setq regxep (concat "[^\\]" regexp))
            (ar-in-delimiter-intern count orig beglist nil regexp)))))))

(defun ar-in-delimiter-intern (count orig beglist &optional first old)
  (let (done name this pps)
    (while (and (not done)(< (point) orig))
      (while (and (re-search-forward regexp orig t 1)
		  (if (save-excursion
			(goto-char (match-beginning 0))
			(ar-escaped))
		      (progn
			(while
			    (and
			     (re-search-forward regexp orig t 1)
			     (save-excursion
			       (goto-char (match-beginning 0))
			       (ar-escaped))))
			t)
		    t)
		  (setq first (point))
		  (not ar-thing-inside-comments)
		  (save-match-data
		    ;; (ar-in-comment-p-atpt)
		    (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
      (if first
          (progn
            (setq count (1+ count))
            (setq beglist (list (match-beginning 0) (match-end 0)))
            (when (eq 1 (% count 2))
              (goto-char (match-beginning 0))
              (setq delimiter (concat "\\([^\\]\\)" (match-string-no-properties 0) "\\|\\(\\\\\\\\\\)" (match-string-no-properties 0)))
              (setq old delimiter)
              (while (and (setq this (re-search-forward delimiter orig t 1))
                          (not ar-thing-inside-comments)
                          (save-match-data
                            ;; (ar-in-comment-p-atpt)
                            (or (progn (setq pps (syntax-ppss)) (nth 4 pps)) (nth 7 pps)))))
              (if this (setq count (1+ count))
                (setq done t)))
            (setq first nil))
        (setq done t)))
    (setq erg (and (< 0 count)(eq 1 (% count 2))))
    (when (and (< 0 count) erg)
      beglist)))

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
	erg last done)
    (cond ((and escaped comment)
	   (while
	       (ar--char-delimiters-forward char orig)
	     (setq counter (1+ counter)))
	   ;; try another one beyond limit set by orig
	   (when (and (not done) (ar--char-delimiters-forward char))
		 ;; don't raise counter, as it must remain uneven
	     (setq done t))
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
	     (while (and (not done) (ar--char-delimiters-forward char))
	       (unless
		   (or (ar-escaped) (ar-in-comment-p))
		 ;; don't raise counter, as it must remain uneven
		 (setq done t)))
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




(defun ar--delimiters-forward-intern (char escaped comment orig)
  "Expects being called from end of a delimited form."
  (let (last)
    (cond ((and escaped comment)
	   (when
	       (and (ar--char-delimiters-forward char) (eq (char-after) char))
	     (forward-char 1)
	     (setq last (point))))
	  (escaped
	   (when
	       (and (ar--char-delimiters-forward char)(eq (char-after) char))
	     (setq pps (parse-partial-sexp (point-min) (point)))
	     (unless
		 (nth 4 pps)
	       (setq last (point)))))
	  (comment
	   (when
	       (and (ar--char-delimiters-forward char)(eq (char-after) char))
	     (unless
		 (ar-escaped)
	       (setq last (point)))))
	  (t (when
		 (and (ar--char-delimiters-forward char)(eq (char-after) char))
	       (unless (or (ar-escaped) (ar-in-comment-p))
		 (setq last (point))))))
    last))

(defun ar-char-delimiters-forward (char &optional escaped comment)
  "Determine forward of forms delimited by a single character.

WRT forms delimited by a string or regexp use ‘end-of-form-base’.

ESCAPED: match also chars which are backslashed.
COMMENT: match also in comments
NO-CHECK: don't consider nesting"
  (let* ((orig (point))
    	 erg)
    ;; maybe not at end yet?
    (setq erg (ar-char-delimiters-end char ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer))
    (if (and erg (< orig (point)))
	(point)

      (setq erg (ar--delimiters-forward-intern char escaped comment orig))
      (when erg
	;; make sure the end of form is reached
	(when (ar-char-delimiters-end char ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)
	  (setq erg (point))))
      (unless erg
	(goto-char orig))
      erg)))

(defun ar-char-paren-delimiters-forward (char &optional escaped comment form)
  "Determine forward of forms delimited by a single character.

WRT forms delimited by a string or regexp use ‘end-of-form-base’.

optional ESCAPED: match also chars which are backslashed.
optional COMMENT: match also in comments
optional FORM: a symbol, for example 'bracketed
when provided, go to the end of FORM"
  (let* ((orig (point))
	 (erg (end-of-form-base (regexp-quote (char-to-string char)) (regexp-quote (char-to-string (ar--return-complement-char-maybe char))) nil 'move 0 nil t 'ar-syntax t)))
    ;; maybe not at end yet?
    (if (and erg (< orig (point)))
	(point)
      (setq erg (ar--delimiters-forward-intern char escaped comment orig))
      (when erg
	;; make sure the end of form is reached
	(setq erg
	      (end-of-form-base (regexp-quote (char-to-string char)) (regexp-quote (char-to-string (ar--return-complement-char-maybe char))) nil 'move 0 nil t 'ar-syntax t))
	erg))))

(provide 'beg-end)

;;; beg-end.el ends here
