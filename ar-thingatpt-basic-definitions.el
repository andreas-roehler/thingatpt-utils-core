;;; ar-thingatpt-basic-definitions.el --- Feste Definitionen -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Keywords: convenience

;; Expression

;;; Commentary:
;;

;;; Code:

(put 'expression 'beginning-op-at
     (lambda ()
       (ar-backward-expression)))

(put 'expression 'end-op-at
   (lambda ()
     (ar-forward-expression)))

;; Block
(put 'block 'beginning-op-at
    'ar-backward-block)

(put 'block 'end-op-at
     (lambda ()
       (ar-forward-block)))

;; Char
(put 'char 'beginning-op-at
   (lambda ()(point) ))

(put 'char 'end-op-at
     (lambda ()(unless (eobp) (goto-char (1+ (point))))))

(put 'char 'forward-op-at
     (lambda ()(unless (eobp) (goto-char (1+ (point))))))

;; String
(defcustom th-string-beg-delimiter "»‘“'\""
  "Specify the string start char."
  :type 'string
  :group 'werkstatt)

(defcustom th-string-end-delimiter "«”’'\""
  "Specify the string end char."
  :type 'string
  :group 'werkstatt)

;; String
(put 'string 'beginning-op-at
     (lambda ()
       (save-restriction
         ;; (widen)
         (or (and ar-use-parse-partial-sexp
                  (let* ((pps (parse-partial-sexp (point-min) (point)))
                         (pos8 (nth 8 pps)))
                    (when (nth 3 pps)
                      (goto-char pos8))))
             (member (char-before) (mapcar 'identity th-string-beg-delimiter))
             ;; (re-search-backward (concat "[" th-string-beg-delimiter "]+") nil 'move 1)
             (and (< 0 (abs (skip-chars-backward (concat "^" (char-to-string 8222) th-string-beg-delimiter))))
                  (prog1 (member (char-before) (mapcar 'identity th-string-beg-delimiter))
                    (backward-char))
                  )
             (when (or (eq 15 (car (syntax-after (point))))(eq 7 (car (syntax-after (point)))))
               (list (point) (save-excursion (skip-chars-forward (char-to-string (char-after)))(point)))))
         (cons (point) (1+ (point)))
         )))

(put 'string 'end-op-at
     (lambda ()
       (let (erg)
         (if (member (char-after) (list ?' ?\"))
             (progn
               (forward-sexp)
               (cons (1- (point)) (point)))
           (skip-chars-forward (concat "^" (char-to-string (setq erg (char-after)))))
           (when (eq (char-after) erg)
             (forward-char 1)
             (cons (1- (point)) (point)))))))

(put 'string 'forward-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward "^\"")))
         (eq (char-after) 34)
         (forward-char 2))))

(put 'string 'backward-op-at
     (lambda ()
       (when (and (< 0 (abs (skip-chars-backward "^\"")))
                  (eq (char-before) 34))
         (backward-char 1))))

;; ;; Strings
;; (put 'string 'beginning-op-at
;;      (lambda ()
;;        (save-restriction
;;       (widen)
;;       (if ar-use-parse-partial-sexp
;;           (let* ((pps (parse-partial-sexp (point-min) (point)))
;;                  (pos8 (nth 8 pps)))
;;             (when (nth 3 pps)
;;               (goto-char pos8)))
;;         (when
;;             (re-search-backward "\\([^\\\\]\\)\\(\"\\)" nil 'move 1)
;;           (goto-char (match-beginning 2))))
;;       (when (looking-at "\"*")
;;         (list (match-beginning 0) (match-end 0))))))

;; (put 'string 'end-op-at
;;      (lambda ()
;;        (save-restriction
;;       (widen)
;;       (forward-char 1)
;;       (if ar-use-parse-partial-sexp
;;           (let* ((orig (point))
;;                  (pps (parse-partial-sexp (point-min) (point)))
;;                  (char (char-to-string (nth 3 pps)))
;;                  (done t))
;;             (progn
;;               (while (and (not (eobp)) (prog1 done (forward-char 1))
;;                           (setq done (skip-chars-forward (concat "^" char)))

;;                           (nth 5 (parse-partial-sexp orig (point)))))
;;               (when (and (< orig (point))(looking-at char))
;;                 (list (match-beginning 0) (match-end 0)))))
;;         (when (re-search-forward "[^\\\\]\"" nil 'move 1)
;;           (list (match-beginning 0) (match-end 0)))))))

;; Abbrev
(put 'abbrev 'beginning-op-at
     (lambda ()
       (when
           (looking-at "[[:alnum:]]")
         (skip-chars-backward "[:alnum:].-")(point))))

(put 'abbrev 'end-op-at
     (lambda ()
       (skip-chars-forward "[:alnum:].-")(point)))

;; Acronym
(put 'acronym 'beginning-op-at
     (lambda ()
       (ar-th-gotobeg 'symbol)))

(put 'acronym 'end-op-at
     (lambda ()
       (when (or (looking-at "[\({]\\([A-Z][A-Za-z -]+\\)[\)}]")
                 (looking-at "\\(\\<[A-Z][A-Za-z]*[A-Z][A-Za-z]*\\>\\)"))
         (goto-char (match-end 0)))))

;; Buffer
(put 'buffer 'beginning-op-at
     (lambda ()
       (let ((pos (point-min)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

(put 'buffer 'end-op-at
     (lambda ()
       (let ((pos (point-max)))
         (unless (eq (point) pos)
           (goto-char pos)
           pos))))

;; Comment
(put 'comment 'beginning-op-at
     (lambda ()
       (let* ((nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg (when nesting
                     (if (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
                         (cons (match-beginning 0) (match-end 0))
                       (beginning-of-form-base comment-start comment-end nil 'move 1 t))))
               last)
         (unless erg
           (when (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
             (setq erg (cons (match-beginning 0) (match-end 0)))
             (setq last (point))
             (skip-chars-backward " \t\r\n\f"))
           (while (and (setq erg (nth 8 (parse-partial-sexp (point-min) (point)))) (goto-char erg) (setq last (point)))
             ;; (unless (bobp) (forward-char -1))
             (skip-chars-backward " \t\r\n\f"))
           (when last (goto-char last))
           (when (looking-at (concat "[ 	]*" (regexp-quote comment-start)))
             (setq erg (cons (match-beginning 0) (match-end 0)))))
         erg)))

(put 'comment 'end-op-at
     (lambda ()
       (let* ((nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg
               (when nesting
                 (when (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
                   (progn
                     (goto-char (match-end 0))
                     (ignore-errors (end-of-form-base comment-start comment-end nil 'move 0 t)))))))
         (unless erg
           (when
               (looking-at (concat "[ \t]*" (regexp-quote comment-start)))
             (setq erg (cons (1- (line-end-position)) (line-end-position)))
             (while (and (not (eobp))(forward-line 1)(or (ar-empty-line-p)(looking-at (concat "[ \t]*" (regexp-quote comment-start)))))
               (setq erg (cons (1- (line-end-position)) (line-end-position))))))
         erg)))

(defun comment-forward-op-intern ()
  (let ((orig (point)))
    (end-of-line)
    (setq orig (point))
    (unless (eobp)
      (skip-chars-forward " \t\r\n\f")
      (if (looking-at comment-start)
          (comment-forward-op-intern)
        (goto-char orig)))))

(put 'comment 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (if (string= comment-end "")
             (comment-forward-op-intern)
           (search-forward comment-end nil t)
           (comment-forward-op-intern))
         (when (< orig (point)) (point)))))

(defun backward-op-at-intern ()
  (let ((this (point))
         pps)
    (skip-chars-backward " \t\r\n\f")
    (unless (bobp)
      (forward-char -1)
      (if (setq pps (and (nth 4 (parse-partial-sexp (point-min) (point)))
                         (nth 8 (parse-partial-sexp (point-min) (point)))))
          (progn
            (goto-char pps)
            (backward-op-at-intern))
        (goto-char this)))))

(put 'comment 'backward-op-at
     (lambda ()
       (let ((orig (point))
             (pps (when (nth 4 (parse-partial-sexp (point-min) (point)))
                    (nth 8 (parse-partial-sexp (point-min) (point))))))
         (if pps
             (progn
               (goto-char pps)
               (backward-op-at-intern))
           (when (search-backward comment-start nil 'move 1)
             (backward-op-at-intern)))
         (when (< (point) orig) (point)))))

;;; CSV
;; Inspired by
;;; csv-mode.el --- major mode for editing comma-separated value files
;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; URL: http://centaur.maths.qmul.ac.uk/Emacs/
(defcustom ar-csv-separator-atpt ";"
  "Char to distinguish datasets in a `comma`-separated row"
  :type 'string
  :group 'werkstatt)
;; (when (boundp 'csv-separators)
;; (setq ar-separator-atpt csv-separators))

(put 'csv 'beginning-op-at
  (lambda ()
    (skip-chars-backward (concat "^" ar-csv-separator-atpt))(point)))

(put 'csv 'end-op-at
  (lambda ()
    (skip-chars-forward (concat "^" ar-csv-separator-atpt))(point)))

;; DATE
(put 'date 'beginning-op-at
   (lambda ()
    ;; provide for the case, we are over a
    ;; string-delimiter as `"'
    (when
      (and (not (eq 32 (if (featurep 'xemacs)
                           (encode-char (char-after) 'ucs)
                         (char-after))))
           (or (bobp)
               (eq 32 (if (featurep 'xemacs)
                          (encode-char (char-before) 'ucs)
                        (char-before)))))
    (forward-char 1)
    ;; as the bounds-function checks position, correct it
    ;; (setq th-orig 1)
    )
  (skip-chars-backward "0-9 .-")
  (skip-chars-forward " ")(point)))

(put 'date 'end-op-at
   (lambda ()
     (skip-chars-forward "0-9 .-")
     (skip-chars-backward " ")(point)))

;; Defun
(put 'defun 'beginning-op-at (lambda (&optional arg) (beginning-of-defun (or arg 1))(point)))

(put 'defun 'end-op-at (lambda (&optional arg)(end-of-defun (or arg 1))(point)))

(defvar delimited-start nil
  "Internal use only.")

(defvar delimited-end nil
  "Internal use only.")

;; Delimited
(defun delimited-atpt-intern--repeat (orig lower-bound upper-bound match-in-comment match-in-string)
  "Internal use only."
  (unless (bobp)
    (forward-char -1)
    (let ((pps (parse-partial-sexp (point-min) (point))))
      (when (and (nth 8 pps)
                 (not ar-match-in-string-or-comment))
        (goto-char (nth 8 pps))
        (unless (bobp)
          (forward-char -1)))
      (unless (looking-at (concat "[" th-begdel "]"))
        (skip-chars-backward (concat "^" th-begdel) lower-bound))
      (delimited-atpt-intern orig lower-bound upper-bound match-in-comment match-in-string))))

;; When at an unary delimiter, like single-quote, check, if it's a closer or opener.
;; 'True'False'True'False'...
;; Check forward from point, if a closer is found
;; Otherwise travel backward for another opener which is closed at or after start
(defun delimited-atpt-intern (orig &optional lower-bound upper-bound match-in-comment match-in-string)
  "Returns borders, a list, if successful."
  (let (erg)
    (or (cond
         ((and (eq (point) orig) (looking-at (concat "[" th-end-delimiter "]")))
          (if
              (progn (unless (eobp) (forward-char 1))
                     (save-match-data (setq erg (beginning-of-form-base (char-to-string (ar--return-complement-char-maybe (char-before))) (char-to-string (char-before)) lower-bound 'move 0 t 'ar-syntax))))
              (progn
                (setq delimited-end (list (match-beginning 0) (match-end 0)))
                (setq delimited-start erg))
            (delimited-atpt-intern--repeat orig lower-bound upper-bound match-in-comment match-in-string)))
         ;; At an unary delimiter
         ;; {-foo1-}
         ;; -|bar-
         ;; look forward first
         ((and (looking-at (concat "[" ar-delimiters-atpt "]"))
               (not (ar-escaped-p))
               (save-excursion
                 (when (< 0 (abs (skip-chars-forward (char-to-string (char-after)))))
                   (forward-char -1))
                 (setq delimited-start (list (match-beginning 0) (match-end 0)))
                 (and
                  ;; only single-char delimiters are supported for number-of-windows
                  ;; jump to the inner char maybe
                  (end-of-form-base (char-to-string (char-after)) (char-to-string (char-after)) upper-bound t 0 nil nil nil match-in-comment match-in-string)
                  (<= orig (point))
                  (setq delimited-end (list (match-beginning 0) (match-end 0)))))
               )
          (if (< (car delimited-start) (car delimited-end))
              delimited-start
            (delimited-atpt-intern--repeat orig lower-bound upper-bound match-in-comment match-in-string)))
         ((and (looking-at (concat "[" th-beg-delimiter "]"))
               (save-excursion
                 (and (save-match-data
                        (and (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) upper-bound t 0 t nil nil match-in-comment match-in-string)
                             (< orig (point))
                             (setq delimited-end (list (match-beginning 0) (match-end 0))))))))
          (list (match-beginning 0) (match-end 0)))
         (t (delimited-atpt-intern--repeat orig lower-bound upper-bound match-in-comment match-in-string)))
        (goto-char orig))))

(put 'delimited 'beginning-op-at
     (lambda ()
       (setq delimited-start nil)
       (setq delimited-end nil)
       (let* ((orig (point))
              (pps (parse-partial-sexp (point-min) (point)))
              (ar-match-in-string-or-comment (nth 8 pps))
              (match-in-comment (nth 4 pps))
              (match-in-string (nth 3 pps))
              (lower-bound (cond
                            ((and (nth 1 pps)(nth 8 pps))
                             (max (nth 1 pps)(nth 8 pps)))
                            ((nth 1 pps))
                            ((nth 8 pps))
                            ((or (eq 4 (car-safe (syntax-after (point))))(eq 7 (car-safe (syntax-after (point)))))
                             (point))))
              (lower-bound-syntax-p (and lower-bound (car-safe (syntax-after lower-bound))))
              (upper-bound (when lower-bound
                             (save-excursion
                               (goto-char lower-bound)
                               (ignore-errors (forward-sexp))
                               (and lower-bound (< lower-bound (point)) (point)))))
	      (upper-bound-syntax-p (and upper-bound (car-safe (syntax-after (- upper-bound 1))))))
         ;; if lower- and upper-bound are syntactically known delimiter, it's done
         (if (and lower-bound  upper-bound (or (eq orig lower-bound)(eq orig upper-bound)) (or (eq 7 lower-bound-syntax-p) (eq 7 upper-bound-syntax-p) (and (eq 4 lower-bound-syntax-p) (eq 5 upper-bound-syntax-p))))
             (progn
               (setq delimited-start (cons lower-bound (+ lower-bound 1))
                     delimited-end (cons (- upper-bound 1) upper-bound))
               delimited-start)
           ;; (save-restriction
           ;;   (narrow-to-region lower-bound upper-bound)
           ;;   (setq delimited-start (delimited-atpt-intern orig lower-bound upper-bound match-in-comment match-in-string)))
           (setq delimited-start (delimited-atpt-intern orig lower-bound upper-bound match-in-comment match-in-string))))))

(put 'delimited 'end-op-at
     (lambda ()
       (if (and delimited-start delimited-end)
           delimited-end
         (let ((begdel (concat th-beg-delimiter ar-delimiters-atpt)))
           (unless (looking-at (concat "[" begdel "]"))
             (funcall (get 'delimited 'beginning-op-at)))
           (if (looking-at (concat "[" begdel "]"))
               (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 t 'ar-syntax)
             (error "'delimited 'end-op-at: Can't see start of delimited form"))))))

(put 'delimited 'forward-op-at
     (lambda ()
       (let* ((begdel (concat th-beg-delimiter ar-delimiters-atpt))
              (erg (or
                    (ar-th-end 'delimited)
                    (unless (looking-at (concat "[" begdel "]"))
                      (funcall (get 'delimited 'beginning-op-at))))))
         (if
             erg
             (goto-char erg)
           (if (looking-at (concat "[" begdel "]"))
               (end-of-form-base (char-to-string (char-after)) (char-to-string (ar--return-complement-char-maybe (char-after))) nil 'move 0 t 'ar-syntax 'forward)
             (skip-chars-forward (concat "^" th-beg-delimiter ar-delimiters-atpt))
             (when (looking-at (concat "[" th-beg-delimiter ar-delimiters-atpt "]"))
               (list (match-beginning 0) (match-end 0))))))))

(put 'delimited 'backward-op-at
     (lambda ()
         (funcall (get 'delimited 'beginning-op-at))))

(defun ar-set-delimiter-zeichen ()
  (setq ar-delimiter-zeichen-atpt
        (if (featurep 'xemacs)
            (encode-char (char-after) 'ucs)
          (char-after))))

(defvar ar-delimiter-zeichen-atpt nil
  "Delimiter char found at place, search it backward then")
(make-variable-buffer-local 'ar-delimiter-zeichen-atpt)

(defvar ar-delimiter-string-atpt nil
  "Delimiter string found at place, search it backward then")
(make-variable-buffer-local 'ar-delimiter-string-atpt)

(defcustom ar-use-parse-partial-sexp t
  "When nil, parse symbolic expressions by regexp. "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-scan-whole-buffer-p nil
  "When non-nil, scan delimiters from point-min.

Otherwise assume being behind an opening delimiter or at a closing "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-delimiters-atpt "\\\\|\"'`#\\$/=?!:*+~§%&\\;@’"
  "Specify the delimiter chars. "
  :type 'string
  :tag "ar-delimiters-atpt"
  :group 'werkstatt)

(setq ar-delimiters-atpt "\\\\|\"'`#\\$/=?!:*+~§%&_\\;@’-")

(defvar th-beg-delimiter "»‘“{<[("
  "Specify the delimiter char.")

(defvar th-beg-delimiter-list (list ?» ?\‘ ?\“ ?{ ?< ?\[ ?\()
  "List the delimiter chars.")

(defvar th-end-delimiter "]}>”)’«"
  "Specify the delimiter char.")

(defvar th-begdel (concat th-beg-delimiter ar-delimiters-atpt)
  "Internally used only.")

;; Email
(put 'email 'beginning-op-at
  (lambda ()
  (when
      (looking-at "[^ \t]")
    (re-search-backward "[,;][[:graph:]]\\|<[[:graph:]]\\|^[[:graph:]]\\|[^[:graph:]][[:graph:]]" (line-beginning-position) t 1)
                        (when (looking-at "[[:space:];,<]")
                                          (forward-char 1)))))

(put 'email 'end-op-at
  (lambda ()
    (when (looking-at "[ <]\\{0,1\\}\\([\041-\132\136-\176]+@[\041-\132\136-\176]+\\)[;,> \t\n]*")
    (goto-char (match-end 1))
    (skip-chars-backward "[:punct:]"))(point)))

;; Filename
(if (featurep 'xemacs)
    (defcustom thingatpt-file-name-chars "@~//A-Za-z0-9ÄÖÜäöüß_.$?={}#%,:-"
      "Characters forseen in filenames. "
      :type 'string
      :group 'werkstatt)

  (defcustom thingatpt-file-name-chars "@~//[:alnum:]_.$?={}#%,:-"
    "Characters forseen in filenames. "
    :type 'string
    :group 'werkstatt))

(put 'filename 'beginning-op-at
     (lambda ()
       (unless
           (member (char-before) (list 32 ?\t 10 ?\r))
         (skip-chars-backward thingatpt-file-name-chars))(point)))

(put 'filename 'end-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward (concat "=" thingatpt-file-name-chars))))
            (skip-chars-backward ": ")(point))))

;; Filename-nondirectory
(if (featurep 'xemacs)
    (defcustom thingatpt-filenamenondirectory-chars "-~A-Za-z0-9ÄÖÜäöüß_.$?={}#%,: "
      "Characters forseen in filenames. "
      :type 'string
      :group 'werkstatt)

  (defcustom thingatpt-filenamenondirectory-chars "-~[:alnum:]_.$?={}#%,"
    "Characters forseen in filenames. "
    :type 'string
    :group 'werkstatt))

(put 'filenamenondirectory 'beginning-op-at
     (lambda ()
       (unless
           (member (char-before) (list 32 ?\t 10 ?\r))
         (skip-chars-backward thingatpt-filenamenondirectory-chars))(point)))

(put 'filenamenondirectory 'end-op-at
     (lambda ()
       (and (< 0 (abs (skip-chars-forward (concat "-=" thingatpt-filenamenondirectory-chars))))
            (skip-chars-backward ": ")(point))))

;; Floats
(put 'float 'beginning-op-at
  (lambda ()
     (when (numberp (read (buffer-substring-no-properties (point) (1+ (point)))))
     (skip-chars-backward "0-9.,"))(point)))

(put 'float 'end-op-at (lambda () (skip-chars-forward "[0-9.,]")(point)))

;; Function
(put 'function 'beginning-op-at
 (lambda ()
   (cond
    ((eq (point) (defun-beginning-position))
     (point))
    (t (beginning-of-defun)
       (point)))))

(put 'function 'end-op-at
  (lambda ()
    (end-of-defun)
    (when (string= major-mode "emacs-lisp-mode")
      (skip-chars-backward " \t\r\n"))(point)))

;; IP
(put 'ip 'beginning-op-at
  (lambda ()
    (unless (looking-at "\\s-")
      (skip-chars-backward "0-9."))(point)))

(put 'ip 'end-op-at
  (lambda ()
    (when (looking-at "[0-9]\\{1,3\\}.[0-9-]\\{1,3\\}.[0-9]\\{1,3\\}.[0-9]\\{1,3\\}")
      (goto-char (match-end 0)))(point)))

;; ISBN
(put 'isbn 'beginning-op-at
  (lambda ()
    (unless (looking-at "\\s-")
      (skip-chars-backward "0-9-")(point))))

(put 'isbn 'end-op-at
  (lambda ()
    (when (looking-at "[0-9]\\{1,3\\}[0-9-]\\{7,12\\}[0-9X]\\{0,1\\}")
      (goto-char (match-end 0)))))

;; Lines
(put 'line 'beginning-op-at (lambda () (beginning-of-line)(point)))

(put 'line 'end-op-at (lambda () (end-of-line)(point)))

;; List
(put 'list 'beginning-op-at
     (lambda ()
       (cond ((eq 4 (car (syntax-after (point))))
              (cons (point) (1+ (point))))
             (t (let ((pps (parse-partial-sexp (point-min) (point))))
                  (when (nth 1 pps)
                    (goto-char (nth 1 pps))
                    (cons (point) (1+ (point)))))))))

(put 'list 'end-op-at
     (lambda ()
       (when (eq 4 (car (syntax-after (point))))
         (forward-sexp)
         (forward-char -1)
         (cons (point)(1+ (point))))))

;; Markup
(defcustom markup-startstring-atpt "<[^<>]+>"
"Defining the beginning of a markup using ar-markup-atpt functions. "
:type 'string
:group 'werkstatt)

(defcustom markup-endstring-atpt "</[^<>]+>"
"Defining the end of a markup using ar-markup-atpt functions. "
:type 'string
:group 'werkstatt)

(put 'markup 'beginning-op-at
  (lambda ()
    (if (ignore-errors (looking-at markup-startstring-atpt))
        (list (match-beginning 0) (match-end 0))
      (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil t nil))))

(put 'markup 'end-op-at
  (lambda ()
    (let ((this-end (when (looking-at markup-startstring-atpt)
                      (match-string-no-properties 0))))
      (when (stringp this-end)
        (setq this-end (replace-regexp-in-string "<" "</" this-end))
        (end-of-form-base markup-startstring-atpt this-end nil 'move nil nil t)))))

;; Markup-no-nest
;; (put 'markup-no-nest 'beginning-op-at
;;      (lambda ()
;;        (if (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)
;;          (unless (bobp) (forward-char -1))
;;          (while (and (not (bobp) (not (ignore-errors (looking-at markup-startstring-atpt)))))
;;            (forward-char -1))
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))
;;
;; (put 'markup-no-nest 'end-op-at
;;      (lambda ()
;;        (when (ignore-errors (looking-at markup-startstring-atpt))
;;          (re-search-forward markup-endstring-atpt nil 'move 1)
;;          (when (ignore-errors (looking-at markup-startstring-atpt))
;;            (point)))))

;; Ml-data
(put 'mldata 'beginning-op-at
  (lambda ()
    (if (ignore-errors (looking-at markup-startstring-atpt))
        (match-end 0)
      (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil t)
      (when (ignore-errors (looking-at markup-startstring-atpt))
        (match-end 0)))))

(put 'mldata 'end-op-at
  (lambda ()
    (when (ignore-errors (looking-at markup-startstring-atpt))
      (end-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t)
      (re-search-backward markup-endstring-atpt nil 'move 1))))

;; Ml-tag
(put 'mltag 'beginning-op-at
     (lambda ()
       (if (ignore-errors
             (or
              (looking-at markup-startstring-atpt)
              (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))
         (unless (bobp) (forward-char -1))
         (while
             (and (not (bobp))
                  (not
                   (ignore-errors
                     (or
                      (looking-at markup-startstring-atpt)
                      (looking-at markup-endstring-atpt)))))
           (forward-char -1))
         (when
             (ignore-errors
               (or
                (looking-at markup-startstring-atpt)
                (looking-at markup-endstring-atpt)))
           (list (point) (1+ (point)))))))

(put 'mltag 'end-op-at
     (lambda ()
       (when (ignore-errors (or
                             (looking-at markup-startstring-atpt)
                             (looking-at markup-endstring-atpt)))
         (list (1- (match-end 0))(match-end 0)))))

;; Number
(put 'number 'beginning-op-at
     (lambda ()
       (let ((case-fold-search t))
         (when (looking-at "[#xo0-9a-f]")
           (cond ((eq (char-after) ?#)
                  (if (looking-at "#x[0-9a-f]+")
                      (point)
                    (when (looking-at "#o[0-9]+")
                      (point))))
                 ((eq (char-after) ?x)
                  (and (eq (char-before) ?#)
                       (progn
                         (forward-char -1)
                         (looking-at "#x[0-9a-f]+"))
                       (point)))
                 ((eq (char-after) ?o)
                  (and (eq (char-before) ?#)
                       (progn
                         (forward-char -1)
                         (looking-at "#o[0-9]+"))
                       (point)))
                 ((looking-back "#x[0-9a-f]*" (line-beginning-position))
                  (skip-chars-backward "^#" (line-beginning-position))
                  (forward-char -1)
                  (point))
                 ((looking-back "#o[0-9]*" (line-beginning-position))
                  (skip-chars-backward "^#" (line-beginning-position))
                  (forward-char -1)
                  (point))
                 ((looking-back "[0-9]+" (line-beginning-position))
                  (skip-chars-backward "0-9" (line-beginning-position))
                  (point))
                 ((looking-at "[0-9]+")
                  (point)))))))

(put 'number 'end-op-at
     (lambda ()
       (let ((case-fold-search t))
         (when (looking-at "[#x0-9a-f]")
           (cond ((looking-at "#x[0-9a-f]+")
                  (forward-char 2)
                  (skip-chars-forward "0-9a-f" (line-end-position))
                  (point))
                 ((looking-at "#o[0-9]+")
                  (forward-char 2)
                  (skip-chars-forward "0-9" (line-end-position))
                  (point))
                 ((looking-at "[0-9]+")
                  (skip-chars-forward "0-9" (line-end-position))
                  (point)))))))

(put 'number 'forward-op-at
     (lambda ()
       (unless (eobp)
         (let ((case-fold-search t)
               (erg
                (when
                    (cond ((looking-at "#[0-9a-fA-F]+|x[0-9]+")
                           (forward-char 2)
                           (skip-chars-forward "0-9a-f" (line-end-position))
                           (and (< 0 (skip-chars-forward "^0-9"))(point)))
                          ((looking-at "#o[0-9]+")
                           (forward-char 2)
                           (skip-chars-forward "0-9" (line-end-position))
                           (and (< 0 (skip-chars-forward "^0-9"))(point)))
                          ((looking-at "[0-9]+")
                           (skip-chars-forward "0-9" (line-end-position))
                           (and (< 0 (skip-chars-forward "^0-9"))(point)))
                          ((re-search-forward "#x[a-fA-F0-9]+\\|#o[0-8]+\\|[0-9]+e?" nil t 1)
                           (when (ignore-errors (match-beginning 0))
                             (goto-char (match-beginning 0)))))
                  (cond ((looking-at "#[xX][a-fA-F0-9]+")
                         (point))
                        ((looking-at "#o[0-9]+")
                         (point))
                        ((looking-at "[0-9]+")
                         (point))
                        ((eobp)
                         nil)))))
           erg))))

(put 'number 'backward-op-at
     (lambda ()
       (unless (bobp)
         (let ((case-fold-search t)
              erg)
          (cond ((and (looking-back "#?x?[0-9a-f]+" (line-beginning-position))
                      (goto-char (match-beginning 0))
                      (ar-number-atpt)))
                (t
                 (while
                     (or
                      (and
                       (re-search-backward "#?[xo]?[a-f0-9]+" nil t 1)
                       (goto-char (match-beginning 0))
                       (not (setq erg (ar-number-atpt))))))))
          erg))))

;; Name
(defcustom ar-name-chars-atpt "a-zA-Z_;-"
"Name is just a identifier for general use, described by chars composing it. "
:type 'regexp
:group 'werkstatt)

(put 'name 'beginning-op-at
  (lambda ()
    (skip-chars-backward ar-name-chars-atpt)
    (point)))

(put 'name 'end-op-at
  (lambda ()
    (when (looking-at (concat "[" ar-name-chars-atpt "]"))
      (skip-chars-forward ar-name-chars-atpt)
      ;; name may contain char `:' but not at the end, as
      ;; messages tend to insert it there
      (skip-chars-forward ar-name-chars-atpt)
      (skip-chars-backward ":")
      (point))))

;; Page
(put 'page 'beginning-op-at
  (lambda ()
    (backward-page)(point)))

(put 'page 'end-op-at
  (lambda ()
    (forward-page)(point)))

;; Paragraph
(defvar ar-this-paragraph-orig nil)

(defun ar-beginning-of-paragraph-intern ()
  (backward-paragraph)
  (skip-chars-forward " \t\r\n\f")
  (point))

(defun ar-end-of-paragraph-intern ()
  (forward-paragraph)
  (skip-chars-backward " \t\r\n\f")
  (point))

(put 'paragraph 'beginning-op-at
     (lambda ()
       (setq ar-this-paragraph-orig (point))
       (back-to-indentation)
       (when (and (eq (point) ar-this-paragraph-orig))
         (skip-chars-backward " \t\r\n\f"))
       (ar-beginning-of-paragraph-intern)))

(put 'paragraph 'end-op-at
     (lambda ()
       (ar-end-of-paragraph-intern)
       (if (eq (point) ar-this-paragraph-orig)
           (progn
             (skip-chars-forward " \t\r\n\f")
             (ar-end-of-paragraph-intern))
         (point))))

;; Paren
(put 'paren 'beginning-op-at
     (lambda ()
       (cond
        ((looking-at "\\s)")
         (forward-char 1) (backward-list 1))
        (t (while
               (and (< 0 (abs (skip-chars-backward "^(")))
                    (nth 8 (parse-partial-sexp (point-min) (point)))))
           (when (eq (char-before) ?\()
             (forward-char -1)
             (cons (point) (1+ (point))))))))

(put 'paren 'end-op-at
     (lambda ()
       (forward-list 1)
             (when (eq (char-before) ?\))
               (cons (1- (point)) (point)))))

;; Phone
(put 'phone 'beginning-op-at
  (lambda ()
    (when
        (and (looking-at "[0-9 \t.()-]")
             (not (eq (char-before) ?+)))
      (re-search-backward "[^0-9 \t.()-][0-9 ()\t-]+" (line-beginning-position) nil 1) (forward-char 1)(point))))

(put 'phone 'end-op-at
  (lambda ()
    (when
        (looking-at "[0-9;, \t()-]")
                    (re-search-forward "[0-9 \t.()-]+[^0-9 \t-]" (1+ (line-end-position)) nil 1) (forward-char -1))(point)))

;; Region
(defvar ar-region-end-atpt nil)
(put 'region 'beginning-op-at
  (lambda ()
    (setq ar-region-end-atpt (region-end))
    (goto-char (region-beginning))))

(put 'region 'end-op-at
  (lambda ()
    (goto-char ar-region-end-atpt)))

;; Sentence
(defvar ar-sentence-end-chars "[.!?]")

(defcustom ar-sentence-end-op-re "[.!?] *$\\|[[:alpha:]][^ \t\r\n\f0-9][.!?] *[^a-z]"
  ""
  :type 'regexp
  :group 'convenience)

(put 'sentence 'beginning-op-at
     (lambda ()
       (if (save-excursion
                 (and (looking-at "[A-Z]")
                      (progn
                        (skip-chars-backward " \t\r\n\f")
                        (or (bobp) (member (char-before) (list 63 ?! ?.))))))
           (point)
         (let ((limit (save-excursion (backward-paragraph)(point))))
           (while
               (and (not (bobp))
                    (or
                     (prog1
                         (re-search-backward "[.!?] *$\\|[.!?] *[^a-z]" limit t 1)
                       (forward-char 1)
                       (skip-chars-forward " \t\r\n\f"))
                     (prog1
                         (backward-paragraph)
                       (skip-chars-forward " \t\r\n\f")))
                    (nth 8 (parse-partial-sexp (point-min) (point))))))
         (point))))

(put 'sentence 'end-op-at
     (lambda ()
       (let ((orig (point)))
         (re-search-forward ar-sentence-end-op-re nil t 1)
         (skip-chars-backward "A-Z")
         (skip-chars-backward " \t\r\n\f")
         (when (< orig (point)) (point)))))

(put 'sentence 'forward-op-at
     (lambda ()
       (unless (eobp) (forward-char 1))
       (let ((orig (point)))
         (re-search-forward ar-sentence-end-op-re nil t 1)
         (skip-chars-backward "(A-Z")
         (skip-chars-backward " \t\r\n\f")
         (when (< orig (point)) (point)))))

(put 'sentence 'backward-op-at
     (lambda ()
       (backward-sentence)))

;; Sexp

(put 'sexp 'beginning-op-at
     (lambda ()
       (ar-backward-sexp)))

(put 'sexp 'end-op-at
     (lambda ()
       (ar-forward-sexp)))

(put 'sexp 'forward-op-at
     (lambda ()
       (ar-forward-sexp)))

(put 'sexp 'backward-op-at
     (lambda ()
       (ar-backward-sexp)))

;; Symbol
(put 'symbol 'beginning-op-at
     (unless (looking-at "\\s-")
       (lambda ()
         (let (erg)
           (while
               (or (when
                       (ar-escaped (if (bobp) (point)(1- (point))))
                     (forward-line -1)
                     (setq erg (point)))
                   (and (< 0 (abs (skip-syntax-backward "w_.'\\")))(setq erg (point)))))
           (unless erg (when (looking-at "[^ ]")(setq erg (point))))
           erg))))

(put 'symbol 'end-op-at
     (lambda ()
       (let (erg)
         (while
             (or (when
                     (ar-escaped)
                   (forward-char 1)
                   (setq erg (point)))
                 (and (< 0 (skip-syntax-forward "w_.'\\"))(setq erg (point)))))
         erg)))

(put 'symbol 'forward-op-at
  (lambda ()
           (skip-syntax-forward "^_")
           (and (< 0 (abs (skip-syntax-forward "_")))
                (point))))

(put 'symbol 'backward-op-at
  (lambda ()
           (skip-syntax-backward "^_")
           (and (< 0 (abs (skip-syntax-backward "_")))
                (point))))

;; Triplebackticked
(put 'triplebackticked 'beginning-op-at
   (lambda ()
       (beginning-of-form-base "```" "```" nil 'move 0 nil 'ar-syntax)))

(put 'triplebackticked 'end-op-at
  (lambda ()
    (end-of-form-base "```" "```" nil 'move 0 nil 'ar-syntax)))

(put 'triplebackticked 'forward-op-at
  (lambda ()
     (end-of-form-base "```" "```" nil 'move 0 nil 'ar-syntax t)))

(put 'triplebackticked 'backward-op-at
  (lambda ()
    (beginning-of-form-base "```" "```" nil 'move 0 nil 'ar-syntax)))

;; Triplequoted
(put 'triplequoted 'beginning-op-at
   (lambda ()
       (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 nil 'ar-syntax)))

(put 'triplequoted 'end-op-at
  (lambda ()
    (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 nil 'ar-syntax)))

(put 'triplequoted 'forward-op-at
  (lambda ()
     (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 nil 'ar-syntax t)))

(put 'triplequoted 'backward-op-at
  (lambda ()
    (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 0 nil 'ar-syntax)))

;; Triplequoted-Dq
(put 'triplequoteddq 'beginning-op-at
   (lambda ()
       (beginning-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 nil 'ar-syntax)))

(put 'triplequoteddq 'end-op-at
  (lambda ()
    (end-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 nil 'ar-syntax)))

(put 'triplequoteddq 'forward-op-at
  (lambda ()
     (end-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 nil 'ar-syntax t)))

(put 'triplequoteddq 'backward-op-at
  (lambda ()
    (beginning-of-form-base "\"\"\"" "\"\"\"" nil 'move 0 nil 'ar-syntax)))

;; Triplequoted-Sq
(put 'triplequotedsq 'beginning-op-at
   (lambda ()
       (beginning-of-form-base "'''" "'''" nil 'move 0 nil 'ar-syntax)))

(put 'triplequotedsq 'end-op-at
  (lambda ()
    (end-of-form-base "'''" "'''" nil 'move 0 nil 'ar-syntax)))

(put 'triplequotedsq 'forward-op-at
  (lambda ()
     (end-of-form-base "'''" "'''" nil 'move 0 nil 'ar-syntax t)))

(put 'triplequotedsq 'backward-op-at
  (lambda ()
    (beginning-of-form-base "'''" "'''" nil 'move 0 nil 'ar-syntax)))

;; Url
;; use thingatpt.el's form here too
(put 'url 'end-op-at (get 'url 'end-op))
(put 'url 'beginning-op-at (get 'url 'beginning-op))

(defcustom url-at-point-chars ":/?#[]@!$&()*+,;=[:alnum:]-._~"
"Chars which might compose a URL. "
:type 'string
:group 'werkstatt)

;; Whitespace
(put 'whitespace 'beginning-op-at
(lambda () (when (looking-at "[ \t]") (skip-chars-backward "[ \t\r\n[:blank:]]")(point))))

(put 'whitespace 'end-op-at (lambda () (skip-chars-forward " \t\r\n[:blank:]")(point)))

;; Word
(put 'word 'beginning-op-at
(lambda () (when (looking-at "\\w")
             (unless (or (looking-back "\\W" (line-beginning-position))(bolp))
               (forward-word -1))
             (point))))

(put 'word 'end-op-at
     (lambda () (when (looking-at "\\w")
		  (forward-word 1)(point))))

;; Word-alpha-only
(put 'wordalphaonly 'beginning-op-at
  (lambda () (when (looking-at "[[:alpha:]]")
             (unless (looking-back "[^[:alpha:]]" (line-beginning-position))
               (skip-chars-backward "[:alpha:]")
               (point)))))

(put 'wordalphaonly 'end-op-at
  (lambda () (when (and (looking-back "[^[:alpha:]]" (line-beginning-position))(looking-at "[[:alpha:]]" (line-beginning-position)))
             (skip-chars-forward "[:alpha:]")
             (point))))

(defun gen--in-string-p-intern (pps)
  (goto-char (nth 8 pps))
  (list (point) (char-after)(skip-chars-forward (char-to-string (char-after)))))

(defun gen-in-string-p ()
  "if inside a double- triple- or singlequoted string,

If non-nil, return a list composed of
- beginning position
- the character used as string-delimiter (in decimal)
- and length of delimiter, commonly 1 or 3 "
  (interactive "p")
  (save-excursion
    (let* ((pps (parse-partial-sexp (point-min) (point)))
           (erg (when (nth 3 pps)
                  (gen--in-string-p-intern pps))))
      (unless erg
        (when (looking-at "\"\\|'")
          (forward-char 1)
          (setq pps (parse-partial-sexp (line-beginning-position) (point)))
          (when (nth 3 pps)
            (setq erg (gen--in-string-p-intern pps)))))

    ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
    erg)))

;;
(defcustom copy-or-alternative "word"
"Copy-or commands may act on thing specified here.

For example when ‘ar-doublequote-or-copy-atpt’ is called with positive
argument but without active region and also thing-at-point
 --i.e. doublequoted here-- doesn't exist,
it would doublequote a word at point "
:type 'string
:group 'werkstatt)

(defcustom ar-install-directory "~/werkstatt"
  "Directory where thingatpt-utils are installed"
  :type 'string
  :group 'werkstatt)

;; (update-directory-autoloads (expand-file-name ar-install-directory))

(defun ar--transform-generic-delimited-atpt (replacement)
  (interactive "*")
  (let* ((bounds (ar-bounds-of-delimited-atpt))
         (startcharnew (if (consp replacement)
                           (car replacement)
                         replacement))
         (endcharnew (if (consp replacement)
                         (cdr replacement)
                       replacement))
         (beg (or (and (numberp (car bounds))(car bounds))
                  (and (numberp (car bounds))(car bounds))))
         (end (or
               (ignore-errors (and (numberp (cdr bounds)) (cdr bounds)))
               (ignore-errors (and (numberp (cadr (cadr bounds)))(cadr (cadr bounds))))
               (ignore-errors (and (numberp (cadr bounds))(cadr bounds)))
               (ignore-errors (and (numberp (cdr (cadr bounds)))(cdr (cadr bounds))))
)))
    (if (ignore-errors (numberp beg))
        (save-excursion
          (goto-char beg)
          (delete-char 1)
          (insert startcharnew)
          (if (numberp end)
              (progn
                (goto-char end)
                (delete-char -1)
                (insert endcharnew))
            (error "ar--transform-generic-delimited-atpt: Don't see end")))
      (error "ar--transform-generic-delimited-atpt: Don't see start"))))

;; ar-insert-thingatpt-th-funktionen start

(defun ar-toggle-thing-copy-region ()
  (interactive)
  (setq thing-copy-region (not thing-copy-region)))

(defun ar-th-bounds (thing &optional no-delimiters)
  "Determine the start and end buffer locations for the THING at point.
  THING is a symbol which specifies the kind entity you want.

  Boolean value NO-DELIMITERS: boundaries are excluded.
  Call THING by his name, i.e. ar-word-atpt etc.

"
  ;; (setq ar-th-bounds-backfix nil)
  (ignore-errors
    (cond ((eq thing 'region)
	   (ignore-errors (cons (region-beginning) (region-end))))
	  ((eq thing 'char)
	   (cons (point) (1+ (point))))
	  (t (save-excursion
	       (save-restriction
		 (let* ((beg-raw (funcall (get thing 'beginning-op-at)))
                        (beg
                         (if (consp beg-raw)
                             (if no-delimiters
                                 (cond ((car-safe (cdr-safe beg-raw)))
                                       ((cdr-safe beg-raw))
                                       (t (cadr beg-raw)))
                               (car beg-raw))
                           beg-raw))
			(end-raw (and beg (goto-char (if (consp beg-raw) (car beg-raw) beg)) (funcall (get thing 'end-op-at))))
                        (end
                         (if (consp end-raw)
                             (if no-delimiters
                                 (car end-raw)
                               (cond ((numberp (cdr-safe end-raw))
                                      (cdr end-raw))
                                     (t (cadr end-raw))))
                           end-raw)))
                   (and beg end (cons beg end))
)))))))

(defun ar-th (thing &optional arg)
  "Returns a buffer substring according to THING.
  THING may be a well known form as ‘symbol’,
  ‘list’, ‘sexp’, ‘defun’ or a newly defined THING.
  When mark-thingatpt is ‘t’ - the default - a found THING
  is set as current region, enabling further action on them

  If NO-DELIMITERS set by user functions, THING returned is
  stripped by delimiters resp. markup
 "
  (condition-case nil
      (let* ((no-delimiters (eq 4 (prefix-numeric-value arg)))
             (iact arg)
	     (bounds (ar-th-bounds thing no-delimiters))
	     (beg (if no-delimiters
		      (cond ((ignore-errors (numberp (car-safe bounds)))
			     (car-safe bounds))
			    ((ignore-errors (car bounds))
			     (car bounds))
			    (t (car-safe bounds)))
		    (cond ((consp bounds)
                           (car bounds))
                          ((listp bounds)
                           (car (flatten-list bounds))))))
             ;;  (ignore-errors (car bounds))
	     ;;  (car bounds))
	     ;; (t (car-safe bounds)))))
	     (end (if no-delimiters
                      (cond
                       ((numberp (cdr-safe bounds))
                        (cdr-safe bounds)))
                    (cond ((consp bounds)
                           (cond ((numberp (cdr-safe bounds))
                                  (cdr bounds))
                                 ((numberp (ignore-errors (cadr bounds)))
                                  (cadr bounds))))
                          ((listp bounds)
                           (car (flatten-list (reverse bounds)))))))
	     erg)
	(when (and beg end)
	  (setq erg
		(buffer-substring-no-properties beg end))
	  (when (or iact thing-copy-region)
	    (ar-th-mark thing nil beg end no-delimiters))
	  (when thing-copy-region (kill-new erg))
	  erg))
    (error nil)))

(defun ar-th-beg (thing &optional no-delimiters)
  "Return beginning position of THING. "
  (condition-case nil
      (let ((bounds (ar-th-bounds thing no-delimiters)))
	(ignore-errors (or (ignore-errors (car bounds)) (car-safe bounds))))
    (error nil)))

(defun ar-th-end (thing &optional no-delimiters)
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
             (end (cond
                   ((numberp (cdr-safe bounds))
                    (cdr-safe bounds))
                   ((numberp (ignore-errors (cadr bounds)))
                    (cadr bounds))
                   ((numberp (ignore-errors (cadr (cadr bounds))))
                    (cadr (cadr bounds)))
                   ((numberp (ignore-errors (cdr (cadr bounds))))
                    (cdr (cadr bounds))))))
	end)
    (error nil)))

(defun ar-th-gotobeg (thing &optional no-delimiters)
  "Goto char beginning, core function "
  ;; (goto-char (car-safe (car-safe (ar-th-bounds thing no-delimiters))))
  (goto-char (car (flatten-list (ar-th-bounds thing no-delimiters)))))

(defun ar-th-gotoend (thing &optional no-delimiters)
  "Goto char end, core function "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters)))
             ;; (end (cond
             ;;       ((numberp (cdr-safe bounds))
             ;;        (cdr-safe bounds))
             ;;       ((numberp (ignore-errors (cadr bounds)))
             ;;        (cadr bounds))
             ;;       ((numberp (ignore-errors (cadr (cadr bounds))))
             ;;        (cadr (cadr bounds)))
             ;;       ((numberp (ignore-errors (cdr (cadr bounds))))
             ;;        (cdr (cadr bounds))))))
	;; (goto-char end)
	(goto-char (cdr bounds))
        (sit-for 0.1)
	;; (forward-char -1)
	(cons (point) (1+ (point))))
    (error (concat (format "%s: " thing) "ar-th-gotoend failed"))))

(defun ar-th-length (thing &optional no-delimiters)
  (ignore-errors
    (let* ((bounds (ar-th-bounds thing no-delimiters))
	   (beg (car bounds))
           (end (cond
               ((numberp (cdr-safe bounds))
                (cdr-safe bounds))
               ((numberp (ignore-errors (cadr bounds)))
                (cadr bounds))
               ((numberp (ignore-errors (cadr (cadr bounds))))
                (cadr (cadr bounds)))
               ((numberp (ignore-errors (cdr (cadr bounds))))
                (cdr (cadr bounds)))))
	   ;;(end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))
	   (length (- end beg)))
      length)))

(defun ar-th-ratio-base (cla elt &optional beg end ratio)
  (let ((beg
         (cond (beg beg)
               ((use-region-p)
                (region-beginning))
               (t
                (funcall (intern-soft (concat "ar-" (format "%s" elt) "-beginning-position-atpt"))))))
	(end
         (cond (end (copy-marker end))
               ((use-region-p)
                (copy-marker (region-end)))
               (t
                (condition-case nil (copy-marker (funcall (intern-soft (concat "ar-" (format "%s" elt) "-end-position-atpt")))) (error nil))))))
    (ar-th-ratio elt cla beg end ratio)))

(defun ar-th-ratio (thing cla &optional beg end ratio no-delimiters)
  (save-excursion
    (ignore-errors
      (let* (bounds
             (beg (or beg (and (setq bounds (ar-th-bounds thing no-delimiters)) (car bounds))))
             (end (or end
                      (cond
                       ((numberp (cdr-safe bounds))
                        (cdr-safe bounds))
                       ((numberp (ignore-errors (cadr bounds)))
                        (cadr bounds))
                       ((numberp (ignore-errors (cadr (cadr bounds))))
                        (cadr (cadr bounds)))
                       ((numberp (ignore-errors (cdr (cadr bounds))))
                        (cdr (cadr bounds))))))
             (matchcount 0)
             (erg 0)
             len)
        (goto-char beg)
        (setq erg
              (cond ((member cla ar-atpt-classes)
                     (if (featurep 'xemacs)
                         (string-to-number (string-strip (count-matches (eval cla)) nil "a-z "))
                       (count-matches (concat "[[:" (format "%s" cla) ":]]") (or beg (point-min)) (or end (point-max)))))
                    (t (if (functionp (intern-soft (concat "ar-forward-" (format "%s" cla) "-atpt")))
                           (progn
                             (while (and (< (point) end)
                                         (funcall (intern-soft (concat "ar-forward-" (format "%s" cla) "-atpt"))))
                               (setq matchcount (1+ matchcount)))
                             matchcount)
                         (while (and (< (point) end)
                                     (search-forward cla end t 1))
                           (setq matchcount (1+ matchcount)))
                         matchcount))))
        (when ratio
          (progn
            (setq len (string-to-number (format "%f" (- end beg))))
            (setq erg (/ matchcount len))
            erg))
        erg))))

(defun ar-th-copy (thing &optional no-delimiters)
  (condition-case nil
      (let ((newcopy (ar-th thing no-delimiters)))
	(when newcopy
          (progn
            (unless (string= newcopy (car kill-ring)) (kill-new newcopy))
            newcopy)))
    (error nil)))

(defun ar-th-trim (thing &optional no-delimiters left right)
  "Trims given THING at point.
If boundaries of thing are know, use ‘ar-th-trim-base’ directly. "
  (let* ((bounds (ar-th-bounds thing no-delimiters))
         (beg (or (ignore-errors (car bounds)) (car-safe bounds)))
         (end (cond
               ((numberp (cdr-safe bounds))
                (cdr-safe bounds))
               ((numberp (ignore-errors (cadr bounds)))
                (cadr bounds))
               ((numberp (ignore-errors (cadr (cadr bounds))))
                (cadr (cadr bounds)))
               ((numberp (ignore-errors (cdr (cadr bounds))))
                (cdr (cadr bounds))))))
    (ar-th-trim-base beg end left right)))

(defun ar-th-trim-base (beg end left right)
  "Trim buffer-substring-point.

Arg LEFT-TRIM: trim left
Arg RIGHT-TRIM: trim right. "
  (save-excursion
    (let ((beg (copy-marker beg))
	  (end (copy-marker end))
	  (old-end end))
      (cond ((and left right)
	     (goto-char end)
	     (delete-char -1)
	     (goto-char beg)
	     (delete-char 1)
	     (eq (marker-position end) (- old-end 2)))
	    (right
	     (goto-char end)
	     (delete-char -1)
	     (eq (marker-position end) (- old-end 1)))
	    (left
	     (goto-char beg)
	     (delete-char 1)
	     (eq (marker-position end) (- old-end 1)))
	    (t (goto-char end)
	       (delete-char -1)
	       (goto-char beg)
	       (delete-char 1)
	       (eq (marker-position end) (- old-end 2)))))))

(defun ar-th-trim-left (thing &optional no-delimiters)
  (ar-th-trim thing no-delimiters t))

(defun ar-th-trim-right (thing &optional no-delimiters)
  (ar-th-trim thing no-delimiters nil t))

(defun ar-th-peel (thing &optional no-delimiters)
  "Remove the outer element of an hierarchical form.

\(foo (bar baz)) --> (bar baz)
--^-----------

\[foo [bar baz]] --> [bar baz]
--^-----------

Inspired by stuff like ‘paredit-splice-sexp-killing-backward’;
instead of working ‘-backward’ or ‘-forward’ deletes expression at point.

"
  (let* ((bounds (ar-th-bounds thing no-delimiters))
	 (beg (car bounds))
	 (end (copy-marker
                     (cond
                      ((numberp (cdr-safe bounds))
                       (cdr-safe bounds))
                      ((numberp (ignore-errors (cadr bounds)))
                       (cadr bounds))
                      ((numberp (ignore-errors (cadr (cadr bounds))))
                       (cadr (cadr bounds)))
                      ((numberp (ignore-errors (cdr (cadr bounds))))
                       (cdr (cadr bounds)))))
                                        ;(or (ignore-errors (cadr (cadr bounds)))(cdr (cadr bounds))(car (cadr bounds)))
                    ))

    (when (eq (point) beg)(forward-char 1))
    (skip-syntax-forward "^(")
    (forward-sexp)
    (delete-region (point) end)
    (backward-sexp)
    (delete-region (point) beg)))

(defun ar-th-comment (thing &optional no-delimiters)
  "Comment or uncomment THING "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (car bounds))
             (end (cond
               ((numberp (cdr-safe bounds))
                (cdr-safe bounds))
               ((numberp (ignore-errors (cadr bounds)))
                (cadr bounds))
               ((numberp (ignore-errors (cadr (cadr bounds))))
                (cadr (cadr bounds)))
               ((numberp (ignore-errors (cdr (cadr bounds))))
                (cdr (cadr bounds)))))
	     ;; (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))
             )
	(when (and beg end)
	      (goto-char beg)
	      (comment-region beg end)))
    (error nil)))

(defun ar-th-mark (thing &optional bounds beg end no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (unless (and beg end) (or bounds (ar-th-bounds thing no-delimiters))))
	     (beg (or beg (ignore-errors (car bounds))))
             (end (or end
                      (cond
                       ;; ((numberp (car-safe bounds))
                       ;;  (car-safe bounds))
                       ((numberp (cdr-safe bounds))
                        (cdr-safe bounds))
                       ((numberp (ignore-errors (cadr bounds)))
                        (cadr bounds))
                       ((numberp (ignore-errors (cadr (cadr bounds))))
                        (cadr (cadr bounds)))
                       ((numberp (ignore-errors (cdr (cadr bounds))))
                        (cdr (cadr bounds)))))
	          ;; (end (or end (or (ignore-errors (cadr (cadr bounds))) (ignore-errors (cdr (cadr bounds))))))
                  ))
        (when (and beg end)
	  (goto-char beg)
	  (push-mark (point) t t)
	  (goto-char end)
	  (exchange-point-and-mark)))
    (error nil)))

;; uses sgml-tag from sgml-mode.el
(defun ar-th-hide (thing &optional beg end no-delimiters)
  "Hide visibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (or (ignore-errors (car bounds))(car-safe bounds)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (if (and beg end)
        (progn
          (hs-make-overlay beg end 'code)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

;;;###autoload
(defun ar-th-show (thing &optional beg end no-delimiters)
  "Remove invisibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (or (ignore-errors (car bounds))(point-min)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr end))(point-max))))
    (if (and beg end)
        (progn
          (hs-discard-overlays beg end)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

(defun ar-th-hide-show (&optional thing beg end no-delimiters)
  "Toggle visibility of existing things at point. "
  (interactive "p")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (beg (or beg (and (use-region-p) (region-beginning))))
        (end (or end (and (use-region-p) (region-end))))
        bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters))
      (setq beg (car bounds))
      (setq end (cadr (cadr bounds))))
    (if (overlays-in beg end)
        (hs-discard-overlays beg end)
      (hs-make-overlay beg end 'code))
    (set-buffer-modified-p modified)))

(defun ar-th-separate (thing &optional no-delimiters)
  "Optional CHECK is ignored "
  (let* ((bounds (ar-th-bounds thing no-delimiters))
	 (beg (car bounds))
	 (end (copy-marker
               (cond
                ((numberp (cdr-safe bounds))
                 (cdr-safe bounds))
                ((numberp (ignore-errors (cadr bounds)))
                 (cadr bounds))
                ((numberp (ignore-errors (cadr (cadr bounds))))
                 (cadr (cadr bounds)))
                ((numberp (ignore-errors (cdr (cadr bounds))))
                 (cdr (cadr bounds)))))))
    (when beg (goto-char beg)
	  (when (or (ar-buffer-narrowed-p) (not (looking-back "^[ \t\f\r]*" (line-beginning-position))))
	    (newline ar-newlines-separate-before))
	  (indent-according-to-mode)
	  (goto-char end)
	  (save-excursion
            (when (not (looking-at "[ \t\f\r]*$"))
	      (newline 1)
	      (indent-according-to-mode))))))

(defun ar-thing-in-thing (thing-1th thing-2th th-function)
  "Addresses things of 1th kind within the borders of the 2th,
If optional positions BEG-2TH END-2TH are given, works on them instead. "
  (let* ((bounds (ar-th-bounds thing-2th))
	 ;; take the inner pos of a delimiter
	 (beg (or
	       (ignore-errors (car (cdr (car-safe bounds))))
	       (ignore-errors (car bounds))
               (car-safe bounds)))
	 ;; take the inner pos of a delimiter
         (end (copy-marker
               (cond
                ((numberp (cdr-safe bounds))
                 (cdr-safe bounds))
                ((numberp (ignore-errors (cadr bounds)))
                 (cadr bounds))
                ((numberp (ignore-errors (cadr (cadr bounds))))
                 (cadr (cadr bounds)))
                ((numberp (ignore-errors (cdr (cadr bounds))))
                 (cdr (cadr bounds))))))
	 (last 1)
         stop
	 inner-end done)
    ;; (sit-for 0.1)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        ;; (if (eq th-function 'ar-th-sort)
        ;;     (ar-th-sort thing-1th nil beg end nil nil nil)
	(when (numberp inner-end) (goto-char inner-end))
	(while
	    (and
             (not stop) (not (or (eobp)
                                 ;; (eq 'char thing-1th)
                                 ))
             (or (not done) (< last (point)))
	     (prog1 (setq last (point))
               (unless (eobp) (funcall th-function thing-1th)))
	     (setq done t))
	  ;; (unless (or (and (eobp) (setq stop t)) (eq 'char thing-1th))w
	  (unless (or (eobp) (eq 'char thing-1th))
            (unless (setq inner-end (ar-th-forward thing-1th 1))
              (setq stop t))))))))

(defun ar-th-kill (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (or (ignore-errors (car bounds)) (ignore-errors (car bounds))))
             (end (cond
                   ((numberp (cdr-safe bounds))
                    (cdr-safe bounds))
                   ((numberp (ignore-errors (cadr bounds)))
                    (cadr bounds))
                   ((numberp (ignore-errors (cadr (cadr bounds))))
                    (cadr (cadr bounds)))
                   ((numberp (ignore-errors (cdr (cadr bounds))))
                    (cdr (cadr bounds)))))
	     ;; (end (and beg (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cdr bounds)))))
             )
	(and beg end
	     (if (eq thing 'comment)
                 (kill-region beg (min (point-max) (1+ end)))
	       (progn
		 (kill-region beg end)
		 t))))
    (error nil)))

(defun ar-th-delete (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (or (ignore-errors (car bounds)) (ignore-errors (car bounds))))
             (end (cond
                   ((numberp (cdr-safe bounds))
                    (cdr-safe bounds))
                   ((numberp (ignore-errors (cadr bounds)))
                    (cadr bounds))
                   ((numberp (ignore-errors (cadr (cadr bounds))))
                    (cadr (cadr bounds)))
                   ((numberp (ignore-errors (cdr (cadr bounds))))
                    (cdr (cadr bounds)))))
	     ;; (end (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cadr (cadr bounds)))))
             )
        (if (eq thing 'comment)
            (delete-region beg (min (point-max) (1+ end)))
          (delete-region beg end)))
    (error nil)))

(defun ar-th-delete-in-region (thing beg end &optional no-delimiters)
  "Delete THING in region. Delete line, if empty afterwards. "
  (save-excursion
    (goto-char beg)
    (let ((orig (point))
          (end (copy-marker end)))
      (while (progn (ar-th-forward thing) (and (< orig (point)) (< (point) end)))
	(let ((bounds (ar-th-bounds thing no-delimiters)))
	  (delete-region (car bounds) (cadr (cadr bounds)))
	  (when (and (empty-line-p) (not (eobp)))
	    (delete-region (line-beginning-position) (1+ (line-end-position)))))))))

(defun ar-th-commatize (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
             (end (cond
                   ((numberp (cdr-safe bounds))
                    (cdr-safe bounds))
                   ((numberp (ignore-errors (cadr bounds)))
                    (cadr bounds))
                   ((numberp (ignore-errors (cadr (cadr bounds))))
                    (cadr (cadr bounds)))
                   ((numberp (ignore-errors (cdr (cadr bounds))))
                    (cdr (cadr bounds))))))
	(goto-char end)
        (insert ","))
    (error nil)))

(defun ar-th-quote (thing &optional no-delimiters)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters))
	     (beg (car bounds)))
         (goto-char beg)
         (insert "'"))
    (error nil)))

;; (defun ar-th-triplequotedq (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "\"\"\"" "\"\"\"" no-delimiters))

;; (defun ar-th-triplequotesq (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "'''" "'''" no-delimiters))

;; (defun ar-th-triplebacktick (thing &optional no-delimiters)
;;   " "
;;   (ar-th-delim thing "```" "```" no-delimiters))

(defun ar-th-interactive-backward-form (ap ep)
  (goto-char ep)
  (push-mark ap)
  (exchange-point-and-mark)
  (kill-new (buffer-substring-no-properties ap ep)))

(defun ar-th-forward-function-call (thing arg)
  (let (erg)
    (while (< 0 arg)
      (setq erg (funcall (get thing 'forward-op-at)))
      (setq arg (1- arg)))
    erg))

(defun ar-th-backward-function-call (arg thing)
  (let (erg)
    (while
	(> 0 arg)
      (setq erg (funcall (get thing 'backward-op-at)))
      (setq arg (1+ arg)))
    erg))

(defun ar-th-forward (thing &optional arg)
  "With positive ARG, go to beginning of next THING.

Return position, if successful, nil otherwise.
Move backward with negative argument "
  (let* ((orig (point))
	 (arg (or arg 1)))
    (if (< 0 arg)
        (progn
          (funcall (get thing 'forward-op-at))
          (and (< orig (point)) (point)))
      (and (funcall (get thing 'backward-op-at))
           (< (point) orig)(point))
      )))

(defun ar-th-un-ml (thing &optional beg end)
  (save-excursion
    (save-restriction
      (when (and beg end)
        (narrow-to-region beg end))
      (let\* ((startstring (eval (intern-soft (concat (format "%s" thing) "-startstring-atpt"))))
             (endstring (concat (eval (intern-soft (concat (format "%s" thing) "-endstring-atpt")))))
             (begstringpos
              (progn
                (beginning-of-form-base startstring endstring)
                (if (looking-at startstring)
                    (list (match-beginning 0) (match-end 0))
                  (error "Can't see startstring"))))
             (thisbeg (copy-marker (car begstringpos)))
             thisend)
        (forward-char 1)
        (end-of-form-base startstring endstring)
        (when (looking-back endstring (line-beginning-position))
          (replace-match "")
          (setq thisend (point-marker))
          (delete-region (car begstringpos) (cadr begstringpos))
          (list thisbeg thisend))))
    (widen)))

(defun ar-th-backward (thing &optional arg)
  "Returns beg and end of THING before point as a list. "
  (ar-th-forward thing (- (or arg 1))))

(defvar paired-start-pos nil)

(defun ar-th-transpose (thing &optional no-delimiters)
  "Returns position, when called from a program
 end of transposed section. "
  (let* ((pos (point-marker))
         (first (ar-th-bounds thing no-delimiters))
         (pos1 (if (ignore-errors (<= (car first) pos))
                   first
                 (ar-th-bounds thing no-delimiters)))
         (pos2 (progn
                  (ar-th-forward thing no-delimiters)
                 (ar-th-bounds thing no-delimiters)))
         (a (car pos1))
         (b (copy-marker (cdr pos1)))
         (c (car pos2))
         (d (copy-marker (cdr pos2))))
    (transpose-regions a b c d)
    d))

(defalias 'ar-th-sort 'sort)
;; credits to sort-subr, sort.el
;; (defun ar-th-sort (thing reverse beg end startkeyfun endkeyfun)
;;   (save-excursion
;;     (save-restriction
;;       (unless (buffer-narrowed-p)(narrow-to-region beg end))
;;       (goto-char (point-min))
;;       (let ((reverse (or reverse nil))
;;             (startkeyfun (or startkeyfun nil))
;;             (endkeyfun (or endkeyfun nil))
;;         (while (not (or (eobp)(stringp (ar-th thing))))
;;           (forward-char 1))
;;         (if (eq thing 'number)
;;           (ar-sort-numbers-subr reverse
;;                       (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
;;                       (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun)
;;           (sort-subr reverse
;;                       (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
;;                       (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun)))))))

;; (defun ar-sort-numbers-subr (reverse nextrecfun endrecfun
;;                                      &optional startkeyfun endkeyfun)
;;   "A patched sort-subr. Divides buffer into records and sort them.

;; We divide the accessible portion of the buffer into disjoint pieces
;; called sort records.  A portion of each sort record (perhaps all of
;; it) is designated as the sort key.  The records are rearranged in the
;; buffer in order by their sort keys.  The records may or may not be
;; contiguous.

;; Usually the records are rearranged in order of ascending sort key.
;; If REVERSE is non-nil, they are rearranged in order of descending sort key.
;; The variable ‘sort-fold-case’ determines whether alphabetic case affects
;; the sort order.

;; The next four arguments are functions to be called to move point
;; across a sort record.  They will be called many times from within sort-subr.

;; NEXTRECFUN is called with point at the end of the previous record.
;; It moves point to the start of the next record.
;; It should move point to the end of the buffer if there are no more records.
;; The first record is assumed to start at the position of point when sort-subr
;; is called.

;; ENDRECFUN is called with point within the record.
;; It should move point to the end of the record.

;; STARTKEYFUN moves from the start of the record to the start of the key.
;; It may return either a non-nil value to be used as the key, or
;; else the key is the substring between the values of point after
;; STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
;; starts at the beginning of the record.

;; ENDKEYFUN moves from the start of the sort key to the end of the sort key.
;; ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
;; same as ENDRECFUN.

;; PREDICATE is the function to use to compare keys.  If keys are numbers,
;; it defaults to `<', otherwise it defaults to `string<'."
;;   ;; Heuristically try to avoid messages if sorting a small amt of text.
;;   (let ((messages (> (- (point-max) (point-min)) 50000)))
;;     (save-excursion
;;       (if messages (message "Finding sort keys..."))
;;       (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
;; 					   startkeyfun endkeyfun))
;; 	     (old (reverse sort-lists))
;; 	     (case-fold-search sort-fold-case))
;; 	(if (null sort-lists)
;; 	    ()
;; 	  (or reverse (setq sort-lists (nreverse sort-lists)))
;; 	  (if messages (message "Sorting records..."))
;; 	  (setq sort-lists
;; 		(sort sort-lists
;;                       (lambda (a b)
;;                         (< (string-to-number (buffer-substring-no-properties (caar a) (cdar a)))(string-to-number (buffer-substring-no-properties (caar b)(cdar b)))))))
;; 	  (if reverse (setq sort-lists (nreverse sort-lists)))
;; 	  (if messages (message "Reordering buffer..."))
;; 	  (sort-reorder-buffer sort-lists old)))
;;       (if messages (message "Reordering buffer... Done"))))
;;   nil)

(defun ar-th-delim-intern (beg end begstr endstr)
  (let ((end (copy-marker end)))
    (goto-char beg)
    (insert begstr)
    (goto-char end)
    (insert endstr)))

(defun ar-th-delim (thing &optional beg-char end-char no-delimiters)
  "Put delimiters around THING."
  (interactive "*")
  (let* ((bounds (ar-th-bounds thing no-delimiters))
         (beg (or (ignore-errors (car bounds))(ignore-errors (car bounds))))
         (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (when (and beg end)
      (ar-th-delim-intern beg end beg-char end-char))))

(defun ar-th-base-copy-or (kind arg &optional)
  "Internally used by ‘ar-parentize-or-copy-atpt’ and the like."
  (let* ((expr (format "%s" kind))
	 (arg (if arg (prefix-numeric-value arg) 1))
	 (suffix
	  (when (or (member kind ar-paired-delimit-aktiv)
		    ;; (loop for e in ar-unpaired-delimit-aktiv if (member kind e) return e))
		    (member kind ar-unpaired-delimit-aktiv))
	    (if (string-match "e$" expr)
		"d" "ed")))
	 beg end erg bounds)
    (cond
     ((eq 2 arg)
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-trim- " expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-trim-" expr suffix "-atpt"))))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt")) arg))))
     ((eq 4 (prefix-numeric-value arg))
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-" expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-" expr suffix "-atpt")) arg))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt"))))))
     ((< arg 0)
      (setq erg (funcall (intern-soft (concat "ar-kill-" expr suffix "-atpt")))))
     ((< 0 arg)
      (or (setq bounds (funcall (intern-soft (concat "ar-bounds-of-" expr suffix "-atpt"))))
	  (setq bounds (funcall (intern-soft (concat "ar-bounds-of-" expr "-" copy-or-alternative "-atpt")))))
      (when bounds
	(setq beg (cond ((ignore-errors (numberp (car-safe bounds)))
			 (car-safe bounds))
			((ignore-errors (car bounds))
			 (car bounds))
			(t (car-safe bounds))))
	(setq end (or (ignore-errors (cadr (cadr bounds)))
		      (ignore-errors (cdr (cadr bounds)))(cdr bounds)))
	(when (and beg end)
	  (setq erg (kill-new (buffer-substring-no-properties beg end)))
	  (goto-char beg)
	  (push-mark (point) t t)
	  (goto-char end))))
     ((use-region-p)
      (setq erg (funcall (intern-soft (concat "ar-" expr "-in-region-atpt"))))))
    erg))

(defvar ar-werkstatt-mode-map nil
    "Keymap used in Sh-Werkstatt mode.")

(define-derived-mode werkstatt emacs-lisp-mode "Werk"
  (use-local-map ar-werkstatt-mode-map)
  (and ar-werkstatt-hs-minor-mode-p
       (add-hook 'ar-werkstatt-mode-hook 'hs-minor-mode)))

(defun ar--transform-delimited-new-delimiter (to)
  "Return the new delimiter - either paired or unpaired. "
  (let ((erg))
    (dolist (ele ar-paired-delimited-passiv-raw)
      (when (member to ele)
	(setq erg (cdr ele))
	(message "%s" (car erg))))
    (unless erg
      (dolist (ele ar-unpaired-delimited-raw)
	(when (member to ele)
	  (setq erg (cdr ele))
	  (message "%s" (car erg)))))
    erg))

(defun ar--transform-insert-opening-delimiter-according-to-type (new-delimiter)
  "If a cons, insert car. "
  (if (string-match "\"" (car new-delimiter))
      (insert "\"")
    (insert (car new-delimiter))))

(defun ar--transform-return-closing-delimiter-according-to-type (new-delimiter)
  "Return closing delimiter. "
  (let ((erg (if (< 1 (length new-delimiter))
		 (cadr new-delimiter)
	       (car new-delimiter))))
    (if (string-match "\"" erg)
	"\""
      erg)))

(defun ar--transform-delimited-intern (from to)
  "Expects string. "
  (save-restriction
    (let* ((bounds (ignore-errors (funcall (car (read-from-string (concat "ar-bounds-of-" from "-atpt"))))))
           (end (copy-marker
                 (cond
                  ((numberp (cdr-safe bounds))
                   (cdr-safe bounds))
                  ((numberp (ignore-errors (cadr bounds)))
                   (cadr bounds))
                  ((numberp (ignore-errors (cadr (cadr bounds))))
                   (cadr (cadr bounds)))
                  ((numberp (ignore-errors (cdr (cadr bounds))))
                   (cdr (cadr bounds))))))
	   ;;(end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	   (new-delimiter (ar--transform-delimited-new-delimiter (car (read-from-string to)))))
      (unless bounds (message (concat "ar--transform-delimited-intern: can't see " from)))
      (unless new-delimiter (message (concat "ar--transform-delimited-intern: can't see " to)))
      (goto-char (car bounds))
      (delete-char 1)
      (ar--transform-insert-opening-delimiter-according-to-type new-delimiter)
      (goto-char end)
      (delete-char -1)
      (insert (ar--transform-return-closing-delimiter-according-to-type new-delimiter)))))

(provide 'ar-thingatpt-basic-definitions)

;;; ar-thingatpt-basic-definitions.el ends here
