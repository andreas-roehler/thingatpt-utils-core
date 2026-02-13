;;; ar-thingatpt-utils-core.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2026 Andreas Röhler, unless
;; indicated otherwise

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

;;; Commentary:

;; Delivers a set of functions to return, mover over or
;; manipulate a given THING. THING may be a well known
;; form as word, paragraph, but also a char class as
;; ‘alnum’ or a new defined thing.

;; For example ‘ar-alnum-atpt’ will return all
;; alpha-numerical chars below and around cursor as a
;; string. ‘ar-bounds-of-alnum-atpt’ returns the
;; borders of that string as a list and so on.

;; ‘ar-delimited-atpt’ returns buffer string between delimiters, defined customizable vars
;; ‘th-beg-delimiter’, ‘th-end-delimiter’, ‘ar-delimiters-atpt’.

;; Presently for a given THING the following is
;; implemented:

;; ar-THING-atpt
;; ar-THING-bounds-atpt
;; ar-THING-beginning-position-atpt
;; ar-THING-end-position-atpt
;; ar-THING-beginning-atpt
;; ar-THING-end-atpt
;; ar-THING-length-atpt
;; ar-THING-copy-atpt
;; ar-THING-kill-atpt
;; ar-THING-forward-atpt
;; ar-THING-backward-atpt
;; ar-THING-transpose-atpt
;; ar-THING-sort-atpt
;; ar-THING-check-atpt

;; Beside of the mentioned above, esists still a couple of
;; functions, whose use is much less probable:

;; ar-THING-slash-atpt
;; ar-THING-doublebackslash-atpt
;; ar-THING-doubleslash-atpt
;; ar-THING-delete-in-region
;; ar-blok-THING-atpt
;; ar-THING-escape-atpt
;; ar-THING-doublequote-atpt
;; ar-THING-doublebackslashparen-atpt
;; ar-THING-dollar-atpt
;; ar-THING-equalize-atpt
;; ar-THING-greaterangle-atpt
;; ar-THING-lesserangle-atpt
;; ar-THING-backslash-atpt
;; ar-THING-brace-atpt
;; ar-THING-bracket-atpt
;; ar-comment-THING-atpt
;; ar-commatize-THING-atpt
;; ar-quote-THING-atpt
;; ar-THING-hyphen-atpt
;; ar-THING-mark-atpt
;; ar-THING-hide-atpt
;; ar-THING-show-atpt
;; ar-THING-hide-show-atpt
;; ar-THING-curvedsinglequote-atpt
;; ar-THING-parentize-atpt
;; ar-THING-separate-atpt
;; ar-THING-singlequote-atpt
;; ar-THING-trim-atpt
;; ar-THING-left-trim-atpt
;; ar-THING-right-trim-atpt
;; ar-underscore-THING-atpt
;; ar-whitespace-THING-atpt

;; To see what's implemented, consult contents of
;; variables at the end of this file as
;; ‘ar-paired-delimit-aktiv’, ‘ar-paired-delimited-passiv’, etc.

;; Call one of the test-functions `C-u ar-th-delimtest'
;; with come chars in scratch-buffer
;; or any else changable buffer to get an impression.

;; The idea comes from Mike Williams
;; <mikew@gopher.dosli.govt.nz>, author of
;; thingatpt.el

;; The goal is to have a set of similar forms. For
;; example, to provide a word with double-quotes around
;; it, call ar-doublequote-word-atpt. In a similar way you
;; may double-quote not just a word, but any object
;; instrumented here as THING. To make parentheses
;; around it call ar-parentize-word-atpt, etc.

;; Move-functions of this package differ from common
;; behaviour in such, as ‘ar-forward-word-atpt’ stops
;; not after THING, but on the last char of
;; THING. That's in order to enable a call of
;; thing-at-point functions at the end
;; position. Otherwise, when cursor stops after word
;; (THING) as does ‘forward-word’, ‘ar-word-atpt’ would return
;; nil.

;; To see other features, maybe try ‘ar-separate-list-atpt’
;; or ‘ar-comment-list-atpt’ while point is inside a
;; list. Try it again with an abstract char-class as
;; [:alnum:], i.e. try ‘ar-comment-alnum-atpt’,
;; ‘ar-brace-alnum-atpt’ etc.

;; This utility comes with test-functions which return
;; the possible results of most functions (exception
;; are the kill-fns). Call th-test, th-mv-test
;; or th-delimtest over text. That-delimtest
;; changes but restores the buffer. Customize the speed
;; of execution via ‘ar-th-test-delay’

;; Diffs to basics of required thingatpt.el:
;; ‘bounds-of-thing-at-point’ is replaced by a new
;; ‘ar-th-bounds’, which now first searches
;; backward. As a consequence several
;; ‘beginning-op-at’ and ‘end-op-at’ constructs had
;; to be rewritten.

;; Behavior in general is not validating; i.e. if you
;; call ar-url-atpt and there is no url, all chars at
;; point may be picked, which could be part of a
;; url. Sometimes, however, a kind of validation may be
;; introduced.

;; If calling from a program `bounds-of-THING-atpt' is
;; recommended as an entry-point. It delivers a list
;; with beg and end positions.

;; In case of trouble, please send me a bug report. Any
;; ideas and comments welcome.

;; You might be interested also to visit Drew Adam's
;; http://www.emacswiki.org/emacs/thingatpt+.el
;; which predates this approach and was helpful writing it.

;; Thing-at-point delivers a portion of the
;; buffer. Thats useful, if THING is not as easy to grasp as a word.
;; For example the first string of an objekt like:

;; ("4[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?C[[:punct:] \t\r\n]?.[[:punct:] \t\r\n]?2[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?6[[:punct:] \t\r\n]?4[[:punct:] \t\r\n]?/[[:punct:] \t\r\n]?0[[:punct:] \t\r\n]?3[[:punct:] \t\r\n]? [[:punct:] \t\r\n]?B" . "blah blub B")

;; Remove comments and put the cursor somewhere into the first
;; string:
;; ‘ar-doublequoted-atpt’ will return it, copied into the kill-ring,
;; enabling yanking it and a lot of further actions.

;; ‘ar-doublequoted-atpt’ here is to
;; (global-set-key [(super \")] 'ar-doublequoted-atpt)

;; alike a range of similar commands exist:
;; (global-set-key [(super \')] 'ar-singlequoted-atpt)
;; (global-set-key [(super \))] 'ar-parentized-atpt)
;; (global-set-key [(super \/)] 'ar-slashed-atpt)
;; (global-set-key [(super \\)] 'ar-backslashed-atpt)
;; (global-set-key [(super \])] 'ar-bracketed-atpt)
;; (global-set-key [(super \})] 'ar-braced-atpt)

;; So far THING is simply picked up.

;; Different approach combines copying, deleting with delimiting

;; if region is active:

;; (global-set-key [(control c) (\")] 'ar-doublequote-or-copy-atpt)

;; will provide doublequotes at beginning and end of region.

;; With negative argument it deletes the doublequoted portion under
;; point.

;; Without any argument these functions return as their simplier
;; counterparts

;; With universal argument [(control u)] delimiters --i.e. doublequotes, slashes, whatever-- are stripped.

;;

;; THING as a buffer substring is determined by
;; move-functions specified for thingatpt, called
;; beginning-op-at and end-op-at. Point is stored
;; after move, beginning and end delivered as pair: as
;; consed bounds-of-thing. It's easy to write your own
;; thing-at-point functions that way. You need the
;; caller and both move forms:

;; (defun MY-FORM-atpt (&optional arg)
;;   " "
;;   (interactive "p")
;;   (ar-th 'MY-FORM arg))

;; (put 'MY-FORM 'beginning-op-at
;;            (lambda () MY-FORWARD-MOVE-CODE))

;; (put 'MY-FORM 'end-op-at
;;      (lambda () MY-BACKWARD-MOVE-CODE))

;; For example if you want to pick all chars at point
;; which are written between a string "AAA" and a
;; "BBB", which may exist as
;; AAA Luckily detected a lot of things! BBB
;; After evaluation of
;; (put 'MY-FORM 'beginning-op-at
;;      (lambda ()
;;        (search-backward "AAA" nil 'move 1)
;;        ;; step chars of search expression back
;;        (forward-char 3)))
;;
;; (put 'MY-FORM 'end-op-at
;;      (lambda ()
;;        (search-forward "BBB" nil 'move 1)
;;        (forward-char -3)))
;; together with the functions definition above, it's ready.
;; M-x MY-FORM-atpt
;; (while point inside) you should see:
;; " Luckily detected a lot of things! "
;; in the minibuffer.

;; Some keys

;; (define-key emacs-lisp-mode-map [(control c)(q)] 'ar-parentized-forward-atpt)
;; (define-key emacs-lisp-mode-map [(super c)())] 'ar-symbol-parentize-atpt)
;; (define-key emacs-lisp-mode-map [(super c)(n)] 'ar-region-parentize-atpt)
;; (global-set-key [(control c)(<)] 'ar-lesserangle-or-copy-atpt)
;; (global-set-key [(control c)(>)] 'ar-greaterangle-or-copy-atpt)
;; (global-set-key [(control c)(")] 'ar-doublequote-or-copy-atpt)
;; (global-set-key [(control c)(')] 'ar-singlequote-or-copy-atpt)
;; (global-set-key [(control c)(()] 'ar-paren-atpt)
;; (global-set-key [(control c)())] 'ar-parentize-or-copy-atpt)
;; (global-set-key [(control c)(/)] 'ar-slash-or-copy-atpt)
;; (global-set-key [(control c)(*)] 'ar-star-or-copy-atpt)

;;; Code:
(require 'ar-subr)
(require 'ar-beg-end)
(require 'hideshow)
(require 'ar-thingatpt-basic-definitions)
(defconst Emacs-Werkstatt-version "1.5")

(when (featurep 'xemacs) (require 'overlay))

(defgroup werkstatt nil
  "Return, mover over or manipulate a given THING."
  :prefix "ar-"
  :group 'matching)

(defcustom sort-fold-case nil
  "Whether alphabetic case affects the sort order

Used by ‘ar-sort-numbers-subr’"

  :type 'boolean
  :group 'werkstatt)

(defcustom ar-werkstatt-hs-minor-mode-p nil
  ""

  :type 'boolean
  :group 'werkstatt)

(defcustom thing-copy-region nil
  "If a found THING should be copied into the kill-ring.

Default is nil"
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-newlines-separate-after 1
  "How many newlines at-th-separate should insert at the end"

  :type 'number
  :group 'werkstatt)

(defcustom ar-newlines-separate-before 1
  "How many newlines at-th-separate should insert at the end"

  :type 'number
  :group 'werkstatt)

;; (defvar th-orig 0
;; "Correct orig according to delimiter-length")

(when (featurep 'xemacs)
  (defcustom alnum "\\sw"
    "Rexexp to specify the character class
Follows word-syntax. Use something like
   \"[a-zA-ZäöüßÄÖÜ0-9]\" maybe instead.
‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom alpha "[a-zA-ZäöüßÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom ascii "[\000-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom blank "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom cntrl "[\000-\006]\016-\037]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom digit "[0-9]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom graph "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom lower "[a-zäöüß]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom multibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom nonascii "[^\040-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom print "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom punct "[.,-_:;?!]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom space "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom unibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom upper "[A-ZÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom xdigit "[0-9.,]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: ‘unibyte’ and ‘multibyte’ class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))


;; ar-insert-put-classes start

;; Alnum
(put 'alnum 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:alnum:]")))
	      (looking-at "[[:alnum:]]"))
	 (cons (point) (1+ (point))))))

(put 'alnum 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:alnum:]"))
	    (cons (1- (point)) (point)))))

(put 'alnum 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:alnum:]")
         (skip-chars-forward "^[:alnum:]")
         (when (< orig (point))
           (point)))))

(put 'alnum 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:alnum :]")
         (skip-chars-backward "^[:alnum:]")
         (when (< (point) orig)
           (point)))))

;; Alpha
(put 'alpha 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:alpha:]")))
	      (looking-at "[[:alpha:]]"))
	 (cons (point) (1+ (point))))))

(put 'alpha 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:alpha:]"))
	    (cons (1- (point)) (point)))))

(put 'alpha 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:alpha:]")
         (skip-chars-forward "^[:alpha:]")
         (when (< orig (point))
           (point)))))

(put 'alpha 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:alpha :]")
         (skip-chars-backward "^[:alpha:]")
         (when (< (point) orig)
           (point)))))

;; Ascii
(put 'ascii 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:ascii:]")))
	      (looking-at "[[:ascii:]]"))
	 (cons (point) (1+ (point))))))

(put 'ascii 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:ascii:]"))
	    (cons (1- (point)) (point)))))

(put 'ascii 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:ascii:]")
         (skip-chars-forward "^[:ascii:]")
         (when (< orig (point))
           (point)))))

(put 'ascii 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:ascii :]")
         (skip-chars-backward "^[:ascii:]")
         (when (< (point) orig)
           (point)))))

;; Blank
(put 'blank 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:blank:]")))
	      (looking-at "[[:blank:]]"))
	 (cons (point) (1+ (point))))))

(put 'blank 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:blank:]"))
	    (cons (1- (point)) (point)))))

(put 'blank 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:blank:]")
         (skip-chars-forward "^[:blank:]")
         (when (< orig (point))
           (point)))))

(put 'blank 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:blank :]")
         (skip-chars-backward "^[:blank:]")
         (when (< (point) orig)
           (point)))))

;; Cntrl
(put 'cntrl 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	      (looking-at "[[:cntrl:]]"))
	 (cons (point) (1+ (point))))))

(put 'cntrl 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:cntrl:]"))
	    (cons (1- (point)) (point)))))

(put 'cntrl 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:cntrl:]")
         (skip-chars-forward "^[:cntrl:]")
         (when (< orig (point))
           (point)))))

(put 'cntrl 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:cntrl :]")
         (skip-chars-backward "^[:cntrl:]")
         (when (< (point) orig)
           (point)))))

;; Digit
(put 'digit 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:digit:]")))
	      (looking-at "[[:digit:]]"))
	 (cons (point) (1+ (point))))))

(put 'digit 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:digit:]"))
	    (cons (1- (point)) (point)))))

(put 'digit 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:digit:]")
         (skip-chars-forward "^[:digit:]")
         (when (< orig (point))
           (point)))))

(put 'digit 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:digit :]")
         (skip-chars-backward "^[:digit:]")
         (when (< (point) orig)
           (point)))))

;; Graph
(put 'graph 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:graph:]")))
	      (looking-at "[[:graph:]]"))
	 (cons (point) (1+ (point))))))

(put 'graph 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:graph:]"))
	    (cons (1- (point)) (point)))))

(put 'graph 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:graph:]")
         (skip-chars-forward "^[:graph:]")
         (when (< orig (point))
           (point)))))

(put 'graph 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:graph :]")
         (skip-chars-backward "^[:graph:]")
         (when (< (point) orig)
           (point)))))

;; Lower
(put 'lower 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:lower:]")))
	      (looking-at "[[:lower:]]"))
	 (cons (point) (1+ (point))))))

(put 'lower 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:lower:]"))
	    (cons (1- (point)) (point)))))

(put 'lower 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:lower:]")
         (skip-chars-forward "^[:lower:]")
         (when (< orig (point))
           (point)))))

(put 'lower 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:lower :]")
         (skip-chars-backward "^[:lower:]")
         (when (< (point) orig)
           (point)))))

;; Nonascii
(put 'nonascii 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	      (looking-at "[[:nonascii:]]"))
	 (cons (point) (1+ (point))))))

(put 'nonascii 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:nonascii:]"))
	    (cons (1- (point)) (point)))))

(put 'nonascii 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:nonascii:]")
         (skip-chars-forward "^[:nonascii:]")
         (when (< orig (point))
           (point)))))

(put 'nonascii 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:nonascii :]")
         (skip-chars-backward "^[:nonascii:]")
         (when (< (point) orig)
           (point)))))

;; Print
(put 'print 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:print:]")))
	      (looking-at "[[:print:]]"))
	 (cons (point) (1+ (point))))))

(put 'print 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:print:]"))
	    (cons (1- (point)) (point)))))

(put 'print 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:print:]")
         (skip-chars-forward "^[:print:]")
         (when (< orig (point))
           (point)))))

(put 'print 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:print :]")
         (skip-chars-backward "^[:print:]")
         (when (< (point) orig)
           (point)))))

;; Punct
(put 'punct 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:punct:]")))
	      (looking-at "[[:punct:]]"))
	 (cons (point) (1+ (point))))))

(put 'punct 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:punct:]"))
	    (cons (1- (point)) (point)))))

(put 'punct 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:punct:]")
         (skip-chars-forward "^[:punct:]")
         (when (< orig (point))
           (point)))))

(put 'punct 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:punct :]")
         (skip-chars-backward "^[:punct:]")
         (when (< (point) orig)
           (point)))))

;; Space
(put 'space 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:space:]")))
	      (looking-at "[[:space:]]"))
	 (cons (point) (1+ (point))))))

(put 'space 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:space:]"))
	    (cons (1- (point)) (point)))))

(put 'space 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:space:]")
         (skip-chars-forward "^[:space:]")
         (when (< orig (point))
           (point)))))

(put 'space 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:space :]")
         (skip-chars-backward "^[:space:]")
         (when (< (point) orig)
           (point)))))

;; Upper
(put 'upper 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:upper:]")))
	      (looking-at "[[:upper:]]"))
	 (cons (point) (1+ (point))))))

(put 'upper 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:upper:]"))
	    (cons (1- (point)) (point)))))

(put 'upper 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:upper:]")
         (skip-chars-forward "^[:upper:]")
         (when (< orig (point))
           (point)))))

(put 'upper 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:upper :]")
         (skip-chars-backward "^[:upper:]")
         (when (< (point) orig)
           (point)))))

;; Xdigit
(put 'xdigit 'beginning-op-at
     (lambda ()
       (when (or
	      (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	      (looking-at "[[:xdigit:]]"))
	 (cons (point) (1+ (point))))))

(put 'xdigit 'end-op-at
     (lambda ()
       (and (< 0 (skip-chars-forward "[:xdigit:]"))
	    (cons (1- (point)) (point)))))

(put 'xdigit 'forward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-forward "[:xdigit:]")
         (skip-chars-forward "^[:xdigit:]")
         (when (< orig (point))
           (point)))))

(put 'xdigit 'backward-op-at
     (lambda ()
       (let ((orig (point)))
         (skip-chars-backward "[:xdigit :]")
         (skip-chars-backward "^[:xdigit:]")
         (when (< (point) orig)
           (point)))))

;; Paired delimited forms start

;; Braced
(put 'braced 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "{" "}" nil 'move 0 nil 'ar-syntax)))

(put 'braced 'end-op-at
     (lambda ()
       (end-of-form-base "{" "}" nil 'move 0 nil 'ar-syntax)))

(put 'braced 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "{" "}" nil 'move 0  nil 'ar-syntax t))))

(put 'braced 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "{" "}" nil 'move 0  nil 'ar-syntax))))

;; Symboled
(put 'symboled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "`" "'" nil 'move 0 nil 'ar-syntax)))

(put 'symboled 'end-op-at
     (lambda ()
       (end-of-form-base "`" "'" nil 'move 0 nil 'ar-syntax)))

(put 'symboled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "`" "'" nil 'move 0  nil 'ar-syntax t))))

(put 'symboled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "`" "'" nil 'move 0  nil 'ar-syntax))))

;; Bracketed
(put 'bracketed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\[" "]" nil 'move 0 t 'ar-syntax)))

(put 'bracketed 'end-op-at
     (lambda ()
       (end-of-form-base "\[" "]" nil 'move 0 t 'ar-syntax)))

(put 'bracketed 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "\[" "]" nil 'move 0  t 'ar-syntax t))))

(put 'bracketed 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "\[" "]" nil 'move 0  t 'ar-syntax))))

;; Lesserangled
(put 'lesserangled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "<" ">" nil 'move 0 nil 'ar-syntax)))

(put 'lesserangled 'end-op-at
     (lambda ()
       (end-of-form-base "<" ">" nil 'move 0 nil 'ar-syntax)))

(put 'lesserangled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "<" ">" nil 'move 0  nil 'ar-syntax t))))

(put 'lesserangled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "<" ">" nil 'move 0  nil 'ar-syntax))))

;; Greaterangled
(put 'greaterangled 'beginning-op-at
     (lambda ()
       (beginning-of-form-base ">" "<" nil 'move 0 nil 'ar-syntax)))

(put 'greaterangled 'end-op-at
     (lambda ()
       (end-of-form-base ">" "<" nil 'move 0 nil 'ar-syntax)))

(put 'greaterangled 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base ">" "<" nil 'move 0  nil 'ar-syntax t))))

(put 'greaterangled 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base ">" "<" nil 'move 0  nil 'ar-syntax))))

;; Curvedsinglequoted
(put 'curvedsinglequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "‘" "’" nil 'move 0 nil 'ar-syntax)))

(put 'curvedsinglequoted 'end-op-at
     (lambda ()
       (end-of-form-base "‘" "’" nil 'move 0 nil 'ar-syntax)))

(put 'curvedsinglequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "‘" "’" nil 'move 0  nil 'ar-syntax t))))

(put 'curvedsinglequoted 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "‘" "’" nil 'move 0  nil 'ar-syntax))))

;; Curveddoublequoted
(put 'curveddoublequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "“" "”" nil 'move 0 nil 'ar-syntax)))

(put 'curveddoublequoted 'end-op-at
     (lambda ()
       (end-of-form-base "“" "”" nil 'move 0 nil 'ar-syntax)))

(put 'curveddoublequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "“" "”" nil 'move 0  nil 'ar-syntax t))))

(put 'curveddoublequoted 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "“" "”" nil 'move 0  nil 'ar-syntax))))

;; Parentized
(put 'parentized 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "(" ")" nil 'move 0 nil 'ar-syntax)))

(put 'parentized 'end-op-at
     (lambda ()
       (end-of-form-base "(" ")" nil 'move 0 nil 'ar-syntax)))

(put 'parentized 'forward-op-at
     (lambda ()
       (unless (eobp)
         (end-of-form-base "(" ")" nil 'move 0  nil 'ar-syntax t))))

(put 'parentized 'backward-op-at
     (lambda ()
       (unless (bobp)
         (beginning-of-form-base "(" ")" nil 'move 0  nil 'ar-syntax))))

;; Paired delimited forms end

;; Unpaired delimited forms start

;; Backslashed
(put 'backslashed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\\" "\\" nil 'move 0 nil 'ar-syntax)))

(put 'backslashed 'end-op-at
     (lambda ()
       (end-of-form-base "\\" "\\" nil 'move 0 nil 'ar-syntax)))

(put 'backslashed 'forward-op-at
     (lambda ()
       (end-of-form-base "\\" "\\" nil 'move 0 nil 'ar-syntax t)))

(put 'backslashed 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\\" "\\" nil 'move 0 nil 'ar-syntax)))


;; Backticked
(put 'backticked 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "`" "`" nil 'move 0 nil 'ar-syntax)))

(put 'backticked 'end-op-at
     (lambda ()
       (end-of-form-base "`" "`" nil 'move 0 nil 'ar-syntax)))

(put 'backticked 'forward-op-at
     (lambda ()
       (end-of-form-base "`" "`" nil 'move 0 nil 'ar-syntax t)))

(put 'backticked 'backward-op-at
     (lambda ()
       (beginning-of-form-base "`" "`" nil 'move 0 nil 'ar-syntax)))


;; Coloned
(put 'coloned 'beginning-op-at
     (lambda ()
       (beginning-of-form-base ":" ":" nil 'move 0 nil 'ar-syntax)))

(put 'coloned 'end-op-at
     (lambda ()
       (end-of-form-base ":" ":" nil 'move 0 nil 'ar-syntax)))

(put 'coloned 'forward-op-at
     (lambda ()
       (end-of-form-base ":" ":" nil 'move 0 nil 'ar-syntax t)))

(put 'coloned 'backward-op-at
     (lambda ()
       (beginning-of-form-base ":" ":" nil 'move 0 nil 'ar-syntax)))


;; Dollared
(put 'dollared 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "$" "$" nil 'move 0 nil 'ar-syntax)))

(put 'dollared 'end-op-at
     (lambda ()
       (end-of-form-base "$" "$" nil 'move 0 nil 'ar-syntax)))

(put 'dollared 'forward-op-at
     (lambda ()
       (end-of-form-base "$" "$" nil 'move 0 nil 'ar-syntax t)))

(put 'dollared 'backward-op-at
     (lambda ()
       (beginning-of-form-base "$" "$" nil 'move 0 nil 'ar-syntax)))


;; Doublequoted
(put 'doublequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "\"" "\"" nil 'move 0 nil 'ar-syntax)))

(put 'doublequoted 'end-op-at
     (lambda ()
       (end-of-form-base "\"" "\"" nil 'move 0 nil 'ar-syntax)))

(put 'doublequoted 'forward-op-at
     (lambda ()
       (end-of-form-base "\"" "\"" nil 'move 0 nil 'ar-syntax t)))

(put 'doublequoted 'backward-op-at
     (lambda ()
       (beginning-of-form-base "\"" "\"" nil 'move 0 nil 'ar-syntax)))


;; Equalized
(put 'equalized 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "=" "=" nil 'move 0 nil 'ar-syntax)))

(put 'equalized 'end-op-at
     (lambda ()
       (end-of-form-base "=" "=" nil 'move 0 nil 'ar-syntax)))

(put 'equalized 'forward-op-at
     (lambda ()
       (end-of-form-base "=" "=" nil 'move 0 nil 'ar-syntax t)))

(put 'equalized 'backward-op-at
     (lambda ()
       (beginning-of-form-base "=" "=" nil 'move 0 nil 'ar-syntax)))


;; Hyphened
(put 'hyphened 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "-" "-" nil 'move 0 nil 'ar-syntax)))

(put 'hyphened 'end-op-at
     (lambda ()
       (end-of-form-base "-" "-" nil 'move 0 nil 'ar-syntax)))

(put 'hyphened 'forward-op-at
     (lambda ()
       (end-of-form-base "-" "-" nil 'move 0 nil 'ar-syntax t)))

(put 'hyphened 'backward-op-at
     (lambda ()
       (beginning-of-form-base "-" "-" nil 'move 0 nil 'ar-syntax)))


;; Singlequoted
(put 'singlequoted 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "'" "'" nil 'move 0 nil 'ar-syntax)))

(put 'singlequoted 'end-op-at
     (lambda ()
       (end-of-form-base "'" "'" nil 'move 0 nil 'ar-syntax)))

(put 'singlequoted 'forward-op-at
     (lambda ()
       (end-of-form-base "'" "'" nil 'move 0 nil 'ar-syntax t)))

(put 'singlequoted 'backward-op-at
     (lambda ()
       (beginning-of-form-base "'" "'" nil 'move 0 nil 'ar-syntax)))


;; Slashed
(put 'slashed 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "/" "/" nil 'move 0 nil 'ar-syntax)))

(put 'slashed 'end-op-at
     (lambda ()
       (end-of-form-base "/" "/" nil 'move 0 nil 'ar-syntax)))

(put 'slashed 'forward-op-at
     (lambda ()
       (end-of-form-base "/" "/" nil 'move 0 nil 'ar-syntax t)))

(put 'slashed 'backward-op-at
     (lambda ()
       (beginning-of-form-base "/" "/" nil 'move 0 nil 'ar-syntax)))


;; Stared
(put 'stared 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "*" "*" nil 'move 0 nil 'ar-syntax)))

(put 'stared 'end-op-at
     (lambda ()
       (end-of-form-base "*" "*" nil 'move 0 nil 'ar-syntax)))

(put 'stared 'forward-op-at
     (lambda ()
       (end-of-form-base "*" "*" nil 'move 0 nil 'ar-syntax t)))

(put 'stared 'backward-op-at
     (lambda ()
       (beginning-of-form-base "*" "*" nil 'move 0 nil 'ar-syntax)))


;; Underscored
(put 'underscored 'beginning-op-at
     (lambda ()
       (beginning-of-form-base "_" "_" nil 'move 0 nil 'ar-syntax)))

(put 'underscored 'end-op-at
     (lambda ()
       (end-of-form-base "_" "_" nil 'move 0 nil 'ar-syntax)))

(put 'underscored 'forward-op-at
     (lambda ()
       (end-of-form-base "_" "_" nil 'move 0 nil 'ar-syntax t)))

(put 'underscored 'backward-op-at
     (lambda ()
       (beginning-of-form-base "_" "_" nil 'move 0 nil 'ar-syntax)))


;; Whitespaced
(put 'whitespaced 'beginning-op-at
     (lambda ()
       (beginning-of-form-base " " " " nil 'move 0 nil 'ar-syntax)))

(put 'whitespaced 'end-op-at
     (lambda ()
       (end-of-form-base " " " " nil 'move 0 nil 'ar-syntax)))

(put 'whitespaced 'forward-op-at
     (lambda ()
       (end-of-form-base " " " " nil 'move 0 nil 'ar-syntax t)))

(put 'whitespaced 'backward-op-at
     (lambda ()
       (beginning-of-form-base " " " " nil 'move 0 nil 'ar-syntax)))


;; Unpaired delimited forms end

;; ML data-forms start

;; Beginendquoted
(put 'beginendquoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\begin{quote}"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\begin{quote}" "\\end{quote}" nil 'move 1 nil nil nil))))

(put 'beginendquoted 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\begin{quote}"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\begin{quote}" "\\end{quote}" nil 'move 1 nil nil nil))))


;; Backslashparened
(put 'backslashparened 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\(" "\\\\)" nil 'move 1 nil nil 'ar-escaped))))

(put 'backslashparened 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\("))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\(" "\\\\)" nil 'move 1 nil nil 'ar-escaped))))


;; Bloked
(put 'bloked 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "{% "))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "{% " " %}" nil 'move 1 nil t nil))))

(put 'bloked 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "{% "))
         (goto-char (match-end 0)) 
         (end-of-form-base "{% " " %}" nil 'move 1 nil t nil))))


;; Doublebackslashed
(put 'doublebackslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\" "\\\\" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\" "\\\\" nil 'move 1 nil nil 'ar-escaped))))


;; Doublebackslashedparen
(put 'doublebackslashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackslashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\\\\\("))
         (goto-char (match-end 0)) 
         (end-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil 'move 1 nil nil 'ar-escaped))))


;; Doublebackticked
(put 'doublebackticked 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "``"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "``" "``" nil 'move 1 nil nil 'ar-escaped))))

(put 'doublebackticked 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "``"))
         (goto-char (match-end 0)) 
         (end-of-form-base "``" "``" nil 'move 1 nil nil 'ar-escaped))))


;; Doubleslashed
(put 'doubleslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "//"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "//" "//" nil 'move 1 nil nil 'ar-escaped))))

(put 'doubleslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "//"))
         (goto-char (match-end 0)) 
         (end-of-form-base "//" "//" nil 'move 1 nil nil 'ar-escaped))))


;; Slashparened
(put 'slashparened 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "////////("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "////////(" "////////)" nil 'move 1 nil nil 'ar-escaped))))

(put 'slashparened 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "////////("))
         (goto-char (match-end 0)) 
         (end-of-form-base "////////(" "////////)" nil 'move 1 nil nil 'ar-escaped))))


;; Tabledatap
(put 'tabledatap 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<td[^>]*>"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<td[^>]*>" "</td>" nil 'move 1 nil nil nil))))

(put 'tabledatap 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<td[^>]*>"))
         (goto-char (match-end 0)) 
         (end-of-form-base "<td[^>]*>" "</td>" nil 'move 1 nil nil nil))))


;; Triplequoted
(put 'triplequoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\"\"\"\\|'''"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))

(put 'triplequoted 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\"\"\"\\|'''"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))


;; Triplequoteddq
(put 'triplequoteddq 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\"\"\"\\|'''"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))

(put 'triplequoteddq 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\"\"\"\\|'''"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))


;; Triplequotedsq
(put 'triplequotedsq 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\"\"\"\\|'''"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))

(put 'triplequotedsq 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\"\"\"\\|'''"))
         (goto-char (match-end 0)) 
         (end-of-form-base "\"\"\"\\|'''" "\"\"\"\\|'''" nil 'move 1 nil nil 'ar-escaped))))


;; Xslstylesheetp
(put 'xslstylesheetp 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move 1 nil nil nil))))

(put 'xslstylesheetp 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
         (goto-char (match-end 0)) 
         (end-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move 1 nil nil nil))))


;; ML data-forms end
;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: start
;;;###autoload
(defun ar-th-symbol (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "`" "'" no-delimiters))
;;;###autoload
(defun ar-th-brace (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "{" "}" no-delimiters))
;;;###autoload
(defun ar-th-bracket (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "[" "]" no-delimiters))
;;;###autoload
(defun ar-th-lesserangle (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "<" ">" no-delimiters))
;;;###autoload
(defun ar-th-greaterangle (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing ">" "<" no-delimiters))
;;;###autoload
(defun ar-th-curvedsinglequote (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "‘" "’" no-delimiters))
;;;###autoload
(defun ar-th-curveddoublequote (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "“" "”" no-delimiters))
;;;###autoload
(defun ar-th-parentize (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "(" ")" no-delimiters))
;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: end


;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: start
;;;###autoload
(defun ar-th-colon (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing ":" ":" no-delimiters))
;;;###autoload
(defun ar-th-cross (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "+" "+" no-delimiters))
;;;###autoload
(defun ar-th-doubleslash (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "//" "//" no-delimiters))
;;;###autoload
(defun ar-th-backslash (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\" "\\" no-delimiters))
;;;###autoload
(defun ar-th-backtick (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "`" "`" no-delimiters))
;;;###autoload
(defun ar-th-dollar (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "$" "$" no-delimiters))
;;;###autoload
(defun ar-th-doublequote (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\"" "\"" no-delimiters))
;;;###autoload
(defun ar-th-equalize (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "=" "=" no-delimiters))
;;;###autoload
(defun ar-th-escape (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\" "\\" no-delimiters))
;;;###autoload
(defun ar-th-hash (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "#" "#" no-delimiters))
;;;###autoload
(defun ar-th-hyphen (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "-" "-" no-delimiters))
;;;###autoload
(defun ar-th-pipe (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "|" "|" no-delimiters))
;;;###autoload
(defun ar-th-singlequote (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "'" "'" no-delimiters))
;;;###autoload
(defun ar-th-slash (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "/" "/" no-delimiters))
;;;###autoload
(defun ar-th-star (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "*" "*" no-delimiters))
;;;###autoload
(defun ar-th-tild (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "~" "~" no-delimiters))
;;;###autoload
(defun ar-th-underscore (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "_" "_" no-delimiters))
;;;###autoload
(defun ar-th-whitespace (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing " " " " no-delimiters))
;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: end

;; ar-atpt-data-forms-aktiv start
;;;###autoload
(defun ar-th-beginendquote (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\begin{quote}" "\\end{quote}" no-delimiters))
;;;###autoload
(defun ar-th-blok (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "{% " " %}" no-delimiters))
;;;###autoload
(defun ar-th-doublebackslash (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\\\" "\\\\" no-delimiters))
;;;###autoload
(defun ar-th-doublebackslashparen (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\\\(" "\\\\)" no-delimiters))
;;;###autoload
(defun ar-th-doublebacktick (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "``" "``" no-delimiters))
;;;###autoload
(defun ar-th-triplebacktick (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "```" "```" no-delimiters))
;;;###autoload
(defun ar-th-backslashparen (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\\(" "\\)" no-delimiters))
;;;###autoload
(defun ar-th-slashparen (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "////(" "////)" no-delimiters))
;;;###autoload
(defun ar-th-triplequotedq (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\"\"\"" "\"\"\"" no-delimiters))
;;;###autoload
(defun ar-th-triplequotesq (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "'''" "'''" no-delimiters))
;;;###autoload
(defun ar-th-triplequoted (thing &optional no-delimiters)
  " "
  (interactive "*P") 
  (ar-th-delim thing "\"\"\"\\|'''" "\"\"\"\\|'''" no-delimiters))
;; ar-atpt-data-forms-aktiv end


;; ar-insert-thingatpt-syntax-funktionen start

;;;###autoload
(defun ar-syntax-class-atpt (&optional pos)
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let* ((pos (or pos (point)))
         (erg (logand (car (syntax-after pos)) 65535)))
    (when erg (message "%s" erg)) erg))

;;;###autoload
(defun syntax-class-bfpt ()
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let ((erg (logand (car (syntax-after (1- (point)))) 65535)))
    (when erg (message "%s" erg)) erg))

;;;###autoload
(defun ar-syntax-atpt (&optional arg docu pos)
  (interactive "p")
  (when pos
    (goto-char pos))
  (let* ((elt (car (if (featurep 'xemacs)
                       (char-syntax (char-after))
                     (syntax-after (point)))))
         (stax (cond ((eq elt 0) "0 whitespace")
                     ((eq elt 5) "5 close parenthesis")
                     ((eq elt 10) "10 character quote")
                     ((eq elt 1) "1 punctuation")
                     ((eq elt 6) "6 expression prefix")
                     ((eq elt 11) "11 comment-start")
                     ((eq elt 2) "2 word")
                     ((eq elt 7) "7 string quote")
                     ((eq elt 12) "12 comment-end")
                     ((eq elt 3) "3 symbol")
                     ((eq elt 8) "8 paired delimiter")
                     ((eq elt 13) "13 inherit")
                     ((eq elt 4) "4 open parenthesis")
                     ((eq elt 9) "9 escape")
                     ((eq elt 14) "14 generic comment")
                     ((eq elt 15) "15 generic string"))))
    (when arg
      (message (format "%s" stax)))
    (if docu
        (format "%s" stax)
      elt)))

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2019-09/msg00562.html
(defun ar-syntax-class-to-char (syntax-class)
  (aref " .w_()'\"$\/<>@!|" syntax-class))

(defun ar--forward-syntax-class-intern (syntax)
  (skip-syntax-forward
   (char-to-string
    (ar-syntax-class-to-char
     (syntax-class syntax)))))

;;;###autoload
(defun ar-forward-syntax-class ()
  "Behavior like forward-same-syntax."
  (interactive)
  (ar--forward-syntax-class-intern (syntax-after (point))))

(setq ar-forward-syntax-classes-list (list 0 1 2 3 6))
;;;###autoload
(defun ar-forward-syntax-classes ()
  "Skip chars forward belonging to syntax-classes ‘ar-forward-syntax-classes-list’"
  (interactive "^p")
  (let ((orig (point))
	done last)
    (while (and (not (eobp))
		(setq last (point))
		(prog1 (not done)
		  (dolist (ele ar-forward-syntax-classes-list)
		    (ar--forward-syntax-class-intern (list ele)))))
      (unless (< last (point))(setq done t)))
    (< orig (point))))

;;;###autoload
(defun ar-syntax-in-region-atpt (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (let (erg)
      (while (< (point) end)
        (setq erg (concat erg "\n" "\"" (char-to-string (char-after)) "\"" "  is  " (ar-syntax-atpt t)))
        (forward-char 1))
      (message "%s" erg)
      erg)))

;;;###autoload
(defun syntax-bfpt (&optional arg)
  (interactive "p")
  (let ((stax (syntax-after (1- (point)))))
    (when arg
      (message (format "%s" stax)))
    stax))


;;;###autoload
(defun ar-beginning-of-indent ()
  "Go to the beginning of a section of equal indent."
  (interactive)
  (let ((indent (current-indentation))
	(last (line-beginning-position)))
    (while (and (not (bobp))
		(progn (forward-line -1)
		       (= indent (current-indentation)))
		(not (ar-empty-line-p))
		(setq last (line-beginning-position))))
    (goto-char last)
    last))

(defun ar--travel-this-indent-backward (&optional indent)
  "Travel current INDENT backward.

With optional INDENT travel bigger or equal indentation"
  (let ((indent (or indent (current-indentation)))
	(last (line-beginning-position)))
    (while (and (not (bobp))
		(progn (forward-line -1)
		       (= indent (current-indentation)))
		(not (ar-empty-line-p))
		(setq last (line-beginning-position))))
    (goto-char last)
    last))

;;;###autoload
(defun ar-backward-indent ()
  "Go to the beginning of a section of equal indent.

If already at the beginning or before a indent, go to next indent upwards
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (unless (bobp)
    (let (erg)
      (setq erg (ar--travel-this-indent-backward))
      (when erg (goto-char erg))
      erg)))

;;;###autoload
(defun ar-end-of-indent ()
  "Go to the end of a section of equal indentation."
  (interactive)
  (let ((last (line-end-position))
	(indent (current-indentation)))
    (while (and (not (eobp)) (progn (forward-line 1) (and (not (ar-empty-line-p)) (= indent (current-indentation))))(setq last (line-end-position))))
    (goto-char last)
    (point)))

(defun ar--travel-this-indent-forward (indent)
  "Internal use.

Travel this INDENT forward"
  (let (last)
    (while (and (progn (forward-line 1)
		       (eq indent (current-indentation)))
		(not (ar-empty-line-p))
		(setq last (line-end-position))))
    (when last (goto-char last))
    last))

;;;###autoload
(defun ar-forward-indent ()
  "Go to the end of a section of equal indentation..

If already at the end, go down to next indent in buffer
Returns final position when called from inside section, nil otherwise"
  (interactive)
  (let (done
	(last (point))
	(orig (point))
	(indent (current-indentation)))
    (while (and (not (eobp)) (not done) (progn (forward-line 1) (back-to-indentation) (or (ar-empty-line-p) (and (<= indent (current-indentation))(< last (point))(setq last (point)))(setq done t))))
      (and (< indent (current-indentation))(setq done t)))
    (if (and last (< orig last))
	(progn (goto-char last)
	       (end-of-line)
	       (skip-chars-backward " \t\r\n\f"))
      (skip-chars-forward " \t\r\n\f")
      (end-of-line)
      (skip-chars-backward " \t\r\n\f"))
    (and (< orig (point))(point))))

;;;###autoload
(defun ar-sort-indent ()
  (interactive)
  (save-excursion
    (let ((beg (ar-beginning-of-indent))
	  (end (ar-end-of-indent)))
      (when (and beg end)
	(save-restriction
	  (narrow-to-region beg end)
	  (sort-lines nil beg end))))))

;;;###autoload
(defun ar-mark-indent ()
  (interactive)
  (let ((beg (ar-beginning-of-indent))
	(end (ar-end-of-indent)))
    (goto-char end)
    (set-mark (point))
    (goto-char beg)
    (exchange-point-and-mark)))


(defvar ar-restricted-delimit-aktiv
  (list
   'colon
   'cross
   'doubleslash
   'backslash
   'backtick
   'dollar
   'doublequote
   'equalize
   'escape
   'hash
   'hyphen
   'pipe
   'singlequote
   'slash
   'star
   'tild
   'whitespace
   )
  "")

(defvar ar-paired-delimited-passiv-raw
  (list
   '(symboled "`" "'")
   '(braced "{" "}")
   '(bracketed "[" "]")
   '(lesserangled "<" ">")
   '(greaterangled ">" "<")
   '(curvedsinglequoted "‘" "’")
   '(parentized "(" ")")))

(defvar ar-unpaired-delimited-raw
  (list
   '(backslashed "\\\\")
   '(backticked "`")
   '(coloned ":")
   '(dollared "$")
   '(doublequoted "\\\"")
   '(equalized "=")
   '(hyphened "-")
   '(singlequoted "'")
   '(slashed "/")
   '(stared "*")
   '(underscored "_")
   '(whitespaced " ")))

(setq ar-paired-delimit-aktiv-raw
      (list
       '(symbol 96 39)
       '(brace 123 125)
       '(bracket 91 93)
       '(lesserangle 60 62)
       '(greaterangle 62 60)
       '(curvedsinglequote 8216 8217)
       '(curveddoublequote 8220 8221)
       '(parentize 40 41)
       ))

(setq ar-paired-delimit-aktiv
      (list
       'symbol
       'brace
       'bracket
       'lesserangle
       'greaterangle
       'curvedsinglequote
       'curveddoublequote
       'parentize
       ))

(setq ar-atpt-classes
      (list
       'alnum
       'alpha
       'ascii
       'blank
       'cntrl
       'digit
       'graph
       'lower
       'nonascii
       'print
       'punct
       'space
       'upper
       'xdigit
       ))

(setq ar-unpaired-delimit-aktiv-raw
      (list
       '(colon ":")
       '(cross "+")
       '(doubleslash "//")
       '(backslash "\\\\")
       '(backtick "`")
       '(dollar "$")
       '(doublequote "\"")
       '(equalize "=")
       '(escape "\\\\")
       '(hash "#")
       '(hyphen "-")
       '(pipe "|")
       '(singlequote "'")
       '(slash "/")
       '(star "*")
       '(tild "~")
       '(underscore "_")
       '(whitespace " ")
       ))

(setq ar-unpaired-delimit-aktiv
      (list
       'colon
       'cross
       'doubleslash
       'backslash
       'backtick
       'dollar
       'doublequote
       'equalize
       'escape
       'hash
       'hyphen
       'pipe
       'singlequote
       'slash
       'star
       'tild
       'underscore
       'whitespace
       ))

(setq ar-unary-operations
      (list
       'commatize
       'quote
       ))

(setq ar-atpt-data-forms-aktiv-raw
      (list
       '("beginendquote" "\\\\begin{quote}" "\\\\end{quote}" nil 'move 1 nil t nil)
       '("blok" "{% " " %}" nil 'move "1" nil t)
       '("doublebackslash" "\\\\\\\\" "\\\\\\\\" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackslashparen" "\\\\\\\\(" "\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("doublebacktick" "``" "``" 'move "1" nil t 'ar-escaped)
       '("triplebacktick" "```" "```" 'move "1" nil t 'ar-escaped)
       '("backslashparen" "\\\\(" "\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("slashparen" "////(" "////)" nil 'move "1" nil nil 'ar-escaped)
       '("triplequotedq" "\\\"\\\"\\\"" nil 'move 1 nil nil 'ar-escaped)
       '("triplequotesq" "'''" nil 'move 1 nil nil 'ar-escaped)
       '("triplequoted" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       ))

(setq ar-atpt-data-forms-aktiv
      (list
       'beginendquote
       'blok
       'doublebackslash
       'doublebackslashparen
       'doublebacktick
       'triplebacktick
       'backslashparen
       'slashparen
       'triplequotedq
       'triplequotesq
       'triplequoted
       ))

(setq ar-atpt-data-forms-passiv-raw
      (list
       '("beginendquoted" "\\\\begin{quote}" "\\\\end{quote}" nil 'move 1 nil nil nil)
       '("backslashparened" "\\\\\\\\(" "\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("bloked" "{% " " %}" nil 'move "1" nil t)
       '("doublebackslashed" "\\\\\\\\" "\\\\\\\\" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackslashedparen" "\\\\\\\\\\\\\\\\(" "\\\\\\\\\\\\\\\\)" nil 'move "1" nil nil 'ar-escaped)
       '("doublebackticked" "``" "``" nil 'move "1" nil nil 'ar-escaped)
       '("doubleslashed" "//" "//" nil 'move "1" nil nil 'ar-escaped)
       '("slashparened" "////////(" "////////)" nil 'move "1" nil nil 'ar-escaped)
       '("tabledatap" "<td[^>]*>" "</td>" nil 'move "1" nil nil nil)
       '("triplequoted" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       '("triplequoteddq" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       '("triplequotedsq" "\\\"\\\"\\\"\\\\|'''" "\\\"\\\"\\\"\\\\|'''" nil 'move 1 nil nil 'ar-escaped)
       '("xslstylesheetp" "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil 'move "1" nil nil nil)
       ))

(setq ar-atpt-data-forms-passiv
      (list
       'beginendquoted
       'backslashparened
       'bloked
       'doublebackslashed
       'doublebackslashedparen
       'doublebackticked
       'doubleslashed
       'slashparened
       'tabledatap
       'triplequoted
       'triplequoteddq
       'triplequotedsq
       'xslstylesheetp
       ))

(setq ar-atpt-python-list
      (list
       'py-block
       'py-block-or-clause
       'py-class
       'py-clause
       'py-def-or-class
       'py-def
       'py-expression
       'py-partial-expression
       'py-statement
       'py-string
       ))

(setq ar-atpt-python-quoted-raw
      (list
       '(triplequoted "\"\"\"\\\\|'''")
       '(triplequoteddq "\"\"\"")
       '(triplequotedsq "'''")
       ))

(setq ar-atpt-python-quoted
      (list
       'triplequoted
       'triplequoteddq
       'triplequotedsq
       ))

(setq ar-atpt-expression-list
      (list
       'block
       'block-or-clause
       'char
       'class
       'clause
       'def-or-class
       'def
       'delimited
       'expression
       'partial-expression
       'statement
       'string
       'symbol
       ))

(setq ar-atpt-markup-list
      (list
       'beginendquote
       'blok
       'doublebackslashed
       'doublebackticked
       'doublebackslashedparen
       'doubleslashed
       'doubleslashedparen
       'markup
       'mldata
       'mlattribut
       'mltag
       'slashedparen
       'tabledata
       'triplebackticked
       'xslstylesheet
       'xsltemplate
       ))

(setq ar-paired-delimited-passiv-raw
      (list
       '(braced "{" "}")
       '(symboled "`" "'")
       '(bracketed "[" "]")
       '(lesserangled "<" ">")
       '(greaterangled ">" "<")
       '(curvedsinglequoted "‘" "’")
       '(curveddoublequoted "“" "”")
       '(parentized "(" ")")
       ))

(setq ar-paired-delimited-passiv
      (list
       'braced
       'symboled
       'bracketed
       'lesserangled
       'greaterangled
       'curvedsinglequoted
       'curveddoublequoted
       'parentized
       ))

(setq ar-unpaired-delimited-passiv-raw
      (list
       '(backslashed "\\\\")
       '(backticked "`")
       '(coloned ":")
       '(crossed "+")
       '(dollared "$")
       '(doublequoted "\\\"")
       '(equalized "=")
       '(hashed "#")
       '(hyphened "-")
       '(piped "-")
       '(singlequoted "'")
       '(slashed "/")
       '(stared "*")
       '(tilded "~")
       '(underscored "_")
       '(whitespaced " ")
       ))

(setq ar-unpaired-delimited-passiv
      (list
       'backslashed
       'backticked
       'coloned
       'crossed
       'dollared
       'doublequoted
       'equalized
       'hashed
       'hyphened
       'piped
       'singlequoted
       'slashed
       'stared
       'tilded
       'underscored
       'whitespaced
       ))

(setq ar-atpt-region-only
      (list
       'region
       ))

(setq ar-atpt-rest-list
      (list
       'lesseranglednested
       'comment
       'csv
       'date
       'email
       'filename
       'filenamenondirectory
       'float
       'function
       'ip
       'isbn
       'line
       'list
       'name
       'number
       'page
       'paragraph
       'phone
       'region
       'sentence
       'sexp
       'shstruct
       'url
       'word
       'wordalphaonly
       ))

(setq ar-atpt-major-forms-restricted-list
      (list
       'page
       'paragraph
       'region
       ))

(setq ar-atpt-counts-list
      (list
       'anglednonest
       'greateranglednested
       'lesseranglednested
       'csv
       'line
       'paragraph
       'region
       'sentence
       'string
       ))




(provide 'ar-thingatpt-utils-core)
;;; ar-thingatpt-utils-core.el ends here
