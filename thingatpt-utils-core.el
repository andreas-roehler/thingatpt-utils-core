;;; thingatpt-utils-core.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Andreas Röhler, unless
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
;; `alnum' or a new defined thing.

;; For example `ar-alnum-atpt' will return all
;; alpha-numerical chars below and around cursor as a
;; string. `ar-bounds-of-alnum-atpt' returns the
;; borders of that string as a list and so on.

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
;; ar-THING-leftrightsinglequote-atpt
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
;; `ar-paired-delimit-aktiv', `ar-paired-delimited-passiv', etc.

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
;; behaviour in such, as `ar-forward-word-atpt' stops
;; not after THING, but on the last char of
;; THING. That's in order to enable a call of
;; thing-at-point functions at the end
;; position. Otherwise, when cursor stops after word
;; (THING) as does `forward-word', `ar-word-atpt' would return
;; nil.

;; To see other features, maybe try `ar-separate-list-atpt'
;; or `ar-comment-list-atpt' while point is inside a
;; list. Try it again with an abstract char-class as
;; [:alnum:], i.e. try `ar-comment-alnum-atpt',
;; `ar-brace-alnum-atpt' etc.

;; This utility comes with test-functions which return
;; the possible results of most functions (exception
;; are the kill-fns). Call th-test, th-mv-test
;; or th-delimtest over text. That-delimtest
;; changes but restores the buffer. Customize the speed
;; of execution via `ar-th-test-delay'

;; Diffs to basics of required thingatpt.el:
;; `bounds-of-thing-at-point' is replaced by a new
;; `ar-th-bounds', which now first searches
;; backward. As a consequence several
;; `beginning-op-at' and `end-op-at' constructs had
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
;; `ar-doublequoted-atpt' will return it, copied into the kill-ring,
;; enabling yanking it and a lot of further actions.

;; `ar-doublequoted-atpt' here is to
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
(require 'beg-end)
(require 'hideshow)
(defconst Emacs-Werkstatt-version "1.5")

(when (featurep 'xemacs) (require 'overlay))

(defgroup werkstatt nil
  "Return, mover over or manipulate a given THING."
  :prefix "ar-"
  :group 'matching)

(defcustom sort-fold-case nil
  "Whether alphabetic case affects the sort order

Used by `ar-sort-numbers-subr'"

  :type 'boolean
  :group 'werkstatt)

(defcustom match-paren-no-use-syntax-pps nil
  "If `match-paren' should avoid scanning lists according to syntax but search regexp based. "
  :type 'boolean
  :group 'werkstatt)

(defcustom ar-werkstatt-hs-minor-mode-p nil
  ""

  :type 'boolean
  :group 'werkstatt)

(defcustom thing-copy-region t
  "If a found THING should be copied into the kill-ring. "
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
`unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom alpha "[a-zA-ZäöüßÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom ascii "[\000-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom blank "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom cntrl "[\000-\006]\016-\037]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom digit "[0-9]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom graph "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom lower "[a-zäöüß]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom multibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom nonascii "[^\040-\177]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom print "[\041-\177\241-\377]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom punct "[.,-_:;?!]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom space "[ \t]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom unibyte "[.]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom upper "[A-ZÄÖÜ]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
    :type 'regexp
    :group 'werkstatt))

(when (featurep 'xemacs)
  (defcustom xdigit "[0-9.,]"
    "Rexexp to specify the character class
Change it, if you want to pick strings differently.
XEmacs-users: `unibyte' and `multibyte' class is unused i.e. set to \".\""
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
       (if (< 0 (skip-chars-forward "[:alnum:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:alnum:]"))
	   (and (< 0 (skip-chars-forward "[:alnum:]"))
		(point))))))

(put 'alnum 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:alnum:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:alnum:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:alnum:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:alpha:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:alpha:]"))
	   (and (< 0 (skip-chars-forward "[:alpha:]"))
		(point))))))

(put 'alpha 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:alpha:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:alpha:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:alpha:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:ascii:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:ascii:]"))
	   (and (< 0 (skip-chars-forward "[:ascii:]"))
		(point))))))

(put 'ascii 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:ascii:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:ascii:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:ascii:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:blank:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:blank:]"))
	   (and (< 0 (skip-chars-forward "[:blank:]"))
		(point))))))

(put 'blank 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:blank:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:blank:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:blank:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:cntrl:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:cntrl:]"))
	   (and (< 0 (skip-chars-forward "[:cntrl:]"))
		(point))))))

(put 'cntrl 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:cntrl:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:cntrl:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:digit:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:digit:]"))
	   (and (< 0 (skip-chars-forward "[:digit:]"))
		(point))))))

(put 'digit 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:digit:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:digit:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:digit:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:graph:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:graph:]"))
	   (and (< 0 (skip-chars-forward "[:graph:]"))
		(point))))))

(put 'graph 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:graph:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:graph:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:graph:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:lower:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:lower:]"))
	   (and (< 0 (skip-chars-forward "[:lower:]"))
		(point))))))

(put 'lower 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:lower:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:lower:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:lower:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:nonascii:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:nonascii:]"))
	   (and (< 0 (skip-chars-forward "[:nonascii:]"))
		(point))))))

(put 'nonascii 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:nonascii:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:nonascii:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:print:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:print:]"))
	   (and (< 0 (skip-chars-forward "[:print:]"))
		(point))))))

(put 'print 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:print:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:print:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:print:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:punct:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:punct:]"))
	   (and (< 0 (skip-chars-forward "[:punct:]"))
		(point))))))

(put 'punct 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:punct:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:punct:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:punct:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:space:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:space:]"))
	   (and (< 0 (skip-chars-forward "[:space:]"))
		(point))))))

(put 'space 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:space:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:space:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:space:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:upper:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:upper:]"))
	   (and (< 0 (skip-chars-forward "[:upper:]"))
		(point))))))

(put 'upper 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:upper:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:upper:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:upper:]")))
	    (point))))))

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
       (if (< 0 (skip-chars-forward "[:xdigit:]"))
	   (point)
	 (when (< 0 (skip-chars-forward "^[:xdigit:]"))
	   (and (< 0 (skip-chars-forward "[:xdigit:]"))
		(point))))))

(put 'xdigit 'backward-op-at
     (lambda ()
       (if (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	   (point)
	 (when (< 0 (abs (skip-chars-backward "^[:xdigit:]")))
	   (and
	    (< 0 (abs (skip-chars-backward "[:xdigit:]")))
	    (point))))))

;; Paired delimited forms start

;; Braced
(put 'braced 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?{ (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "{" "}" nil 'move 0 nil nil 'ar-syntax t))))

(put 'braced 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?{ (char-after)))
	 (forward-char 1))
       (end-of-form-base "{" "}" nil 'move 0 nil nil 'ar-syntax t)))

(put 'braced 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?{ ar-thing-escaped ar-thing-inside-comments 'braced))))

;; Bracketed
(put 'bracketed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?\[ (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "\\[" "\]" nil 'move 0 nil t 'ar-syntax t))))

(put 'bracketed 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?\[ (char-after)))
	 (forward-char 1))
       (end-of-form-base "\\[" "\]" nil 'move 0 nil t 'ar-syntax t)))

(put 'bracketed 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?\[ ar-thing-escaped ar-thing-inside-comments 'bracketed))))

;; Lesserangled
(put 'lesserangled 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?< (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "<" ">" nil 'move 0 nil nil 'ar-syntax t))))

(put 'lesserangled 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?< (char-after)))
	 (forward-char 1))
       (end-of-form-base "<" ">" nil 'move 0 nil nil 'ar-syntax t)))

(put 'lesserangled 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?< ar-thing-escaped ar-thing-inside-comments 'lesserangled))))

;; Greaterangled
(put 'greaterangled 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?> (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base ">" "<" nil 'move 0 nil nil 'ar-syntax t))))

(put 'greaterangled 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?> (char-after)))
	 (forward-char 1))
       (end-of-form-base ">" "<" nil 'move 0 nil nil 'ar-syntax t)))

(put 'greaterangled 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?> ar-thing-escaped ar-thing-inside-comments 'greaterangled))))

;; Leftrightsinglequoted
(put 'leftrightsinglequoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?‘ (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "‘" "’" nil 'move 0 nil nil 'ar-syntax t))))

(put 'leftrightsinglequoted 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?‘ (char-after)))
	 (forward-char 1))
       (end-of-form-base "‘" "’" nil 'move 0 nil nil 'ar-syntax t)))

(put 'leftrightsinglequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?‘ ar-thing-escaped ar-thing-inside-comments 'leftrightsinglequoted))))

;; Leftrightdoublequoted
(put 'leftrightdoublequoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?“ (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "“" "”" nil 'move 0 nil nil 'ar-syntax t))))

(put 'leftrightdoublequoted 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?“ (char-after)))
	 (forward-char 1))
       (end-of-form-base "“" "”" nil 'move 0 nil nil 'ar-syntax t)))

(put 'leftrightdoublequoted 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?“ ar-thing-escaped ar-thing-inside-comments 'leftrightdoublequoted))))

;; Parentized
(put 'parentized 'beginning-op-at
     (lambda ()
       (if (ignore-errors (char-equal ?\( (char-after)))
	   (list (point) (1+ (point)))
	 (beginning-of-form-base "\(" "\)" nil 'move 0 nil nil 'ar-syntax t))))

(put 'parentized 'end-op-at
     (lambda ()
       (when (ignore-errors (char-equal ?\( (char-after)))
	 (forward-char 1))
       (end-of-form-base "\(" "\)" nil 'move 0 nil nil 'ar-syntax t)))

(put 'parentized 'forward-op-at
     (lambda ()
       (unless (eobp)
	 (forward-char 1)
	 (ar-char-paren-delimiters-forward ?\( ar-thing-escaped ar-thing-inside-comments 'parentized))))


;; Paired delimited forms end

;; Unpaired delimited forms start

;; Backslashed
(put 'backslashed 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?\\ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'backslashed 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?\\)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?\\ ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'backslashed 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?\\)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?\\ ar-thing-escaped ar-thing-inside-comments)))

(put 'backslashed 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?\\ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Backticked
(put 'backticked 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?` ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'backticked 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?`)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?` ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'backticked 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?`)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?` ar-thing-escaped ar-thing-inside-comments)))

(put 'backticked 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?` ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Coloned
(put 'coloned 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?: ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'coloned 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?:)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?: ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'coloned 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?:)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?: ar-thing-escaped ar-thing-inside-comments)))

(put 'coloned 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?: ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Dollared
(put 'dollared 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?$ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'dollared 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?$)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?$ ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'dollared 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?$)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?$ ar-thing-escaped ar-thing-inside-comments)))

(put 'dollared 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?$ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Doublequoted
(put 'doublequoted 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?\" ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'doublequoted 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?\")
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?\" ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'doublequoted 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?\")
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?\" ar-thing-escaped ar-thing-inside-comments)))

(put 'doublequoted 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?\" ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Equalized
(put 'equalized 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?= ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'equalized 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?=)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?= ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'equalized 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?=)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?= ar-thing-escaped ar-thing-inside-comments)))

(put 'equalized 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?= ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Hyphened
(put 'hyphened 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?- ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'hyphened 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?-)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?- ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'hyphened 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?-)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?- ar-thing-escaped ar-thing-inside-comments)))

(put 'hyphened 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?- ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Singlequoted
(put 'singlequoted 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?' ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'singlequoted 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?')
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?' ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'singlequoted 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?')
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?' ar-thing-escaped ar-thing-inside-comments)))

(put 'singlequoted 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?' ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Slashed
(put 'slashed 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?/ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'slashed 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?/)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?/ ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'slashed 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?/)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?/ ar-thing-escaped ar-thing-inside-comments)))

(put 'slashed 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?/ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Stared
(put 'stared 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?* ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'stared 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?*)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?* ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'stared 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?*)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?* ar-thing-escaped ar-thing-inside-comments)))

(put 'stared 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?* ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Underscored
(put 'underscored 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?_ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'underscored 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ?_)
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?_ ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'underscored 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ?_)
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?_ ar-thing-escaped ar-thing-inside-comments)))

(put 'underscored 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?_ ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Whitespaced
(put 'whitespaced 'beginning-op-at
     (lambda ()
       (let ((beg (ar-char-delimiters-beginning ?  ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))
	 (when beg
	   (cons beg (1+ beg))))))

(put 'whitespaced 'end-op-at
     (lambda ()
       (when (char-equal (char-after) ? )
         (forward-char 1))
       (let ((end (ar-char-delimiters-end ?  ar-thing-escaped ar-thing-inside-comments)))
         (when end
	   (cons (1- end) end)))))

(put 'whitespaced 'forward-op-at
     (lambda ()
       ;; (when (char-equal (char-after) ? )
       ;;   (forward-char 1))
       (ar-char-delimiters-forward ?  ar-thing-escaped ar-thing-inside-comments)))

(put 'whitespaced 'backward-op-at
     (lambda ()
       (ar-char-delimiters-beginning ?  ar-thing-escaped ar-thing-inside-comments ar-scan-whole-buffer)))


;; Unpaired delimited forms end


;; Expression
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

;; Block-Or-Clause
(put 'block-or-clause 'beginning-op-at
     (lambda ()
       (ar-backward-block-or-clause)))

(put 'block-or-clause 'end-op-at
     (lambda ()
       (ar-forward-block-or-clause)))

;; Def-Or-Class
(put 'def-or-class 'beginning-op-at
     (lambda ()
       (ar-backward-def-or-class)))

(put 'def-or-class 'end-op-at
     (lambda ()
       (ar-forward-def-or-class)))

;; Class
(put 'class 'beginning-op-at
     (lambda ()
       (ar-backward-class)))

(put 'class 'end-op-at
     (lambda ()
       (ar-forward-class)))

;; Clause
(put 'clause 'beginning-op-at
     (lambda ()
       (ar-backward-clause)))

(put 'clause 'end-op-at
     (lambda ()
       (ar-forward-clause)))

;; Def
(put 'def 'beginning-op-at
     (lambda ()
       (ar-backward-def)))

(put 'def 'end-op-at
     (lambda ()
       (ar-forward-def)))

;; Statement
(put 'statement 'beginning-op-at
     (lambda ()
       (ar-backward-statement)))

(put 'statement 'end-op-at
     (lambda ()
       (ar-forward-statement)))

(put 'statement 'backward-op-at
     (lambda ()
       (ar-backward-statement)))

(put 'statement 'forward-op-at
     (lambda ()
       (ar-forward-statement)))

;; String
(put 'string 'beginning-op-at
     (lambda ()
       (save-restriction
	 (widen)
	 (if ar-use-parse-partial-sexp
	     (let* ((pps (parse-partial-sexp (point-min) (point)))
		    (pos8 (nth 8 pps)))
	       (when (nth 3 pps)
		 (goto-char pos8)))
	   (when
	       (re-search-backward "\\([^\\\\]\\)\\(\"\\)" nil 'move 1)
	     (goto-char (match-beginning 2))))
	 (when (or (eq 15 (car (syntax-after (point))))(eq 7 (car (syntax-after (point)))))
	   (list (point) (save-excursion  (skip-chars-forward (char-to-string (char-after)))(point)))))))

(put 'string 'end-op-at
     (lambda ()
       (forward-sexp)
       (cons  (1- (point)) (point))))

;; ;; Strings
;; (put 'string 'beginning-op-at
;;      (lambda ()
;;        (save-restriction
;; 	 (widen)
;; 	 (if ar-use-parse-partial-sexp
;; 	     (let* ((pps (parse-partial-sexp (point-min) (point)))
;; 		    (pos8 (nth 8 pps)))
;; 	       (when (nth 3 pps)
;; 		 (goto-char pos8)))
;; 	   (when
;; 	       (re-search-backward "\\([^\\\\]\\)\\(\"\\)" nil 'move 1)
;; 	     (goto-char (match-beginning 2))))
;; 	 (when (looking-at "\"*")
;; 	   (list (match-beginning 0) (match-end 0))))))

;; (put 'string 'end-op-at
;;      (lambda ()
;;        (save-restriction
;; 	 (widen)
;; 	 (forward-char 1)
;; 	 (if ar-use-parse-partial-sexp
;; 	     (let* ((orig (point))
;; 		    (pps (parse-partial-sexp (point-min) (point)))
;; 		    (char (char-to-string (nth 3 pps)))
;; 		    (done t))
;; 	       (progn
;; 		 (while (and (not (eobp)) (prog1 done (forward-char 1))
;; 			     (setq done (skip-chars-forward (concat "^" char)))

;; 			     (nth 5 (parse-partial-sexp orig (point)))))
;; 		 (when (and (< orig (point))(looking-at char))
;; 		   (list (match-beginning 0) (match-end 0)))))
;; 	   (when (re-search-forward "[^\\\\]\"" nil 'move 1)
;; 	     (list (match-beginning 0) (match-end 0)))))))

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
       (let* ((orig (point))
              (nesting (not (or (string= "" comment-end)(eq 10 comment-end))))
              (erg (when nesting
                     (if (looking-at comment-start)
                         (cons (match-beginning 0) (match-end 0))
                       (beginning-of-form-base comment-start comment-end nil 'move 1 t))))
              last)
         (unless erg
           (when (looking-at comment-start)
	     (setq erg (cons (match-beginning 0) (match-end 0)))
	     (skip-chars-backward " \t\r\n\f"))
           (while (and (setq last (point))
                       (setq erg (nth 8 (syntax-ppss)))
                       (goto-char erg)
                       (skip-chars-backward " \t\r\n\f")))
           (skip-chars-forward " \t\r\n\f")
	   (when (looking-at comment-start)
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
  (let ((this (point)))
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

;; Delimited
(put 'delimited 'beginning-op-at
     (lambda ()
       (let ((begdel (concat th-beg-delimiter ar-delimiters-atpt))
             (pps (syntax-ppss))
	     erg)
         (cond ((nth 8 pps)
		(or
		 ;; in string
		 (ignore-errors (goto-char (and (nth 3 pps) (nth 8 pps))))
		 ;; in comment
		 (goto-char (nth 8 pps))))
	       ((looking-at (concat "[" th-beg-delimiter "]")))
	       ((eq 5 (car (syntax-after (point))))
		(forward-char 1)
		(forward-list -1))
               ((eq (char-after) 93)
                (beginning-of-form-base "[" "]" nil 'move nil nil t))
	       ((eq (char-after) ?')
                (beginning-of-form-base "[`']" "'" nil 'move nil nil t))
               ((eq (char-after) ?})
                (beginning-of-form-base "{" "}" nil 'move nil nil t))
               ((eq (char-after) 41)
                (beginning-of-form-base "(" ")" nil 'move nil nil t))
               ((looking-at (concat "[" begdel "][
]"))
		(progn
		  (ar-set-delimiter-zeichen)
		  (when (eq (char-after) ar-delimiter-zeichen-atpt)
		    (forward-char -1))
		  (skip-chars-backward (concat "^" (char-to-string ar-delimiter-zeichen-atpt))))
		(when (eq (char-before) ar-delimiter-zeichen-atpt)
		  (forward-char -1)))
               ((nth 1 pps)
		;; brace etc. not covered by (nth 1 pps)
		(skip-chars-backward (concat "^" begdel))
		(when (looking-back (concat "[" begdel "]") (line-beginning-position))
		  (forward-char -1)))
               (t (when (and (not (bobp))(re-search-backward (concat "[" begdel "]") nil 'move 1))
		    ;; (re-search-backward (concat "[" begdel "]") nil 'move 1))
		    (if
			(looking-at (concat "[" begdel "]"))
			(progn
			  (ar-set-delimiter-zeichen)
			  (when (< 0 (setq erg (abs (skip-chars-backward (char-to-string ar-delimiter-zeichen-atpt)))))
			    (setq ar-delimiter-string-atpt (buffer-substring-no-properties (point) (+ erg (point) 1)))))
		      (setq ar-delimiter-zeichen-atpt nil)
		      (setq ar-delimiter-string-atpt nil)))))
	 (cond ((or (looking-at (concat "[" th-beg-delimiter "]"))(looking-at (concat "[" ar-delimiters-atpt "]")))
		(setq ar-delimiter-string-atpt (match-string-no-properties 0))

		(cons (match-beginning 0) (match-end 0)))))))

(put 'delimited 'end-op-at
     (lambda ()
       (if (ignore-errors (looking-at (regexp-quote ar-delimiter-string-atpt)))
	   (progn
	     (goto-char (match-end 0))
	     (if (< (- (match-end 0) (match-beginning 0)) 2)
		 (progn
		   (while
		       (and
			(search-forward (char-to-string (ar--return-complement-char-maybe (char-before))) nil t 1)
			(ar-escaped)
			(nth 8 (parse-partial-sexp (line-beginning-position) (point)))))
		   (cons (match-beginning 0) (match-end 0)))
	       (when (search-forward ar-delimiter-string-atpt nil t 1)
		 (cons (match-beginning 0) (match-end 0)))))
	 (let ((orig (point))
	       (begdel (concat th-beg-delimiter ar-delimiters-atpt))
	       (enddel (or (and ar-delimiter-zeichen-atpt (setq ar-delimiter-zeichen-atpt (ar--return-complement-char-maybe ar-delimiter-zeichen-atpt))) ar-delimiter-string-atpt (concat th-end-delimiter ar-delimiters-atpt))))
	   (if (and enddel (characterp enddel) (setq enddel (char-to-string enddel)))
	       (while (and
		       (prog1
			   (< 0 (abs (skip-chars-forward (concat "^" enddel))))
			 (forward-char 1))
		       (nth 8 (parse-partial-sexp orig (point)))))
	     (cond
	      ((eq 4 (car (syntax-after (point))))
	       (when (looking-at "{")
		 (goto-char (match-end 0))
		 (end-of-form-base "{" "}" nil 'move nil nil t)))
	      ((eq (char-after) 40)
	       (forward-list 1)
	       ;; forward-list doesn't set match-data
	       (looking-back ")" (line-beginning-position)))
	      ((eq (char-after) ar-delimiter-zeichen-atpt)
	       (forward-char 1)
	       (skip-chars-forward (concat "^" (char-to-string ar-delimiter-zeichen-atpt))))
	      ((looking-at (concat "[" begdel "]"))
	       (goto-char (match-end 0))
	       (re-search-forward (concat "[" enddel "]") nil t 1))
	      ((looking-at (concat "[" ar-delimiters-atpt "]"))
	       (ar-set-delimiter-zeichen)
	       (forward-char 1)
	       (if
		   (eq 92 ar-delimiter-zeichen-atpt)
		   (search-forward (prin1-to-string 92))
		 (while (and (search-forward (char-to-string ar-delimiter-zeichen-atpt))
			     (ar-escaped)))))
	      (t (forward-char 1)
		 (search-forward (char-to-string ar-delimiter-zeichen-atpt) nil nil 1))))
	   (setq ar-delimiter-zeichen-atpt nil)
	   (if (eq 5 (car (syntax-after (1- (point)))))
	       (cons (1- (point)) (point))
	     (when (looking-back (concat "[" enddel "]") (line-beginning-position))
	       (cons (match-beginning 0) (match-end 0))))))))

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

(defcustom ar-delimiters-atpt "\"'#\$/=?!:*+~§%&-_\;"
  "Specify the delimiter chars. "
  :type 'string
  :group 'werkstatt)

(defcustom th-beg-delimiter "‘“{<[("
  "Specify the delimiter char."
  :type 'string
  :group 'werkstatt)

(defcustom th-end-delimiter "]}>”)’"
  "Specify the delimiter char."
  :type 'string
  :group 'werkstatt)

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
	 (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t))))

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
	 (beginning-of-form-base markup-startstring-atpt markup-endstring-atpt nil 'move nil nil t)
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
       (let ((case-fold-search t)
	     erg)
	 (cond ((looking-at "#?x?[0-9a-f]+")
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
	       (t
		(while
		    (or
		     (and
		      (re-search-forward "#?[xo]?[a-f0-9]+" nil t 1)
		      (nth 8 (parse-partial-sexp (point-min) (point))))
		     (forward-char -1)
		     (cond ((looking-at "#[xX][a-fA-F0-9]+")
			    (setq erg (point))
			    nil)
			   ((looking-at "#o[0-9]+")
			    (setq erg (point))
			    nil)
			   ((looking-at "[0-9]+")
			    (setq erg (point))
			    nil)
			   (t (forward-char 1)
			      (unless (eobp)
				(forward-char 1)
				(not (eobp)))))))))
	 erg)))

(put 'number 'backward-op-at
     (lambda ()
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
	 erg)))

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
		    (nth 8 (syntax-ppss)))))
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
       (unless (member (char-before) (list 32 ?\t 10 ?\r)) (ignore-errors (backward-sexp)))(point)))

(put 'sexp 'end-op-at
     (lambda ()
       (forward-sexp)(point)))

;; Sh-struct
(put 'shstruct 'beginning-op-at
     'sh-beginning-of-form)

(put 'shstruct 'end-op-at
     (lambda ()
       (when (looking-at ar-beginning-shstruct-atpt)
	 (sh-end-of-form)
	 (forward-char 1)(point))))

(put 'shstruct 'forward-op-at
     (lambda ()
       (re-search-forward ar-beginning-shstruct-atpt nil 'move 1)))

(put 'shstruct 'backward-op-at
     (lambda ()
       (re-search-backward ar-end-shstruct-atpt nil 'move 1)))

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
	   (unless erg (when (looking-at "[^ ]")(setq erg (point)))     )
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

;; Triplequoted
(put 'triplequoted 'beginning-op-at
     (lambda ()
       (let* ((triplequoted "\"\"\"\\|'''")
	      (bounds (ar-in-delimiter-base triplequoted)))
	 (when (car-safe bounds)
	   (goto-char (car-safe bounds))
	   bounds))))

(put 'triplequoted 'end-op-at
     (lambda ()
       (let* ((triplequoted "\"\"\"\\|'''")
	      (erg (looking-at triplequoted)))
	 (when erg
	   (setq triplequoted (match-string-no-properties 0))
	   (goto-char (match-end 0))
	   (setq erg (end-of-form-base triplequoted triplequoted nil 'move 0 nil nil 'ar-escaped)))
	 erg)))

(put 'triplequoted 'forward-op-at
     (lambda ()
       (let ((triplequoted "\"\"\"\\|'''")
	     bounds)
	 (while (and (search-forward triplequoted nil 'move 1)
		     (not (ar-in-delimiter-base triplequoted))))
	 (unless (eobp)
	   (setq bounds (end-of-form-base triplequoted triplequoted nil 'move 0 nil nil 'ar-escaped))
	   (ignore-errors (goto-char (1- (cadr bounds)))))
	 bounds)))

(put 'triplequoted 'backward-op-at
     (lambda ()
       (let ((triplequoted "\"\"\"\\|'''")
	     erg)
	 (while (and (search-backward triplequoted nil 'move 1)
		     (not (setq erg (ar-in-delimiter-base triplequoted)))))
	 (when erg (goto-char erg))
	 erg)))

;; Triplequoted-Dq
(put 'triplequoteddq 'beginning-op-at
     (lambda ()
       (let* ((triplequoteddq "\"\"\"")
	      (bounds (ar-in-delimiter-base triplequoteddq)))
	 (when (car-safe bounds)
	   (goto-char (car-safe bounds))
	   bounds))))

(put 'triplequoteddq 'end-op-at
     (lambda ()
       (let* ((triplequoteddq "\"\"\"")
	      (erg (looking-at triplequoteddq)))
	 (when erg
	   (goto-char (match-end 0))
	   (while (and (search-forward triplequoteddq nil 'move 1)
		       (ar-in-delimiter-base triplequoteddq)))
	   (when (looking-back triplequoteddq (line-beginning-position))
	     (list (match-beginning 0) (match-end 0))
	     )))))

(put 'triplequoteddq 'forward-op-at
     (lambda ()
       (let ((triplequoteddq "\"\"\""))
         (while (and (search-forward triplequoteddq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoteddq))))
         )))

(put 'triplequoteddq 'backward-op-at
     (lambda ()
       (let ((triplequoteddq "\"\"\""))
         (while (and (search-backward triplequoteddq nil 'move 1)
                     (not (ar-in-delimiter-base triplequoteddq)))))))

;; Triplequoted-Sq
(put 'triplequotedsq 'beginning-op-at
     (lambda ()
       (let* ((triplequotedsq "'''")
              (bounds (ar-in-delimiter-base triplequotedsq)))
         (when (car-safe bounds)
           (goto-char (car-safe bounds))
           bounds))))

(put 'triplequotedsq 'end-op-at
     (lambda ()
       (let* ((triplequotedsq "'''")
	      (erg (looking-at triplequotedsq)))
	 (when erg
	   (goto-char (match-end 0))
	   (while (and (search-forward triplequotedsq nil 'move 1)
		       (ar-in-delimiter-base triplequotedsq)))
	   (when (looking-back triplequotedsq (line-beginning-position)) 
	     (list (match-beginning 0) (match-end 0)))))))

(put 'triplequotedsq 'forward-op-at
     (lambda ()
       (let ((triplequotedsq "'''"))
         (while (and (search-forward triplequotedsq nil 'move 1)
                     (not (ar-in-delimiter-base triplequotedsq)))))))

(put 'triplequotedsq 'backward-op-at
     (lambda ()
       (let ((triplequotedsq "'''"))
         (while (and (search-backward triplequotedsq nil 'move 1)
                     (not (ar-in-delimiter-base triplequotedsq)))))))

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
     (lambda () (when (and (or (bolp)(looking-back "\\W"))(looking-at "\\w"))
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
		  (gen-in-string-p-intern pps))))
      (unless erg
	(when (looking-at "\"\\|'")
	  (forward-char 1)
	  (setq pps (parse-partial-sexp (line-beginning-position) (point)))
	  (when (nth 3 pps)
	    (setq erg (gen-in-string-p-intern pps)))))

      ;; (list (nth 8 pps) (char-before) (1+ (skip-chars-forward (char-to-string (char-before)))))
      erg)))

;;
(defcustom copy-or-alternative "word"
  "Copy-or commands may act on thing specified here.

For example when `ar-doublequote-or-copy-atpt' is called with positive
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

(defun ar-trim-symbol-atpt ()
  (interactive "*")
  (let ((erg (ar-bounds-of-symbol-atpt)))
    (goto-char (cdr erg))
    (delete-char -1)
    (goto-char (car erg))
    (delete-char 1))) 

(defun ar-trim-region-atpt ()
  (interactive "*")
  (let ((erg (ar-bounds-of-region-atpt)))
    (goto-char (cdr erg))
    (delete-char -1)
    (goto-char (car erg))
    (delete-char 1))) 


;; ML data-forms start

;; Beginendquoted
(put 'beginendquoted 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\begin{quote}"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\begin{quote}" "\\end{quote}" nil (quote move) 1 nil nil nil))))

(put 'beginendquoted 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\begin{quote}"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "\\begin{quote}" "\\end{quote}" nil (quote move) 1 nil nil nil))))


;; Blok
(put 'blok 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "{% "))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "{% " " %}" nil (quote move) 1 nil t nil))))

(put 'blok 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "{% "))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "{% " " %}" nil (quote move) 1 nil t nil))))


;; Doublebackslashed
(put 'doublebackslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\" "\\\\" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doublebackslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "\\\\" "\\\\" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Doublebackticked
(put 'doublebackticked 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "``"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "``" "``" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doublebackticked 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "``"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "``" "``" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Doubleslashed
(put 'doubleslashed 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "//"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "//" "//" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doubleslashed 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "//"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "//" "//" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Doublebackslashedparen
(put 'doublebackslashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'doublebackslashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\\\\\("))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "\\\\\\\\(" "\\\\\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Tabledatap
(put 'tabledatap 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<td[^>]*>"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<td[^>]*>" "</td>" nil (quote move) 1 nil nil nil))))

(put 'tabledatap 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<td[^>]*>"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "<td[^>]*>" "</td>" nil (quote move) 1 nil nil nil))))


;; Backslashedparen
(put 'backslashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "\\\\("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "\\\\(" "\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'backslashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "\\\\("))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "\\\\(" "\\\\)" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Slashedparen
(put 'slashedparen 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "////////("))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "////////(" "////////)" nil (quote move) 1 nil nil (quote ar-escaped)))))

(put 'slashedparen 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "////////("))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "////////(" "////////)" nil (quote move) 1 nil nil (quote ar-escaped)))))


;; Xslstylesheetp
(put 'xslstylesheetp 'beginning-op-at
     (lambda ()
       (if (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
           (list (match-beginning 0) (match-end 0))
         (beginning-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil (quote move) 1 nil nil nil))))

(put 'xslstylesheetp 'end-op-at
     (lambda ()
       (when (ignore-errors (looking-at "<xsl:stylesheet[^<]+>.*$"))
	 (goto-char (match-end 0)) 
	 (end-of-form-base "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil (quote move) 1 nil nil nil))))


;; ML data-forms end

;; ar-insert-thingatpt-th-funktionen start

;;;###autoload
(defun ar-th (thing &optional no-delimiters iact check)
  "Returns a buffer substring according to THING.
  THING may be a well known form as `symbol',
  `list', `sexp', `defun' or a newly defined THING.
  When mark-thingatpt is `t' - the default - a found THING
  is set as current region, enabling further action on them

  If NO-DELIMITERS set by user functions, THING returned is
  stripped by delimiters resp. markup

  Optional CHECK will count for nesting, otherwise being behind an opening or at a closing delimiter is assumed

 "
  (condition-case nil
      (let* ((no-delimiters (or no-delimiters (eq 4 (prefix-numeric-value no-delimiters))))
	     (check (or check ar-scan-whole-buffer-p))
	     (bounds (ar-th-bounds thing no-delimiters check))
	     (beg (if no-delimiters
		      (cond ((ignore-errors (numberp (car-safe bounds)))
			     (car-safe bounds))
			    ((ignore-errors (caar bounds))
			     (caar bounds))
			    (t (car-safe bounds)))
		    (cond ((ignore-errors (caar bounds))
			   (caar bounds))
			  (t (car-safe bounds)))))
	     (end (if no-delimiters (car-safe (cdr-safe bounds)) (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(cdr bounds))))
	     erg)
	(when (and beg end)
	  (setq erg
		(buffer-substring-no-properties beg end))
	  (when thing-copy-region
	    (ar-th-mark thing nil beg end no-delimiters check))
	  (when thing-copy-region (kill-new erg))
	  erg))
    (error nil)))

(defun ar--th-bounds-char-return (beg end &optional check orig no-delimiters)
  (when (and beg end
	     (not (eq beg end))
	     (or (eobp)
		 (or check
		     (<= orig end))))
    (if no-delimiters
	(cons (1+ beg) (1- beg))
      (cons beg end))))

(defun ar--th-bounds-list-return (beg end &optional iact check orig no-delimiters)
  ;; (message "%s" no-delimiters)
  (let (erg)
    (when
	(and beg end
	     (not (eq beg end))
	     (or (eobp)
		 (or check
		     (<= orig (or
			       (ignore-errors (cadr end))
			       (ignore-errors (cdr end)))))))
      (if no-delimiters
	  (progn
	    (push (car end) erg)
	    (push (cdr beg) erg))
	(push end erg)
	(push beg erg))
      erg)))

;;;###autoload
(defun ar-th-bounds (thing &optional no-delimiters iact check)
  "Determine the start and end buffer locations for the THING at point.
  THING is a symbol which specifies the kind entity you want.

  A boolean value NO-DELIMITERS says if THING boundaries should extend to markups, delimiters or not.
  Call THING by his name, i.e. ar-word-atpt etc. IACT is t, if function has been called interactively

With CHECK count nesting

With NO-DELIMITERS
Returns two lists composed of positions of delimiters 

NO-CHECK assumes being at or behind a closing delimiter, doesn't check for nesting.

"
  (ignore-errors
    (if (eq thing 'region)
	(ignore-errors (cons (region-beginning) (region-end)))
      (save-excursion
	(let* ((orig (point))
               ;; (scan-whole-buffer check)
	       (beg (funcall (get thing 'beginning-op-at)))
	       (end (and beg (funcall (get thing 'end-op-at)))))
	  (if (numberp beg)
	      (ar--th-bounds-char-return beg end check orig no-delimiters)
	    (ar--th-bounds-list-return beg end iact check orig no-delimiters)))))))

(defun ar-th-beg (thing &optional arg iact check)
  "Return beginning position of THING. "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing arg iact check))
	     (beg (or (ignore-errors (caar bounds)) (car-safe bounds))))
	(when iact
	  ;; (message "   %s " beg)
          (kill-new (format "%s" beg)))
	beg)
    (error nil)))

;;;###autoload
(defun ar-th-end (thing &optional arg iact check)
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing arg check))
	     (end (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds)))))
	;; (when iact
	;; (message "   %s "  end))
	end)
    (error nil)))

;;;###autoload
(defun ar-th-gotobeg (thing &optional arg iact check)
  "Goto char beginning, core function "
  (condition-case nil
      (funcall (get thing 'beginning-op-at))
    ;; (let* ((bounds (ar-th-bounds thing arg iact check))
    ;; 	     (beg (caar bounds)))
    ;; 	(when iact
    ;; 	  (message "   %s " beg))
    ;; 	(goto-char beg))
    (error nil)))

;;;###autoload
(defun ar-th-gotoend (thing &optional arg iact check)
  "Goto char end, core function "
  (condition-case nil
      (funcall (get thing 'end-op-at))
    ;; (let* ((bounds (ar-th-bounds thing arg check))
    ;; 	     (end (car (cadr bounds))))
    ;; 	(when iact
    ;; 	  (message "   %s " end))
    ;; 	(if (eq thing 'paragraph)
    ;; 	    (goto-char end)
    ;; 	  (goto-char end)))
    (error nil)))

;;;###autoload
(defun ar-th-length (thing &optional arg iact check)
  (ignore-errors
    (let* ((bounds (ar-th-bounds thing arg check))
	   (beg (caar bounds))
	   (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))
	   (length (- end beg)))
      (when iact
        (message "   %s " (format "%s" length)))
      length)))

(defun ar-th-ratio-base (cla elt &optional beg end ratio iact)
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
    (ar-th-ratio elt cla beg end ratio iact)))

(defun ar-th-ratio (thing cla &optional beg end ratio iact no-delimiters check)
  (save-excursion
    (ignore-errors
      (let* (bounds
             (beg (or beg (and (setq bounds (ar-th-bounds thing no-delimiters iact check)) (caar bounds))))
             (end (or end (cadr (cadr bounds))))
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

;;;###autoload
(defun ar-th-copy (thing &optional arg iact)
  (condition-case nil
      (let ((newcopy (ar-th thing)))
	(when newcopy
          (progn
            (unless (string= newcopy (car kill-ring)) (kill-new newcopy))
            (when iact
              (message "   %s" newcopy))
            newcopy)))
    (error nil)))

;;;###autoload
(defun ar-th-trim (thing &optional no-delimiters iact check left right)
  "Trims given THING at point.
If boundaries of thing are know, use `ar-th-trim-base' directly. "
  (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
         (beg (or (ignore-errors (caar bounds)) (car-safe bounds)))
         (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (ar-th-trim-base beg end left right)))

(defun ar-th-trim-base (beg end left right)
  "Trim buffer-substring resp. to args starting-point, end-point, left-trim, right-trim. "
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
	     (eq (marker-position end) (- old-end 2))))))

;;;###autoload
(defun ar-th-trim-left (thing &optional no-delimiters iact check)
  (ar-th-trim thing no-delimiters iact check t))

;;;###autoload
(defun ar-th-trim-right (thing &optional no-delimiters iact check)
  (ar-th-trim thing no-delimiters iact check nil t))

;;;###autoload
(defun ar-th-peel (thing &optional no-delimiters iact check)
  "Remove the outer element of an hierarchical form.

\(foo (bar baz)) --> (bar baz)
--^-----------

\[foo [bar baz]] --> [bar baz]
--^-----------

Inspired by stuff like `paredit-splice-sexp-killing-backward'; however, instead of working `-backward' or `-forward' deletes expression at point.

"
  (let* ((outer (ar-th-bounds thing no-delimiters iact check))
	 (outer-start (caar outer))
	 (outer-end (copy-marker (or (ignore-errors (cadr (cadr outer)))(cdr (cadr outer))(car (cadr outer)))))
	 inner-start inner-end)
    (when (eq (point) outer-start)(forward-char 1))
    (skip-syntax-forward "^(")
    (setq inner-start (copy-marker (point)))
    (forward-sexp)
    (delete-region (point) outer-end)
    (backward-sexp)
    (delete-region (point) outer-start)))

(defun ar-th-comment (thing &optional no-delimiters iact check)
  "Comment or uncomment THING "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
	     (beg (caar bounds))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	(when (and beg end)
	  (goto-char beg)
	  (comment-region beg end)))
    (error nil)))

;;;###autoload
(defun ar-th-mark (thing &optional bounds beg end no-delimiters iact check)
  " "
  (condition-case nil
      (let* ((bounds (unless (and beg end) (or bounds (ar-th-bounds thing no-delimiters iact check))))
	     (beg (or beg (ignore-errors (caar bounds))))
	     (end (or end (or (ignore-errors (cadr (cadr bounds))) (ignore-errors (cdr (cadr bounds)))))))
	(when (and beg end)
	  (goto-char beg)
	  (push-mark (point) t t)
	  (goto-char end)
	  (exchange-point-and-mark)))
    (error nil)))

;; uses sgml-tag from sgml-mode.el
;;;###autoload
(defun ar-th-hide (thing &optional beg end no-delimiters iact check)
  "Hide visibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters iact check))
      (setq beg (or (ignore-errors (caar bounds))(car-safe bounds)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr bounds)))))
    (if (and beg end)
        (progn
          (hs-make-overlay beg end 'code)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

;;;###autoload
(defun ar-th-show (thing &optional beg end no-delimiters iact check)
  "Remove invisibility of existing things at point. "
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t) bounds)
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters iact check))
      (setq beg (or (ignore-errors (caar bounds))(point-min)))
      (setq end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cdr end))(point-max))))
    (if (and beg end)
        (progn
          (hs-discard-overlays beg end)
          (set-buffer-modified-p modified))
      (error (concat "No " (format "%s" thing) " at point!")))))

;;;###autoload
(defun ar-th-hide-show (&optional thing beg end no-delimiters iact check)
  "Toggle visibility of existing things at point. "
  (interactive "p")
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        bounds beg end)
    (setq beg (or beg (and (use-region-p) (region-beginning))))
    (setq end (or end (and (use-region-p) (region-end))))
    (unless (and beg end)
      (setq bounds (ar-th-bounds thing no-delimiters iact check))
      (setq beg (caar bounds))
      (setq end (cadr (cadr bounds))))
    (if (overlays-in beg end)
        (hs-discard-overlays beg end)
      (hs-make-overlay beg end 'code))
    (set-buffer-modified-p modified)))

;;;###autoload
(defun ar-th-separate (thing &optional arg iact check)
  "Optional CHECK is ignored "
  (let* ((bounds (ar-th-bounds thing arg iact check))
         (beg (copy-marker (caar bounds)))
         (end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))))
    (goto-char beg)
    (when (not (looking-back "^[ \t\r\f\n]*" (line-beginning-position)))
      (newline ar-newlines-separate-before))
    (indent-according-to-mode)
    (goto-char end)
    (when (not (looking-at "^[ \t\r\f\n]*$"))
      (newline))))

;;;###autoload
(defun ar-thing-in-thing (thing-1th thing-2th th-function &optional iact beg-2th end-2th)
  "Addresses things of 1th kind within the borders of the 2th,
If optional positions BEG-2TH END-2TH are given, works on them instead. "
  (let* ((bounds (ar-th-bounds thing-2th))
	 (beg (or (ignore-errors (caar bounds))(car-safe bounds)))
	 (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(cdr-safe bounds)))
	 (orig beg)
         ar-scan-whole-buffer)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (if (eq th-function 'ar-th-sort)
            (ar-th-sort thing-1th nil beg end nil nil nil)
          (while (and (not (eobp))
		      (ar-th-forward thing-1th 1 iact t)
		      (<= orig (point)))
	    (setq orig (point))
	    (funcall th-function thing-1th nil iact ar-scan-whole-buffer)
	    ;; forward might stop at the opener
	    ;; ‘ar-scan-whole-buffer’ is let-bound to nil here
	    (unless (eobp)
	      (when (member thing-1th ar-unpaired-delimited-passiv)
		(forward-char 1)))

	    (when (< (point) orig)(goto-char orig))))))))

;;;###autoload
(defun ar-th-kill (thing &optional no-delimiters iact check)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
	     (beg (or (ignore-errors (caar bounds)) (ignore-errors (car bounds))))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cadr (cadr bounds))))))
	(kill-region beg end))
    (error nil)))

(defun ar-th-delete (thing &optional no-delimiters iact check)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
	     (beg (or (ignore-errors (caar bounds)) (ignore-errors (car bounds))))
	     (end (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr bounds))(ignore-errors (cadr (cadr bounds))))))
	(delete-region beg end))
    (error nil)))

(defun ar-th-delete-in-region (thing beg end &optional iact check no-delimiters)
  "Delete THING in region. Delete line, if empty afterwards. "
  (condition-case nil
      (save-excursion
        (goto-char beg)
	(let ((orig (point)))
	  (while (progn (ar-th-forward thing) (< orig (point)))
	    (let ((bounds (ar-th-bounds thing no-delimiters iact check)))
	      (delete-region (caar bounds) (cadr (cadr bounds)))
	      ;; (when iact (message "%s at pos %d %d %s " thing (caar bounds) (cadr (cadr bounds)) "deleted"))
	      (when (and (empty-line-p) (not (eobp)))
		(delete-region (line-beginning-position) (1+ (line-end-position))))))))))

(defun ar-th-commatize (thing &optional no-delimiters iact check)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
	     (beg (caar bounds))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	(goto-char end)
        (insert ","))
    (error nil)))

(defun ar-th-quote (thing &optional no-delimiters iact check)
  " "
  (condition-case nil
      (let* ((bounds (ar-th-bounds thing no-delimiters iact check))
	     (beg (caar bounds))
	     (end (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	(goto-char beg)
        (insert "'"))
    (error nil)))

(defun ar-th-interactive-backward-form (ap ep)
  (goto-char ep)
  (push-mark ap)
  (exchange-point-and-mark)
  (kill-new (buffer-substring-no-properties ap ep)))

(defun ar-th-set-bounds (thing &optional no-delimiters iact check)
  "Sets values of `bounds', `ap' and `ep' -- beg- and endpoint. "
  (setq bounds (ar-th-bounds thing no-delimiters iact check))
  (setq ap (caar bounds))
  (setq ep (or (ignore-errors (cdr (cadr bounds)))(cadr (cadr bounds)))))

(defun ar-th-backward-fallback (arg thing)
  (let (bounds ap ep last)
    (while
	(> 0 arg)
      (setq arg (1+ arg))
      (ar-th-set-bounds thing))
    (when ap
      (if iact
	  (ar-th-interactive-backward-form ap ep)
	(goto-char ap))
      (point))))

(defun ar-th-forward-fallback (arg after thing)
  (let (bounds ap ep last)
    (while (< 0 arg)
      (setq arg (1- arg))
      (ar-th-set-bounds thing)
      (when ep
	(if after (goto-char ep)(goto-char (1- ep)))
	(setq last (point))))
    last))

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

(defun ar-th-forward (thing &optional arg iact after)
  "Return end-position, if successful, nil otherwise.

searches backward with negative argument "
  (let ((orig (point))
	(arg (or arg 1))
	erg)
    (if (< 0 arg)
	(progn
	  (if (functionp (get thing 'forward-op-at))
	      (setq erg (ar-th-forward-function-call thing arg))
	    (setq erg (ar-th-forward-fallback arg after thing)))
	  (when (or (ignore-errors (< orig erg)) (ignore-errors (< orig (car-safe (cdr-safe erg)))))
	    (point)))
      (when (functionp (get thing 'backward-op-at))
	(progn
	  (or
	   (setq erg (ar-th-backward-function-call arg thing))
	   (setq erg (ar-th-backward-fallback arg thing)))
	  (when (< erg orig)
	    erg))))))

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
	       (setq thisend (copy-marker (point)))
	       (delete-region (car begstringpos) (cadr begstringpos))
	       (list thisbeg thisend))))
    (widen)))

(defun ar-th-backward (thing &optional arg iact check)
  "Returns beg and end of THING before point as a list. "
  (condition-case nil
      (ar-th-forward thing (- (or arg 1)) iact check)
    (error nil)))

(defvar paired-start-pos nil)

(defun ar-th-transpose (thing &optional no-delimiters iact check)
  "Returns position, when called from a program
 end of transposed section. "
  (let* ((pos (copy-marker (point)))
         (first (ar-th-bounds thing no-delimiters iact check))
         (pos1 (if (ignore-errors (<= (car first) pos))
                   first
                 (ar-th-bounds thing no-delimiters iact check)))
         (pos2 (progn
                 (when (ignore-errors (< 1 arg))
                   (ar-th-forward thing no-delimiters iact check))
                 (ar-th-bounds thing no-delimiters iact check)))
         (a (car pos1))
         (b (copy-marker (cdr pos1)))
         (c (car pos2))
         (d (copy-marker (cdr pos2))))
    (transpose-regions a b c d)
    (if iact
        (progn
          (message "%s" (point))
          (point))
      (goto-char d)
      d)))

;; credits to sort-subr, sort.el
;; (reverse nextrecfun endrecfun &optional startkeyfun endkeyfun predicate)
(defun ar-th-sort (thing reverse beg end startkeyfun endkeyfun predicate)
  (save-excursion
    (save-restriction
      (unless (buffer-narrowed-p)(narrow-to-region beg end))
      (goto-char (point-min))
      (let ((reverse (or reverse nil))
            (startkeyfun (or startkeyfun nil))
            (endkeyfun (or endkeyfun nil))
            (predicate (or predicate nil))
            (this-beg (ar-th-beg thing 0)))
        (while (not (or (eobp)(stringp (ar-th thing))))
          (forward-char 1))
        (if (eq thing 'number)
	    (ar-sort-numbers-subr reverse
				  (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
				  (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun predicate)
          (sort-subr reverse
		     (function (lambda () (if (ar-th-forward thing) (ar-th-gotobeg thing) (goto-char (point-max)))))
		     (function (lambda () (ar-th-gotoend thing)(forward-char 1))) startkeyfun endkeyfun predicate))))))

(defun ar-sort-numbers-subr (reverse nextrecfun endrecfun
                                     &optional startkeyfun endkeyfun predicate)
  "A patched sort-subr. Divides buffer into records and sort them.

We divide the accessible portion of the buffer into disjoint pieces
called sort records.  A portion of each sort record (perhaps all of
it) is designated as the sort key.  The records are rearranged in the
buffer in order by their sort keys.  The records may or may not be
contiguous.

Usually the records are rearranged in order of ascending sort key.
If REVERSE is non-nil, they are rearranged in order of descending sort key.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.

The next four arguments are functions to be called to move point
across a sort record.  They will be called many times from within sort-subr.

NEXTRECFUN is called with point at the end of the previous record.
It moves point to the start of the next record.
It should move point to the end of the buffer if there are no more records.
The first record is assumed to start at the position of point when sort-subr
is called.

ENDRECFUN is called with point within the record.
It should move point to the end of the record.

STARTKEYFUN moves from the start of the record to the start of the key.
It may return either a non-nil value to be used as the key, or
else the key is the substring between the values of point after
STARTKEYFUN and ENDKEYFUN are called.  If STARTKEYFUN is nil, the key
starts at the beginning of the record.

ENDKEYFUN moves from the start of the sort key to the end of the sort key.
ENDKEYFUN may be nil if STARTKEYFUN returns a value or if it would be the
same as ENDRECFUN.

PREDICATE is the function to use to compare keys.  If keys are numbers,
it defaults to `<', otherwise it defaults to `string<'."
  ;; Heuristically try to avoid messages if sorting a small amt of text.
  (let ((messages (> (- (point-max) (point-min)) 50000)))
    (save-excursion
      (if messages (message "Finding sort keys..."))
      (let* ((sort-lists (sort-build-lists nextrecfun endrecfun
					   startkeyfun endkeyfun))
	     (old (reverse sort-lists))
	     (case-fold-search sort-fold-case))
	(if (null sort-lists)
	    ()
	  (or reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Sorting records..."))
	  (setq sort-lists
		(sort sort-lists
                      (lambda (a b)
                        (< (string-to-number (buffer-substring-no-properties (caar a) (cdar a)))(string-to-number (buffer-substring-no-properties (caar b)(cdar b)))))))
	  (if reverse (setq sort-lists (nreverse sort-lists)))
	  (if messages (message "Reordering buffer..."))
	  (sort-reorder-buffer sort-lists old)))
      (if messages (message "Reordering buffer... Done"))))
  nil)

(defun delim-slash-function (arg)
  "Insert ARG backslashes. "
  (when arg
    (dotimes (i arg) (insert 47))))

(defun ar-th-delim-intern (thing &optional beg end beg-char end-char arg)
  (ignore-errors
    (let* ((begstr (or beg-char th-beg-delimiter))
           (endstr (or end-char th-end-delimiter)))
      (when beg
        (goto-char beg)
        (delim-slash-function arg)
        (insert begstr)
        (goto-char end)
        (delim-slash-function arg)
        (insert endstr)
        (setq done t))
      (when (< (point) end)
        (ar-th-forward thing)
        (ar-th-delim-intern thing beg end beg-char end-char)))))

(defun ar-th-delim (thing &optional arg beg-char end-char iact beg end)
  "Process begin and end of region according to value of
  `delim-action'
  If no region is active, process borders of THING-at-point
  according to value of delim-action-beginning- resp. -end-position
  Default is symbol-at.
  With  or arg `escaped' to `t': insert escaped doublequotes"
  (interactive "*p")
  (save-restriction
    (let ((orig (point))
	  (beg (or beg (when (use-region-p) (region-beginning))))
	  (end (ignore-errors (copy-marker (or (car-safe end) end (when (use-region-p) (region-end))))))
	  done narrow)
      (when (and beg end)
	;; narrowing avoids scanning the whole buffer
	(narrow-to-region beg end)
	(ar-th-delim-intern thing beg end beg-char end-char)))))

(defun ar-th-base-copy-or (kind arg &optional check)
  " "
  (let* ((expr (format "%s" kind))
	 (arg (if arg (prefix-numeric-value arg) 1))
	 (suffix
	  (when (or (member kind ar-paired-delimit-aktiv)
		    ;; (loop for e in ar-unpaired-delimit-aktiv if (member kind e) return e))
		    (member kind ar-unpaired-delimit-aktiv))
	    (if (string-match "e$" expr)
		"d" "ed")))
	 erg)
    (cond
     ((eq 2 arg)
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-trim- " expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-trim-" expr suffix "-atpt"))))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt")) arg))))
     ((< 0 arg)
      (if (use-region-p)
	  (setq erg (funcall (intern-soft (concat "ar-" expr "-in-region-atpt"))))
	(or (setq erg (funcall (intern-soft (concat "ar-" expr suffix "-atpt")) arg))
	    (funcall (intern-soft (concat "ar-" expr "-" copy-or-alternative "-atpt"))))))

     (t (setq erg (funcall (intern-soft (concat "ar-kill-" expr suffix "-atpt"))))))))

(defvar ar-werkstatt-mode-map nil
  "Keymap used in Sh-Werkstatt mode.")


(define-derived-mode werkstatt emacs-lisp-mode "Werk"
  ;; (kill-all-local-variables)
  ;; (setq major-mode 'ar-werkstatt
  ;; mode-name "Sh-Werkstatt")
  (use-local-map ar-werkstatt-mode-map)
  (and ar-werkstatt-hs-minor-mode-p
       (add-hook 'ar-werkstatt-mode-hook 'hs-minor-mode)))

(defun ar-th-delimit--intern (thing string1 string2 &optional arg iact check)
  (let* ((bounds (ar-th-bounds thing (eq 4 (prefix-numeric-value arg)) iact check))
         (beg (or (ignore-errors (caar bounds))(car-safe bounds)))
	 (end (or (ignore-errors (cdr (cadr bounds)))(ignore-errors (cadr (cadr bounds)))(cdr-safe bounds))))
    (ar-th-delim thing arg string1 string2 iact beg end)))

;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: start

;;;###autoload
(defun ar-th-brace (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "{" "}" arg iact check))

;;;###autoload
(defun ar-th-bracket (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "[" "]" arg iact check))

;;;###autoload
(defun ar-th-lesserangle (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "<" ">" arg iact check))

;;;###autoload
(defun ar-th-greaterangle (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing ">" "<" arg iact check))

;;;###autoload
(defun ar-th-leftrightsinglequote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "‘" "’" arg iact check))

;;;###autoload
(defun ar-th-leftrightdoublequote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "“" "”" arg iact check))

;;;###autoload
(defun ar-th-parentize (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "(" ")" arg iact check))
;; ar-insert-delimit-forms-intern ar-paired-delimit-aktiv-raw: end


;; ar-triplequote-raw start

;;;###autoload
(defun ar-th-triplequote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\"\"\"\\\\\\\\\|\'\'\'" "\"\"\"\\\\\\\\\|\'\'\'" arg iact check))

;;;###autoload
(defun ar-th-triplequotedq (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\"\"\"" "\"\"\"" arg iact check))

;;;###autoload
(defun ar-th-triplequotesq (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "'''" "'''" arg iact check))

;;;###autoload
(defun ar-th-triplebacktick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "```" "```" arg iact check))

;; ar-triplequote-raw end

;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: start

;;;###autoload
(defun ar-th-colon (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing ":" ":" arg iact check))

;;;###autoload
(defun ar-th-cross (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "+" "+" arg iact check))

;;;###autoload
(defun ar-th-doubleslash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "//" "//" arg iact check))

;;;###autoload
(defun ar-th-backslash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\" "\\" arg iact check))

;;;###autoload
(defun ar-th-backtick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "`" "`" arg iact check))

;;;###autoload
(defun ar-th-dollar (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "$" "$" arg iact check))

;;;###autoload
(defun ar-th-doublebacktick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "``" "``" arg iact check))

;;;###autoload
(defun ar-th-doublequote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\"" "\"" arg iact check))

;;;###autoload
(defun ar-th-equalize (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "=" "=" arg iact check))

;;;###autoload
(defun ar-th-escape (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\" "\\" arg iact check))

;;;###autoload
(defun ar-th-hash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "#" "#" arg iact check))

;;;###autoload
(defun ar-th-hyphen (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "-" "-" arg iact check))

;;;###autoload
(defun ar-th-singlequote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "'" "'" arg iact check))

;;;###autoload
(defun ar-th-slash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "/" "/" arg iact check))

;;;###autoload
(defun ar-th-star (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "*" "*" arg iact check))

;;;###autoload
(defun ar-th-tild (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "~" "~" arg iact check))

;;;###autoload
(defun ar-th-triplebacktick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "```" "```" arg iact check))

;;;###autoload
(defun ar-th-underscore (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "_" "_" arg iact check))

;;;###autoload
(defun ar-th-whitespace (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing " " " " arg iact check))
;; ar-insert-delimit-forms-intern ar-unpaired-delimit-aktiv-raw: end

;; ar-atpt-data-forms-aktiv start

;;;###autoload
(defun ar-th-beginendquote (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\begin{quote}" "\\end{quote}" arg iact check))

;;;###autoload
(defun ar-th-blok (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "{% " " %}" arg iact check))

;;;###autoload
(defun ar-th-doublebackslash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\\\" "\\\\" arg iact check))

;;;###autoload
(defun ar-th-doublebackslashparen (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\\\(" "\\\\)" arg iact check))

;;;###autoload
(defun ar-th-doublebacktick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "``" "``" arg iact check))

;;;###autoload
(defun ar-th-triplebacktick (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "```" "```" arg iact check))

;;;###autoload
(defun ar-th-doubleslash (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "//" "//" arg iact check))

;;;###autoload
(defun ar-th-backslashparen (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "\\(" "\\)" arg iact check))

;;;###autoload
(defun ar-th-slashparen (thing &optional arg iact check)
  " "
  (ar-th-delimit--intern thing "////(" "////)" arg iact check))
;; ar-atpt-data-forms-aktiv end


(defun ar-th-slash-base (thing startstring endstring &optional arg iact)
  (let* ((bounds (ar-th-bounds thing arg iact))
         (beg (caar bounds))
         (end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds)))))))
    (goto-char end)
    (forward-char -1)
    (insert endstring)
    (goto-char beg)
    (insert startstring)
    (goto-char (1+ end))))


;; ar-insert-thingatpt-syntax-funktionen start

(defun ar-syntax-class-atpt (&optional pos)
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let* ((pos (or pos (point)))
	 (erg (logand (car (syntax-after pos)) 65535)))
    (when arg (message "%s" erg)) erg))

(defun syntax-class-bfpt (&optional arg) 
  "Return the syntax class part of the syntax at point. "
  (interactive "p")
  (let ((erg (logand (car (syntax-after (1- (point)))) 65535)))
    (when arg (message "%s" erg)) erg))

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

(defun syntax-bfpt (&optional arg) 
  (interactive "p")
  (let ((stax (syntax-after (1- (point)))))
    (when arg
      (message (format "%s" stax)))
    stax))

(defvar ar-paired-delimited-passiv-raw
  (list
   '(braced "{" "}")
   '(bracketed "[" "]")
   '(lesserangled "<" ">")
   '(greaterangled ">" "<")
   '(leftrightsinglequoted "‘" "’")
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
  (insert (car new-delimiter)))

(defun ar--transform-return-closing-delimiter-according-to-type (new-delimiter)
  "Return closing delimiter. "
  (if (< 1 (length new-delimiter))
      (cadr new-delimiter)
    (car new-delimiter)))

;; (ar--transform-delimited-new-delimiter t)

;; ar-paired-delimited-passiv-raw
;; ar-unpaired-delimited-raw
(defun ar--transform-delimited-intern (from to)
  "Expects string. "
  (let* ((bounds (ignore-errors (funcall (car (read-from-string (concat "ar-bounds-of-" from "-atpt"))))))
	 (end (copy-marker (or (ignore-errors (cadr (cadr bounds)))(ignore-errors (cdr (cadr bounds))))))
	 (new-delimiter (ar--transform-delimited-new-delimiter (car (read-from-string to)))))
    (if (and bounds new-delimiter)
	(progn
	  ;; (funcall (car (read-from-string (concat "ar-trim-" from "-atpt"))))
	  (goto-char (caar bounds))
	  (delete-char 1) 
	  ;; (insert "[")
	  (ar--transform-insert-opening-delimiter-according-to-type new-delimiter)
	  (goto-char end)
	  (delete-char -1) 
	  ;; (insert "]")
	  (insert (ar--transform-return-closing-delimiter-according-to-type new-delimiter)))
      (message (concat "ar--transform-delimited-intern: can't see " from)))))
(setq ar-paired-delimit-aktiv-raw
      (list
       '(brace 123 125)
       '(bracket 91 93)
       '(lesserangle 60 62)
       '(greaterangle 62 60)
       '(leftrightsinglequote 8216 8217)
       '(leftrightdoublequote 8220 8221)
       '(parentize 40 41)
       ))

(setq ar-paired-delimit-aktiv
      (list
       'brace
       'bracket
       'lesserangle
       'greaterangle
       'leftrightsinglequote
       'leftrightdoublequote
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
       '(doublebacktick "``")
       '(doublequote "\"")
       '(equalize "=")
       '(escape "\\\\")
       '(hash "#")
       '(hyphen "-")
       '(singlequote "'")
       '(slash "/")
       '(star "*")
       '(tild "~")
       '(triplebacktick "```")
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
       'doublebacktick
       'doublequote
       'equalize
       'escape
       'hash
       'hyphen
       'singlequote
       'slash
       'star
       'tild
       'triplebacktick
       'underscore
       'whitespace
       ))

(setq ar-unary-operations
      (list
       'commatize
       'quote
       ))

(setq ar-unpaired-delimited-passiv-zahlenform-raw
      (list
       '(backslashed 92)
       '(backticked 96)
       '(coloned 58)
       '(crossed 43)
       '(dollared 36)
       '(doublequoted 34)
       '(equalized 61)
       '(hashed 35)
       '(hyphened 45)
       '(singlequoted 39)
       '(slashed 47)
       '(stared 42)
       '(tilded 126)
       '(underscored 95)
       '(whitespaced 32)
       ))

(setq ar-unpaired-delimited-passiv-zahlenform
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
       'singlequoted
       'slashed
       'stared
       'tilded
       'underscored
       'whitespaced
       ))

(setq ar-atpt-data-forms-aktiv-raw
      (list
       '("beginendquote" "\\\\begin{quote}" "\\\\end{quote}" nil (quote move) 1 nil t nil)
       '("blok" "{% " " %}" nil (quote move) "1" nil t)
       '("doublebackslash" "\\\\\\\\" "\\\\\\\\" nil (quote move) "1" nil nil (quote ar-escaped))
       '("doublebackslashparen" "\\\\\\\\(" "\\\\\\\\)" nil (quote move) "1" nil nil (quote ar-escaped))
       '("doublebacktick" "``" "``" (quote move) "1" nil t (quote ar-escaped))
       '("triplebacktick" "```" "```" (quote move) "1" nil t (quote ar-escaped))
       '("doubleslash" "//" "//" nil (quote move) "1" nil t (quote ar-escaped))
       '("backslashparen" "\\\\(" "\\\\)" nil (quote move) "1" nil nil (quote ar-escaped))
       '("slashparen" "////(" "////)" nil (quote move) "1" nil nil (quote ar-escaped))
       ))

(setq ar-atpt-data-forms-aktiv
      (list
       'beginendquote
       'blok
       'doublebackslash
       'doublebackslashparen
       'doublebacktick
       'triplebacktick
       'doubleslash
       'backslashparen
       'slashparen
       ))

(setq ar-atpt-data-forms-passiv-raw
      (list
       '("beginendquoted" "\\\\begin{quote}" "\\\\end{quote}" nil (quote move) 1 nil nil nil)
       '("blok" "{% " " %}" nil (quote move) "1" nil t)
       '("doublebackslashed" "\\\\\\\\" "\\\\\\\\" nil (quote move) "1" nil nil (quote ar-escaped))
       '("doublebackticked" "``" "``" nil (quote move) "1" nil nil (quote ar-escaped))
       '("doubleslashed" "//" "//" nil (quote move) "1" nil nil (quote ar-escaped))
       '("doublebackslashedparen" "\\\\\\\\\\\\\\\\(" "\\\\\\\\\\\\\\\\)" nil (quote move) "1" nil nil (quote ar-escaped))
       '("tabledatap" "<td[^>]*>" "</td>" nil (quote move) "1" nil nil nil)
       '("backslashedparen" "\\\\\\\\(" "\\\\\\\\)" nil (quote move) "1" nil nil (quote ar-escaped))
       '("slashedparen" "////////(" "////////)" nil (quote move) "1" nil nil (quote ar-escaped))
       '("xslstylesheetp" "<xsl:stylesheet[^<]+>.*$" "</xsl:stylesheet>" nil (quote move) "1" nil nil nil)
       ))

(setq ar-atpt-data-forms-passiv
      (list
       'beginendquoted
       'blok
       'doublebackslashed
       'doublebackticked
       'doubleslashed
       'doublebackslashedparen
       'tabledatap
       'backslashedparen
       'slashedparen
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

(setq ar-triplequote-raw
      (list
       '(triplequote "\"\"\"\\\\|'''")
       '(triplequotedq "\"\"\"")
       '(triplequotesq "'''")
       '(triplebacktick "```")
       ))

(setq ar-triplequote
      (list
       'triplequote
       'triplequotedq
       'triplequotesq
       'triplebacktick
       ))

(setq ar-atpt-expression-list
      (list
       'block
       'block-or-clause
       'class
       'clause
       'def-or-class
       'def
       'delimited
       'expression
       'partial-expression
       'statement
       'string
       ))

(setq ar-expression-triplequote-raw
      (list
       '(triplequote "\"\"\"\\\\|'''")
       '(triplequotedq "\"\"\"")
       '(triplequotesq "'''")
       '(triplebacktick "```")
       ))

(setq ar-expression-triplequote
      (list
       'triplequote
       'triplequotedq
       'triplequotesq
       'triplebacktick
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
       'xslstylesheet
       'xsltemplate
       ))

(setq ar-paired-delimited-passiv-raw
      (list
       '(braced "{" "}")
       '(bracketed "[" "]")
       '(lesserangled "<" ">")
       '(greaterangled ">" "<")
       '(leftrightsinglequoted "‘" "’")
       '(leftrightdoublequoted "“" "”")
       '(parentized "(" ")")
       ))

(setq ar-paired-delimited-passiv
      (list
       'braced
       'bracketed
       'lesserangled
       'greaterangled
       'leftrightsinglequoted
       'leftrightdoublequoted
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
       'greateranglednested
       'lesseranglednested
       'buffer
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
       'symbol
       'url
       'word
       'wordalphaonly
       ))

(setq ar-atpt-major-forms-restricted-list
      (list
       'buffer
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
       'buffer
       ))




(provide 'thingatpt-utils-core)
;;; thingatpt-utils-core.el ends here
