;;; thingatpt-utils-map --- the menu


;; Copyright (C) 2010-2023 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; Keywords: convenience

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:


(setq ar-werkstatt-mode-map
  (let ((map emacs-lisp-mode-map))
    (and (ignore-errors (require 'easymenu) t)
         (easy-menu-define
           ar-werkstatt-menu map "Werkstatt Mode menu"
           `("Werkstatt"
             ;; ("Move"

;; ar-thing-at-point-utils-nodelim-core-menu: ar-atpt-classes end
("Forms"
("Delimited"

  ["braced at point"  ar-braced-atpt
   :help " ‘ar-braced-atpt’
   Return characters of [:braced:] delimited form at point as string if any, nil otherwise. "]

  ["bracketed at point"  ar-bracketed-atpt
   :help " ‘ar-bracketed-atpt’
   Return characters of [:bracketed:] delimited form at point as string if any, nil otherwise. "]

  ["lesserangled at point"  ar-lesserangled-atpt
   :help " ‘ar-lesserangled-atpt’
   Return characters of [:lesserangled:] delimited form at point as string if any, nil otherwise. "]

  ["greaterangled at point"  ar-greaterangled-atpt
   :help " ‘ar-greaterangled-atpt’
   Return characters of [:greaterangled:] delimited form at point as string if any, nil otherwise. "]

  ["curvedsinglequoted at point"  ar-curvedsinglequoted-atpt
   :help " ‘ar-curvedsinglequoted-atpt’
   Return characters of [:curvedsinglequoted:] delimited form at point as string if any, nil otherwise. "]

  ["parentized at point"  ar-parentized-atpt
   :help " ‘ar-parentized-atpt’
   Return characters of [:parentized:] delimited form at point as string if any, nil otherwise. "]

 "-"

  ["backslashed at point"  ar-backslashed-atpt
   :help " ‘ar-backslashed-atpt’
   Return characters of [:backslashed:] delimited form at point as string if any, nil otherwise. "]

  ["dollared at point"  ar-dollared-atpt
   :help " ‘ar-dollared-atpt’
   Return characters of [:dollared:] delimited form at point as string if any, nil otherwise. "]

  ["doublequoted at point"  ar-doublequoted-atpt
   :help " ‘ar-doublequoted-atpt’
   Return characters of [:doublequoted:] delimited form at point as string if any, nil otherwise. "]

  ["equalized at point"  ar-equalized-atpt
   :help " ‘ar-equalized-atpt’
   Return characters of [:equalized:] delimited form at point as string if any, nil otherwise. "]

  ["hyphened at point"  ar-hyphened-atpt
   :help " ‘ar-hyphened-atpt’
   Return characters of [:hyphened:] delimited form at point as string if any, nil otherwise. "]

  ["quoted at point"  ar-quoted-atpt
   :help " ‘ar-quoted-atpt’
   Return characters of [:quoted:] delimited form at point as string if any, nil otherwise. "]

  ["singlequoted at point"  ar-singlequoted-atpt
   :help " ‘ar-singlequoted-atpt’
   Return characters of [:singlequoted:] delimited form at point as string if any, nil otherwise. "]

  ["slashed at point"  ar-slashed-atpt
   :help " ‘ar-slashed-atpt’
   Return characters of [:slashed:] delimited form at point as string if any, nil otherwise. "]

  ["underscored at point"  ar-underscored-atpt
   :help " ‘ar-underscored-atpt’
   Return characters of [:underscored:] delimited form at point as string if any, nil otherwise. "]

  ["whitespaced at point"  ar-whitespaced-atpt
   :help " ‘ar-whitespaced-atpt’
   Return characters of [:whitespaced:] delimited form at point as string if any, nil otherwise. "]

)

("Character Classes"

  ["[:alnum:] character class at point"  ar-alnum-atpt
   :help " ‘ar-alnum-atpt’
   Return characters of class [:alnum:] at point as string if any, nil otherwise. "]

  ["[:alpha:] character class at point"  ar-alpha-atpt
   :help " ‘ar-alpha-atpt’
   Return characters of class [:alpha:] at point as string if any, nil otherwise. "]

  ["[:ascii:] character class at point"  ar-ascii-atpt
   :help " ‘ar-ascii-atpt’
   Return characters of class [:ascii:] at point as string if any, nil otherwise. "]

  ["[:blank:] character class at point"  ar-blank-atpt
   :help " ‘ar-blank-atpt’
   Return characters of class [:blank:] at point as string if any, nil otherwise. "]

  ["[:cntrl:] character class at point"  ar-cntrl-atpt
   :help " ‘ar-cntrl-atpt’
   Return characters of class [:cntrl:] at point as string if any, nil otherwise. "]

  ["[:digit:] character class at point"  ar-digit-atpt
   :help " ‘ar-digit-atpt’
   Return characters of class [:digit:] at point as string if any, nil otherwise. "]

  ["[:graph:] character class at point"  ar-graph-atpt
   :help " ‘ar-graph-atpt’
   Return characters of class [:graph:] at point as string if any, nil otherwise. "]

  ["[:lower:] character class at point"  ar-lower-atpt
   :help " ‘ar-lower-atpt’
   Return characters of class [:lower:] at point as string if any, nil otherwise. "]

  ["[:nonascii:] character class at point"  ar-nonascii-atpt
   :help " ‘ar-nonascii-atpt’
   Return characters of class [:nonascii:] at point as string if any, nil otherwise. "]

  ["[:print:] character class at point"  ar-print-atpt
   :help " ‘ar-print-atpt’
   Return characters of class [:print:] at point as string if any, nil otherwise. "]

  ["[:punct:] character class at point"  ar-punct-atpt
   :help " ‘ar-punct-atpt’
   Return characters of class [:punct:] at point as string if any, nil otherwise. "]

  ["[:space:] character class at point"  ar-space-atpt
   :help " ‘ar-space-atpt’
   Return characters of class [:space:] at point as string if any, nil otherwise. "]

  ["[:upper:] character class at point"  ar-upper-atpt
   :help " ‘ar-upper-atpt’
   Return characters of class [:upper:] at point as string if any, nil otherwise. "]

  ["[:xdigit:] character class at point"  ar-xdigit-atpt
   :help " ‘ar-xdigit-atpt’
   Return characters of class [:xdigit:] at point as string if any, nil otherwise. "]

)

("Other"

  ["Angled-No-Nest at point"  ar-anglednonest-atpt
   :help " ‘ar-anglednonest-atpt’
   Return characters of ANGLED-NO-NEST at point as string if any, nil otherwise. "]

  ["Greater-Angled-Nested at point"  ar-greateranglednested-atpt
   :help " ‘ar-greateranglednested-atpt’
   Return characters of GREATER-ANGLED-NESTED at point as string if any, nil otherwise. "]

  ["Lesser-Angled-Nested at point"  ar-lesseranglednested-atpt
   :help " ‘ar-lesseranglednested-atpt’
   Return characters of LESSER-ANGLED-NESTED at point as string if any, nil otherwise. "]

  ["Buffer at point"  ar-buffer-atpt
   :help " ‘ar-buffer-atpt’
   Return characters of BUFFER at point as string if any, nil otherwise. "]

  ["Comment at point"  ar-comment-atpt
   :help " ‘ar-comment-atpt’
   Return characters of COMMENT at point as string if any, nil otherwise. "]

  ["Csv at point"  ar-csv-atpt
   :help " ‘ar-csv-atpt’
   Return characters of CSV at point as string if any, nil otherwise. "]

  ["Date at point"  ar-date-atpt
   :help " ‘ar-date-atpt’
   Return characters of DATE at point as string if any, nil otherwise. "]

  ["Defun at point"  ar-defun-atpt
   :help " ‘ar-defun-atpt’
   Return characters of DEFUN at point as string if any, nil otherwise. "]

  ["Delimited at point"  ar-delimited-atpt
   :help " ‘ar-delimited-atpt’
   Return characters of DELIMITED at point as string if any, nil otherwise. "]

  ["Email at point"  ar-email-atpt
   :help " ‘ar-email-atpt’
   Return characters of EMAIL at point as string if any, nil otherwise. "]

  ["Filename at point"  ar-filename-atpt
   :help " ‘ar-filename-atpt’
   Return characters of FILENAME at point as string if any, nil otherwise. "]

  ["Filename-nondirectory at point"  ar-filenamenondirectory-atpt
   :help " ‘ar-filename-atpt’
   Return characters of nondirectory-part of FILENAME at point as string if any, nil otherwise. "]

  ["Float at point"  ar-float-atpt
   :help " ‘ar-float-atpt’
   Return characters of FLOAT at point as string if any, nil otherwise. "]

  ["Function at point"  ar-function-atpt
   :help " ‘ar-function-atpt’
   Return characters of FUNCTION at point as string if any, nil otherwise. "]

  ["Ip at point"  ar-ip-atpt
   :help " ‘ar-ip-atpt’
   Return characters of IP at point as string if any, nil otherwise. "]

  ["Isbn at point"  ar-isbn-atpt
   :help " ‘ar-isbn-atpt’
   Return characters of ISBN at point as string if any, nil otherwise. "]

  ["Line at point"  ar-line-atpt
   :help " ‘ar-line-atpt’
   Return characters of LINE at point as string if any, nil otherwise. "]

  ["Name at point"  ar-name-atpt
   :help " ‘ar-name-atpt’
   Return characters of NAME at point as string if any, nil otherwise. "]

  ["Number at point"  ar-number-atpt
   :help " ‘ar-number-atpt’
   Return characters of NUMBER at point as string if any, nil otherwise. "]

  ["Page at point"  ar-page-atpt
   :help " ‘ar-page-atpt’
   Return characters of PAGE at point as string if any, nil otherwise. "]

  ["Paragraph at point"  ar-paragraph-atpt
   :help " ‘ar-paragraph-atpt’
   Return characters of PARAGRAPH at point as string if any, nil otherwise. "]

  ["Paren at point"  ar-paren-atpt
   :help " ‘ar-paren-atpt’
   Return characters of PAREN at point as string if any, nil otherwise. "]

  ["Phone at point"  ar-phone-atpt
   :help " ‘ar-phone-atpt’
   Return characters of PHONE at point as string if any, nil otherwise. "]

  ["Region at point"  ar-region-atpt
   :help " ‘ar-region-atpt’
   Return characters of REGION at point as string if any, nil otherwise. "]

  ["Sentence at point"  ar-sentence-atpt
   :help " ‘ar-sentence-atpt’
   Return characters of SENTENCE at point as string if any, nil otherwise. "]

  ["Sexp at point"  ar-sexp-atpt
   :help " ‘ar-sexp-atpt’
   Return characters of SEXP at point as string if any, nil otherwise. "]

  ["String at point"  ar-string-atpt
   :help " ‘ar-string-atpt’
   Return characters of STRING at point as string if any, nil otherwise. "]

  ["Sh-Struct at point"  ar-shstruct-atpt
   :help " ‘ar-shstruct-atpt’
   Return characters of SH-STRUCT at point as string if any, nil otherwise. "]

  ["Symbol at point"  ar-symbol-atpt
   :help " ‘ar-symbol-atpt’
   Return characters of SYMBOL at point as string if any, nil otherwise. "]

  ["Url at point"  ar-url-atpt
   :help " ‘ar-url-atpt’
   Return characters of URL at point as string if any, nil otherwise. "]

  ["Word at point"  ar-word-atpt
   :help " ‘ar-word-atpt’
   Return characters of WORD at point as string if any, nil otherwise. "]

  ["Word-Alpha-Only at point"  ar-wordalphaonly-atpt
   :help " ‘ar-wordalphaonly-atpt’
   Return characters of WORD-ALPHA-ONLY at point as string if any, nil otherwise. "]

 )
)
 ("Edit"
 ("Copy"
 ("Character classes"

  ["Copy [:alnum:] char class at point"  ar-copy-alnum-atpt
   :help " ‘ar-copy-alnum-atpt’
   Copy ALNUM at point if any, nil otherwise. "]

  ["Copy [:alpha:] char class at point"  ar-copy-alpha-atpt
   :help " ‘ar-copy-alpha-atpt’
   Copy ALPHA at point if any, nil otherwise. "]

  ["Copy [:ascii:] char class at point"  ar-copy-ascii-atpt
   :help " ‘ar-copy-ascii-atpt’
   Copy ASCII at point if any, nil otherwise. "]

  ["Copy [:blank:] char class at point"  ar-copy-blank-atpt
   :help " ‘ar-copy-blank-atpt’
   Copy BLANK at point if any, nil otherwise. "]

  ["Copy [:cntrl:] char class at point"  ar-copy-cntrl-atpt
   :help " ‘ar-copy-cntrl-atpt’
   Copy CNTRL at point if any, nil otherwise. "]

  ["Copy [:digit:] char class at point"  ar-copy-digit-atpt
   :help " ‘ar-copy-digit-atpt’
   Copy DIGIT at point if any, nil otherwise. "]

  ["Copy [:graph:] char class at point"  ar-copy-graph-atpt
   :help " ‘ar-copy-graph-atpt’
   Copy GRAPH at point if any, nil otherwise. "]

  ["Copy [:lower:] char class at point"  ar-copy-lower-atpt
   :help " ‘ar-copy-lower-atpt’
   Copy LOWER at point if any, nil otherwise. "]

  ["Copy [:nonascii:] char class at point"  ar-copy-nonascii-atpt
   :help " ‘ar-copy-nonascii-atpt’
   Copy NONASCII at point if any, nil otherwise. "]

  ["Copy [:print:] char class at point"  ar-copy-print-atpt
   :help " ‘ar-copy-print-atpt’
   Copy PRINT at point if any, nil otherwise. "]

  ["Copy [:punct:] char class at point"  ar-copy-punct-atpt
   :help " ‘ar-copy-punct-atpt’
   Copy PUNCT at point if any, nil otherwise. "]

  ["Copy [:space:] char class at point"  ar-copy-space-atpt
   :help " ‘ar-copy-space-atpt’
   Copy SPACE at point if any, nil otherwise. "]

  ["Copy [:upper:] char class at point"  ar-copy-upper-atpt
   :help " ‘ar-copy-upper-atpt’
   Copy UPPER at point if any, nil otherwise. "]

  ["Copy [:xdigit:] char class at point"  ar-copy-xdigit-atpt
   :help " ‘ar-copy-xdigit-atpt’
   Copy XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Copy BRACED at point"  ar-copy-braced-atpt
   :help " ‘ar-copy-braced-atpt’
   Copy BRACED at point if any, nil otherwise. "]

  ["Copy BRACKETED at point"  ar-copy-bracketed-atpt
   :help " ‘ar-copy-bracketed-atpt’
   Copy BRACKETED at point if any, nil otherwise. "]

  ["Copy LESSER-ANGLED at point"  ar-copy-lesserangled-atpt
   :help " ‘ar-copy-lesserangled-atpt’
   Copy LESSER-ANGLED at point if any, nil otherwise. "]

  ["Copy GREATER-ANGLED at point"  ar-copy-greaterangled-atpt
   :help " ‘ar-copy-greaterangled-atpt’
   Copy GREATER-ANGLED at point if any, nil otherwise. "]

  ["Copy LEFT-RIGHT-SINGLEQUOTED at point"  ar-copy-curvedsinglequoted-atpt
   :help " ‘ar-copy-curvedsinglequoted-atpt’
   Copy LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Copy PARENTIZED at point"  ar-copy-parentized-atpt
   :help " ‘ar-copy-parentized-atpt’
   Copy PARENTIZED at point if any, nil otherwise. "]

  ["Copy BACKSLASHED at point"  ar-copy-backslashed-atpt
   :help " ‘ar-copy-backslashed-atpt’
   Copy BACKSLASHED at point if any, nil otherwise. "]

  ["Copy DOLLARED at point"  ar-copy-dollared-atpt
   :help " ‘ar-copy-dollared-atpt’
   Copy DOLLARED at point if any, nil otherwise. "]

  ["Copy DOUBLEQUOTED at point"  ar-copy-doublequoted-atpt
   :help " ‘ar-copy-doublequoted-atpt’
   Copy DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Copy EQUALIZED at point"  ar-copy-equalized-atpt
   :help " ‘ar-copy-equalized-atpt’
   Copy EQUALIZED at point if any, nil otherwise. "]

  ["Copy HYPHENED at point"  ar-copy-hyphened-atpt
   :help " ‘ar-copy-hyphened-atpt’
   Copy HYPHENED at point if any, nil otherwise. "]

  ["Copy QUOTED at point"  ar-copy-quoted-atpt
   :help " ‘ar-copy-quoted-atpt’
   Copy QUOTED at point if any, nil otherwise. "]

  ["Copy SINGLEQUOTED at point"  ar-copy-singlequoted-atpt
   :help " ‘ar-copy-singlequoted-atpt’
   Copy SINGLEQUOTED at point if any, nil otherwise. "]

  ["Copy SLASHED at point"  ar-copy-slashed-atpt
   :help " ‘ar-copy-slashed-atpt’
   Copy SLASHED at point if any, nil otherwise. "]

  ["Copy UNDERSCORED at point"  ar-copy-underscored-atpt
   :help " ‘ar-copy-underscored-atpt’
   Copy UNDERSCORED at point if any, nil otherwise. "]

  ["Copy WHITESPACED at point"  ar-copy-whitespaced-atpt
   :help " ‘ar-copy-whitespaced-atpt’
   Copy WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Copy ANGLED-NO-NEST at point"  ar-copyanglednonest-atpt
   :help " ‘ar-copyanglednonest-atpt’
   Copy ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Copy GREATER-ANGLED-NESTED at point"  ar-copy-greateranglednested-atpt
   :help " ‘ar-copy-greateranglednested-atpt’
   Copy GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Copy LESSER-ANGLED-NESTED at point"  ar-copy-lesseranglednested-atpt
   :help " ‘ar-copy-lesseranglednested-atpt’
   Copy LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Copy BUFFER at point"  ar-copy-buffer-atpt
   :help " ‘ar-copy-buffer-atpt’
   Copy BUFFER at point if any, nil otherwise. "]

  ["Copy COMMENT at point"  ar-copy-comment-atpt
   :help " ‘ar-copy-comment-atpt’
   Copy COMMENT at point if any, nil otherwise. "]

  ["Copy CSV at point"  ar-copy-csv-atpt
   :help " ‘ar-copy-csv-atpt’
   Copy CSV at point if any, nil otherwise. "]

  ["Copy DATE at point"  ar-copy-date-atpt
   :help " ‘ar-copy-date-atpt’
   Copy DATE at point if any, nil otherwise. "]

  ["Copy DEFUN at point"  ar-copy-defun-atpt
   :help " ‘ar-copy-defun-atpt’
   Copy DEFUN at point if any, nil otherwise. "]

  ["Copy DELIMITED at point"  ar-copy-delimited-atpt
   :help " ‘ar-copy-delimited-atpt’
   Copy DELIMITED at point if any, nil otherwise. "]

  ["Copy EMAIL at point"  ar-copy-email-atpt
   :help " ‘ar-copy-email-atpt’
   Copy EMAIL at point if any, nil otherwise. "]

  ["Copy FILENAME at point"  ar-copy-filename-atpt
   :help " ‘ar-copy-filename-atpt’
   Copy FILENAME at point if any, nil otherwise. "]

  ["Copy FILENAME without directory"  ar-copy-filenamenondirectory-atpt
   :help " ‘ar-copy-filename-atpt’
   Copy FILENAME without directory at point if any, nil otherwise. "]

  ["Copy FLOAT at point"  ar-copy-float-atpt
   :help " ‘ar-copy-float-atpt’
   Copy FLOAT at point if any, nil otherwise. "]

  ["Copy FUNCTION at point"  ar-copy-function-atpt
   :help " ‘ar-copy-function-atpt’
   Copy FUNCTION at point if any, nil otherwise. "]

  ["Copy IP at point"  ar-copy-ip-atpt
   :help " ‘ar-copy-ip-atpt’
   Copy IP at point if any, nil otherwise. "]

  ["Copy ISBN at point"  ar-copy-isbn-atpt
   :help " ‘ar-copy-isbn-atpt’
   Copy ISBN at point if any, nil otherwise. "]

  ["Copy LINE at point"  ar-copy-line-atpt
   :help " ‘ar-copy-line-atpt’
   Copy LINE at point if any, nil otherwise. "]

  ["Copy NAME at point"  ar-copy-name-atpt
   :help " ‘ar-copy-name-atpt’
   Copy NAME at point if any, nil otherwise. "]

  ["Copy NUMBER at point"  ar-copy-number-atpt
   :help " ‘ar-copy-number-atpt’
   Copy NUMBER at point if any, nil otherwise. "]

  ["Copy PAGE at point"  ar-copy-page-atpt
   :help " ‘ar-copy-page-atpt’
   Copy PAGE at point if any, nil otherwise. "]

  ["Copy PARAGRAPH at point"  ar-copy-paragraph-atpt
   :help " ‘ar-copy-paragraph-atpt’
   Copy PARAGRAPH at point if any, nil otherwise. "]

  ["Copy PAREN at point"  ar-copy-paren-atpt
   :help " ‘ar-copy-paren-atpt’
   Copy PAREN at point if any, nil otherwise. "]

  ["Copy PHONE at point"  ar-copy-phone-atpt
   :help " ‘ar-copy-phone-atpt’
   Copy PHONE at point if any, nil otherwise. "]

  ["Copy REGION at point"  ar-copy-region-atpt
   :help " ‘ar-copy-region-atpt’
   Copy REGION at point if any, nil otherwise. "]

  ["Copy SENTENCE at point"  ar-copy-sentence-atpt
   :help " ‘ar-copy-sentence-atpt’
   Copy SENTENCE at point if any, nil otherwise. "]

  ["Copy SEXP at point"  ar-copy-sexp-atpt
   :help " ‘ar-copy-sexp-atpt’
   Copy SEXP at point if any, nil otherwise. "]

  ["Copy STRING at point"  ar-copy-string-atpt
   :help " ‘ar-copy-string-atpt’
   Copy STRING at point if any, nil otherwise. "]

  ["Copy SH-STRUCT at point"  ar-copy-shstruct-atpt
   :help " ‘ar-copy-shstruct-atpt’
   Copy SH-STRUCT at point if any, nil otherwise. "]

  ["Copy SYMBOL at point"  ar-copy-symbol-atpt
   :help " ‘ar-copy-symbol-atpt’
   Copy SYMBOL at point if any, nil otherwise. "]

  ["Copy URL at point"  ar-copy-url-atpt
   :help " ‘ar-copy-url-atpt’
   Copy URL at point if any, nil otherwise. "]

  ["Copy WORD at point"  ar-copy-word-atpt
   :help " ‘ar-copy-word-atpt’
   Copy WORD at point if any, nil otherwise. "]

  ["Copy WORD-ALPHA-ONLY at point"  ar-copy-wordalphaonly-atpt
   :help " ‘ar-copy-wordalphaonly-atpt’
   Copy WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
 ("Delete"
 ("Character classes"

  ["Delete [:alnum:] char class at point"  ar-delete-alnum-atpt
   :help " ‘ar-delete-alnum-atpt’
   Delete ALNUM at point if any, nil otherwise. "]

  ["Delete [:alpha:] char class at point"  ar-delete-alpha-atpt
   :help " ‘ar-delete-alpha-atpt’
   Delete ALPHA at point if any, nil otherwise. "]

  ["Delete [:ascii:] char class at point"  ar-delete-ascii-atpt
   :help " ‘ar-delete-ascii-atpt’
   Delete ASCII at point if any, nil otherwise. "]

  ["Delete [:blank:] char class at point"  ar-delete-blank-atpt
   :help " ‘ar-delete-blank-atpt’
   Delete BLANK at point if any, nil otherwise. "]

  ["Delete [:cntrl:] char class at point"  ar-delete-cntrl-atpt
   :help " ‘ar-delete-cntrl-atpt’
   Delete CNTRL at point if any, nil otherwise. "]

  ["Delete [:digit:] char class at point"  ar-delete-digit-atpt
   :help " ‘ar-delete-digit-atpt’
   Delete DIGIT at point if any, nil otherwise. "]

  ["Delete [:graph:] char class at point"  ar-delete-graph-atpt
   :help " ‘ar-delete-graph-atpt’
   Delete GRAPH at point if any, nil otherwise. "]

  ["Delete [:lower:] char class at point"  ar-delete-lower-atpt
   :help " ‘ar-delete-lower-atpt’
   Delete LOWER at point if any, nil otherwise. "]

  ["Delete [:nonascii:] char class at point"  ar-delete-nonascii-atpt
   :help " ‘ar-delete-nonascii-atpt’
   Delete NONASCII at point if any, nil otherwise. "]

  ["Delete [:print:] char class at point"  ar-delete-print-atpt
   :help " ‘ar-delete-print-atpt’
   Delete PRINT at point if any, nil otherwise. "]

  ["Delete [:punct:] char class at point"  ar-delete-punct-atpt
   :help " ‘ar-delete-punct-atpt’
   Delete PUNCT at point if any, nil otherwise. "]

  ["Delete [:space:] char class at point"  ar-delete-space-atpt
   :help " ‘ar-delete-space-atpt’
   Delete SPACE at point if any, nil otherwise. "]

  ["Delete [:upper:] char class at point"  ar-delete-upper-atpt
   :help " ‘ar-delete-upper-atpt’
   Delete UPPER at point if any, nil otherwise. "]

  ["Delete [:xdigit:] char class at point"  ar-delete-xdigit-atpt
   :help " ‘ar-delete-xdigit-atpt’
   Delete XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Delete BRACED at point"  ar-delete-braced-atpt
   :help " ‘ar-delete-braced-atpt’
   Delete BRACED at point if any, nil otherwise. "]

  ["Delete BRACKETED at point"  ar-delete-bracketed-atpt
   :help " ‘ar-delete-bracketed-atpt’
   Delete BRACKETED at point if any, nil otherwise. "]

  ["Delete LESSER-ANGLED at point"  ar-delete-lesserangled-atpt
   :help " ‘ar-delete-lesserangled-atpt’
   Delete LESSER-ANGLED at point if any, nil otherwise. "]

  ["Delete GREATER-ANGLED at point"  ar-delete-greaterangled-atpt
   :help " ‘ar-delete-greaterangled-atpt’
   Delete GREATER-ANGLED at point if any, nil otherwise. "]

  ["Delete LEFT-RIGHT-SINGLEQUOTED at point"  ar-delete-curvedsinglequoted-atpt
   :help " ‘ar-delete-curvedsinglequoted-atpt’
   Delete LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Delete PARENTIZED at point"  ar-delete-parentized-atpt
   :help " ‘ar-delete-parentized-atpt’
   Delete PARENTIZED at point if any, nil otherwise. "]

  ["Delete BACKSLASHED at point"  ar-delete-backslashed-atpt
   :help " ‘ar-delete-backslashed-atpt’
   Delete BACKSLASHED at point if any, nil otherwise. "]

  ["Delete DOLLARED at point"  ar-delete-dollared-atpt
   :help " ‘ar-delete-dollared-atpt’
   Delete DOLLARED at point if any, nil otherwise. "]

  ["Delete DOUBLEQUOTED at point"  ar-delete-doublequoted-atpt
   :help " ‘ar-delete-doublequoted-atpt’
   Delete DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Delete EQUALIZED at point"  ar-delete-equalized-atpt
   :help " ‘ar-delete-equalized-atpt’
   Delete EQUALIZED at point if any, nil otherwise. "]

  ["Delete HYPHENED at point"  ar-delete-hyphened-atpt
   :help " ‘ar-delete-hyphened-atpt’
   Delete HYPHENED at point if any, nil otherwise. "]

  ["Delete QUOTED at point"  ar-delete-quoted-atpt
   :help " ‘ar-delete-quoted-atpt’
   Delete QUOTED at point if any, nil otherwise. "]

  ["Delete SINGLEQUOTED at point"  ar-delete-singlequoted-atpt
   :help " ‘ar-delete-singlequoted-atpt’
   Delete SINGLEQUOTED at point if any, nil otherwise. "]

  ["Delete SLASHED at point"  ar-delete-slashed-atpt
   :help " ‘ar-delete-slashed-atpt’
   Delete SLASHED at point if any, nil otherwise. "]

  ["Delete UNDERSCORED at point"  ar-delete-underscored-atpt
   :help " ‘ar-delete-underscored-atpt’
   Delete UNDERSCORED at point if any, nil otherwise. "]

  ["Delete WHITESPACED at point"  ar-delete-whitespaced-atpt
   :help " ‘ar-delete-whitespaced-atpt’
   Delete WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Delete ANGLED-NO-NEST at point"  ar-deleteanglednonest-atpt
   :help " ‘ar-deleteanglednonest-atpt’
   Delete ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Delete GREATER-ANGLED-NESTED at point"  ar-delete-greateranglednested-atpt
   :help " ‘ar-delete-greateranglednested-atpt’
   Delete GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Delete LESSER-ANGLED-NESTED at point"  ar-delete-lesseranglednested-atpt
   :help " ‘ar-delete-lesseranglednested-atpt’
   Delete LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Delete BUFFER at point"  ar-delete-buffer-atpt
   :help " ‘ar-delete-buffer-atpt’
   Delete BUFFER at point if any, nil otherwise. "]

  ["Delete COMMENT at point"  ar-delete-comment-atpt
   :help " ‘ar-delete-comment-atpt’
   Delete COMMENT at point if any, nil otherwise. "]

  ["Delete CSV at point"  ar-delete-csv-atpt
   :help " ‘ar-delete-csv-atpt’
   Delete CSV at point if any, nil otherwise. "]

  ["Delete DATE at point"  ar-delete-date-atpt
   :help " ‘ar-delete-date-atpt’
   Delete DATE at point if any, nil otherwise. "]

  ["Delete DEFUN at point"  ar-delete-defun-atpt
   :help " ‘ar-delete-defun-atpt’
   Delete DEFUN at point if any, nil otherwise. "]

  ["Delete DELIMITED at point"  ar-delete-delimited-atpt
   :help " ‘ar-delete-delimited-atpt’
   Delete DELIMITED at point if any, nil otherwise. "]

  ["Delete EMAIL at point"  ar-delete-email-atpt
   :help " ‘ar-delete-email-atpt’
   Delete EMAIL at point if any, nil otherwise. "]

  ["Delete FILENAME at point"  ar-delete-filename-atpt
   :help " ‘ar-delete-filename-atpt’
   Delete FILENAME at point if any, nil otherwise. "]

  ["Delete FLOAT at point"  ar-delete-float-atpt
   :help " ‘ar-delete-float-atpt’
   Delete FLOAT at point if any, nil otherwise. "]

  ["Delete FUNCTION at point"  ar-delete-function-atpt
   :help " ‘ar-delete-function-atpt’
   Delete FUNCTION at point if any, nil otherwise. "]

  ["Delete IP at point"  ar-delete-ip-atpt
   :help " ‘ar-delete-ip-atpt’
   Delete IP at point if any, nil otherwise. "]

  ["Delete ISBN at point"  ar-delete-isbn-atpt
   :help " ‘ar-delete-isbn-atpt’
   Delete ISBN at point if any, nil otherwise. "]

  ["Delete LINE at point"  ar-delete-line-atpt
   :help " ‘ar-delete-line-atpt’
   Delete LINE at point if any, nil otherwise. "]

  ["Delete NAME at point"  ar-delete-name-atpt
   :help " ‘ar-delete-name-atpt’
   Delete NAME at point if any, nil otherwise. "]

  ["Delete NUMBER at point"  ar-delete-number-atpt
   :help " ‘ar-delete-number-atpt’
   Delete NUMBER at point if any, nil otherwise. "]

  ["Delete PAGE at point"  ar-delete-page-atpt
   :help " ‘ar-delete-page-atpt’
   Delete PAGE at point if any, nil otherwise. "]

  ["Delete PARAGRAPH at point"  ar-delete-paragraph-atpt
   :help " ‘ar-delete-paragraph-atpt’
   Delete PARAGRAPH at point if any, nil otherwise. "]

  ["Delete PAREN at point"  ar-delete-paren-atpt
   :help " ‘ar-delete-paren-atpt’
   Delete PAREN at point if any, nil otherwise. "]

  ["Delete PHONE at point"  ar-delete-phone-atpt
   :help " ‘ar-delete-phone-atpt’
   Delete PHONE at point if any, nil otherwise. "]

  ["Delete REGION at point"  ar-delete-region-atpt
   :help " ‘ar-delete-region-atpt’
   Delete REGION at point if any, nil otherwise. "]

  ["Delete SENTENCE at point"  ar-delete-sentence-atpt
   :help " ‘ar-delete-sentence-atpt’
   Delete SENTENCE at point if any, nil otherwise. "]

  ["Delete SEXP at point"  ar-delete-sexp-atpt
   :help " ‘ar-delete-sexp-atpt’
   Delete SEXP at point if any, nil otherwise. "]

  ["Delete STRING at point"  ar-delete-string-atpt
   :help " ‘ar-delete-string-atpt’
   Delete STRING at point if any, nil otherwise. "]

  ["Delete SH-STRUCT at point"  ar-delete-shstruct-atpt
   :help " ‘ar-delete-shstruct-atpt’
   Delete SH-STRUCT at point if any, nil otherwise. "]

  ["Delete SYMBOL at point"  ar-delete-symbol-atpt
   :help " ‘ar-delete-symbol-atpt’
   Delete SYMBOL at point if any, nil otherwise. "]

  ["Delete URL at point"  ar-delete-url-atpt
   :help " ‘ar-delete-url-atpt’
   Delete URL at point if any, nil otherwise. "]

  ["Delete WORD at point"  ar-delete-word-atpt
   :help " ‘ar-delete-word-atpt’
   Delete WORD at point if any, nil otherwise. "]

  ["Delete WORD-ALPHA-ONLY at point"  ar-delete-wordalphaonly-atpt
   :help " ‘ar-delete-wordalphaonly-atpt’
   Delete WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Bracket"

 ("Character classes"

  ["Bracket [:alnum:] char class at point"  ar-bracket-alnum-atpt
   :help " ‘ar-bracket-alnum-atpt’
   Bracket ALNUM at point if any, nil otherwise. "]

  ["Bracket [:alpha:] char class at point"  ar-bracket-alpha-atpt
   :help " ‘ar-bracket-alpha-atpt’
   Bracket ALPHA at point if any, nil otherwise. "]

  ["Bracket [:ascii:] char class at point"  ar-bracket-ascii-atpt
   :help " ‘ar-bracket-ascii-atpt’
   Bracket ASCII at point if any, nil otherwise. "]

  ["Bracket [:blank:] char class at point"  ar-bracket-blank-atpt
   :help " ‘ar-bracket-blank-atpt’
   Bracket BLANK at point if any, nil otherwise. "]

  ["Bracket [:cntrl:] char class at point"  ar-bracket-cntrl-atpt
   :help " ‘ar-bracket-cntrl-atpt’
   Bracket CNTRL at point if any, nil otherwise. "]

  ["Bracket [:digit:] char class at point"  ar-bracket-digit-atpt
   :help " ‘ar-bracket-digit-atpt’
   Bracket DIGIT at point if any, nil otherwise. "]

  ["Bracket [:graph:] char class at point"  ar-bracket-graph-atpt
   :help " ‘ar-bracket-graph-atpt’
   Bracket GRAPH at point if any, nil otherwise. "]

  ["Bracket [:lower:] char class at point"  ar-bracket-lower-atpt
   :help " ‘ar-bracket-lower-atpt’
   Bracket LOWER at point if any, nil otherwise. "]

  ["Bracket [:nonascii:] char class at point"  ar-bracket-nonascii-atpt
   :help " ‘ar-bracket-nonascii-atpt’
   Bracket NONASCII at point if any, nil otherwise. "]

  ["Bracket [:print:] char class at point"  ar-bracket-print-atpt
   :help " ‘ar-bracket-print-atpt’
   Bracket PRINT at point if any, nil otherwise. "]

  ["Bracket [:punct:] char class at point"  ar-bracket-punct-atpt
   :help " ‘ar-bracket-punct-atpt’
   Bracket PUNCT at point if any, nil otherwise. "]

  ["Bracket [:space:] char class at point"  ar-bracket-space-atpt
   :help " ‘ar-bracket-space-atpt’
   Bracket SPACE at point if any, nil otherwise. "]

  ["Bracket [:upper:] char class at point"  ar-bracket-upper-atpt
   :help " ‘ar-bracket-upper-atpt’
   Bracket UPPER at point if any, nil otherwise. "]

  ["Bracket [:xdigit:] char class at point"  ar-bracket-xdigit-atpt
   :help " ‘ar-bracket-xdigit-atpt’
   Bracket XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Bracket BRACED at point"  ar-bracket-braced-atpt
   :help " ‘ar-bracket-braced-atpt’
   Bracket BRACED at point if any, nil otherwise. "]

  ["Bracket BRACKETED at point"  ar-bracket-bracketed-atpt
   :help " ‘ar-bracket-bracketed-atpt’
   Bracket BRACKETED at point if any, nil otherwise. "]

  ["Bracket LESSER-ANGLED at point"  ar-bracket-lesserangled-atpt
   :help " ‘ar-bracket-lesserangled-atpt’
   Bracket LESSER-ANGLED at point if any, nil otherwise. "]

  ["Bracket GREATER-ANGLED at point"  ar-bracket-greaterangled-atpt
   :help " ‘ar-bracket-greaterangled-atpt’
   Bracket GREATER-ANGLED at point if any, nil otherwise. "]

  ["Bracket LEFT-RIGHT-SINGLEQUOTED at point"  ar-bracket-curvedsinglequoted-atpt
   :help " ‘ar-bracket-curvedsinglequoted-atpt’
   Bracket LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Bracket PARENTIZED at point"  ar-bracket-parentized-atpt
   :help " ‘ar-bracket-parentized-atpt’
   Bracket PARENTIZED at point if any, nil otherwise. "]

  ["Bracket BACKSLASHED at point"  ar-bracket-backslashed-atpt
   :help " ‘ar-bracket-backslashed-atpt’
   Bracket BACKSLASHED at point if any, nil otherwise. "]

  ["Bracket DOLLARED at point"  ar-bracket-dollared-atpt
   :help " ‘ar-bracket-dollared-atpt’
   Bracket DOLLARED at point if any, nil otherwise. "]

  ["Bracket DOUBLEQUOTED at point"  ar-bracket-doublequoted-atpt
   :help " ‘ar-bracket-doublequoted-atpt’
   Bracket DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Bracket EQUALIZED at point"  ar-bracket-equalized-atpt
   :help " ‘ar-bracket-equalized-atpt’
   Bracket EQUALIZED at point if any, nil otherwise. "]

  ["Bracket HYPHENED at point"  ar-bracket-hyphened-atpt
   :help " ‘ar-bracket-hyphened-atpt’
   Bracket HYPHENED at point if any, nil otherwise. "]

  ["Bracket QUOTED at point"  ar-bracket-quoted-atpt
   :help " ‘ar-bracket-quoted-atpt’
   Bracket QUOTED at point if any, nil otherwise. "]

  ["Bracket SINGLEQUOTED at point"  ar-bracket-singlequoted-atpt
   :help " ‘ar-bracket-singlequoted-atpt’
   Bracket SINGLEQUOTED at point if any, nil otherwise. "]

  ["Bracket SLASHED at point"  ar-bracket-slashed-atpt
   :help " ‘ar-bracket-slashed-atpt’
   Bracket SLASHED at point if any, nil otherwise. "]

  ["Bracket UNDERSCORED at point"  ar-bracket-underscored-atpt
   :help " ‘ar-bracket-underscored-atpt’
   Bracket UNDERSCORED at point if any, nil otherwise. "]

  ["Bracket WHITESPACED at point"  ar-bracket-whitespaced-atpt
   :help " ‘ar-bracket-whitespaced-atpt’
   Bracket WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Bracket ANGLED-NO-NEST at point"  ar-bracketanglednonest-atpt
   :help " ‘ar-bracketanglednonest-atpt’
   Bracket ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Bracket GREATER-ANGLED-NESTED at point"  ar-bracket-greateranglednested-atpt
   :help " ‘ar-bracket-greateranglednested-atpt’
   Bracket GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Bracket LESSER-ANGLED-NESTED at point"  ar-bracket-lesseranglednested-atpt
   :help " ‘ar-bracket-lesseranglednested-atpt’
   Bracket LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Bracket BUFFER at point"  ar-bracket-buffer-atpt
   :help " ‘ar-bracket-buffer-atpt’
   Bracket BUFFER at point if any, nil otherwise. "]

  ["Bracket COMMENT at point"  ar-bracket-comment-atpt
   :help " ‘ar-bracket-comment-atpt’
   Bracket COMMENT at point if any, nil otherwise. "]

  ["Bracket CSV at point"  ar-bracket-csv-atpt
   :help " ‘ar-bracket-csv-atpt’
   Bracket CSV at point if any, nil otherwise. "]

  ["Bracket DATE at point"  ar-bracket-date-atpt
   :help " ‘ar-bracket-date-atpt’
   Bracket DATE at point if any, nil otherwise. "]

  ["Bracket DEFUN at point"  ar-bracket-defun-atpt
   :help " ‘ar-bracket-defun-atpt’
   Bracket DEFUN at point if any, nil otherwise. "]

  ["Bracket DELIMITED at point"  ar-bracket-delimited-atpt
   :help " ‘ar-bracket-delimited-atpt’
   Bracket DELIMITED at point if any, nil otherwise. "]

  ["Bracket EMAIL at point"  ar-bracket-email-atpt
   :help " ‘ar-bracket-email-atpt’
   Bracket EMAIL at point if any, nil otherwise. "]

  ["Bracket FILENAME at point"  ar-bracket-filename-atpt
   :help " ‘ar-bracket-filename-atpt’
   Bracket FILENAME at point if any, nil otherwise. "]

  ["Bracket FLOAT at point"  ar-bracket-float-atpt
   :help " ‘ar-bracket-float-atpt’
   Bracket FLOAT at point if any, nil otherwise. "]

  ["Bracket FUNCTION at point"  ar-bracket-function-atpt
   :help " ‘ar-bracket-function-atpt’
   Bracket FUNCTION at point if any, nil otherwise. "]

  ["Bracket IP at point"  ar-bracket-ip-atpt
   :help " ‘ar-bracket-ip-atpt’
   Bracket IP at point if any, nil otherwise. "]

  ["Bracket ISBN at point"  ar-bracket-isbn-atpt
   :help " ‘ar-bracket-isbn-atpt’
   Bracket ISBN at point if any, nil otherwise. "]

  ["Bracket LINE at point"  ar-bracket-line-atpt
   :help " ‘ar-bracket-line-atpt’
   Bracket LINE at point if any, nil otherwise. "]

  ["Bracket NAME at point"  ar-bracket-name-atpt
   :help " ‘ar-bracket-name-atpt’
   Bracket NAME at point if any, nil otherwise. "]

  ["Bracket NUMBER at point"  ar-bracket-number-atpt
   :help " ‘ar-bracket-number-atpt’
   Bracket NUMBER at point if any, nil otherwise. "]

  ["Bracket PAGE at point"  ar-bracket-page-atpt
   :help " ‘ar-bracket-page-atpt’
   Bracket PAGE at point if any, nil otherwise. "]

  ["Bracket PARAGRAPH at point"  ar-bracket-paragraph-atpt
   :help " ‘ar-bracket-paragraph-atpt’
   Bracket PARAGRAPH at point if any, nil otherwise. "]

  ["Bracket PAREN at point"  ar-bracket-paren-atpt
   :help " ‘ar-bracket-paren-atpt’
   Bracket PAREN at point if any, nil otherwise. "]

  ["Bracket PHONE at point"  ar-bracket-phone-atpt
   :help " ‘ar-bracket-phone-atpt’
   Bracket PHONE at point if any, nil otherwise. "]

  ["Bracket REGION at point"  ar-bracket-region-atpt
   :help " ‘ar-bracket-region-atpt’
   Bracket REGION at point if any, nil otherwise. "]

  ["Bracket SENTENCE at point"  ar-bracket-sentence-atpt
   :help " ‘ar-bracket-sentence-atpt’
   Bracket SENTENCE at point if any, nil otherwise. "]

  ["Bracket SEXP at point"  ar-bracket-sexp-atpt
   :help " ‘ar-bracket-sexp-atpt’
   Bracket SEXP at point if any, nil otherwise. "]

  ["Bracket STRING at point"  ar-bracket-string-atpt
   :help " ‘ar-bracket-string-atpt’
   Bracket STRING at point if any, nil otherwise. "]

  ["Bracket SH-STRUCT at point"  ar-bracket-shstruct-atpt
   :help " ‘ar-bracket-shstruct-atpt’
   Bracket SH-STRUCT at point if any, nil otherwise. "]

  ["Bracket SYMBOL at point"  ar-bracket-symbol-atpt
   :help " ‘ar-bracket-symbol-atpt’
   Bracket SYMBOL at point if any, nil otherwise. "]

  ["Bracket URL at point"  ar-bracket-url-atpt
   :help " ‘ar-bracket-url-atpt’
   Bracket URL at point if any, nil otherwise. "]

  ["Bracket WORD at point"  ar-bracket-word-atpt
   :help " ‘ar-bracket-word-atpt’
   Bracket WORD at point if any, nil otherwise. "]

  ["Bracket WORD-ALPHA-ONLY at point"  ar-bracket-wordalphaonly-atpt
   :help " ‘ar-bracket-wordalphaonly-atpt’
   Bracket WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Commatize"

 ("Character classes"

  ["Commatize [:alnum:] char class at point"  ar-commatize-alnum-atpt
   :help " ‘ar-commatize-alnum-atpt’
   Commatize ALNUM at point if any, nil otherwise. "]

  ["Commatize [:alpha:] char class at point"  ar-commatize-alpha-atpt
   :help " ‘ar-commatize-alpha-atpt’
   Commatize ALPHA at point if any, nil otherwise. "]

  ["Commatize [:ascii:] char class at point"  ar-commatize-ascii-atpt
   :help " ‘ar-commatize-ascii-atpt’
   Commatize ASCII at point if any, nil otherwise. "]

  ["Commatize [:blank:] char class at point"  ar-commatize-blank-atpt
   :help " ‘ar-commatize-blank-atpt’
   Commatize BLANK at point if any, nil otherwise. "]

  ["Commatize [:cntrl:] char class at point"  ar-commatize-cntrl-atpt
   :help " ‘ar-commatize-cntrl-atpt’
   Commatize CNTRL at point if any, nil otherwise. "]

  ["Commatize [:digit:] char class at point"  ar-commatize-digit-atpt
   :help " ‘ar-commatize-digit-atpt’
   Commatize DIGIT at point if any, nil otherwise. "]

  ["Commatize [:graph:] char class at point"  ar-commatize-graph-atpt
   :help " ‘ar-commatize-graph-atpt’
   Commatize GRAPH at point if any, nil otherwise. "]

  ["Commatize [:lower:] char class at point"  ar-commatize-lower-atpt
   :help " ‘ar-commatize-lower-atpt’
   Commatize LOWER at point if any, nil otherwise. "]

  ["Commatize [:nonascii:] char class at point"  ar-commatize-nonascii-atpt
   :help " ‘ar-commatize-nonascii-atpt’
   Commatize NONASCII at point if any, nil otherwise. "]

  ["Commatize [:print:] char class at point"  ar-commatize-print-atpt
   :help " ‘ar-commatize-print-atpt’
   Commatize PRINT at point if any, nil otherwise. "]

  ["Commatize [:punct:] char class at point"  ar-commatize-punct-atpt
   :help " ‘ar-commatize-punct-atpt’
   Commatize PUNCT at point if any, nil otherwise. "]

  ["Commatize [:space:] char class at point"  ar-commatize-space-atpt
   :help " ‘ar-commatize-space-atpt’
   Commatize SPACE at point if any, nil otherwise. "]

  ["Commatize [:upper:] char class at point"  ar-commatize-upper-atpt
   :help " ‘ar-commatize-upper-atpt’
   Commatize UPPER at point if any, nil otherwise. "]

  ["Commatize [:xdigit:] char class at point"  ar-commatize-xdigit-atpt
   :help " ‘ar-commatize-xdigit-atpt’
   Commatize XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Commatize BRACED at point"  ar-commatize-braced-atpt
   :help " ‘ar-commatize-braced-atpt’
   Commatize BRACED at point if any, nil otherwise. "]

  ["Commatize BRACKETED at point"  ar-commatize-bracketed-atpt
   :help " ‘ar-commatize-bracketed-atpt’
   Commatize BRACKETED at point if any, nil otherwise. "]

  ["Commatize LESSER-ANGLED at point"  ar-commatize-lesserangled-atpt
   :help " ‘ar-commatize-lesserangled-atpt’
   Commatize LESSER-ANGLED at point if any, nil otherwise. "]

  ["Commatize GREATER-ANGLED at point"  ar-commatize-greaterangled-atpt
   :help " ‘ar-commatize-greaterangled-atpt’
   Commatize GREATER-ANGLED at point if any, nil otherwise. "]

  ["Commatize LEFT-RIGHT-SINGLEQUOTED at point"  ar-commatize-curvedsinglequoted-atpt
   :help " ‘ar-commatize-curvedsinglequoted-atpt’
   Commatize LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Commatize PARENTIZED at point"  ar-commatize-parentized-atpt
   :help " ‘ar-commatize-parentized-atpt’
   Commatize PARENTIZED at point if any, nil otherwise. "]

  ["Commatize BACKSLASHED at point"  ar-commatize-backslashed-atpt
   :help " ‘ar-commatize-backslashed-atpt’
   Commatize BACKSLASHED at point if any, nil otherwise. "]

  ["Commatize DOLLARED at point"  ar-commatize-dollared-atpt
   :help " ‘ar-commatize-dollared-atpt’
   Commatize DOLLARED at point if any, nil otherwise. "]

  ["Commatize DOUBLEQUOTED at point"  ar-commatize-doublequoted-atpt
   :help " ‘ar-commatize-doublequoted-atpt’
   Commatize DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Commatize EQUALIZED at point"  ar-commatize-equalized-atpt
   :help " ‘ar-commatize-equalized-atpt’
   Commatize EQUALIZED at point if any, nil otherwise. "]

  ["Commatize HYPHENED at point"  ar-commatize-hyphened-atpt
   :help " ‘ar-commatize-hyphened-atpt’
   Commatize HYPHENED at point if any, nil otherwise. "]

  ["Commatize QUOTED at point"  ar-commatize-quoted-atpt
   :help " ‘ar-commatize-quoted-atpt’
   Commatize QUOTED at point if any, nil otherwise. "]

  ["Commatize SINGLEQUOTED at point"  ar-commatize-singlequoted-atpt
   :help " ‘ar-commatize-singlequoted-atpt’
   Commatize SINGLEQUOTED at point if any, nil otherwise. "]

  ["Commatize SLASHED at point"  ar-commatize-slashed-atpt
   :help " ‘ar-commatize-slashed-atpt’
   Commatize SLASHED at point if any, nil otherwise. "]

  ["Commatize UNDERSCORED at point"  ar-commatize-underscored-atpt
   :help " ‘ar-commatize-underscored-atpt’
   Commatize UNDERSCORED at point if any, nil otherwise. "]

  ["Commatize WHITESPACED at point"  ar-commatize-whitespaced-atpt
   :help " ‘ar-commatize-whitespaced-atpt’
   Commatize WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Commatize ANGLED-NO-NEST at point"  ar-commatizeanglednonest-atpt
   :help " ‘ar-commatizeanglednonest-atpt’
   Commatize ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Commatize GREATER-ANGLED-NESTED at point"  ar-commatize-greateranglednested-atpt
   :help " ‘ar-commatize-greateranglednested-atpt’
   Commatize GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Commatize LESSER-ANGLED-NESTED at point"  ar-commatize-lesseranglednested-atpt
   :help " ‘ar-commatize-lesseranglednested-atpt’
   Commatize LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Commatize BUFFER at point"  ar-commatize-buffer-atpt
   :help " ‘ar-commatize-buffer-atpt’
   Commatize BUFFER at point if any, nil otherwise. "]

  ["Commatize COMMENT at point"  ar-commatize-comment-atpt
   :help " ‘ar-commatize-comment-atpt’
   Commatize COMMENT at point if any, nil otherwise. "]

  ["Commatize CSV at point"  ar-commatize-csv-atpt
   :help " ‘ar-commatize-csv-atpt’
   Commatize CSV at point if any, nil otherwise. "]

  ["Commatize DATE at point"  ar-commatize-date-atpt
   :help " ‘ar-commatize-date-atpt’
   Commatize DATE at point if any, nil otherwise. "]

  ["Commatize DEFUN at point"  ar-commatize-defun-atpt
   :help " ‘ar-commatize-defun-atpt’
   Commatize DEFUN at point if any, nil otherwise. "]

  ["Commatize DELIMITED at point"  ar-commatize-delimited-atpt
   :help " ‘ar-commatize-delimited-atpt’
   Commatize DELIMITED at point if any, nil otherwise. "]

  ["Commatize EMAIL at point"  ar-commatize-email-atpt
   :help " ‘ar-commatize-email-atpt’
   Commatize EMAIL at point if any, nil otherwise. "]

  ["Commatize FILENAME at point"  ar-commatize-filename-atpt
   :help " ‘ar-commatize-filename-atpt’
   Commatize FILENAME at point if any, nil otherwise. "]

  ["Commatize FLOAT at point"  ar-commatize-float-atpt
   :help " ‘ar-commatize-float-atpt’
   Commatize FLOAT at point if any, nil otherwise. "]

  ["Commatize FUNCTION at point"  ar-commatize-function-atpt
   :help " ‘ar-commatize-function-atpt’
   Commatize FUNCTION at point if any, nil otherwise. "]

  ["Commatize IP at point"  ar-commatize-ip-atpt
   :help " ‘ar-commatize-ip-atpt’
   Commatize IP at point if any, nil otherwise. "]

  ["Commatize ISBN at point"  ar-commatize-isbn-atpt
   :help " ‘ar-commatize-isbn-atpt’
   Commatize ISBN at point if any, nil otherwise. "]

  ["Commatize LINE at point"  ar-commatize-line-atpt
   :help " ‘ar-commatize-line-atpt’
   Commatize LINE at point if any, nil otherwise. "]

  ["Commatize NAME at point"  ar-commatize-name-atpt
   :help " ‘ar-commatize-name-atpt’
   Commatize NAME at point if any, nil otherwise. "]

  ["Commatize NUMBER at point"  ar-commatize-number-atpt
   :help " ‘ar-commatize-number-atpt’
   Commatize NUMBER at point if any, nil otherwise. "]

  ["Commatize PAGE at point"  ar-commatize-page-atpt
   :help " ‘ar-commatize-page-atpt’
   Commatize PAGE at point if any, nil otherwise. "]

  ["Commatize PARAGRAPH at point"  ar-commatize-paragraph-atpt
   :help " ‘ar-commatize-paragraph-atpt’
   Commatize PARAGRAPH at point if any, nil otherwise. "]

  ["Commatize PAREN at point"  ar-commatize-paren-atpt
   :help " ‘ar-commatize-paren-atpt’
   Commatize PAREN at point if any, nil otherwise. "]

  ["Commatize PHONE at point"  ar-commatize-phone-atpt
   :help " ‘ar-commatize-phone-atpt’
   Commatize PHONE at point if any, nil otherwise. "]

  ["Commatize REGION at point"  ar-commatize-region-atpt
   :help " ‘ar-commatize-region-atpt’
   Commatize REGION at point if any, nil otherwise. "]

  ["Commatize SENTENCE at point"  ar-commatize-sentence-atpt
   :help " ‘ar-commatize-sentence-atpt’
   Commatize SENTENCE at point if any, nil otherwise. "]

  ["Commatize SEXP at point"  ar-commatize-sexp-atpt
   :help " ‘ar-commatize-sexp-atpt’
   Commatize SEXP at point if any, nil otherwise. "]

  ["Commatize STRING at point"  ar-commatize-string-atpt
   :help " ‘ar-commatize-string-atpt’
   Commatize STRING at point if any, nil otherwise. "]

  ["Commatize SH-STRUCT at point"  ar-commatize-shstruct-atpt
   :help " ‘ar-commatize-shstruct-atpt’
   Commatize SH-STRUCT at point if any, nil otherwise. "]

  ["Commatize SYMBOL at point"  ar-commatize-symbol-atpt
   :help " ‘ar-commatize-symbol-atpt’
   Commatize SYMBOL at point if any, nil otherwise. "]

  ["Commatize URL at point"  ar-commatize-url-atpt
   :help " ‘ar-commatize-url-atpt’
   Commatize URL at point if any, nil otherwise. "]

  ["Commatize WORD at point"  ar-commatize-word-atpt
   :help " ‘ar-commatize-word-atpt’
   Commatize WORD at point if any, nil otherwise. "]

  ["Commatize WORD-ALPHA-ONLY at point"  ar-commatize-wordalphaonly-atpt
   :help " ‘ar-commatize-wordalphaonly-atpt’
   Commatize WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Hide"

 ("Character classes"

  ["Hide [:alnum:] char class at point"  ar-hide-alnum-atpt
   :help " ‘ar-hide-alnum-atpt’
   Hide ALNUM at point if any, nil otherwise. "]

  ["Hide [:alpha:] char class at point"  ar-hide-alpha-atpt
   :help " ‘ar-hide-alpha-atpt’
   Hide ALPHA at point if any, nil otherwise. "]

  ["Hide [:ascii:] char class at point"  ar-hide-ascii-atpt
   :help " ‘ar-hide-ascii-atpt’
   Hide ASCII at point if any, nil otherwise. "]

  ["Hide [:blank:] char class at point"  ar-hide-blank-atpt
   :help " ‘ar-hide-blank-atpt’
   Hide BLANK at point if any, nil otherwise. "]

  ["Hide [:cntrl:] char class at point"  ar-hide-cntrl-atpt
   :help " ‘ar-hide-cntrl-atpt’
   Hide CNTRL at point if any, nil otherwise. "]

  ["Hide [:digit:] char class at point"  ar-hide-digit-atpt
   :help " ‘ar-hide-digit-atpt’
   Hide DIGIT at point if any, nil otherwise. "]

  ["Hide [:graph:] char class at point"  ar-hide-graph-atpt
   :help " ‘ar-hide-graph-atpt’
   Hide GRAPH at point if any, nil otherwise. "]

  ["Hide [:lower:] char class at point"  ar-hide-lower-atpt
   :help " ‘ar-hide-lower-atpt’
   Hide LOWER at point if any, nil otherwise. "]

  ["Hide [:nonascii:] char class at point"  ar-hide-nonascii-atpt
   :help " ‘ar-hide-nonascii-atpt’
   Hide NONASCII at point if any, nil otherwise. "]

  ["Hide [:print:] char class at point"  ar-hide-print-atpt
   :help " ‘ar-hide-print-atpt’
   Hide PRINT at point if any, nil otherwise. "]

  ["Hide [:punct:] char class at point"  ar-hide-punct-atpt
   :help " ‘ar-hide-punct-atpt’
   Hide PUNCT at point if any, nil otherwise. "]

  ["Hide [:space:] char class at point"  ar-hide-space-atpt
   :help " ‘ar-hide-space-atpt’
   Hide SPACE at point if any, nil otherwise. "]

  ["Hide [:upper:] char class at point"  ar-hide-upper-atpt
   :help " ‘ar-hide-upper-atpt’
   Hide UPPER at point if any, nil otherwise. "]

  ["Hide [:xdigit:] char class at point"  ar-hide-xdigit-atpt
   :help " ‘ar-hide-xdigit-atpt’
   Hide XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Hide BRACED at point"  ar-hide-braced-atpt
   :help " ‘ar-hide-braced-atpt’
   Hide BRACED at point if any, nil otherwise. "]

  ["Hide BRACKETED at point"  ar-hide-bracketed-atpt
   :help " ‘ar-hide-bracketed-atpt’
   Hide BRACKETED at point if any, nil otherwise. "]

  ["Hide LESSER-ANGLED at point"  ar-hide-lesserangled-atpt
   :help " ‘ar-hide-lesserangled-atpt’
   Hide LESSER-ANGLED at point if any, nil otherwise. "]

  ["Hide GREATER-ANGLED at point"  ar-hide-greaterangled-atpt
   :help " ‘ar-hide-greaterangled-atpt’
   Hide GREATER-ANGLED at point if any, nil otherwise. "]

  ["Hide LEFT-RIGHT-SINGLEQUOTED at point"  ar-hide-curvedsinglequoted-atpt
   :help " ‘ar-hide-curvedsinglequoted-atpt’
   Hide LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hide PARENTIZED at point"  ar-hide-parentized-atpt
   :help " ‘ar-hide-parentized-atpt’
   Hide PARENTIZED at point if any, nil otherwise. "]

  ["Hide BACKSLASHED at point"  ar-hide-backslashed-atpt
   :help " ‘ar-hide-backslashed-atpt’
   Hide BACKSLASHED at point if any, nil otherwise. "]

  ["Hide DOLLARED at point"  ar-hide-dollared-atpt
   :help " ‘ar-hide-dollared-atpt’
   Hide DOLLARED at point if any, nil otherwise. "]

  ["Hide DOUBLEQUOTED at point"  ar-hide-doublequoted-atpt
   :help " ‘ar-hide-doublequoted-atpt’
   Hide DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Hide EQUALIZED at point"  ar-hide-equalized-atpt
   :help " ‘ar-hide-equalized-atpt’
   Hide EQUALIZED at point if any, nil otherwise. "]

  ["Hide HYPHENED at point"  ar-hide-hyphened-atpt
   :help " ‘ar-hide-hyphened-atpt’
   Hide HYPHENED at point if any, nil otherwise. "]

  ["Hide QUOTED at point"  ar-hide-quoted-atpt
   :help " ‘ar-hide-quoted-atpt’
   Hide QUOTED at point if any, nil otherwise. "]

  ["Hide SINGLEQUOTED at point"  ar-hide-singlequoted-atpt
   :help " ‘ar-hide-singlequoted-atpt’
   Hide SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hide SLASHED at point"  ar-hide-slashed-atpt
   :help " ‘ar-hide-slashed-atpt’
   Hide SLASHED at point if any, nil otherwise. "]

  ["Hide UNDERSCORED at point"  ar-hide-underscored-atpt
   :help " ‘ar-hide-underscored-atpt’
   Hide UNDERSCORED at point if any, nil otherwise. "]

  ["Hide WHITESPACED at point"  ar-hide-whitespaced-atpt
   :help " ‘ar-hide-whitespaced-atpt’
   Hide WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Hide ANGLED-NO-NEST at point"  ar-hideanglednonest-atpt
   :help " ‘ar-hideanglednonest-atpt’
   Hide ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Hide GREATER-ANGLED-NESTED at point"  ar-hide-greateranglednested-atpt
   :help " ‘ar-hide-greateranglednested-atpt’
   Hide GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hide LESSER-ANGLED-NESTED at point"  ar-hide-lesseranglednested-atpt
   :help " ‘ar-hide-lesseranglednested-atpt’
   Hide LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hide BUFFER at point"  ar-hide-buffer-atpt
   :help " ‘ar-hide-buffer-atpt’
   Hide BUFFER at point if any, nil otherwise. "]

  ["Hide COMMENT at point"  ar-hide-comment-atpt
   :help " ‘ar-hide-comment-atpt’
   Hide COMMENT at point if any, nil otherwise. "]

  ["Hide CSV at point"  ar-hide-csv-atpt
   :help " ‘ar-hide-csv-atpt’
   Hide CSV at point if any, nil otherwise. "]

  ["Hide DATE at point"  ar-hide-date-atpt
   :help " ‘ar-hide-date-atpt’
   Hide DATE at point if any, nil otherwise. "]

  ["Hide DEFUN at point"  ar-hide-defun-atpt
   :help " ‘ar-hide-defun-atpt’
   Hide DEFUN at point if any, nil otherwise. "]

  ["Hide DELIMITED at point"  ar-hide-delimited-atpt
   :help " ‘ar-hide-delimited-atpt’
   Hide DELIMITED at point if any, nil otherwise. "]

  ["Hide EMAIL at point"  ar-hide-email-atpt
   :help " ‘ar-hide-email-atpt’
   Hide EMAIL at point if any, nil otherwise. "]

  ["Hide FILENAME at point"  ar-hide-filename-atpt
   :help " ‘ar-hide-filename-atpt’
   Hide FILENAME at point if any, nil otherwise. "]

  ["Hide FLOAT at point"  ar-hide-float-atpt
   :help " ‘ar-hide-float-atpt’
   Hide FLOAT at point if any, nil otherwise. "]

  ["Hide FUNCTION at point"  ar-hide-function-atpt
   :help " ‘ar-hide-function-atpt’
   Hide FUNCTION at point if any, nil otherwise. "]

  ["Hide IP at point"  ar-hide-ip-atpt
   :help " ‘ar-hide-ip-atpt’
   Hide IP at point if any, nil otherwise. "]

  ["Hide ISBN at point"  ar-hide-isbn-atpt
   :help " ‘ar-hide-isbn-atpt’
   Hide ISBN at point if any, nil otherwise. "]

  ["Hide LINE at point"  ar-hide-line-atpt
   :help " ‘ar-hide-line-atpt’
   Hide LINE at point if any, nil otherwise. "]

  ["Hide NAME at point"  ar-hide-name-atpt
   :help " ‘ar-hide-name-atpt’
   Hide NAME at point if any, nil otherwise. "]

  ["Hide NUMBER at point"  ar-hide-number-atpt
   :help " ‘ar-hide-number-atpt’
   Hide NUMBER at point if any, nil otherwise. "]

  ["Hide PAGE at point"  ar-hide-page-atpt
   :help " ‘ar-hide-page-atpt’
   Hide PAGE at point if any, nil otherwise. "]

  ["Hide PARAGRAPH at point"  ar-hide-paragraph-atpt
   :help " ‘ar-hide-paragraph-atpt’
   Hide PARAGRAPH at point if any, nil otherwise. "]

  ["Hide PAREN at point"  ar-hide-paren-atpt
   :help " ‘ar-hide-paren-atpt’
   Hide PAREN at point if any, nil otherwise. "]

  ["Hide PHONE at point"  ar-hide-phone-atpt
   :help " ‘ar-hide-phone-atpt’
   Hide PHONE at point if any, nil otherwise. "]

  ["Hide REGION at point"  ar-hide-region-atpt
   :help " ‘ar-hide-region-atpt’
   Hide REGION at point if any, nil otherwise. "]

  ["Hide SENTENCE at point"  ar-hide-sentence-atpt
   :help " ‘ar-hide-sentence-atpt’
   Hide SENTENCE at point if any, nil otherwise. "]

  ["Hide SEXP at point"  ar-hide-sexp-atpt
   :help " ‘ar-hide-sexp-atpt’
   Hide SEXP at point if any, nil otherwise. "]

  ["Hide STRING at point"  ar-hide-string-atpt
   :help " ‘ar-hide-string-atpt’
   Hide STRING at point if any, nil otherwise. "]

  ["Hide SH-STRUCT at point"  ar-hide-shstruct-atpt
   :help " ‘ar-hide-shstruct-atpt’
   Hide SH-STRUCT at point if any, nil otherwise. "]

  ["Hide SYMBOL at point"  ar-hide-symbol-atpt
   :help " ‘ar-hide-symbol-atpt’
   Hide SYMBOL at point if any, nil otherwise. "]

  ["Hide URL at point"  ar-hide-url-atpt
   :help " ‘ar-hide-url-atpt’
   Hide URL at point if any, nil otherwise. "]

  ["Hide WORD at point"  ar-hide-word-atpt
   :help " ‘ar-hide-word-atpt’
   Hide WORD at point if any, nil otherwise. "]

  ["Hide WORD-ALPHA-ONLY at point"  ar-hide-wordalphaonly-atpt
   :help " ‘ar-hide-wordalphaonly-atpt’
   Hide WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Hide-Show"

 ("Character classes"

  ["Hide-Show [:alnum:] char class at point"  ar-hide-show-alnum-atpt
   :help " ‘ar-hide-show-alnum-atpt’
   Hide-Show ALNUM at point if any, nil otherwise. "]

  ["Hide-Show [:alpha:] char class at point"  ar-hide-show-alpha-atpt
   :help " ‘ar-hide-show-alpha-atpt’
   Hide-Show ALPHA at point if any, nil otherwise. "]

  ["Hide-Show [:ascii:] char class at point"  ar-hide-show-ascii-atpt
   :help " ‘ar-hide-show-ascii-atpt’
   Hide-Show ASCII at point if any, nil otherwise. "]

  ["Hide-Show [:blank:] char class at point"  ar-hide-show-blank-atpt
   :help " ‘ar-hide-show-blank-atpt’
   Hide-Show BLANK at point if any, nil otherwise. "]

  ["Hide-Show [:cntrl:] char class at point"  ar-hide-show-cntrl-atpt
   :help " ‘ar-hide-show-cntrl-atpt’
   Hide-Show CNTRL at point if any, nil otherwise. "]

  ["Hide-Show [:digit:] char class at point"  ar-hide-show-digit-atpt
   :help " ‘ar-hide-show-digit-atpt’
   Hide-Show DIGIT at point if any, nil otherwise. "]

  ["Hide-Show [:graph:] char class at point"  ar-hide-show-graph-atpt
   :help " ‘ar-hide-show-graph-atpt’
   Hide-Show GRAPH at point if any, nil otherwise. "]

  ["Hide-Show [:lower:] char class at point"  ar-hide-show-lower-atpt
   :help " ‘ar-hide-show-lower-atpt’
   Hide-Show LOWER at point if any, nil otherwise. "]

  ["Hide-Show [:nonascii:] char class at point"  ar-hide-show-nonascii-atpt
   :help " ‘ar-hide-show-nonascii-atpt’
   Hide-Show NONASCII at point if any, nil otherwise. "]

  ["Hide-Show [:print:] char class at point"  ar-hide-show-print-atpt
   :help " ‘ar-hide-show-print-atpt’
   Hide-Show PRINT at point if any, nil otherwise. "]

  ["Hide-Show [:punct:] char class at point"  ar-hide-show-punct-atpt
   :help " ‘ar-hide-show-punct-atpt’
   Hide-Show PUNCT at point if any, nil otherwise. "]

  ["Hide-Show [:space:] char class at point"  ar-hide-show-space-atpt
   :help " ‘ar-hide-show-space-atpt’
   Hide-Show SPACE at point if any, nil otherwise. "]

  ["Hide-Show [:upper:] char class at point"  ar-hide-show-upper-atpt
   :help " ‘ar-hide-show-upper-atpt’
   Hide-Show UPPER at point if any, nil otherwise. "]

  ["Hide-Show [:xdigit:] char class at point"  ar-hide-show-xdigit-atpt
   :help " ‘ar-hide-show-xdigit-atpt’
   Hide-Show XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Hide-Show BRACED at point"  ar-hide-show-braced-atpt
   :help " ‘ar-hide-show-braced-atpt’
   Hide-Show BRACED at point if any, nil otherwise. "]

  ["Hide-Show BRACKETED at point"  ar-hide-show-bracketed-atpt
   :help " ‘ar-hide-show-bracketed-atpt’
   Hide-Show BRACKETED at point if any, nil otherwise. "]

  ["Hide-Show LESSER-ANGLED at point"  ar-hide-show-lesserangled-atpt
   :help " ‘ar-hide-show-lesserangled-atpt’
   Hide-Show LESSER-ANGLED at point if any, nil otherwise. "]

  ["Hide-Show GREATER-ANGLED at point"  ar-hide-show-greaterangled-atpt
   :help " ‘ar-hide-show-greaterangled-atpt’
   Hide-Show GREATER-ANGLED at point if any, nil otherwise. "]

  ["Hide-Show LEFT-RIGHT-SINGLEQUOTED at point"  ar-hide-show-curvedsinglequoted-atpt
   :help " ‘ar-hide-show-curvedsinglequoted-atpt’
   Hide-Show LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hide-Show PARENTIZED at point"  ar-hide-show-parentized-atpt
   :help " ‘ar-hide-show-parentized-atpt’
   Hide-Show PARENTIZED at point if any, nil otherwise. "]

  ["Hide-Show BACKSLASHED at point"  ar-hide-show-backslashed-atpt
   :help " ‘ar-hide-show-backslashed-atpt’
   Hide-Show BACKSLASHED at point if any, nil otherwise. "]

  ["Hide-Show DOLLARED at point"  ar-hide-show-dollared-atpt
   :help " ‘ar-hide-show-dollared-atpt’
   Hide-Show DOLLARED at point if any, nil otherwise. "]

  ["Hide-Show DOUBLEQUOTED at point"  ar-hide-show-doublequoted-atpt
   :help " ‘ar-hide-show-doublequoted-atpt’
   Hide-Show DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Hide-Show EQUALIZED at point"  ar-hide-show-equalized-atpt
   :help " ‘ar-hide-show-equalized-atpt’
   Hide-Show EQUALIZED at point if any, nil otherwise. "]

  ["Hide-Show HYPHENED at point"  ar-hide-show-hyphened-atpt
   :help " ‘ar-hide-show-hyphened-atpt’
   Hide-Show HYPHENED at point if any, nil otherwise. "]

  ["Hide-Show QUOTED at point"  ar-hide-show-quoted-atpt
   :help " ‘ar-hide-show-quoted-atpt’
   Hide-Show QUOTED at point if any, nil otherwise. "]

  ["Hide-Show SINGLEQUOTED at point"  ar-hide-show-singlequoted-atpt
   :help " ‘ar-hide-show-singlequoted-atpt’
   Hide-Show SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hide-Show SLASHED at point"  ar-hide-show-slashed-atpt
   :help " ‘ar-hide-show-slashed-atpt’
   Hide-Show SLASHED at point if any, nil otherwise. "]

  ["Hide-Show UNDERSCORED at point"  ar-hide-show-underscored-atpt
   :help " ‘ar-hide-show-underscored-atpt’
   Hide-Show UNDERSCORED at point if any, nil otherwise. "]

  ["Hide-Show WHITESPACED at point"  ar-hide-show-whitespaced-atpt
   :help " ‘ar-hide-show-whitespaced-atpt’
   Hide-Show WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Hide-Show ANGLED-NO-NEST at point"  ar-hide-showanglednonest-atpt
   :help " ‘ar-hide-showanglednonest-atpt’
   Hide-Show ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Hide-Show GREATER-ANGLED-NESTED at point"  ar-hide-show-greateranglednested-atpt
   :help " ‘ar-hide-show-greateranglednested-atpt’
   Hide-Show GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hide-Show LESSER-ANGLED-NESTED at point"  ar-hide-show-lesseranglednested-atpt
   :help " ‘ar-hide-show-lesseranglednested-atpt’
   Hide-Show LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hide-Show BUFFER at point"  ar-hide-show-buffer-atpt
   :help " ‘ar-hide-show-buffer-atpt’
   Hide-Show BUFFER at point if any, nil otherwise. "]

  ["Hide-Show COMMENT at point"  ar-hide-show-comment-atpt
   :help " ‘ar-hide-show-comment-atpt’
   Hide-Show COMMENT at point if any, nil otherwise. "]

  ["Hide-Show CSV at point"  ar-hide-show-csv-atpt
   :help " ‘ar-hide-show-csv-atpt’
   Hide-Show CSV at point if any, nil otherwise. "]

  ["Hide-Show DATE at point"  ar-hide-show-date-atpt
   :help " ‘ar-hide-show-date-atpt’
   Hide-Show DATE at point if any, nil otherwise. "]

  ["Hide-Show DEFUN at point"  ar-hide-show-defun-atpt
   :help " ‘ar-hide-show-defun-atpt’
   Hide-Show DEFUN at point if any, nil otherwise. "]

  ["Hide-Show DELIMITED at point"  ar-hide-show-delimited-atpt
   :help " ‘ar-hide-show-delimited-atpt’
   Hide-Show DELIMITED at point if any, nil otherwise. "]

  ["Hide-Show EMAIL at point"  ar-hide-show-email-atpt
   :help " ‘ar-hide-show-email-atpt’
   Hide-Show EMAIL at point if any, nil otherwise. "]

  ["Hide-Show FILENAME at point"  ar-hide-show-filename-atpt
   :help " ‘ar-hide-show-filename-atpt’
   Hide-Show FILENAME at point if any, nil otherwise. "]

  ["Hide-Show FLOAT at point"  ar-hide-show-float-atpt
   :help " ‘ar-hide-show-float-atpt’
   Hide-Show FLOAT at point if any, nil otherwise. "]

  ["Hide-Show FUNCTION at point"  ar-hide-show-function-atpt
   :help " ‘ar-hide-show-function-atpt’
   Hide-Show FUNCTION at point if any, nil otherwise. "]

  ["Hide-Show IP at point"  ar-hide-show-ip-atpt
   :help " ‘ar-hide-show-ip-atpt’
   Hide-Show IP at point if any, nil otherwise. "]

  ["Hide-Show ISBN at point"  ar-hide-show-isbn-atpt
   :help " ‘ar-hide-show-isbn-atpt’
   Hide-Show ISBN at point if any, nil otherwise. "]

  ["Hide-Show LINE at point"  ar-hide-show-line-atpt
   :help " ‘ar-hide-show-line-atpt’
   Hide-Show LINE at point if any, nil otherwise. "]

  ["Hide-Show NAME at point"  ar-hide-show-name-atpt
   :help " ‘ar-hide-show-name-atpt’
   Hide-Show NAME at point if any, nil otherwise. "]

  ["Hide-Show NUMBER at point"  ar-hide-show-number-atpt
   :help " ‘ar-hide-show-number-atpt’
   Hide-Show NUMBER at point if any, nil otherwise. "]

  ["Hide-Show PAGE at point"  ar-hide-show-page-atpt
   :help " ‘ar-hide-show-page-atpt’
   Hide-Show PAGE at point if any, nil otherwise. "]

  ["Hide-Show PARAGRAPH at point"  ar-hide-show-paragraph-atpt
   :help " ‘ar-hide-show-paragraph-atpt’
   Hide-Show PARAGRAPH at point if any, nil otherwise. "]

  ["Hide-Show PAREN at point"  ar-hide-show-paren-atpt
   :help " ‘ar-hide-show-paren-atpt’
   Hide-Show PAREN at point if any, nil otherwise. "]

  ["Hide-Show PHONE at point"  ar-hide-show-phone-atpt
   :help " ‘ar-hide-show-phone-atpt’
   Hide-Show PHONE at point if any, nil otherwise. "]

  ["Hide-Show REGION at point"  ar-hide-show-region-atpt
   :help " ‘ar-hide-show-region-atpt’
   Hide-Show REGION at point if any, nil otherwise. "]

  ["Hide-Show SENTENCE at point"  ar-hide-show-sentence-atpt
   :help " ‘ar-hide-show-sentence-atpt’
   Hide-Show SENTENCE at point if any, nil otherwise. "]

  ["Hide-Show SEXP at point"  ar-hide-show-sexp-atpt
   :help " ‘ar-hide-show-sexp-atpt’
   Hide-Show SEXP at point if any, nil otherwise. "]

  ["Hide-Show STRING at point"  ar-hide-show-string-atpt
   :help " ‘ar-hide-show-string-atpt’
   Hide-Show STRING at point if any, nil otherwise. "]

  ["Hide-Show SH-STRUCT at point"  ar-hide-show-shstruct-atpt
   :help " ‘ar-hide-show-shstruct-atpt’
   Hide-Show SH-STRUCT at point if any, nil otherwise. "]

  ["Hide-Show SYMBOL at point"  ar-hide-show-symbol-atpt
   :help " ‘ar-hide-show-symbol-atpt’
   Hide-Show SYMBOL at point if any, nil otherwise. "]

  ["Hide-Show URL at point"  ar-hide-show-url-atpt
   :help " ‘ar-hide-show-url-atpt’
   Hide-Show URL at point if any, nil otherwise. "]

  ["Hide-Show WORD at point"  ar-hide-show-word-atpt
   :help " ‘ar-hide-show-word-atpt’
   Hide-Show WORD at point if any, nil otherwise. "]

  ["Hide-Show WORD-ALPHA-ONLY at point"  ar-hide-show-wordalphaonly-atpt
   :help " ‘ar-hide-show-wordalphaonly-atpt’
   Hide-Show WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Highlight"

 ("Character classes"

  ["Highlight [:alnum:] char class at point"  ar-highlight-alnum-atpt
   :help " ‘ar-highlight-alnum-atpt’
   Highlight ALNUM at point if any, nil otherwise. "]

  ["Highlight [:alpha:] char class at point"  ar-highlight-alpha-atpt
   :help " ‘ar-highlight-alpha-atpt’
   Highlight ALPHA at point if any, nil otherwise. "]

  ["Highlight [:ascii:] char class at point"  ar-highlight-ascii-atpt
   :help " ‘ar-highlight-ascii-atpt’
   Highlight ASCII at point if any, nil otherwise. "]

  ["Highlight [:blank:] char class at point"  ar-highlight-blank-atpt
   :help " ‘ar-highlight-blank-atpt’
   Highlight BLANK at point if any, nil otherwise. "]

  ["Highlight [:cntrl:] char class at point"  ar-highlight-cntrl-atpt
   :help " ‘ar-highlight-cntrl-atpt’
   Highlight CNTRL at point if any, nil otherwise. "]

  ["Highlight [:digit:] char class at point"  ar-highlight-digit-atpt
   :help " ‘ar-highlight-digit-atpt’
   Highlight DIGIT at point if any, nil otherwise. "]

  ["Highlight [:graph:] char class at point"  ar-highlight-graph-atpt
   :help " ‘ar-highlight-graph-atpt’
   Highlight GRAPH at point if any, nil otherwise. "]

  ["Highlight [:lower:] char class at point"  ar-highlight-lower-atpt
   :help " ‘ar-highlight-lower-atpt’
   Highlight LOWER at point if any, nil otherwise. "]

  ["Highlight [:nonascii:] char class at point"  ar-highlight-nonascii-atpt
   :help " ‘ar-highlight-nonascii-atpt’
   Highlight NONASCII at point if any, nil otherwise. "]

  ["Highlight [:print:] char class at point"  ar-highlight-print-atpt
   :help " ‘ar-highlight-print-atpt’
   Highlight PRINT at point if any, nil otherwise. "]

  ["Highlight [:punct:] char class at point"  ar-highlight-punct-atpt
   :help " ‘ar-highlight-punct-atpt’
   Highlight PUNCT at point if any, nil otherwise. "]

  ["Highlight [:space:] char class at point"  ar-highlight-space-atpt
   :help " ‘ar-highlight-space-atpt’
   Highlight SPACE at point if any, nil otherwise. "]

  ["Highlight [:upper:] char class at point"  ar-highlight-upper-atpt
   :help " ‘ar-highlight-upper-atpt’
   Highlight UPPER at point if any, nil otherwise. "]

  ["Highlight [:xdigit:] char class at point"  ar-highlight-xdigit-atpt
   :help " ‘ar-highlight-xdigit-atpt’
   Highlight XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Highlight BRACED at point"  ar-highlight-braced-atpt
   :help " ‘ar-highlight-braced-atpt’
   Highlight BRACED at point if any, nil otherwise. "]

  ["Highlight BRACKETED at point"  ar-highlight-bracketed-atpt
   :help " ‘ar-highlight-bracketed-atpt’
   Highlight BRACKETED at point if any, nil otherwise. "]

  ["Highlight LESSER-ANGLED at point"  ar-highlight-lesserangled-atpt
   :help " ‘ar-highlight-lesserangled-atpt’
   Highlight LESSER-ANGLED at point if any, nil otherwise. "]

  ["Highlight GREATER-ANGLED at point"  ar-highlight-greaterangled-atpt
   :help " ‘ar-highlight-greaterangled-atpt’
   Highlight GREATER-ANGLED at point if any, nil otherwise. "]

  ["Highlight LEFT-RIGHT-SINGLEQUOTED at point"  ar-highlight-curvedsinglequoted-atpt
   :help " ‘ar-highlight-curvedsinglequoted-atpt’
   Highlight LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Highlight PARENTIZED at point"  ar-highlight-parentized-atpt
   :help " ‘ar-highlight-parentized-atpt’
   Highlight PARENTIZED at point if any, nil otherwise. "]

  ["Highlight BACKSLASHED at point"  ar-highlight-backslashed-atpt
   :help " ‘ar-highlight-backslashed-atpt’
   Highlight BACKSLASHED at point if any, nil otherwise. "]

  ["Highlight DOLLARED at point"  ar-highlight-dollared-atpt
   :help " ‘ar-highlight-dollared-atpt’
   Highlight DOLLARED at point if any, nil otherwise. "]

  ["Highlight DOUBLEQUOTED at point"  ar-highlight-doublequoted-atpt
   :help " ‘ar-highlight-doublequoted-atpt’
   Highlight DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Highlight EQUALIZED at point"  ar-highlight-equalized-atpt
   :help " ‘ar-highlight-equalized-atpt’
   Highlight EQUALIZED at point if any, nil otherwise. "]

  ["Highlight HYPHENED at point"  ar-highlight-hyphened-atpt
   :help " ‘ar-highlight-hyphened-atpt’
   Highlight HYPHENED at point if any, nil otherwise. "]

  ["Highlight QUOTED at point"  ar-highlight-quoted-atpt
   :help " ‘ar-highlight-quoted-atpt’
   Highlight QUOTED at point if any, nil otherwise. "]

  ["Highlight SINGLEQUOTED at point"  ar-highlight-singlequoted-atpt
   :help " ‘ar-highlight-singlequoted-atpt’
   Highlight SINGLEQUOTED at point if any, nil otherwise. "]

  ["Highlight SLASHED at point"  ar-highlight-slashed-atpt
   :help " ‘ar-highlight-slashed-atpt’
   Highlight SLASHED at point if any, nil otherwise. "]

  ["Highlight UNDERSCORED at point"  ar-highlight-underscored-atpt
   :help " ‘ar-highlight-underscored-atpt’
   Highlight UNDERSCORED at point if any, nil otherwise. "]

  ["Highlight WHITESPACED at point"  ar-highlight-whitespaced-atpt
   :help " ‘ar-highlight-whitespaced-atpt’
   Highlight WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Highlight ANGLED-NO-NEST at point"  ar-highlightanglednonest-atpt
   :help " ‘ar-highlightanglednonest-atpt’
   Highlight ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Highlight GREATER-ANGLED-NESTED at point"  ar-highlight-greateranglednested-atpt
   :help " ‘ar-highlight-greateranglednested-atpt’
   Highlight GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Highlight LESSER-ANGLED-NESTED at point"  ar-highlight-lesseranglednested-atpt
   :help " ‘ar-highlight-lesseranglednested-atpt’
   Highlight LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Highlight BUFFER at point"  ar-highlight-buffer-atpt
   :help " ‘ar-highlight-buffer-atpt’
   Highlight BUFFER at point if any, nil otherwise. "]

  ["Highlight COMMENT at point"  ar-highlight-comment-atpt
   :help " ‘ar-highlight-comment-atpt’
   Highlight COMMENT at point if any, nil otherwise. "]

  ["Highlight CSV at point"  ar-highlight-csv-atpt
   :help " ‘ar-highlight-csv-atpt’
   Highlight CSV at point if any, nil otherwise. "]

  ["Highlight DATE at point"  ar-highlight-date-atpt
   :help " ‘ar-highlight-date-atpt’
   Highlight DATE at point if any, nil otherwise. "]

  ["Highlight DEFUN at point"  ar-highlight-defun-atpt
   :help " ‘ar-highlight-defun-atpt’
   Highlight DEFUN at point if any, nil otherwise. "]

  ["Highlight DELIMITED at point"  ar-highlight-delimited-atpt
   :help " ‘ar-highlight-delimited-atpt’
   Highlight DELIMITED at point if any, nil otherwise. "]

  ["Highlight EMAIL at point"  ar-highlight-email-atpt
   :help " ‘ar-highlight-email-atpt’
   Highlight EMAIL at point if any, nil otherwise. "]

  ["Highlight FILENAME at point"  ar-highlight-filename-atpt
   :help " ‘ar-highlight-filename-atpt’
   Highlight FILENAME at point if any, nil otherwise. "]

  ["Highlight FLOAT at point"  ar-highlight-float-atpt
   :help " ‘ar-highlight-float-atpt’
   Highlight FLOAT at point if any, nil otherwise. "]

  ["Highlight FUNCTION at point"  ar-highlight-function-atpt
   :help " ‘ar-highlight-function-atpt’
   Highlight FUNCTION at point if any, nil otherwise. "]

  ["Highlight IP at point"  ar-highlight-ip-atpt
   :help " ‘ar-highlight-ip-atpt’
   Highlight IP at point if any, nil otherwise. "]

  ["Highlight ISBN at point"  ar-highlight-isbn-atpt
   :help " ‘ar-highlight-isbn-atpt’
   Highlight ISBN at point if any, nil otherwise. "]

  ["Highlight LINE at point"  ar-highlight-line-atpt
   :help " ‘ar-highlight-line-atpt’
   Highlight LINE at point if any, nil otherwise. "]

  ["Highlight NAME at point"  ar-highlight-name-atpt
   :help " ‘ar-highlight-name-atpt’
   Highlight NAME at point if any, nil otherwise. "]

  ["Highlight NUMBER at point"  ar-highlight-number-atpt
   :help " ‘ar-highlight-number-atpt’
   Highlight NUMBER at point if any, nil otherwise. "]

  ["Highlight PAGE at point"  ar-highlight-page-atpt
   :help " ‘ar-highlight-page-atpt’
   Highlight PAGE at point if any, nil otherwise. "]

  ["Highlight PARAGRAPH at point"  ar-highlight-paragraph-atpt
   :help " ‘ar-highlight-paragraph-atpt’
   Highlight PARAGRAPH at point if any, nil otherwise. "]

  ["Highlight PAREN at point"  ar-highlight-paren-atpt
   :help " ‘ar-highlight-paren-atpt’
   Highlight PAREN at point if any, nil otherwise. "]

  ["Highlight PHONE at point"  ar-highlight-phone-atpt
   :help " ‘ar-highlight-phone-atpt’
   Highlight PHONE at point if any, nil otherwise. "]

  ["Highlight REGION at point"  ar-highlight-region-atpt
   :help " ‘ar-highlight-region-atpt’
   Highlight REGION at point if any, nil otherwise. "]

  ["Highlight SENTENCE at point"  ar-highlight-sentence-atpt
   :help " ‘ar-highlight-sentence-atpt’
   Highlight SENTENCE at point if any, nil otherwise. "]

  ["Highlight SEXP at point"  ar-highlight-sexp-atpt
   :help " ‘ar-highlight-sexp-atpt’
   Highlight SEXP at point if any, nil otherwise. "]

  ["Highlight STRING at point"  ar-highlight-string-atpt
   :help " ‘ar-highlight-string-atpt’
   Highlight STRING at point if any, nil otherwise. "]

  ["Highlight SH-STRUCT at point"  ar-highlight-shstruct-atpt
   :help " ‘ar-highlight-shstruct-atpt’
   Highlight SH-STRUCT at point if any, nil otherwise. "]

  ["Highlight SYMBOL at point"  ar-highlight-symbol-atpt
   :help " ‘ar-highlight-symbol-atpt’
   Highlight SYMBOL at point if any, nil otherwise. "]

  ["Highlight URL at point"  ar-highlight-url-atpt
   :help " ‘ar-highlight-url-atpt’
   Highlight URL at point if any, nil otherwise. "]

  ["Highlight WORD at point"  ar-highlight-word-atpt
   :help " ‘ar-highlight-word-atpt’
   Highlight WORD at point if any, nil otherwise. "]

  ["Highlight WORD-ALPHA-ONLY at point"  ar-highlight-wordalphaonly-atpt
   :help " ‘ar-highlight-wordalphaonly-atpt’
   Highlight WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Hyphen"

 ("Character classes"

  ["Hyphen [:alnum:] char class at point"  ar-hyphen-alnum-atpt
   :help " ‘ar-hyphen-alnum-atpt’
   Hyphen ALNUM at point if any, nil otherwise. "]

  ["Hyphen [:alpha:] char class at point"  ar-hyphen-alpha-atpt
   :help " ‘ar-hyphen-alpha-atpt’
   Hyphen ALPHA at point if any, nil otherwise. "]

  ["Hyphen [:ascii:] char class at point"  ar-hyphen-ascii-atpt
   :help " ‘ar-hyphen-ascii-atpt’
   Hyphen ASCII at point if any, nil otherwise. "]

  ["Hyphen [:blank:] char class at point"  ar-hyphen-blank-atpt
   :help " ‘ar-hyphen-blank-atpt’
   Hyphen BLANK at point if any, nil otherwise. "]

  ["Hyphen [:cntrl:] char class at point"  ar-hyphen-cntrl-atpt
   :help " ‘ar-hyphen-cntrl-atpt’
   Hyphen CNTRL at point if any, nil otherwise. "]

  ["Hyphen [:digit:] char class at point"  ar-hyphen-digit-atpt
   :help " ‘ar-hyphen-digit-atpt’
   Hyphen DIGIT at point if any, nil otherwise. "]

  ["Hyphen [:graph:] char class at point"  ar-hyphen-graph-atpt
   :help " ‘ar-hyphen-graph-atpt’
   Hyphen GRAPH at point if any, nil otherwise. "]

  ["Hyphen [:lower:] char class at point"  ar-hyphen-lower-atpt
   :help " ‘ar-hyphen-lower-atpt’
   Hyphen LOWER at point if any, nil otherwise. "]

  ["Hyphen [:nonascii:] char class at point"  ar-hyphen-nonascii-atpt
   :help " ‘ar-hyphen-nonascii-atpt’
   Hyphen NONASCII at point if any, nil otherwise. "]

  ["Hyphen [:print:] char class at point"  ar-hyphen-print-atpt
   :help " ‘ar-hyphen-print-atpt’
   Hyphen PRINT at point if any, nil otherwise. "]

  ["Hyphen [:punct:] char class at point"  ar-hyphen-punct-atpt
   :help " ‘ar-hyphen-punct-atpt’
   Hyphen PUNCT at point if any, nil otherwise. "]

  ["Hyphen [:space:] char class at point"  ar-hyphen-space-atpt
   :help " ‘ar-hyphen-space-atpt’
   Hyphen SPACE at point if any, nil otherwise. "]

  ["Hyphen [:upper:] char class at point"  ar-hyphen-upper-atpt
   :help " ‘ar-hyphen-upper-atpt’
   Hyphen UPPER at point if any, nil otherwise. "]

  ["Hyphen [:xdigit:] char class at point"  ar-hyphen-xdigit-atpt
   :help " ‘ar-hyphen-xdigit-atpt’
   Hyphen XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Hyphen BRACED at point"  ar-hyphen-braced-atpt
   :help " ‘ar-hyphen-braced-atpt’
   Hyphen BRACED at point if any, nil otherwise. "]

  ["Hyphen BRACKETED at point"  ar-hyphen-bracketed-atpt
   :help " ‘ar-hyphen-bracketed-atpt’
   Hyphen BRACKETED at point if any, nil otherwise. "]

  ["Hyphen LESSER-ANGLED at point"  ar-hyphen-lesserangled-atpt
   :help " ‘ar-hyphen-lesserangled-atpt’
   Hyphen LESSER-ANGLED at point if any, nil otherwise. "]

  ["Hyphen GREATER-ANGLED at point"  ar-hyphen-greaterangled-atpt
   :help " ‘ar-hyphen-greaterangled-atpt’
   Hyphen GREATER-ANGLED at point if any, nil otherwise. "]

  ["Hyphen LEFT-RIGHT-SINGLEQUOTED at point"  ar-hyphen-curvedsinglequoted-atpt
   :help " ‘ar-hyphen-curvedsinglequoted-atpt’
   Hyphen LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hyphen PARENTIZED at point"  ar-hyphen-parentized-atpt
   :help " ‘ar-hyphen-parentized-atpt’
   Hyphen PARENTIZED at point if any, nil otherwise. "]

  ["Hyphen BACKSLASHED at point"  ar-hyphen-backslashed-atpt
   :help " ‘ar-hyphen-backslashed-atpt’
   Hyphen BACKSLASHED at point if any, nil otherwise. "]

  ["Hyphen DOLLARED at point"  ar-hyphen-dollared-atpt
   :help " ‘ar-hyphen-dollared-atpt’
   Hyphen DOLLARED at point if any, nil otherwise. "]

  ["Hyphen DOUBLEQUOTED at point"  ar-hyphen-doublequoted-atpt
   :help " ‘ar-hyphen-doublequoted-atpt’
   Hyphen DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Hyphen EQUALIZED at point"  ar-hyphen-equalized-atpt
   :help " ‘ar-hyphen-equalized-atpt’
   Hyphen EQUALIZED at point if any, nil otherwise. "]

  ["Hyphen HYPHENED at point"  ar-hyphen-hyphened-atpt
   :help " ‘ar-hyphen-hyphened-atpt’
   Hyphen HYPHENED at point if any, nil otherwise. "]

  ["Hyphen QUOTED at point"  ar-hyphen-quoted-atpt
   :help " ‘ar-hyphen-quoted-atpt’
   Hyphen QUOTED at point if any, nil otherwise. "]

  ["Hyphen SINGLEQUOTED at point"  ar-hyphen-singlequoted-atpt
   :help " ‘ar-hyphen-singlequoted-atpt’
   Hyphen SINGLEQUOTED at point if any, nil otherwise. "]

  ["Hyphen SLASHED at point"  ar-hyphen-slashed-atpt
   :help " ‘ar-hyphen-slashed-atpt’
   Hyphen SLASHED at point if any, nil otherwise. "]

  ["Hyphen UNDERSCORED at point"  ar-hyphen-underscored-atpt
   :help " ‘ar-hyphen-underscored-atpt’
   Hyphen UNDERSCORED at point if any, nil otherwise. "]

  ["Hyphen WHITESPACED at point"  ar-hyphen-whitespaced-atpt
   :help " ‘ar-hyphen-whitespaced-atpt’
   Hyphen WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Hyphen ANGLED-NO-NEST at point"  ar-hyphenanglednonest-atpt
   :help " ‘ar-hyphenanglednonest-atpt’
   Hyphen ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Hyphen GREATER-ANGLED-NESTED at point"  ar-hyphen-greateranglednested-atpt
   :help " ‘ar-hyphen-greateranglednested-atpt’
   Hyphen GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hyphen LESSER-ANGLED-NESTED at point"  ar-hyphen-lesseranglednested-atpt
   :help " ‘ar-hyphen-lesseranglednested-atpt’
   Hyphen LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Hyphen BUFFER at point"  ar-hyphen-buffer-atpt
   :help " ‘ar-hyphen-buffer-atpt’
   Hyphen BUFFER at point if any, nil otherwise. "]

  ["Hyphen COMMENT at point"  ar-hyphen-comment-atpt
   :help " ‘ar-hyphen-comment-atpt’
   Hyphen COMMENT at point if any, nil otherwise. "]

  ["Hyphen CSV at point"  ar-hyphen-csv-atpt
   :help " ‘ar-hyphen-csv-atpt’
   Hyphen CSV at point if any, nil otherwise. "]

  ["Hyphen DATE at point"  ar-hyphen-date-atpt
   :help " ‘ar-hyphen-date-atpt’
   Hyphen DATE at point if any, nil otherwise. "]

  ["Hyphen DEFUN at point"  ar-hyphen-defun-atpt
   :help " ‘ar-hyphen-defun-atpt’
   Hyphen DEFUN at point if any, nil otherwise. "]

  ["Hyphen DELIMITED at point"  ar-hyphen-delimited-atpt
   :help " ‘ar-hyphen-delimited-atpt’
   Hyphen DELIMITED at point if any, nil otherwise. "]

  ["Hyphen EMAIL at point"  ar-hyphen-email-atpt
   :help " ‘ar-hyphen-email-atpt’
   Hyphen EMAIL at point if any, nil otherwise. "]

  ["Hyphen FILENAME at point"  ar-hyphen-filename-atpt
   :help " ‘ar-hyphen-filename-atpt’
   Hyphen FILENAME at point if any, nil otherwise. "]

  ["Hyphen FLOAT at point"  ar-hyphen-float-atpt
   :help " ‘ar-hyphen-float-atpt’
   Hyphen FLOAT at point if any, nil otherwise. "]

  ["Hyphen FUNCTION at point"  ar-hyphen-function-atpt
   :help " ‘ar-hyphen-function-atpt’
   Hyphen FUNCTION at point if any, nil otherwise. "]

  ["Hyphen IP at point"  ar-hyphen-ip-atpt
   :help " ‘ar-hyphen-ip-atpt’
   Hyphen IP at point if any, nil otherwise. "]

  ["Hyphen ISBN at point"  ar-hyphen-isbn-atpt
   :help " ‘ar-hyphen-isbn-atpt’
   Hyphen ISBN at point if any, nil otherwise. "]

  ["Hyphen LINE at point"  ar-hyphen-line-atpt
   :help " ‘ar-hyphen-line-atpt’
   Hyphen LINE at point if any, nil otherwise. "]

  ["Hyphen NAME at point"  ar-hyphen-name-atpt
   :help " ‘ar-hyphen-name-atpt’
   Hyphen NAME at point if any, nil otherwise. "]

  ["Hyphen NUMBER at point"  ar-hyphen-number-atpt
   :help " ‘ar-hyphen-number-atpt’
   Hyphen NUMBER at point if any, nil otherwise. "]

  ["Hyphen PAGE at point"  ar-hyphen-page-atpt
   :help " ‘ar-hyphen-page-atpt’
   Hyphen PAGE at point if any, nil otherwise. "]

  ["Hyphen PARAGRAPH at point"  ar-hyphen-paragraph-atpt
   :help " ‘ar-hyphen-paragraph-atpt’
   Hyphen PARAGRAPH at point if any, nil otherwise. "]

  ["Hyphen PAREN at point"  ar-hyphen-paren-atpt
   :help " ‘ar-hyphen-paren-atpt’
   Hyphen PAREN at point if any, nil otherwise. "]

  ["Hyphen PHONE at point"  ar-hyphen-phone-atpt
   :help " ‘ar-hyphen-phone-atpt’
   Hyphen PHONE at point if any, nil otherwise. "]

  ["Hyphen REGION at point"  ar-hyphen-region-atpt
   :help " ‘ar-hyphen-region-atpt’
   Hyphen REGION at point if any, nil otherwise. "]

  ["Hyphen SENTENCE at point"  ar-hyphen-sentence-atpt
   :help " ‘ar-hyphen-sentence-atpt’
   Hyphen SENTENCE at point if any, nil otherwise. "]

  ["Hyphen SEXP at point"  ar-hyphen-sexp-atpt
   :help " ‘ar-hyphen-sexp-atpt’
   Hyphen SEXP at point if any, nil otherwise. "]

  ["Hyphen STRING at point"  ar-hyphen-string-atpt
   :help " ‘ar-hyphen-string-atpt’
   Hyphen STRING at point if any, nil otherwise. "]

  ["Hyphen SH-STRUCT at point"  ar-hyphen-shstruct-atpt
   :help " ‘ar-hyphen-shstruct-atpt’
   Hyphen SH-STRUCT at point if any, nil otherwise. "]

  ["Hyphen SYMBOL at point"  ar-hyphen-symbol-atpt
   :help " ‘ar-hyphen-symbol-atpt’
   Hyphen SYMBOL at point if any, nil otherwise. "]

  ["Hyphen URL at point"  ar-hyphen-url-atpt
   :help " ‘ar-hyphen-url-atpt’
   Hyphen URL at point if any, nil otherwise. "]

  ["Hyphen WORD at point"  ar-hyphen-word-atpt
   :help " ‘ar-hyphen-word-atpt’
   Hyphen WORD at point if any, nil otherwise. "]

  ["Hyphen WORD-ALPHA-ONLY at point"  ar-hyphen-wordalphaonly-atpt
   :help " ‘ar-hyphen-wordalphaonly-atpt’
   Hyphen WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Kill"

 ("Character classes"

  ["Kill [:alnum:] char class at point"  ar-kill-alnum-atpt
   :help " ‘ar-kill-alnum-atpt’
   Kill ALNUM at point if any, nil otherwise. "]

  ["Kill [:alpha:] char class at point"  ar-kill-alpha-atpt
   :help " ‘ar-kill-alpha-atpt’
   Kill ALPHA at point if any, nil otherwise. "]

  ["Kill [:ascii:] char class at point"  ar-kill-ascii-atpt
   :help " ‘ar-kill-ascii-atpt’
   Kill ASCII at point if any, nil otherwise. "]

  ["Kill [:blank:] char class at point"  ar-kill-blank-atpt
   :help " ‘ar-kill-blank-atpt’
   Kill BLANK at point if any, nil otherwise. "]

  ["Kill [:cntrl:] char class at point"  ar-kill-cntrl-atpt
   :help " ‘ar-kill-cntrl-atpt’
   Kill CNTRL at point if any, nil otherwise. "]

  ["Kill [:digit:] char class at point"  ar-kill-digit-atpt
   :help " ‘ar-kill-digit-atpt’
   Kill DIGIT at point if any, nil otherwise. "]

  ["Kill [:graph:] char class at point"  ar-kill-graph-atpt
   :help " ‘ar-kill-graph-atpt’
   Kill GRAPH at point if any, nil otherwise. "]

  ["Kill [:lower:] char class at point"  ar-kill-lower-atpt
   :help " ‘ar-kill-lower-atpt’
   Kill LOWER at point if any, nil otherwise. "]

  ["Kill [:nonascii:] char class at point"  ar-kill-nonascii-atpt
   :help " ‘ar-kill-nonascii-atpt’
   Kill NONASCII at point if any, nil otherwise. "]

  ["Kill [:print:] char class at point"  ar-kill-print-atpt
   :help " ‘ar-kill-print-atpt’
   Kill PRINT at point if any, nil otherwise. "]

  ["Kill [:punct:] char class at point"  ar-kill-punct-atpt
   :help " ‘ar-kill-punct-atpt’
   Kill PUNCT at point if any, nil otherwise. "]

  ["Kill [:space:] char class at point"  ar-kill-space-atpt
   :help " ‘ar-kill-space-atpt’
   Kill SPACE at point if any, nil otherwise. "]

  ["Kill [:upper:] char class at point"  ar-kill-upper-atpt
   :help " ‘ar-kill-upper-atpt’
   Kill UPPER at point if any, nil otherwise. "]

  ["Kill [:xdigit:] char class at point"  ar-kill-xdigit-atpt
   :help " ‘ar-kill-xdigit-atpt’
   Kill XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Kill BRACED at point"  ar-kill-braced-atpt
   :help " ‘ar-kill-braced-atpt’
   Kill BRACED at point if any, nil otherwise. "]

  ["Kill BRACKETED at point"  ar-kill-bracketed-atpt
   :help " ‘ar-kill-bracketed-atpt’
   Kill BRACKETED at point if any, nil otherwise. "]

  ["Kill LESSER-ANGLED at point"  ar-kill-lesserangled-atpt
   :help " ‘ar-kill-lesserangled-atpt’
   Kill LESSER-ANGLED at point if any, nil otherwise. "]

  ["Kill GREATER-ANGLED at point"  ar-kill-greaterangled-atpt
   :help " ‘ar-kill-greaterangled-atpt’
   Kill GREATER-ANGLED at point if any, nil otherwise. "]

  ["Kill LEFT-RIGHT-SINGLEQUOTED at point"  ar-kill-curvedsinglequoted-atpt
   :help " ‘ar-kill-curvedsinglequoted-atpt’
   Kill LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Kill PARENTIZED at point"  ar-kill-parentized-atpt
   :help " ‘ar-kill-parentized-atpt’
   Kill PARENTIZED at point if any, nil otherwise. "]

  ["Kill BACKSLASHED at point"  ar-kill-backslashed-atpt
   :help " ‘ar-kill-backslashed-atpt’
   Kill BACKSLASHED at point if any, nil otherwise. "]

  ["Kill DOLLARED at point"  ar-kill-dollared-atpt
   :help " ‘ar-kill-dollared-atpt’
   Kill DOLLARED at point if any, nil otherwise. "]

  ["Kill DOUBLEQUOTED at point"  ar-kill-doublequoted-atpt
   :help " ‘ar-kill-doublequoted-atpt’
   Kill DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Kill EQUALIZED at point"  ar-kill-equalized-atpt
   :help " ‘ar-kill-equalized-atpt’
   Kill EQUALIZED at point if any, nil otherwise. "]

  ["Kill HYPHENED at point"  ar-kill-hyphened-atpt
   :help " ‘ar-kill-hyphened-atpt’
   Kill HYPHENED at point if any, nil otherwise. "]

  ["Kill QUOTED at point"  ar-kill-quoted-atpt
   :help " ‘ar-kill-quoted-atpt’
   Kill QUOTED at point if any, nil otherwise. "]

  ["Kill SINGLEQUOTED at point"  ar-kill-singlequoted-atpt
   :help " ‘ar-kill-singlequoted-atpt’
   Kill SINGLEQUOTED at point if any, nil otherwise. "]

  ["Kill SLASHED at point"  ar-kill-slashed-atpt
   :help " ‘ar-kill-slashed-atpt’
   Kill SLASHED at point if any, nil otherwise. "]

  ["Kill UNDERSCORED at point"  ar-kill-underscored-atpt
   :help " ‘ar-kill-underscored-atpt’
   Kill UNDERSCORED at point if any, nil otherwise. "]

  ["Kill WHITESPACED at point"  ar-kill-whitespaced-atpt
   :help " ‘ar-kill-whitespaced-atpt’
   Kill WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Kill ANGLED-NO-NEST at point"  ar-killanglednonest-atpt
   :help " ‘ar-killanglednonest-atpt’
   Kill ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Kill GREATER-ANGLED-NESTED at point"  ar-kill-greateranglednested-atpt
   :help " ‘ar-kill-greateranglednested-atpt’
   Kill GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Kill LESSER-ANGLED-NESTED at point"  ar-kill-lesseranglednested-atpt
   :help " ‘ar-kill-lesseranglednested-atpt’
   Kill LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Kill BUFFER at point"  ar-kill-buffer-atpt
   :help " ‘ar-kill-buffer-atpt’
   Kill BUFFER at point if any, nil otherwise. "]

  ["Kill COMMENT at point"  ar-kill-comment-atpt
   :help " ‘ar-kill-comment-atpt’
   Kill COMMENT at point if any, nil otherwise. "]

  ["Kill CSV at point"  ar-kill-csv-atpt
   :help " ‘ar-kill-csv-atpt’
   Kill CSV at point if any, nil otherwise. "]

  ["Kill DATE at point"  ar-kill-date-atpt
   :help " ‘ar-kill-date-atpt’
   Kill DATE at point if any, nil otherwise. "]

  ["Kill DEFUN at point"  ar-kill-defun-atpt
   :help " ‘ar-kill-defun-atpt’
   Kill DEFUN at point if any, nil otherwise. "]

  ["Kill DELIMITED at point"  ar-kill-delimited-atpt
   :help " ‘ar-kill-delimited-atpt’
   Kill DELIMITED at point if any, nil otherwise. "]

  ["Kill EMAIL at point"  ar-kill-email-atpt
   :help " ‘ar-kill-email-atpt’
   Kill EMAIL at point if any, nil otherwise. "]

  ["Kill FILENAME at point"  ar-kill-filename-atpt
   :help " ‘ar-kill-filename-atpt’
   Kill FILENAME at point if any, nil otherwise. "]

  ["Kill FLOAT at point"  ar-kill-float-atpt
   :help " ‘ar-kill-float-atpt’
   Kill FLOAT at point if any, nil otherwise. "]

  ["Kill FUNCTION at point"  ar-kill-function-atpt
   :help " ‘ar-kill-function-atpt’
   Kill FUNCTION at point if any, nil otherwise. "]

  ["Kill IP at point"  ar-kill-ip-atpt
   :help " ‘ar-kill-ip-atpt’
   Kill IP at point if any, nil otherwise. "]

  ["Kill ISBN at point"  ar-kill-isbn-atpt
   :help " ‘ar-kill-isbn-atpt’
   Kill ISBN at point if any, nil otherwise. "]

  ["Kill LINE at point"  ar-kill-line-atpt
   :help " ‘ar-kill-line-atpt’
   Kill LINE at point if any, nil otherwise. "]

  ["Kill NAME at point"  ar-kill-name-atpt
   :help " ‘ar-kill-name-atpt’
   Kill NAME at point if any, nil otherwise. "]

  ["Kill NUMBER at point"  ar-kill-number-atpt
   :help " ‘ar-kill-number-atpt’
   Kill NUMBER at point if any, nil otherwise. "]

  ["Kill PAGE at point"  ar-kill-page-atpt
   :help " ‘ar-kill-page-atpt’
   Kill PAGE at point if any, nil otherwise. "]

  ["Kill PARAGRAPH at point"  ar-kill-paragraph-atpt
   :help " ‘ar-kill-paragraph-atpt’
   Kill PARAGRAPH at point if any, nil otherwise. "]

  ["Kill PAREN at point"  ar-kill-paren-atpt
   :help " ‘ar-kill-paren-atpt’
   Kill PAREN at point if any, nil otherwise. "]

  ["Kill PHONE at point"  ar-kill-phone-atpt
   :help " ‘ar-kill-phone-atpt’
   Kill PHONE at point if any, nil otherwise. "]

  ["Kill REGION at point"  ar-kill-region-atpt
   :help " ‘ar-kill-region-atpt’
   Kill REGION at point if any, nil otherwise. "]

  ["Kill SENTENCE at point"  ar-kill-sentence-atpt
   :help " ‘ar-kill-sentence-atpt’
   Kill SENTENCE at point if any, nil otherwise. "]

  ["Kill SEXP at point"  ar-kill-sexp-atpt
   :help " ‘ar-kill-sexp-atpt’
   Kill SEXP at point if any, nil otherwise. "]

  ["Kill STRING at point"  ar-kill-string-atpt
   :help " ‘ar-kill-string-atpt’
   Kill STRING at point if any, nil otherwise. "]

  ["Kill SH-STRUCT at point"  ar-kill-shstruct-atpt
   :help " ‘ar-kill-shstruct-atpt’
   Kill SH-STRUCT at point if any, nil otherwise. "]

  ["Kill SYMBOL at point"  ar-kill-symbol-atpt
   :help " ‘ar-kill-symbol-atpt’
   Kill SYMBOL at point if any, nil otherwise. "]

  ["Kill URL at point"  ar-kill-url-atpt
   :help " ‘ar-kill-url-atpt’
   Kill URL at point if any, nil otherwise. "]

  ["Kill WORD at point"  ar-kill-word-atpt
   :help " ‘ar-kill-word-atpt’
   Kill WORD at point if any, nil otherwise. "]

  ["Kill WORD-ALPHA-ONLY at point"  ar-kill-wordalphaonly-atpt
   :help " ‘ar-kill-wordalphaonly-atpt’
   Kill WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  
  ("Mark"

 ("Character classes"

  ["Mark [:alnum:] char class at point"  ar-mark-alnum-atpt
   :help " ‘ar-mark-alnum-atpt’
   Mark ALNUM at point if any, nil otherwise. "]

  ["Mark [:alpha:] char class at point"  ar-mark-alpha-atpt
   :help " ‘ar-mark-alpha-atpt’
   Mark ALPHA at point if any, nil otherwise. "]

  ["Mark [:ascii:] char class at point"  ar-mark-ascii-atpt
   :help " ‘ar-mark-ascii-atpt’
   Mark ASCII at point if any, nil otherwise. "]

  ["Mark [:blank:] char class at point"  ar-mark-blank-atpt
   :help " ‘ar-mark-blank-atpt’
   Mark BLANK at point if any, nil otherwise. "]

  ["Mark [:cntrl:] char class at point"  ar-mark-cntrl-atpt
   :help " ‘ar-mark-cntrl-atpt’
   Mark CNTRL at point if any, nil otherwise. "]

  ["Mark [:digit:] char class at point"  ar-mark-digit-atpt
   :help " ‘ar-mark-digit-atpt’
   Mark DIGIT at point if any, nil otherwise. "]

  ["Mark [:graph:] char class at point"  ar-mark-graph-atpt
   :help " ‘ar-mark-graph-atpt’
   Mark GRAPH at point if any, nil otherwise. "]

  ["Mark [:lower:] char class at point"  ar-mark-lower-atpt
   :help " ‘ar-mark-lower-atpt’
   Mark LOWER at point if any, nil otherwise. "]

  ["Mark [:nonascii:] char class at point"  ar-mark-nonascii-atpt
   :help " ‘ar-mark-nonascii-atpt’
   Mark NONASCII at point if any, nil otherwise. "]

  ["Mark [:print:] char class at point"  ar-mark-print-atpt
   :help " ‘ar-mark-print-atpt’
   Mark PRINT at point if any, nil otherwise. "]

  ["Mark [:punct:] char class at point"  ar-mark-punct-atpt
   :help " ‘ar-mark-punct-atpt’
   Mark PUNCT at point if any, nil otherwise. "]

  ["Mark [:space:] char class at point"  ar-mark-space-atpt
   :help " ‘ar-mark-space-atpt’
   Mark SPACE at point if any, nil otherwise. "]

  ["Mark [:upper:] char class at point"  ar-mark-upper-atpt
   :help " ‘ar-mark-upper-atpt’
   Mark UPPER at point if any, nil otherwise. "]

  ["Mark [:xdigit:] char class at point"  ar-mark-xdigit-atpt
   :help " ‘ar-mark-xdigit-atpt’
   Mark XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Mark BRACED at point"  ar-mark-braced-atpt
   :help " ‘ar-mark-braced-atpt’
   Mark BRACED at point if any, nil otherwise. "]

  ["Mark BRACKETED at point"  ar-mark-bracketed-atpt
   :help " ‘ar-mark-bracketed-atpt’
   Mark BRACKETED at point if any, nil otherwise. "]

  ["Mark LESSER-ANGLED at point"  ar-mark-lesserangled-atpt
   :help " ‘ar-mark-lesserangled-atpt’
   Mark LESSER-ANGLED at point if any, nil otherwise. "]

  ["Mark GREATER-ANGLED at point"  ar-mark-greaterangled-atpt
   :help " ‘ar-mark-greaterangled-atpt’
   Mark GREATER-ANGLED at point if any, nil otherwise. "]

  ["Mark LEFT-RIGHT-SINGLEQUOTED at point"  ar-mark-curvedsinglequoted-atpt
   :help " ‘ar-mark-curvedsinglequoted-atpt’
   Mark LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Mark PARENTIZED at point"  ar-mark-parentized-atpt
   :help " ‘ar-mark-parentized-atpt’
   Mark PARENTIZED at point if any, nil otherwise. "]

  ["Mark BACKSLASHED at point"  ar-mark-backslashed-atpt
   :help " ‘ar-mark-backslashed-atpt’
   Mark BACKSLASHED at point if any, nil otherwise. "]

  ["Mark DOLLARED at point"  ar-mark-dollared-atpt
   :help " ‘ar-mark-dollared-atpt’
   Mark DOLLARED at point if any, nil otherwise. "]

  ["Mark DOUBLEQUOTED at point"  ar-mark-doublequoted-atpt
   :help " ‘ar-mark-doublequoted-atpt’
   Mark DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Mark EQUALIZED at point"  ar-mark-equalized-atpt
   :help " ‘ar-mark-equalized-atpt’
   Mark EQUALIZED at point if any, nil otherwise. "]

  ["Mark HYPHENED at point"  ar-mark-hyphened-atpt
   :help " ‘ar-mark-hyphened-atpt’
   Mark HYPHENED at point if any, nil otherwise. "]

  ["Mark QUOTED at point"  ar-mark-quoted-atpt
   :help " ‘ar-mark-quoted-atpt’
   Mark QUOTED at point if any, nil otherwise. "]

  ["Mark SINGLEQUOTED at point"  ar-mark-singlequoted-atpt
   :help " ‘ar-mark-singlequoted-atpt’
   Mark SINGLEQUOTED at point if any, nil otherwise. "]

  ["Mark SLASHED at point"  ar-mark-slashed-atpt
   :help " ‘ar-mark-slashed-atpt’
   Mark SLASHED at point if any, nil otherwise. "]

  ["Mark UNDERSCORED at point"  ar-mark-underscored-atpt
   :help " ‘ar-mark-underscored-atpt’
   Mark UNDERSCORED at point if any, nil otherwise. "]

  ["Mark WHITESPACED at point"  ar-mark-whitespaced-atpt
   :help " ‘ar-mark-whitespaced-atpt’
   Mark WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Mark ANGLED-NO-NEST at point"  ar-markanglednonest-atpt
   :help " ‘ar-markanglednonest-atpt’
   Mark ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Mark GREATER-ANGLED-NESTED at point"  ar-mark-greateranglednested-atpt
   :help " ‘ar-mark-greateranglednested-atpt’
   Mark GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Mark LESSER-ANGLED-NESTED at point"  ar-mark-lesseranglednested-atpt
   :help " ‘ar-mark-lesseranglednested-atpt’
   Mark LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Mark BUFFER at point"  ar-mark-buffer-atpt
   :help " ‘ar-mark-buffer-atpt’
   Mark BUFFER at point if any, nil otherwise. "]

  ["Mark COMMENT at point"  ar-mark-comment-atpt
   :help " ‘ar-mark-comment-atpt’
   Mark COMMENT at point if any, nil otherwise. "]

  ["Mark CSV at point"  ar-mark-csv-atpt
   :help " ‘ar-mark-csv-atpt’
   Mark CSV at point if any, nil otherwise. "]

  ["Mark DATE at point"  ar-mark-date-atpt
   :help " ‘ar-mark-date-atpt’
   Mark DATE at point if any, nil otherwise. "]

  ["Mark DEFUN at point"  ar-mark-defun-atpt
   :help " ‘ar-mark-defun-atpt’
   Mark DEFUN at point if any, nil otherwise. "]

  ["Mark DELIMITED at point"  ar-mark-delimited-atpt
   :help " ‘ar-mark-delimited-atpt’
   Mark DELIMITED at point if any, nil otherwise. "]

  ["Mark EMAIL at point"  ar-mark-email-atpt
   :help " ‘ar-mark-email-atpt’
   Mark EMAIL at point if any, nil otherwise. "]

  ["Mark FILENAME at point"  ar-mark-filename-atpt
   :help " ‘ar-mark-filename-atpt’
   Mark FILENAME at point if any, nil otherwise. "]

  ["Mark FLOAT at point"  ar-mark-float-atpt
   :help " ‘ar-mark-float-atpt’
   Mark FLOAT at point if any, nil otherwise. "]

  ["Mark FUNCTION at point"  ar-mark-function-atpt
   :help " ‘ar-mark-function-atpt’
   Mark FUNCTION at point if any, nil otherwise. "]

  ["Mark IP at point"  ar-mark-ip-atpt
   :help " ‘ar-mark-ip-atpt’
   Mark IP at point if any, nil otherwise. "]

  ["Mark ISBN at point"  ar-mark-isbn-atpt
   :help " ‘ar-mark-isbn-atpt’
   Mark ISBN at point if any, nil otherwise. "]

  ["Mark LINE at point"  ar-mark-line-atpt
   :help " ‘ar-mark-line-atpt’
   Mark LINE at point if any, nil otherwise. "]

  ["Mark NAME at point"  ar-mark-name-atpt
   :help " ‘ar-mark-name-atpt’
   Mark NAME at point if any, nil otherwise. "]

  ["Mark NUMBER at point"  ar-mark-number-atpt
   :help " ‘ar-mark-number-atpt’
   Mark NUMBER at point if any, nil otherwise. "]

  ["Mark PAGE at point"  ar-mark-page-atpt
   :help " ‘ar-mark-page-atpt’
   Mark PAGE at point if any, nil otherwise. "]

  ["Mark PARAGRAPH at point"  ar-mark-paragraph-atpt
   :help " ‘ar-mark-paragraph-atpt’
   Mark PARAGRAPH at point if any, nil otherwise. "]

  ["Mark PAREN at point"  ar-mark-paren-atpt
   :help " ‘ar-mark-paren-atpt’
   Mark PAREN at point if any, nil otherwise. "]

  ["Mark PHONE at point"  ar-mark-phone-atpt
   :help " ‘ar-mark-phone-atpt’
   Mark PHONE at point if any, nil otherwise. "]

  ["Mark REGION at point"  ar-mark-region-atpt
   :help " ‘ar-mark-region-atpt’
   Mark REGION at point if any, nil otherwise. "]

  ["Mark SENTENCE at point"  ar-mark-sentence-atpt
   :help " ‘ar-mark-sentence-atpt’
   Mark SENTENCE at point if any, nil otherwise. "]

  ["Mark SEXP at point"  ar-mark-sexp-atpt
   :help " ‘ar-mark-sexp-atpt’
   Mark SEXP at point if any, nil otherwise. "]

  ["Mark STRING at point"  ar-mark-string-atpt
   :help " ‘ar-mark-string-atpt’
   Mark STRING at point if any, nil otherwise. "]

  ["Mark SH-STRUCT at point"  ar-mark-shstruct-atpt
   :help " ‘ar-mark-shstruct-atpt’
   Mark SH-STRUCT at point if any, nil otherwise. "]

  ["Mark SYMBOL at point"  ar-mark-symbol-atpt
   :help " ‘ar-mark-symbol-atpt’
   Mark SYMBOL at point if any, nil otherwise. "]

  ["Mark URL at point"  ar-mark-url-atpt
   :help " ‘ar-mark-url-atpt’
   Mark URL at point if any, nil otherwise. "]

  ["Mark WORD at point"  ar-mark-word-atpt
   :help " ‘ar-mark-word-atpt’
   Mark WORD at point if any, nil otherwise. "]

  ["Mark WORD-ALPHA-ONLY at point"  ar-mark-wordalphaonly-atpt
   :help " ‘ar-mark-wordalphaonly-atpt’
   Mark WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Parentize"

 ("Character classes"

  ["Parentize [:alnum:] char class at point"  ar-parentize-alnum-atpt
   :help " ‘ar-parentize-alnum-atpt’
   Parentize ALNUM at point if any, nil otherwise. "]

  ["Parentize [:alpha:] char class at point"  ar-parentize-alpha-atpt
   :help " ‘ar-parentize-alpha-atpt’
   Parentize ALPHA at point if any, nil otherwise. "]

  ["Parentize [:ascii:] char class at point"  ar-parentize-ascii-atpt
   :help " ‘ar-parentize-ascii-atpt’
   Parentize ASCII at point if any, nil otherwise. "]

  ["Parentize [:blank:] char class at point"  ar-parentize-blank-atpt
   :help " ‘ar-parentize-blank-atpt’
   Parentize BLANK at point if any, nil otherwise. "]

  ["Parentize [:cntrl:] char class at point"  ar-parentize-cntrl-atpt
   :help " ‘ar-parentize-cntrl-atpt’
   Parentize CNTRL at point if any, nil otherwise. "]

  ["Parentize [:digit:] char class at point"  ar-parentize-digit-atpt
   :help " ‘ar-parentize-digit-atpt’
   Parentize DIGIT at point if any, nil otherwise. "]

  ["Parentize [:graph:] char class at point"  ar-parentize-graph-atpt
   :help " ‘ar-parentize-graph-atpt’
   Parentize GRAPH at point if any, nil otherwise. "]

  ["Parentize [:lower:] char class at point"  ar-parentize-lower-atpt
   :help " ‘ar-parentize-lower-atpt’
   Parentize LOWER at point if any, nil otherwise. "]

  ["Parentize [:nonascii:] char class at point"  ar-parentize-nonascii-atpt
   :help " ‘ar-parentize-nonascii-atpt’
   Parentize NONASCII at point if any, nil otherwise. "]

  ["Parentize [:print:] char class at point"  ar-parentize-print-atpt
   :help " ‘ar-parentize-print-atpt’
   Parentize PRINT at point if any, nil otherwise. "]

  ["Parentize [:punct:] char class at point"  ar-parentize-punct-atpt
   :help " ‘ar-parentize-punct-atpt’
   Parentize PUNCT at point if any, nil otherwise. "]

  ["Parentize [:space:] char class at point"  ar-parentize-space-atpt
   :help " ‘ar-parentize-space-atpt’
   Parentize SPACE at point if any, nil otherwise. "]

  ["Parentize [:upper:] char class at point"  ar-parentize-upper-atpt
   :help " ‘ar-parentize-upper-atpt’
   Parentize UPPER at point if any, nil otherwise. "]

  ["Parentize [:xdigit:] char class at point"  ar-parentize-xdigit-atpt
   :help " ‘ar-parentize-xdigit-atpt’
   Parentize XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Parentize BRACED at point"  ar-parentize-braced-atpt
   :help " ‘ar-parentize-braced-atpt’
   Parentize BRACED at point if any, nil otherwise. "]

  ["Parentize BRACKETED at point"  ar-parentize-bracketed-atpt
   :help " ‘ar-parentize-bracketed-atpt’
   Parentize BRACKETED at point if any, nil otherwise. "]

  ["Parentize LESSER-ANGLED at point"  ar-parentize-lesserangled-atpt
   :help " ‘ar-parentize-lesserangled-atpt’
   Parentize LESSER-ANGLED at point if any, nil otherwise. "]

  ["Parentize GREATER-ANGLED at point"  ar-parentize-greaterangled-atpt
   :help " ‘ar-parentize-greaterangled-atpt’
   Parentize GREATER-ANGLED at point if any, nil otherwise. "]

  ["Parentize LEFT-RIGHT-SINGLEQUOTED at point"  ar-parentize-curvedsinglequoted-atpt
   :help " ‘ar-parentize-curvedsinglequoted-atpt’
   Parentize LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Parentize PARENTIZED at point"  ar-parentize-parentized-atpt
   :help " ‘ar-parentize-parentized-atpt’
   Parentize PARENTIZED at point if any, nil otherwise. "]

  ["Parentize BACKSLASHED at point"  ar-parentize-backslashed-atpt
   :help " ‘ar-parentize-backslashed-atpt’
   Parentize BACKSLASHED at point if any, nil otherwise. "]

  ["Parentize DOLLARED at point"  ar-parentize-dollared-atpt
   :help " ‘ar-parentize-dollared-atpt’
   Parentize DOLLARED at point if any, nil otherwise. "]

  ["Parentize DOUBLEQUOTED at point"  ar-parentize-doublequoted-atpt
   :help " ‘ar-parentize-doublequoted-atpt’
   Parentize DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Parentize EQUALIZED at point"  ar-parentize-equalized-atpt
   :help " ‘ar-parentize-equalized-atpt’
   Parentize EQUALIZED at point if any, nil otherwise. "]

  ["Parentize HYPHENED at point"  ar-parentize-hyphened-atpt
   :help " ‘ar-parentize-hyphened-atpt’
   Parentize HYPHENED at point if any, nil otherwise. "]

  ["Parentize QUOTED at point"  ar-parentize-quoted-atpt
   :help " ‘ar-parentize-quoted-atpt’
   Parentize QUOTED at point if any, nil otherwise. "]

  ["Parentize SINGLEQUOTED at point"  ar-parentize-singlequoted-atpt
   :help " ‘ar-parentize-singlequoted-atpt’
   Parentize SINGLEQUOTED at point if any, nil otherwise. "]

  ["Parentize SLASHED at point"  ar-parentize-slashed-atpt
   :help " ‘ar-parentize-slashed-atpt’
   Parentize SLASHED at point if any, nil otherwise. "]

  ["Parentize UNDERSCORED at point"  ar-parentize-underscored-atpt
   :help " ‘ar-parentize-underscored-atpt’
   Parentize UNDERSCORED at point if any, nil otherwise. "]

  ["Parentize WHITESPACED at point"  ar-parentize-whitespaced-atpt
   :help " ‘ar-parentize-whitespaced-atpt’
   Parentize WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Parentize ANGLED-NO-NEST at point"  ar-parentizeanglednonest-atpt
   :help " ‘ar-parentizeanglednonest-atpt’
   Parentize ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Parentize GREATER-ANGLED-NESTED at point"  ar-parentize-greateranglednested-atpt
   :help " ‘ar-parentize-greateranglednested-atpt’
   Parentize GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Parentize LESSER-ANGLED-NESTED at point"  ar-parentize-lesseranglednested-atpt
   :help " ‘ar-parentize-lesseranglednested-atpt’
   Parentize LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Parentize BUFFER at point"  ar-parentize-buffer-atpt
   :help " ‘ar-parentize-buffer-atpt’
   Parentize BUFFER at point if any, nil otherwise. "]

  ["Parentize COMMENT at point"  ar-parentize-comment-atpt
   :help " ‘ar-parentize-comment-atpt’
   Parentize COMMENT at point if any, nil otherwise. "]

  ["Parentize CSV at point"  ar-parentize-csv-atpt
   :help " ‘ar-parentize-csv-atpt’
   Parentize CSV at point if any, nil otherwise. "]

  ["Parentize DATE at point"  ar-parentize-date-atpt
   :help " ‘ar-parentize-date-atpt’
   Parentize DATE at point if any, nil otherwise. "]

  ["Parentize DEFUN at point"  ar-parentize-defun-atpt
   :help " ‘ar-parentize-defun-atpt’
   Parentize DEFUN at point if any, nil otherwise. "]

  ["Parentize DELIMITED at point"  ar-parentize-delimited-atpt
   :help " ‘ar-parentize-delimited-atpt’
   Parentize DELIMITED at point if any, nil otherwise. "]

  ["Parentize EMAIL at point"  ar-parentize-email-atpt
   :help " ‘ar-parentize-email-atpt’
   Parentize EMAIL at point if any, nil otherwise. "]

  ["Parentize FILENAME at point"  ar-parentize-filename-atpt
   :help " ‘ar-parentize-filename-atpt’
   Parentize FILENAME at point if any, nil otherwise. "]

  ["Parentize FLOAT at point"  ar-parentize-float-atpt
   :help " ‘ar-parentize-float-atpt’
   Parentize FLOAT at point if any, nil otherwise. "]

  ["Parentize FUNCTION at point"  ar-parentize-function-atpt
   :help " ‘ar-parentize-function-atpt’
   Parentize FUNCTION at point if any, nil otherwise. "]

  ["Parentize IP at point"  ar-parentize-ip-atpt
   :help " ‘ar-parentize-ip-atpt’
   Parentize IP at point if any, nil otherwise. "]

  ["Parentize ISBN at point"  ar-parentize-isbn-atpt
   :help " ‘ar-parentize-isbn-atpt’
   Parentize ISBN at point if any, nil otherwise. "]

  ["Parentize LINE at point"  ar-parentize-line-atpt
   :help " ‘ar-parentize-line-atpt’
   Parentize LINE at point if any, nil otherwise. "]

  ["Parentize NAME at point"  ar-parentize-name-atpt
   :help " ‘ar-parentize-name-atpt’
   Parentize NAME at point if any, nil otherwise. "]

  ["Parentize NUMBER at point"  ar-parentize-number-atpt
   :help " ‘ar-parentize-number-atpt’
   Parentize NUMBER at point if any, nil otherwise. "]

  ["Parentize PAGE at point"  ar-parentize-page-atpt
   :help " ‘ar-parentize-page-atpt’
   Parentize PAGE at point if any, nil otherwise. "]

  ["Parentize PARAGRAPH at point"  ar-parentize-paragraph-atpt
   :help " ‘ar-parentize-paragraph-atpt’
   Parentize PARAGRAPH at point if any, nil otherwise. "]

  ["Parentize PAREN at point"  ar-parentize-paren-atpt
   :help " ‘ar-parentize-paren-atpt’
   Parentize PAREN at point if any, nil otherwise. "]

  ["Parentize PHONE at point"  ar-parentize-phone-atpt
   :help " ‘ar-parentize-phone-atpt’
   Parentize PHONE at point if any, nil otherwise. "]

  ["Parentize REGION at point"  ar-parentize-region-atpt
   :help " ‘ar-parentize-region-atpt’
   Parentize REGION at point if any, nil otherwise. "]

  ["Parentize SENTENCE at point"  ar-parentize-sentence-atpt
   :help " ‘ar-parentize-sentence-atpt’
   Parentize SENTENCE at point if any, nil otherwise. "]

  ["Parentize SEXP at point"  ar-parentize-sexp-atpt
   :help " ‘ar-parentize-sexp-atpt’
   Parentize SEXP at point if any, nil otherwise. "]

  ["Parentize STRING at point"  ar-parentize-string-atpt
   :help " ‘ar-parentize-string-atpt’
   Parentize STRING at point if any, nil otherwise. "]

  ["Parentize SH-STRUCT at point"  ar-parentize-shstruct-atpt
   :help " ‘ar-parentize-shstruct-atpt’
   Parentize SH-STRUCT at point if any, nil otherwise. "]

  ["Parentize SYMBOL at point"  ar-parentize-symbol-atpt
   :help " ‘ar-parentize-symbol-atpt’
   Parentize SYMBOL at point if any, nil otherwise. "]

  ["Parentize URL at point"  ar-parentize-url-atpt
   :help " ‘ar-parentize-url-atpt’
   Parentize URL at point if any, nil otherwise. "]

  ["Parentize WORD at point"  ar-parentize-word-atpt
   :help " ‘ar-parentize-word-atpt’
   Parentize WORD at point if any, nil otherwise. "]

  ["Parentize WORD-ALPHA-ONLY at point"  ar-parentize-wordalphaonly-atpt
   :help " ‘ar-parentize-wordalphaonly-atpt’
   Parentize WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Quote"

 ("Character classes"

  ["Quote [:alnum:] char class at point"  ar-quote-alnum-atpt
   :help " ‘ar-quote-alnum-atpt’
   Quote ALNUM at point if any, nil otherwise. "]

  ["Quote [:alpha:] char class at point"  ar-quote-alpha-atpt
   :help " ‘ar-quote-alpha-atpt’
   Quote ALPHA at point if any, nil otherwise. "]

  ["Quote [:ascii:] char class at point"  ar-quote-ascii-atpt
   :help " ‘ar-quote-ascii-atpt’
   Quote ASCII at point if any, nil otherwise. "]

  ["Quote [:blank:] char class at point"  ar-quote-blank-atpt
   :help " ‘ar-quote-blank-atpt’
   Quote BLANK at point if any, nil otherwise. "]

  ["Quote [:cntrl:] char class at point"  ar-quote-cntrl-atpt
   :help " ‘ar-quote-cntrl-atpt’
   Quote CNTRL at point if any, nil otherwise. "]

  ["Quote [:digit:] char class at point"  ar-quote-digit-atpt
   :help " ‘ar-quote-digit-atpt’
   Quote DIGIT at point if any, nil otherwise. "]

  ["Quote [:graph:] char class at point"  ar-quote-graph-atpt
   :help " ‘ar-quote-graph-atpt’
   Quote GRAPH at point if any, nil otherwise. "]

  ["Quote [:lower:] char class at point"  ar-quote-lower-atpt
   :help " ‘ar-quote-lower-atpt’
   Quote LOWER at point if any, nil otherwise. "]

  ["Quote [:nonascii:] char class at point"  ar-quote-nonascii-atpt
   :help " ‘ar-quote-nonascii-atpt’
   Quote NONASCII at point if any, nil otherwise. "]

  ["Quote [:print:] char class at point"  ar-quote-print-atpt
   :help " ‘ar-quote-print-atpt’
   Quote PRINT at point if any, nil otherwise. "]

  ["Quote [:punct:] char class at point"  ar-quote-punct-atpt
   :help " ‘ar-quote-punct-atpt’
   Quote PUNCT at point if any, nil otherwise. "]

  ["Quote [:space:] char class at point"  ar-quote-space-atpt
   :help " ‘ar-quote-space-atpt’
   Quote SPACE at point if any, nil otherwise. "]

  ["Quote [:upper:] char class at point"  ar-quote-upper-atpt
   :help " ‘ar-quote-upper-atpt’
   Quote UPPER at point if any, nil otherwise. "]

  ["Quote [:xdigit:] char class at point"  ar-quote-xdigit-atpt
   :help " ‘ar-quote-xdigit-atpt’
   Quote XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Quote BRACED at point"  ar-quote-braced-atpt
   :help " ‘ar-quote-braced-atpt’
   Quote BRACED at point if any, nil otherwise. "]

  ["Quote BRACKETED at point"  ar-quote-bracketed-atpt
   :help " ‘ar-quote-bracketed-atpt’
   Quote BRACKETED at point if any, nil otherwise. "]

  ["Quote LESSER-ANGLED at point"  ar-quote-lesserangled-atpt
   :help " ‘ar-quote-lesserangled-atpt’
   Quote LESSER-ANGLED at point if any, nil otherwise. "]

  ["Quote GREATER-ANGLED at point"  ar-quote-greaterangled-atpt
   :help " ‘ar-quote-greaterangled-atpt’
   Quote GREATER-ANGLED at point if any, nil otherwise. "]

  ["Quote LEFT-RIGHT-SINGLEQUOTED at point"  ar-quote-curvedsinglequoted-atpt
   :help " ‘ar-quote-curvedsinglequoted-atpt’
   Quote LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Quote PARENTIZED at point"  ar-quote-parentized-atpt
   :help " ‘ar-quote-parentized-atpt’
   Quote PARENTIZED at point if any, nil otherwise. "]

  ["Quote BACKSLASHED at point"  ar-quote-backslashed-atpt
   :help " ‘ar-quote-backslashed-atpt’
   Quote BACKSLASHED at point if any, nil otherwise. "]

  ["Quote DOLLARED at point"  ar-quote-dollared-atpt
   :help " ‘ar-quote-dollared-atpt’
   Quote DOLLARED at point if any, nil otherwise. "]

  ["Quote DOUBLEQUOTED at point"  ar-quote-doublequoted-atpt
   :help " ‘ar-quote-doublequoted-atpt’
   Quote DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Quote EQUALIZED at point"  ar-quote-equalized-atpt
   :help " ‘ar-quote-equalized-atpt’
   Quote EQUALIZED at point if any, nil otherwise. "]

  ["Quote HYPHENED at point"  ar-quote-hyphened-atpt
   :help " ‘ar-quote-hyphened-atpt’
   Quote HYPHENED at point if any, nil otherwise. "]

  ["Quote QUOTED at point"  ar-quote-quoted-atpt
   :help " ‘ar-quote-quoted-atpt’
   Quote QUOTED at point if any, nil otherwise. "]

  ["Quote SINGLEQUOTED at point"  ar-quote-singlequoted-atpt
   :help " ‘ar-quote-singlequoted-atpt’
   Quote SINGLEQUOTED at point if any, nil otherwise. "]

  ["Quote SLASHED at point"  ar-quote-slashed-atpt
   :help " ‘ar-quote-slashed-atpt’
   Quote SLASHED at point if any, nil otherwise. "]

  ["Quote UNDERSCORED at point"  ar-quote-underscored-atpt
   :help " ‘ar-quote-underscored-atpt’
   Quote UNDERSCORED at point if any, nil otherwise. "]

  ["Quote WHITESPACED at point"  ar-quote-whitespaced-atpt
   :help " ‘ar-quote-whitespaced-atpt’
   Quote WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Quote ANGLED-NO-NEST at point"  ar-quoteanglednonest-atpt
   :help " ‘ar-quoteanglednonest-atpt’
   Quote ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Quote GREATER-ANGLED-NESTED at point"  ar-quote-greateranglednested-atpt
   :help " ‘ar-quote-greateranglednested-atpt’
   Quote GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Quote LESSER-ANGLED-NESTED at point"  ar-quote-lesseranglednested-atpt
   :help " ‘ar-quote-lesseranglednested-atpt’
   Quote LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Quote BUFFER at point"  ar-quote-buffer-atpt
   :help " ‘ar-quote-buffer-atpt’
   Quote BUFFER at point if any, nil otherwise. "]

  ["Quote COMMENT at point"  ar-quote-comment-atpt
   :help " ‘ar-quote-comment-atpt’
   Quote COMMENT at point if any, nil otherwise. "]

  ["Quote CSV at point"  ar-quote-csv-atpt
   :help " ‘ar-quote-csv-atpt’
   Quote CSV at point if any, nil otherwise. "]

  ["Quote DATE at point"  ar-quote-date-atpt
   :help " ‘ar-quote-date-atpt’
   Quote DATE at point if any, nil otherwise. "]

  ["Quote DEFUN at point"  ar-quote-defun-atpt
   :help " ‘ar-quote-defun-atpt’
   Quote DEFUN at point if any, nil otherwise. "]

  ["Quote DELIMITED at point"  ar-quote-delimited-atpt
   :help " ‘ar-quote-delimited-atpt’
   Quote DELIMITED at point if any, nil otherwise. "]

  ["Quote EMAIL at point"  ar-quote-email-atpt
   :help " ‘ar-quote-email-atpt’
   Quote EMAIL at point if any, nil otherwise. "]

  ["Quote FILENAME at point"  ar-quote-filename-atpt
   :help " ‘ar-quote-filename-atpt’
   Quote FILENAME at point if any, nil otherwise. "]

  ["Quote FLOAT at point"  ar-quote-float-atpt
   :help " ‘ar-quote-float-atpt’
   Quote FLOAT at point if any, nil otherwise. "]

  ["Quote FUNCTION at point"  ar-quote-function-atpt
   :help " ‘ar-quote-function-atpt’
   Quote FUNCTION at point if any, nil otherwise. "]

  ["Quote IP at point"  ar-quote-ip-atpt
   :help " ‘ar-quote-ip-atpt’
   Quote IP at point if any, nil otherwise. "]

  ["Quote ISBN at point"  ar-quote-isbn-atpt
   :help " ‘ar-quote-isbn-atpt’
   Quote ISBN at point if any, nil otherwise. "]

  ["Quote LINE at point"  ar-quote-line-atpt
   :help " ‘ar-quote-line-atpt’
   Quote LINE at point if any, nil otherwise. "]

  ["Quote NAME at point"  ar-quote-name-atpt
   :help " ‘ar-quote-name-atpt’
   Quote NAME at point if any, nil otherwise. "]

  ["Quote NUMBER at point"  ar-quote-number-atpt
   :help " ‘ar-quote-number-atpt’
   Quote NUMBER at point if any, nil otherwise. "]

  ["Quote PAGE at point"  ar-quote-page-atpt
   :help " ‘ar-quote-page-atpt’
   Quote PAGE at point if any, nil otherwise. "]

  ["Quote PARAGRAPH at point"  ar-quote-paragraph-atpt
   :help " ‘ar-quote-paragraph-atpt’
   Quote PARAGRAPH at point if any, nil otherwise. "]

  ["Quote PAREN at point"  ar-quote-paren-atpt
   :help " ‘ar-quote-paren-atpt’
   Quote PAREN at point if any, nil otherwise. "]

  ["Quote PHONE at point"  ar-quote-phone-atpt
   :help " ‘ar-quote-phone-atpt’
   Quote PHONE at point if any, nil otherwise. "]

  ["Quote REGION at point"  ar-quote-region-atpt
   :help " ‘ar-quote-region-atpt’
   Quote REGION at point if any, nil otherwise. "]

  ["Quote SENTENCE at point"  ar-quote-sentence-atpt
   :help " ‘ar-quote-sentence-atpt’
   Quote SENTENCE at point if any, nil otherwise. "]

  ["Quote SEXP at point"  ar-quote-sexp-atpt
   :help " ‘ar-quote-sexp-atpt’
   Quote SEXP at point if any, nil otherwise. "]

  ["Quote STRING at point"  ar-quote-string-atpt
   :help " ‘ar-quote-string-atpt’
   Quote STRING at point if any, nil otherwise. "]

  ["Quote SH-STRUCT at point"  ar-quote-shstruct-atpt
   :help " ‘ar-quote-shstruct-atpt’
   Quote SH-STRUCT at point if any, nil otherwise. "]

  ["Quote SYMBOL at point"  ar-quote-symbol-atpt
   :help " ‘ar-quote-symbol-atpt’
   Quote SYMBOL at point if any, nil otherwise. "]

  ["Quote URL at point"  ar-quote-url-atpt
   :help " ‘ar-quote-url-atpt’
   Quote URL at point if any, nil otherwise. "]

  ["Quote WORD at point"  ar-quote-word-atpt
   :help " ‘ar-quote-word-atpt’
   Quote WORD at point if any, nil otherwise. "]

  ["Quote WORD-ALPHA-ONLY at point"  ar-quote-wordalphaonly-atpt
   :help " ‘ar-quote-wordalphaonly-atpt’
   Quote WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Right-Singlequote"

 ("Character classes"

  ["Right-Singlequote [:alnum:] char class at point"  ar-right-singlequote-alnum-atpt
   :help " ‘ar-right-singlequote-alnum-atpt’
   Right-Singlequote ALNUM at point if any, nil otherwise. "]

  ["Right-Singlequote [:alpha:] char class at point"  ar-right-singlequote-alpha-atpt
   :help " ‘ar-right-singlequote-alpha-atpt’
   Right-Singlequote ALPHA at point if any, nil otherwise. "]

  ["Right-Singlequote [:ascii:] char class at point"  ar-right-singlequote-ascii-atpt
   :help " ‘ar-right-singlequote-ascii-atpt’
   Right-Singlequote ASCII at point if any, nil otherwise. "]

  ["Right-Singlequote [:blank:] char class at point"  ar-right-singlequote-blank-atpt
   :help " ‘ar-right-singlequote-blank-atpt’
   Right-Singlequote BLANK at point if any, nil otherwise. "]

  ["Right-Singlequote [:cntrl:] char class at point"  ar-right-singlequote-cntrl-atpt
   :help " ‘ar-right-singlequote-cntrl-atpt’
   Right-Singlequote CNTRL at point if any, nil otherwise. "]

  ["Right-Singlequote [:digit:] char class at point"  ar-right-singlequote-digit-atpt
   :help " ‘ar-right-singlequote-digit-atpt’
   Right-Singlequote DIGIT at point if any, nil otherwise. "]

  ["Right-Singlequote [:graph:] char class at point"  ar-right-singlequote-graph-atpt
   :help " ‘ar-right-singlequote-graph-atpt’
   Right-Singlequote GRAPH at point if any, nil otherwise. "]

  ["Right-Singlequote [:lower:] char class at point"  ar-right-singlequote-lower-atpt
   :help " ‘ar-right-singlequote-lower-atpt’
   Right-Singlequote LOWER at point if any, nil otherwise. "]

  ["Right-Singlequote [:nonascii:] char class at point"  ar-right-singlequote-nonascii-atpt
   :help " ‘ar-right-singlequote-nonascii-atpt’
   Right-Singlequote NONASCII at point if any, nil otherwise. "]

  ["Right-Singlequote [:print:] char class at point"  ar-right-singlequote-print-atpt
   :help " ‘ar-right-singlequote-print-atpt’
   Right-Singlequote PRINT at point if any, nil otherwise. "]

  ["Right-Singlequote [:punct:] char class at point"  ar-right-singlequote-punct-atpt
   :help " ‘ar-right-singlequote-punct-atpt’
   Right-Singlequote PUNCT at point if any, nil otherwise. "]

  ["Right-Singlequote [:space:] char class at point"  ar-right-singlequote-space-atpt
   :help " ‘ar-right-singlequote-space-atpt’
   Right-Singlequote SPACE at point if any, nil otherwise. "]

  ["Right-Singlequote [:upper:] char class at point"  ar-right-singlequote-upper-atpt
   :help " ‘ar-right-singlequote-upper-atpt’
   Right-Singlequote UPPER at point if any, nil otherwise. "]

  ["Right-Singlequote [:xdigit:] char class at point"  ar-right-singlequote-xdigit-atpt
   :help " ‘ar-right-singlequote-xdigit-atpt’
   Right-Singlequote XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Right-Singlequote BRACED at point"  ar-right-singlequote-braced-atpt
   :help " ‘ar-right-singlequote-braced-atpt’
   Right-Singlequote BRACED at point if any, nil otherwise. "]

  ["Right-Singlequote BRACKETED at point"  ar-right-singlequote-bracketed-atpt
   :help " ‘ar-right-singlequote-bracketed-atpt’
   Right-Singlequote BRACKETED at point if any, nil otherwise. "]

  ["Right-Singlequote LESSER-ANGLED at point"  ar-right-singlequote-lesserangled-atpt
   :help " ‘ar-right-singlequote-lesserangled-atpt’
   Right-Singlequote LESSER-ANGLED at point if any, nil otherwise. "]

  ["Right-Singlequote GREATER-ANGLED at point"  ar-right-singlequote-greaterangled-atpt
   :help " ‘ar-right-singlequote-greaterangled-atpt’
   Right-Singlequote GREATER-ANGLED at point if any, nil otherwise. "]

  ["Right-Singlequote LEFT-RIGHT-SINGLEQUOTED at point"  ar-right-singlequote-curvedsinglequoted-atpt
   :help " ‘ar-right-singlequote-curvedsinglequoted-atpt’
   Right-Singlequote LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Right-Singlequote PARENTIZED at point"  ar-right-singlequote-parentized-atpt
   :help " ‘ar-right-singlequote-parentized-atpt’
   Right-Singlequote PARENTIZED at point if any, nil otherwise. "]

  ["Right-Singlequote BACKSLASHED at point"  ar-right-singlequote-backslashed-atpt
   :help " ‘ar-right-singlequote-backslashed-atpt’
   Right-Singlequote BACKSLASHED at point if any, nil otherwise. "]

  ["Right-Singlequote DOLLARED at point"  ar-right-singlequote-dollared-atpt
   :help " ‘ar-right-singlequote-dollared-atpt’
   Right-Singlequote DOLLARED at point if any, nil otherwise. "]

  ["Right-Singlequote DOUBLEQUOTED at point"  ar-right-singlequote-doublequoted-atpt
   :help " ‘ar-right-singlequote-doublequoted-atpt’
   Right-Singlequote DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Right-Singlequote EQUALIZED at point"  ar-right-singlequote-equalized-atpt
   :help " ‘ar-right-singlequote-equalized-atpt’
   Right-Singlequote EQUALIZED at point if any, nil otherwise. "]

  ["Right-Singlequote HYPHENED at point"  ar-right-singlequote-hyphened-atpt
   :help " ‘ar-right-singlequote-hyphened-atpt’
   Right-Singlequote HYPHENED at point if any, nil otherwise. "]

  ["Right-Singlequote QUOTED at point"  ar-right-singlequote-quoted-atpt
   :help " ‘ar-right-singlequote-quoted-atpt’
   Right-Singlequote QUOTED at point if any, nil otherwise. "]

  ["Right-Singlequote SINGLEQUOTED at point"  ar-right-singlequote-singlequoted-atpt
   :help " ‘ar-right-singlequote-singlequoted-atpt’
   Right-Singlequote SINGLEQUOTED at point if any, nil otherwise. "]

  ["Right-Singlequote SLASHED at point"  ar-right-singlequote-slashed-atpt
   :help " ‘ar-right-singlequote-slashed-atpt’
   Right-Singlequote SLASHED at point if any, nil otherwise. "]

  ["Right-Singlequote UNDERSCORED at point"  ar-right-singlequote-underscored-atpt
   :help " ‘ar-right-singlequote-underscored-atpt’
   Right-Singlequote UNDERSCORED at point if any, nil otherwise. "]

  ["Right-Singlequote WHITESPACED at point"  ar-right-singlequote-whitespaced-atpt
   :help " ‘ar-right-singlequote-whitespaced-atpt’
   Right-Singlequote WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Right-Singlequote ANGLED-NO-NEST at point"  ar-right-singlequoteanglednonest-atpt
   :help " ‘ar-right-singlequoteanglednonest-atpt’
   Right-Singlequote ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Right-Singlequote GREATER-ANGLED-NESTED at point"  ar-right-singlequote-greateranglednested-atpt
   :help " ‘ar-right-singlequote-greateranglednested-atpt’
   Right-Singlequote GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Right-Singlequote LESSER-ANGLED-NESTED at point"  ar-right-singlequote-lesseranglednested-atpt
   :help " ‘ar-right-singlequote-lesseranglednested-atpt’
   Right-Singlequote LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Right-Singlequote BUFFER at point"  ar-right-singlequote-buffer-atpt
   :help " ‘ar-right-singlequote-buffer-atpt’
   Right-Singlequote BUFFER at point if any, nil otherwise. "]

  ["Right-Singlequote COMMENT at point"  ar-right-singlequote-comment-atpt
   :help " ‘ar-right-singlequote-comment-atpt’
   Right-Singlequote COMMENT at point if any, nil otherwise. "]

  ["Right-Singlequote CSV at point"  ar-right-singlequote-csv-atpt
   :help " ‘ar-right-singlequote-csv-atpt’
   Right-Singlequote CSV at point if any, nil otherwise. "]

  ["Right-Singlequote DATE at point"  ar-right-singlequote-date-atpt
   :help " ‘ar-right-singlequote-date-atpt’
   Right-Singlequote DATE at point if any, nil otherwise. "]

  ["Right-Singlequote DEFUN at point"  ar-right-singlequote-defun-atpt
   :help " ‘ar-right-singlequote-defun-atpt’
   Right-Singlequote DEFUN at point if any, nil otherwise. "]

  ["Right-Singlequote DELIMITED at point"  ar-right-singlequote-delimited-atpt
   :help " ‘ar-right-singlequote-delimited-atpt’
   Right-Singlequote DELIMITED at point if any, nil otherwise. "]

  ["Right-Singlequote EMAIL at point"  ar-right-singlequote-email-atpt
   :help " ‘ar-right-singlequote-email-atpt’
   Right-Singlequote EMAIL at point if any, nil otherwise. "]

  ["Right-Singlequote FILENAME at point"  ar-right-singlequote-filename-atpt
   :help " ‘ar-right-singlequote-filename-atpt’
   Right-Singlequote FILENAME at point if any, nil otherwise. "]

  ["Right-Singlequote FLOAT at point"  ar-right-singlequote-float-atpt
   :help " ‘ar-right-singlequote-float-atpt’
   Right-Singlequote FLOAT at point if any, nil otherwise. "]

  ["Right-Singlequote FUNCTION at point"  ar-right-singlequote-function-atpt
   :help " ‘ar-right-singlequote-function-atpt’
   Right-Singlequote FUNCTION at point if any, nil otherwise. "]

  ["Right-Singlequote IP at point"  ar-right-singlequote-ip-atpt
   :help " ‘ar-right-singlequote-ip-atpt’
   Right-Singlequote IP at point if any, nil otherwise. "]

  ["Right-Singlequote ISBN at point"  ar-right-singlequote-isbn-atpt
   :help " ‘ar-right-singlequote-isbn-atpt’
   Right-Singlequote ISBN at point if any, nil otherwise. "]

  ["Right-Singlequote LINE at point"  ar-right-singlequote-line-atpt
   :help " ‘ar-right-singlequote-line-atpt’
   Right-Singlequote LINE at point if any, nil otherwise. "]

  ["Right-Singlequote NAME at point"  ar-right-singlequote-name-atpt
   :help " ‘ar-right-singlequote-name-atpt’
   Right-Singlequote NAME at point if any, nil otherwise. "]

  ["Right-Singlequote NUMBER at point"  ar-right-singlequote-number-atpt
   :help " ‘ar-right-singlequote-number-atpt’
   Right-Singlequote NUMBER at point if any, nil otherwise. "]

  ["Right-Singlequote PAGE at point"  ar-right-singlequote-page-atpt
   :help " ‘ar-right-singlequote-page-atpt’
   Right-Singlequote PAGE at point if any, nil otherwise. "]

  ["Right-Singlequote PARAGRAPH at point"  ar-right-singlequote-paragraph-atpt
   :help " ‘ar-right-singlequote-paragraph-atpt’
   Right-Singlequote PARAGRAPH at point if any, nil otherwise. "]

  ["Right-Singlequote PAREN at point"  ar-right-singlequote-paren-atpt
   :help " ‘ar-right-singlequote-paren-atpt’
   Right-Singlequote PAREN at point if any, nil otherwise. "]

  ["Right-Singlequote PHONE at point"  ar-right-singlequote-phone-atpt
   :help " ‘ar-right-singlequote-phone-atpt’
   Right-Singlequote PHONE at point if any, nil otherwise. "]

  ["Right-Singlequote REGION at point"  ar-right-singlequote-region-atpt
   :help " ‘ar-right-singlequote-region-atpt’
   Right-Singlequote REGION at point if any, nil otherwise. "]

  ["Right-Singlequote SENTENCE at point"  ar-right-singlequote-sentence-atpt
   :help " ‘ar-right-singlequote-sentence-atpt’
   Right-Singlequote SENTENCE at point if any, nil otherwise. "]

  ["Right-Singlequote SEXP at point"  ar-right-singlequote-sexp-atpt
   :help " ‘ar-right-singlequote-sexp-atpt’
   Right-Singlequote SEXP at point if any, nil otherwise. "]

  ["Right-Singlequote STRING at point"  ar-right-singlequote-string-atpt
   :help " ‘ar-right-singlequote-string-atpt’
   Right-Singlequote STRING at point if any, nil otherwise. "]

  ["Right-Singlequote SH-STRUCT at point"  ar-right-singlequote-shstruct-atpt
   :help " ‘ar-right-singlequote-shstruct-atpt’
   Right-Singlequote SH-STRUCT at point if any, nil otherwise. "]

  ["Right-Singlequote SYMBOL at point"  ar-right-singlequote-symbol-atpt
   :help " ‘ar-right-singlequote-symbol-atpt’
   Right-Singlequote SYMBOL at point if any, nil otherwise. "]

  ["Right-Singlequote URL at point"  ar-right-singlequote-url-atpt
   :help " ‘ar-right-singlequote-url-atpt’
   Right-Singlequote URL at point if any, nil otherwise. "]

  ["Right-Singlequote WORD at point"  ar-right-singlequote-word-atpt
   :help " ‘ar-right-singlequote-word-atpt’
   Right-Singlequote WORD at point if any, nil otherwise. "]

  ["Right-Singlequote WORD-ALPHA-ONLY at point"  ar-right-singlequote-wordalphaonly-atpt
   :help " ‘ar-right-singlequote-wordalphaonly-atpt’
   Right-Singlequote WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Separate"

 ("Character classes"

  ["Separate [:alnum:] char class at point"  ar-separate-alnum-atpt
   :help " ‘ar-separate-alnum-atpt’
   Separate ALNUM at point if any, nil otherwise. "]

  ["Separate [:alpha:] char class at point"  ar-separate-alpha-atpt
   :help " ‘ar-separate-alpha-atpt’
   Separate ALPHA at point if any, nil otherwise. "]

  ["Separate [:ascii:] char class at point"  ar-separate-ascii-atpt
   :help " ‘ar-separate-ascii-atpt’
   Separate ASCII at point if any, nil otherwise. "]

  ["Separate [:blank:] char class at point"  ar-separate-blank-atpt
   :help " ‘ar-separate-blank-atpt’
   Separate BLANK at point if any, nil otherwise. "]

  ["Separate [:cntrl:] char class at point"  ar-separate-cntrl-atpt
   :help " ‘ar-separate-cntrl-atpt’
   Separate CNTRL at point if any, nil otherwise. "]

  ["Separate [:digit:] char class at point"  ar-separate-digit-atpt
   :help " ‘ar-separate-digit-atpt’
   Separate DIGIT at point if any, nil otherwise. "]

  ["Separate [:graph:] char class at point"  ar-separate-graph-atpt
   :help " ‘ar-separate-graph-atpt’
   Separate GRAPH at point if any, nil otherwise. "]

  ["Separate [:lower:] char class at point"  ar-separate-lower-atpt
   :help " ‘ar-separate-lower-atpt’
   Separate LOWER at point if any, nil otherwise. "]

  ["Separate [:nonascii:] char class at point"  ar-separate-nonascii-atpt
   :help " ‘ar-separate-nonascii-atpt’
   Separate NONASCII at point if any, nil otherwise. "]

  ["Separate [:print:] char class at point"  ar-separate-print-atpt
   :help " ‘ar-separate-print-atpt’
   Separate PRINT at point if any, nil otherwise. "]

  ["Separate [:punct:] char class at point"  ar-separate-punct-atpt
   :help " ‘ar-separate-punct-atpt’
   Separate PUNCT at point if any, nil otherwise. "]

  ["Separate [:space:] char class at point"  ar-separate-space-atpt
   :help " ‘ar-separate-space-atpt’
   Separate SPACE at point if any, nil otherwise. "]

  ["Separate [:upper:] char class at point"  ar-separate-upper-atpt
   :help " ‘ar-separate-upper-atpt’
   Separate UPPER at point if any, nil otherwise. "]

  ["Separate [:xdigit:] char class at point"  ar-separate-xdigit-atpt
   :help " ‘ar-separate-xdigit-atpt’
   Separate XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Separate BRACED at point"  ar-separate-braced-atpt
   :help " ‘ar-separate-braced-atpt’
   Separate BRACED at point if any, nil otherwise. "]

  ["Separate BRACKETED at point"  ar-separate-bracketed-atpt
   :help " ‘ar-separate-bracketed-atpt’
   Separate BRACKETED at point if any, nil otherwise. "]

  ["Separate LESSER-ANGLED at point"  ar-separate-lesserangled-atpt
   :help " ‘ar-separate-lesserangled-atpt’
   Separate LESSER-ANGLED at point if any, nil otherwise. "]

  ["Separate GREATER-ANGLED at point"  ar-separate-greaterangled-atpt
   :help " ‘ar-separate-greaterangled-atpt’
   Separate GREATER-ANGLED at point if any, nil otherwise. "]

  ["Separate LEFT-RIGHT-SINGLEQUOTED at point"  ar-separate-curvedsinglequoted-atpt
   :help " ‘ar-separate-curvedsinglequoted-atpt’
   Separate LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Separate PARENTIZED at point"  ar-separate-parentized-atpt
   :help " ‘ar-separate-parentized-atpt’
   Separate PARENTIZED at point if any, nil otherwise. "]

  ["Separate BACKSLASHED at point"  ar-separate-backslashed-atpt
   :help " ‘ar-separate-backslashed-atpt’
   Separate BACKSLASHED at point if any, nil otherwise. "]

  ["Separate DOLLARED at point"  ar-separate-dollared-atpt
   :help " ‘ar-separate-dollared-atpt’
   Separate DOLLARED at point if any, nil otherwise. "]

  ["Separate DOUBLEQUOTED at point"  ar-separate-doublequoted-atpt
   :help " ‘ar-separate-doublequoted-atpt’
   Separate DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Separate EQUALIZED at point"  ar-separate-equalized-atpt
   :help " ‘ar-separate-equalized-atpt’
   Separate EQUALIZED at point if any, nil otherwise. "]

  ["Separate HYPHENED at point"  ar-separate-hyphened-atpt
   :help " ‘ar-separate-hyphened-atpt’
   Separate HYPHENED at point if any, nil otherwise. "]

  ["Separate QUOTED at point"  ar-separate-quoted-atpt
   :help " ‘ar-separate-quoted-atpt’
   Separate QUOTED at point if any, nil otherwise. "]

  ["Separate SINGLEQUOTED at point"  ar-separate-singlequoted-atpt
   :help " ‘ar-separate-singlequoted-atpt’
   Separate SINGLEQUOTED at point if any, nil otherwise. "]

  ["Separate SLASHED at point"  ar-separate-slashed-atpt
   :help " ‘ar-separate-slashed-atpt’
   Separate SLASHED at point if any, nil otherwise. "]

  ["Separate UNDERSCORED at point"  ar-separate-underscored-atpt
   :help " ‘ar-separate-underscored-atpt’
   Separate UNDERSCORED at point if any, nil otherwise. "]

  ["Separate WHITESPACED at point"  ar-separate-whitespaced-atpt
   :help " ‘ar-separate-whitespaced-atpt’
   Separate WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Separate ANGLED-NO-NEST at point"  ar-separateanglednonest-atpt
   :help " ‘ar-separateanglednonest-atpt’
   Separate ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Separate GREATER-ANGLED-NESTED at point"  ar-separate-greateranglednested-atpt
   :help " ‘ar-separate-greateranglednested-atpt’
   Separate GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Separate LESSER-ANGLED-NESTED at point"  ar-separate-lesseranglednested-atpt
   :help " ‘ar-separate-lesseranglednested-atpt’
   Separate LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Separate BUFFER at point"  ar-separate-buffer-atpt
   :help " ‘ar-separate-buffer-atpt’
   Separate BUFFER at point if any, nil otherwise. "]

  ["Separate COMMENT at point"  ar-separate-comment-atpt
   :help " ‘ar-separate-comment-atpt’
   Separate COMMENT at point if any, nil otherwise. "]

  ["Separate CSV at point"  ar-separate-csv-atpt
   :help " ‘ar-separate-csv-atpt’
   Separate CSV at point if any, nil otherwise. "]

  ["Separate DATE at point"  ar-separate-date-atpt
   :help " ‘ar-separate-date-atpt’
   Separate DATE at point if any, nil otherwise. "]

  ["Separate DEFUN at point"  ar-separate-defun-atpt
   :help " ‘ar-separate-defun-atpt’
   Separate DEFUN at point if any, nil otherwise. "]

  ["Separate DELIMITED at point"  ar-separate-delimited-atpt
   :help " ‘ar-separate-delimited-atpt’
   Separate DELIMITED at point if any, nil otherwise. "]

  ["Separate EMAIL at point"  ar-separate-email-atpt
   :help " ‘ar-separate-email-atpt’
   Separate EMAIL at point if any, nil otherwise. "]

  ["Separate FILENAME at point"  ar-separate-filename-atpt
   :help " ‘ar-separate-filename-atpt’
   Separate FILENAME at point if any, nil otherwise. "]

  ["Separate FLOAT at point"  ar-separate-float-atpt
   :help " ‘ar-separate-float-atpt’
   Separate FLOAT at point if any, nil otherwise. "]

  ["Separate FUNCTION at point"  ar-separate-function-atpt
   :help " ‘ar-separate-function-atpt’
   Separate FUNCTION at point if any, nil otherwise. "]

  ["Separate IP at point"  ar-separate-ip-atpt
   :help " ‘ar-separate-ip-atpt’
   Separate IP at point if any, nil otherwise. "]

  ["Separate ISBN at point"  ar-separate-isbn-atpt
   :help " ‘ar-separate-isbn-atpt’
   Separate ISBN at point if any, nil otherwise. "]

  ["Separate LINE at point"  ar-separate-line-atpt
   :help " ‘ar-separate-line-atpt’
   Separate LINE at point if any, nil otherwise. "]

  ["Separate NAME at point"  ar-separate-name-atpt
   :help " ‘ar-separate-name-atpt’
   Separate NAME at point if any, nil otherwise. "]

  ["Separate NUMBER at point"  ar-separate-number-atpt
   :help " ‘ar-separate-number-atpt’
   Separate NUMBER at point if any, nil otherwise. "]

  ["Separate PAGE at point"  ar-separate-page-atpt
   :help " ‘ar-separate-page-atpt’
   Separate PAGE at point if any, nil otherwise. "]

  ["Separate PARAGRAPH at point"  ar-separate-paragraph-atpt
   :help " ‘ar-separate-paragraph-atpt’
   Separate PARAGRAPH at point if any, nil otherwise. "]

  ["Separate PAREN at point"  ar-separate-paren-atpt
   :help " ‘ar-separate-paren-atpt’
   Separate PAREN at point if any, nil otherwise. "]

  ["Separate PHONE at point"  ar-separate-phone-atpt
   :help " ‘ar-separate-phone-atpt’
   Separate PHONE at point if any, nil otherwise. "]

  ["Separate REGION at point"  ar-separate-region-atpt
   :help " ‘ar-separate-region-atpt’
   Separate REGION at point if any, nil otherwise. "]

  ["Separate SENTENCE at point"  ar-separate-sentence-atpt
   :help " ‘ar-separate-sentence-atpt’
   Separate SENTENCE at point if any, nil otherwise. "]

  ["Separate SEXP at point"  ar-separate-sexp-atpt
   :help " ‘ar-separate-sexp-atpt’
   Separate SEXP at point if any, nil otherwise. "]

  ["Separate STRING at point"  ar-separate-string-atpt
   :help " ‘ar-separate-string-atpt’
   Separate STRING at point if any, nil otherwise. "]

  ["Separate SH-STRUCT at point"  ar-separate-shstruct-atpt
   :help " ‘ar-separate-shstruct-atpt’
   Separate SH-STRUCT at point if any, nil otherwise. "]

  ["Separate SYMBOL at point"  ar-separate-symbol-atpt
   :help " ‘ar-separate-symbol-atpt’
   Separate SYMBOL at point if any, nil otherwise. "]

  ["Separate URL at point"  ar-separate-url-atpt
   :help " ‘ar-separate-url-atpt’
   Separate URL at point if any, nil otherwise. "]

  ["Separate WORD at point"  ar-separate-word-atpt
   :help " ‘ar-separate-word-atpt’
   Separate WORD at point if any, nil otherwise. "]

  ["Separate WORD-ALPHA-ONLY at point"  ar-separate-wordalphaonly-atpt
   :help " ‘ar-separate-wordalphaonly-atpt’
   Separate WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Show"

 ("Character classes"

  ["Show [:alnum:] char class at point"  ar-show-alnum-atpt
   :help " ‘ar-show-alnum-atpt’
   Show ALNUM at point if any, nil otherwise. "]

  ["Show [:alpha:] char class at point"  ar-show-alpha-atpt
   :help " ‘ar-show-alpha-atpt’
   Show ALPHA at point if any, nil otherwise. "]

  ["Show [:ascii:] char class at point"  ar-show-ascii-atpt
   :help " ‘ar-show-ascii-atpt’
   Show ASCII at point if any, nil otherwise. "]

  ["Show [:blank:] char class at point"  ar-show-blank-atpt
   :help " ‘ar-show-blank-atpt’
   Show BLANK at point if any, nil otherwise. "]

  ["Show [:cntrl:] char class at point"  ar-show-cntrl-atpt
   :help " ‘ar-show-cntrl-atpt’
   Show CNTRL at point if any, nil otherwise. "]

  ["Show [:digit:] char class at point"  ar-show-digit-atpt
   :help " ‘ar-show-digit-atpt’
   Show DIGIT at point if any, nil otherwise. "]

  ["Show [:graph:] char class at point"  ar-show-graph-atpt
   :help " ‘ar-show-graph-atpt’
   Show GRAPH at point if any, nil otherwise. "]

  ["Show [:lower:] char class at point"  ar-show-lower-atpt
   :help " ‘ar-show-lower-atpt’
   Show LOWER at point if any, nil otherwise. "]

  ["Show [:nonascii:] char class at point"  ar-show-nonascii-atpt
   :help " ‘ar-show-nonascii-atpt’
   Show NONASCII at point if any, nil otherwise. "]

  ["Show [:print:] char class at point"  ar-show-print-atpt
   :help " ‘ar-show-print-atpt’
   Show PRINT at point if any, nil otherwise. "]

  ["Show [:punct:] char class at point"  ar-show-punct-atpt
   :help " ‘ar-show-punct-atpt’
   Show PUNCT at point if any, nil otherwise. "]

  ["Show [:space:] char class at point"  ar-show-space-atpt
   :help " ‘ar-show-space-atpt’
   Show SPACE at point if any, nil otherwise. "]

  ["Show [:upper:] char class at point"  ar-show-upper-atpt
   :help " ‘ar-show-upper-atpt’
   Show UPPER at point if any, nil otherwise. "]

  ["Show [:xdigit:] char class at point"  ar-show-xdigit-atpt
   :help " ‘ar-show-xdigit-atpt’
   Show XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Show BRACED at point"  ar-show-braced-atpt
   :help " ‘ar-show-braced-atpt’
   Show BRACED at point if any, nil otherwise. "]

  ["Show BRACKETED at point"  ar-show-bracketed-atpt
   :help " ‘ar-show-bracketed-atpt’
   Show BRACKETED at point if any, nil otherwise. "]

  ["Show LESSER-ANGLED at point"  ar-show-lesserangled-atpt
   :help " ‘ar-show-lesserangled-atpt’
   Show LESSER-ANGLED at point if any, nil otherwise. "]

  ["Show GREATER-ANGLED at point"  ar-show-greaterangled-atpt
   :help " ‘ar-show-greaterangled-atpt’
   Show GREATER-ANGLED at point if any, nil otherwise. "]

  ["Show LEFT-RIGHT-SINGLEQUOTED at point"  ar-show-curvedsinglequoted-atpt
   :help " ‘ar-show-curvedsinglequoted-atpt’
   Show LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Show PARENTIZED at point"  ar-show-parentized-atpt
   :help " ‘ar-show-parentized-atpt’
   Show PARENTIZED at point if any, nil otherwise. "]

  ["Show BACKSLASHED at point"  ar-show-backslashed-atpt
   :help " ‘ar-show-backslashed-atpt’
   Show BACKSLASHED at point if any, nil otherwise. "]

  ["Show DOLLARED at point"  ar-show-dollared-atpt
   :help " ‘ar-show-dollared-atpt’
   Show DOLLARED at point if any, nil otherwise. "]

  ["Show DOUBLEQUOTED at point"  ar-show-doublequoted-atpt
   :help " ‘ar-show-doublequoted-atpt’
   Show DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Show EQUALIZED at point"  ar-show-equalized-atpt
   :help " ‘ar-show-equalized-atpt’
   Show EQUALIZED at point if any, nil otherwise. "]

  ["Show HYPHENED at point"  ar-show-hyphened-atpt
   :help " ‘ar-show-hyphened-atpt’
   Show HYPHENED at point if any, nil otherwise. "]

  ["Show QUOTED at point"  ar-show-quoted-atpt
   :help " ‘ar-show-quoted-atpt’
   Show QUOTED at point if any, nil otherwise. "]

  ["Show SINGLEQUOTED at point"  ar-show-singlequoted-atpt
   :help " ‘ar-show-singlequoted-atpt’
   Show SINGLEQUOTED at point if any, nil otherwise. "]

  ["Show SLASHED at point"  ar-show-slashed-atpt
   :help " ‘ar-show-slashed-atpt’
   Show SLASHED at point if any, nil otherwise. "]

  ["Show UNDERSCORED at point"  ar-show-underscored-atpt
   :help " ‘ar-show-underscored-atpt’
   Show UNDERSCORED at point if any, nil otherwise. "]

  ["Show WHITESPACED at point"  ar-show-whitespaced-atpt
   :help " ‘ar-show-whitespaced-atpt’
   Show WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Show ANGLED-NO-NEST at point"  ar-showanglednonest-atpt
   :help " ‘ar-showanglednonest-atpt’
   Show ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Show GREATER-ANGLED-NESTED at point"  ar-show-greateranglednested-atpt
   :help " ‘ar-show-greateranglednested-atpt’
   Show GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Show LESSER-ANGLED-NESTED at point"  ar-show-lesseranglednested-atpt
   :help " ‘ar-show-lesseranglednested-atpt’
   Show LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Show BUFFER at point"  ar-show-buffer-atpt
   :help " ‘ar-show-buffer-atpt’
   Show BUFFER at point if any, nil otherwise. "]

  ["Show COMMENT at point"  ar-show-comment-atpt
   :help " ‘ar-show-comment-atpt’
   Show COMMENT at point if any, nil otherwise. "]

  ["Show CSV at point"  ar-show-csv-atpt
   :help " ‘ar-show-csv-atpt’
   Show CSV at point if any, nil otherwise. "]

  ["Show DATE at point"  ar-show-date-atpt
   :help " ‘ar-show-date-atpt’
   Show DATE at point if any, nil otherwise. "]

  ["Show DEFUN at point"  ar-show-defun-atpt
   :help " ‘ar-show-defun-atpt’
   Show DEFUN at point if any, nil otherwise. "]

  ["Show DELIMITED at point"  ar-show-delimited-atpt
   :help " ‘ar-show-delimited-atpt’
   Show DELIMITED at point if any, nil otherwise. "]

  ["Show EMAIL at point"  ar-show-email-atpt
   :help " ‘ar-show-email-atpt’
   Show EMAIL at point if any, nil otherwise. "]

  ["Show FILENAME at point"  ar-show-filename-atpt
   :help " ‘ar-show-filename-atpt’
   Show FILENAME at point if any, nil otherwise. "]

  ["Show FLOAT at point"  ar-show-float-atpt
   :help " ‘ar-show-float-atpt’
   Show FLOAT at point if any, nil otherwise. "]

  ["Show FUNCTION at point"  ar-show-function-atpt
   :help " ‘ar-show-function-atpt’
   Show FUNCTION at point if any, nil otherwise. "]

  ["Show IP at point"  ar-show-ip-atpt
   :help " ‘ar-show-ip-atpt’
   Show IP at point if any, nil otherwise. "]

  ["Show ISBN at point"  ar-show-isbn-atpt
   :help " ‘ar-show-isbn-atpt’
   Show ISBN at point if any, nil otherwise. "]

  ["Show LINE at point"  ar-show-line-atpt
   :help " ‘ar-show-line-atpt’
   Show LINE at point if any, nil otherwise. "]

  ["Show NAME at point"  ar-show-name-atpt
   :help " ‘ar-show-name-atpt’
   Show NAME at point if any, nil otherwise. "]

  ["Show NUMBER at point"  ar-show-number-atpt
   :help " ‘ar-show-number-atpt’
   Show NUMBER at point if any, nil otherwise. "]

  ["Show PAGE at point"  ar-show-page-atpt
   :help " ‘ar-show-page-atpt’
   Show PAGE at point if any, nil otherwise. "]

  ["Show PARAGRAPH at point"  ar-show-paragraph-atpt
   :help " ‘ar-show-paragraph-atpt’
   Show PARAGRAPH at point if any, nil otherwise. "]

  ["Show PAREN at point"  ar-show-paren-atpt
   :help " ‘ar-show-paren-atpt’
   Show PAREN at point if any, nil otherwise. "]

  ["Show PHONE at point"  ar-show-phone-atpt
   :help " ‘ar-show-phone-atpt’
   Show PHONE at point if any, nil otherwise. "]

  ["Show REGION at point"  ar-show-region-atpt
   :help " ‘ar-show-region-atpt’
   Show REGION at point if any, nil otherwise. "]

  ["Show SENTENCE at point"  ar-show-sentence-atpt
   :help " ‘ar-show-sentence-atpt’
   Show SENTENCE at point if any, nil otherwise. "]

  ["Show SEXP at point"  ar-show-sexp-atpt
   :help " ‘ar-show-sexp-atpt’
   Show SEXP at point if any, nil otherwise. "]

  ["Show STRING at point"  ar-show-string-atpt
   :help " ‘ar-show-string-atpt’
   Show STRING at point if any, nil otherwise. "]

  ["Show SH-STRUCT at point"  ar-show-shstruct-atpt
   :help " ‘ar-show-shstruct-atpt’
   Show SH-STRUCT at point if any, nil otherwise. "]

  ["Show SYMBOL at point"  ar-show-symbol-atpt
   :help " ‘ar-show-symbol-atpt’
   Show SYMBOL at point if any, nil otherwise. "]

  ["Show URL at point"  ar-show-url-atpt
   :help " ‘ar-show-url-atpt’
   Show URL at point if any, nil otherwise. "]

  ["Show WORD at point"  ar-show-word-atpt
   :help " ‘ar-show-word-atpt’
   Show WORD at point if any, nil otherwise. "]

  ["Show WORD-ALPHA-ONLY at point"  ar-show-wordalphaonly-atpt
   :help " ‘ar-show-wordalphaonly-atpt’
   Show WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Triplequote-Dq"

 ("Character classes"

  ["Triplequote-Dq [:alnum:] char class at point"  ar-triplequotedq-alnum-atpt
   :help " ‘ar-triplequotedq-alnum-atpt’
   Triplequote-Dq ALNUM at point if any, nil otherwise. "]

  ["Triplequote-Dq [:alpha:] char class at point"  ar-triplequotedq-alpha-atpt
   :help " ‘ar-triplequotedq-alpha-atpt’
   Triplequote-Dq ALPHA at point if any, nil otherwise. "]

  ["Triplequote-Dq [:ascii:] char class at point"  ar-triplequotedq-ascii-atpt
   :help " ‘ar-triplequotedq-ascii-atpt’
   Triplequote-Dq ASCII at point if any, nil otherwise. "]

  ["Triplequote-Dq [:blank:] char class at point"  ar-triplequotedq-blank-atpt
   :help " ‘ar-triplequotedq-blank-atpt’
   Triplequote-Dq BLANK at point if any, nil otherwise. "]

  ["Triplequote-Dq [:cntrl:] char class at point"  ar-triplequotedq-cntrl-atpt
   :help " ‘ar-triplequotedq-cntrl-atpt’
   Triplequote-Dq CNTRL at point if any, nil otherwise. "]

  ["Triplequote-Dq [:digit:] char class at point"  ar-triplequotedq-digit-atpt
   :help " ‘ar-triplequotedq-digit-atpt’
   Triplequote-Dq DIGIT at point if any, nil otherwise. "]

  ["Triplequote-Dq [:graph:] char class at point"  ar-triplequotedq-graph-atpt
   :help " ‘ar-triplequotedq-graph-atpt’
   Triplequote-Dq GRAPH at point if any, nil otherwise. "]

  ["Triplequote-Dq [:lower:] char class at point"  ar-triplequotedq-lower-atpt
   :help " ‘ar-triplequotedq-lower-atpt’
   Triplequote-Dq LOWER at point if any, nil otherwise. "]

  ["Triplequote-Dq [:nonascii:] char class at point"  ar-triplequotedq-nonascii-atpt
   :help " ‘ar-triplequotedq-nonascii-atpt’
   Triplequote-Dq NONASCII at point if any, nil otherwise. "]

  ["Triplequote-Dq [:print:] char class at point"  ar-triplequotedq-print-atpt
   :help " ‘ar-triplequotedq-print-atpt’
   Triplequote-Dq PRINT at point if any, nil otherwise. "]

  ["Triplequote-Dq [:punct:] char class at point"  ar-triplequotedq-punct-atpt
   :help " ‘ar-triplequotedq-punct-atpt’
   Triplequote-Dq PUNCT at point if any, nil otherwise. "]

  ["Triplequote-Dq [:space:] char class at point"  ar-triplequotedq-space-atpt
   :help " ‘ar-triplequotedq-space-atpt’
   Triplequote-Dq SPACE at point if any, nil otherwise. "]

  ["Triplequote-Dq [:upper:] char class at point"  ar-triplequotedq-upper-atpt
   :help " ‘ar-triplequotedq-upper-atpt’
   Triplequote-Dq UPPER at point if any, nil otherwise. "]

  ["Triplequote-Dq [:xdigit:] char class at point"  ar-triplequotedq-xdigit-atpt
   :help " ‘ar-triplequotedq-xdigit-atpt’
   Triplequote-Dq XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Triplequote-Dq BRACED at point"  ar-triplequotedq-braced-atpt
   :help " ‘ar-triplequotedq-braced-atpt’
   Triplequote-Dq BRACED at point if any, nil otherwise. "]

  ["Triplequote-Dq BRACKETED at point"  ar-triplequotedq-bracketed-atpt
   :help " ‘ar-triplequotedq-bracketed-atpt’
   Triplequote-Dq BRACKETED at point if any, nil otherwise. "]

  ["Triplequote-Dq LESSER-ANGLED at point"  ar-triplequotedq-lesserangled-atpt
   :help " ‘ar-triplequotedq-lesserangled-atpt’
   Triplequote-Dq LESSER-ANGLED at point if any, nil otherwise. "]

  ["Triplequote-Dq GREATER-ANGLED at point"  ar-triplequotedq-greaterangled-atpt
   :help " ‘ar-triplequotedq-greaterangled-atpt’
   Triplequote-Dq GREATER-ANGLED at point if any, nil otherwise. "]

  ["Triplequote-Dq LEFT-RIGHT-SINGLEQUOTED at point"  ar-triplequotedq-curvedsinglequoted-atpt
   :help " ‘ar-triplequotedq-curvedsinglequoted-atpt’
   Triplequote-Dq LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Dq PARENTIZED at point"  ar-triplequotedq-parentized-atpt
   :help " ‘ar-triplequotedq-parentized-atpt’
   Triplequote-Dq PARENTIZED at point if any, nil otherwise. "]

  ["Triplequote-Dq BACKSLASHED at point"  ar-triplequotedq-backslashed-atpt
   :help " ‘ar-triplequotedq-backslashed-atpt’
   Triplequote-Dq BACKSLASHED at point if any, nil otherwise. "]

  ["Triplequote-Dq DOLLARED at point"  ar-triplequotedq-dollared-atpt
   :help " ‘ar-triplequotedq-dollared-atpt’
   Triplequote-Dq DOLLARED at point if any, nil otherwise. "]

  ["Triplequote-Dq DOUBLEQUOTED at point"  ar-triplequotedq-doublequoted-atpt
   :help " ‘ar-triplequotedq-doublequoted-atpt’
   Triplequote-Dq DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Dq EQUALIZED at point"  ar-triplequotedq-equalized-atpt
   :help " ‘ar-triplequotedq-equalized-atpt’
   Triplequote-Dq EQUALIZED at point if any, nil otherwise. "]

  ["Triplequote-Dq HYPHENED at point"  ar-triplequotedq-hyphened-atpt
   :help " ‘ar-triplequotedq-hyphened-atpt’
   Triplequote-Dq HYPHENED at point if any, nil otherwise. "]

  ["Triplequote-Dq QUOTED at point"  ar-triplequotedq-quoted-atpt
   :help " ‘ar-triplequotedq-quoted-atpt’
   Triplequote-Dq QUOTED at point if any, nil otherwise. "]

  ["Triplequote-Dq SINGLEQUOTED at point"  ar-triplequotedq-singlequoted-atpt
   :help " ‘ar-triplequotedq-singlequoted-atpt’
   Triplequote-Dq SINGLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Dq SLASHED at point"  ar-triplequotedq-slashed-atpt
   :help " ‘ar-triplequotedq-slashed-atpt’
   Triplequote-Dq SLASHED at point if any, nil otherwise. "]

  ["Triplequote-Dq UNDERSCORED at point"  ar-triplequotedq-underscored-atpt
   :help " ‘ar-triplequotedq-underscored-atpt’
   Triplequote-Dq UNDERSCORED at point if any, nil otherwise. "]

  ["Triplequote-Dq WHITESPACED at point"  ar-triplequotedq-whitespaced-atpt
   :help " ‘ar-triplequotedq-whitespaced-atpt’
   Triplequote-Dq WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Triplequote-Dq ANGLED-NO-NEST at point"  ar-triplequotedqanglednonest-atpt
   :help " ‘ar-triplequotedqanglednonest-atpt’
   Triplequote-Dq ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Triplequote-Dq GREATER-ANGLED-NESTED at point"  ar-triplequotedq-greateranglednested-atpt
   :help " ‘ar-triplequotedq-greateranglednested-atpt’
   Triplequote-Dq GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Triplequote-Dq LESSER-ANGLED-NESTED at point"  ar-triplequotedq-lesseranglednested-atpt
   :help " ‘ar-triplequotedq-lesseranglednested-atpt’
   Triplequote-Dq LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Triplequote-Dq BUFFER at point"  ar-triplequotedq-buffer-atpt
   :help " ‘ar-triplequotedq-buffer-atpt’
   Triplequote-Dq BUFFER at point if any, nil otherwise. "]

  ["Triplequote-Dq COMMENT at point"  ar-triplequotedq-comment-atpt
   :help " ‘ar-triplequotedq-comment-atpt’
   Triplequote-Dq COMMENT at point if any, nil otherwise. "]

  ["Triplequote-Dq CSV at point"  ar-triplequotedq-csv-atpt
   :help " ‘ar-triplequotedq-csv-atpt’
   Triplequote-Dq CSV at point if any, nil otherwise. "]

  ["Triplequote-Dq DATE at point"  ar-triplequotedq-date-atpt
   :help " ‘ar-triplequotedq-date-atpt’
   Triplequote-Dq DATE at point if any, nil otherwise. "]

  ["Triplequote-Dq DEFUN at point"  ar-triplequotedq-defun-atpt
   :help " ‘ar-triplequotedq-defun-atpt’
   Triplequote-Dq DEFUN at point if any, nil otherwise. "]

  ["Triplequote-Dq DELIMITED at point"  ar-triplequotedq-delimited-atpt
   :help " ‘ar-triplequotedq-delimited-atpt’
   Triplequote-Dq DELIMITED at point if any, nil otherwise. "]

  ["Triplequote-Dq EMAIL at point"  ar-triplequotedq-email-atpt
   :help " ‘ar-triplequotedq-email-atpt’
   Triplequote-Dq EMAIL at point if any, nil otherwise. "]

  ["Triplequote-Dq FILENAME at point"  ar-triplequotedq-filename-atpt
   :help " ‘ar-triplequotedq-filename-atpt’
   Triplequote-Dq FILENAME at point if any, nil otherwise. "]

  ["Triplequote-Dq FLOAT at point"  ar-triplequotedq-float-atpt
   :help " ‘ar-triplequotedq-float-atpt’
   Triplequote-Dq FLOAT at point if any, nil otherwise. "]

  ["Triplequote-Dq FUNCTION at point"  ar-triplequotedq-function-atpt
   :help " ‘ar-triplequotedq-function-atpt’
   Triplequote-Dq FUNCTION at point if any, nil otherwise. "]

  ["Triplequote-Dq IP at point"  ar-triplequotedq-ip-atpt
   :help " ‘ar-triplequotedq-ip-atpt’
   Triplequote-Dq IP at point if any, nil otherwise. "]

  ["Triplequote-Dq ISBN at point"  ar-triplequotedq-isbn-atpt
   :help " ‘ar-triplequotedq-isbn-atpt’
   Triplequote-Dq ISBN at point if any, nil otherwise. "]

  ["Triplequote-Dq LINE at point"  ar-triplequotedq-line-atpt
   :help " ‘ar-triplequotedq-line-atpt’
   Triplequote-Dq LINE at point if any, nil otherwise. "]

  ["Triplequote-Dq NAME at point"  ar-triplequotedq-name-atpt
   :help " ‘ar-triplequotedq-name-atpt’
   Triplequote-Dq NAME at point if any, nil otherwise. "]

  ["Triplequote-Dq NUMBER at point"  ar-triplequotedq-number-atpt
   :help " ‘ar-triplequotedq-number-atpt’
   Triplequote-Dq NUMBER at point if any, nil otherwise. "]

  ["Triplequote-Dq PAGE at point"  ar-triplequotedq-page-atpt
   :help " ‘ar-triplequotedq-page-atpt’
   Triplequote-Dq PAGE at point if any, nil otherwise. "]

  ["Triplequote-Dq PARAGRAPH at point"  ar-triplequotedq-paragraph-atpt
   :help " ‘ar-triplequotedq-paragraph-atpt’
   Triplequote-Dq PARAGRAPH at point if any, nil otherwise. "]

  ["Triplequote-Dq PAREN at point"  ar-triplequotedq-paren-atpt
   :help " ‘ar-triplequotedq-paren-atpt’
   Triplequote-Dq PAREN at point if any, nil otherwise. "]

  ["Triplequote-Dq PHONE at point"  ar-triplequotedq-phone-atpt
   :help " ‘ar-triplequotedq-phone-atpt’
   Triplequote-Dq PHONE at point if any, nil otherwise. "]

  ["Triplequote-Dq REGION at point"  ar-triplequotedq-region-atpt
   :help " ‘ar-triplequotedq-region-atpt’
   Triplequote-Dq REGION at point if any, nil otherwise. "]

  ["Triplequote-Dq SENTENCE at point"  ar-triplequotedq-sentence-atpt
   :help " ‘ar-triplequotedq-sentence-atpt’
   Triplequote-Dq SENTENCE at point if any, nil otherwise. "]

  ["Triplequote-Dq SEXP at point"  ar-triplequotedq-sexp-atpt
   :help " ‘ar-triplequotedq-sexp-atpt’
   Triplequote-Dq SEXP at point if any, nil otherwise. "]

  ["Triplequote-Dq STRING at point"  ar-triplequotedq-string-atpt
   :help " ‘ar-triplequotedq-string-atpt’
   Triplequote-Dq STRING at point if any, nil otherwise. "]

  ["Triplequote-Dq SH-STRUCT at point"  ar-triplequotedq-shstruct-atpt
   :help " ‘ar-triplequotedq-shstruct-atpt’
   Triplequote-Dq SH-STRUCT at point if any, nil otherwise. "]

  ["Triplequote-Dq SYMBOL at point"  ar-triplequotedq-symbol-atpt
   :help " ‘ar-triplequotedq-symbol-atpt’
   Triplequote-Dq SYMBOL at point if any, nil otherwise. "]

  ["Triplequote-Dq URL at point"  ar-triplequotedq-url-atpt
   :help " ‘ar-triplequotedq-url-atpt’
   Triplequote-Dq URL at point if any, nil otherwise. "]

  ["Triplequote-Dq WORD at point"  ar-triplequotedq-word-atpt
   :help " ‘ar-triplequotedq-word-atpt’
   Triplequote-Dq WORD at point if any, nil otherwise. "]

  ["Triplequote-Dq WORD-ALPHA-ONLY at point"  ar-triplequotedq-wordalphaonly-atpt
   :help " ‘ar-triplequotedq-wordalphaonly-atpt’
   Triplequote-Dq WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Triplequote-Sq"

 ("Character classes"

  ["Triplequote-Sq [:alnum:] char class at point"  ar-triplequotesq-alnum-atpt
   :help " ‘ar-triplequotesq-alnum-atpt’
   Triplequote-Sq ALNUM at point if any, nil otherwise. "]

  ["Triplequote-Sq [:alpha:] char class at point"  ar-triplequotesq-alpha-atpt
   :help " ‘ar-triplequotesq-alpha-atpt’
   Triplequote-Sq ALPHA at point if any, nil otherwise. "]

  ["Triplequote-Sq [:ascii:] char class at point"  ar-triplequotesq-ascii-atpt
   :help " ‘ar-triplequotesq-ascii-atpt’
   Triplequote-Sq ASCII at point if any, nil otherwise. "]

  ["Triplequote-Sq [:blank:] char class at point"  ar-triplequotesq-blank-atpt
   :help " ‘ar-triplequotesq-blank-atpt’
   Triplequote-Sq BLANK at point if any, nil otherwise. "]

  ["Triplequote-Sq [:cntrl:] char class at point"  ar-triplequotesq-cntrl-atpt
   :help " ‘ar-triplequotesq-cntrl-atpt’
   Triplequote-Sq CNTRL at point if any, nil otherwise. "]

  ["Triplequote-Sq [:digit:] char class at point"  ar-triplequotesq-digit-atpt
   :help " ‘ar-triplequotesq-digit-atpt’
   Triplequote-Sq DIGIT at point if any, nil otherwise. "]

  ["Triplequote-Sq [:graph:] char class at point"  ar-triplequotesq-graph-atpt
   :help " ‘ar-triplequotesq-graph-atpt’
   Triplequote-Sq GRAPH at point if any, nil otherwise. "]

  ["Triplequote-Sq [:lower:] char class at point"  ar-triplequotesq-lower-atpt
   :help " ‘ar-triplequotesq-lower-atpt’
   Triplequote-Sq LOWER at point if any, nil otherwise. "]

  ["Triplequote-Sq [:nonascii:] char class at point"  ar-triplequotesq-nonascii-atpt
   :help " ‘ar-triplequotesq-nonascii-atpt’
   Triplequote-Sq NONASCII at point if any, nil otherwise. "]

  ["Triplequote-Sq [:print:] char class at point"  ar-triplequotesq-print-atpt
   :help " ‘ar-triplequotesq-print-atpt’
   Triplequote-Sq PRINT at point if any, nil otherwise. "]

  ["Triplequote-Sq [:punct:] char class at point"  ar-triplequotesq-punct-atpt
   :help " ‘ar-triplequotesq-punct-atpt’
   Triplequote-Sq PUNCT at point if any, nil otherwise. "]

  ["Triplequote-Sq [:space:] char class at point"  ar-triplequotesq-space-atpt
   :help " ‘ar-triplequotesq-space-atpt’
   Triplequote-Sq SPACE at point if any, nil otherwise. "]

  ["Triplequote-Sq [:upper:] char class at point"  ar-triplequotesq-upper-atpt
   :help " ‘ar-triplequotesq-upper-atpt’
   Triplequote-Sq UPPER at point if any, nil otherwise. "]

  ["Triplequote-Sq [:xdigit:] char class at point"  ar-triplequotesq-xdigit-atpt
   :help " ‘ar-triplequotesq-xdigit-atpt’
   Triplequote-Sq XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Triplequote-Sq BRACED at point"  ar-triplequotesq-braced-atpt
   :help " ‘ar-triplequotesq-braced-atpt’
   Triplequote-Sq BRACED at point if any, nil otherwise. "]

  ["Triplequote-Sq BRACKETED at point"  ar-triplequotesq-bracketed-atpt
   :help " ‘ar-triplequotesq-bracketed-atpt’
   Triplequote-Sq BRACKETED at point if any, nil otherwise. "]

  ["Triplequote-Sq LESSER-ANGLED at point"  ar-triplequotesq-lesserangled-atpt
   :help " ‘ar-triplequotesq-lesserangled-atpt’
   Triplequote-Sq LESSER-ANGLED at point if any, nil otherwise. "]

  ["Triplequote-Sq GREATER-ANGLED at point"  ar-triplequotesq-greaterangled-atpt
   :help " ‘ar-triplequotesq-greaterangled-atpt’
   Triplequote-Sq GREATER-ANGLED at point if any, nil otherwise. "]

  ["Triplequote-Sq LEFT-RIGHT-SINGLEQUOTED at point"  ar-triplequotesq-curvedsinglequoted-atpt
   :help " ‘ar-triplequotesq-curvedsinglequoted-atpt’
   Triplequote-Sq LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Sq PARENTIZED at point"  ar-triplequotesq-parentized-atpt
   :help " ‘ar-triplequotesq-parentized-atpt’
   Triplequote-Sq PARENTIZED at point if any, nil otherwise. "]

  ["Triplequote-Sq BACKSLASHED at point"  ar-triplequotesq-backslashed-atpt
   :help " ‘ar-triplequotesq-backslashed-atpt’
   Triplequote-Sq BACKSLASHED at point if any, nil otherwise. "]

  ["Triplequote-Sq DOLLARED at point"  ar-triplequotesq-dollared-atpt
   :help " ‘ar-triplequotesq-dollared-atpt’
   Triplequote-Sq DOLLARED at point if any, nil otherwise. "]

  ["Triplequote-Sq DOUBLEQUOTED at point"  ar-triplequotesq-doublequoted-atpt
   :help " ‘ar-triplequotesq-doublequoted-atpt’
   Triplequote-Sq DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Sq EQUALIZED at point"  ar-triplequotesq-equalized-atpt
   :help " ‘ar-triplequotesq-equalized-atpt’
   Triplequote-Sq EQUALIZED at point if any, nil otherwise. "]

  ["Triplequote-Sq HYPHENED at point"  ar-triplequotesq-hyphened-atpt
   :help " ‘ar-triplequotesq-hyphened-atpt’
   Triplequote-Sq HYPHENED at point if any, nil otherwise. "]

  ["Triplequote-Sq QUOTED at point"  ar-triplequotesq-quoted-atpt
   :help " ‘ar-triplequotesq-quoted-atpt’
   Triplequote-Sq QUOTED at point if any, nil otherwise. "]

  ["Triplequote-Sq SINGLEQUOTED at point"  ar-triplequotesq-singlequoted-atpt
   :help " ‘ar-triplequotesq-singlequoted-atpt’
   Triplequote-Sq SINGLEQUOTED at point if any, nil otherwise. "]

  ["Triplequote-Sq SLASHED at point"  ar-triplequotesq-slashed-atpt
   :help " ‘ar-triplequotesq-slashed-atpt’
   Triplequote-Sq SLASHED at point if any, nil otherwise. "]

  ["Triplequote-Sq UNDERSCORED at point"  ar-triplequotesq-underscored-atpt
   :help " ‘ar-triplequotesq-underscored-atpt’
   Triplequote-Sq UNDERSCORED at point if any, nil otherwise. "]

  ["Triplequote-Sq WHITESPACED at point"  ar-triplequotesq-whitespaced-atpt
   :help " ‘ar-triplequotesq-whitespaced-atpt’
   Triplequote-Sq WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Triplequote-Sq ANGLED-NO-NEST at point"  ar-triplequotesqanglednonest-atpt
   :help " ‘ar-triplequotesqanglednonest-atpt’
   Triplequote-Sq ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Triplequote-Sq GREATER-ANGLED-NESTED at point"  ar-triplequotesq-greateranglednested-atpt
   :help " ‘ar-triplequotesq-greateranglednested-atpt’
   Triplequote-Sq GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Triplequote-Sq LESSER-ANGLED-NESTED at point"  ar-triplequotesq-lesseranglednested-atpt
   :help " ‘ar-triplequotesq-lesseranglednested-atpt’
   Triplequote-Sq LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Triplequote-Sq BUFFER at point"  ar-triplequotesq-buffer-atpt
   :help " ‘ar-triplequotesq-buffer-atpt’
   Triplequote-Sq BUFFER at point if any, nil otherwise. "]

  ["Triplequote-Sq COMMENT at point"  ar-triplequotesq-comment-atpt
   :help " ‘ar-triplequotesq-comment-atpt’
   Triplequote-Sq COMMENT at point if any, nil otherwise. "]

  ["Triplequote-Sq CSV at point"  ar-triplequotesq-csv-atpt
   :help " ‘ar-triplequotesq-csv-atpt’
   Triplequote-Sq CSV at point if any, nil otherwise. "]

  ["Triplequote-Sq DATE at point"  ar-triplequotesq-date-atpt
   :help " ‘ar-triplequotesq-date-atpt’
   Triplequote-Sq DATE at point if any, nil otherwise. "]

  ["Triplequote-Sq DEFUN at point"  ar-triplequotesq-defun-atpt
   :help " ‘ar-triplequotesq-defun-atpt’
   Triplequote-Sq DEFUN at point if any, nil otherwise. "]

  ["Triplequote-Sq DELIMITED at point"  ar-triplequotesq-delimited-atpt
   :help " ‘ar-triplequotesq-delimited-atpt’
   Triplequote-Sq DELIMITED at point if any, nil otherwise. "]

  ["Triplequote-Sq EMAIL at point"  ar-triplequotesq-email-atpt
   :help " ‘ar-triplequotesq-email-atpt’
   Triplequote-Sq EMAIL at point if any, nil otherwise. "]

  ["Triplequote-Sq FILENAME at point"  ar-triplequotesq-filename-atpt
   :help " ‘ar-triplequotesq-filename-atpt’
   Triplequote-Sq FILENAME at point if any, nil otherwise. "]

  ["Triplequote-Sq FLOAT at point"  ar-triplequotesq-float-atpt
   :help " ‘ar-triplequotesq-float-atpt’
   Triplequote-Sq FLOAT at point if any, nil otherwise. "]

  ["Triplequote-Sq FUNCTION at point"  ar-triplequotesq-function-atpt
   :help " ‘ar-triplequotesq-function-atpt’
   Triplequote-Sq FUNCTION at point if any, nil otherwise. "]

  ["Triplequote-Sq IP at point"  ar-triplequotesq-ip-atpt
   :help " ‘ar-triplequotesq-ip-atpt’
   Triplequote-Sq IP at point if any, nil otherwise. "]

  ["Triplequote-Sq ISBN at point"  ar-triplequotesq-isbn-atpt
   :help " ‘ar-triplequotesq-isbn-atpt’
   Triplequote-Sq ISBN at point if any, nil otherwise. "]

  ["Triplequote-Sq LINE at point"  ar-triplequotesq-line-atpt
   :help " ‘ar-triplequotesq-line-atpt’
   Triplequote-Sq LINE at point if any, nil otherwise. "]

  ["Triplequote-Sq NAME at point"  ar-triplequotesq-name-atpt
   :help " ‘ar-triplequotesq-name-atpt’
   Triplequote-Sq NAME at point if any, nil otherwise. "]

  ["Triplequote-Sq NUMBER at point"  ar-triplequotesq-number-atpt
   :help " ‘ar-triplequotesq-number-atpt’
   Triplequote-Sq NUMBER at point if any, nil otherwise. "]

  ["Triplequote-Sq PAGE at point"  ar-triplequotesq-page-atpt
   :help " ‘ar-triplequotesq-page-atpt’
   Triplequote-Sq PAGE at point if any, nil otherwise. "]

  ["Triplequote-Sq PARAGRAPH at point"  ar-triplequotesq-paragraph-atpt
   :help " ‘ar-triplequotesq-paragraph-atpt’
   Triplequote-Sq PARAGRAPH at point if any, nil otherwise. "]

  ["Triplequote-Sq PAREN at point"  ar-triplequotesq-paren-atpt
   :help " ‘ar-triplequotesq-paren-atpt’
   Triplequote-Sq PAREN at point if any, nil otherwise. "]

  ["Triplequote-Sq PHONE at point"  ar-triplequotesq-phone-atpt
   :help " ‘ar-triplequotesq-phone-atpt’
   Triplequote-Sq PHONE at point if any, nil otherwise. "]

  ["Triplequote-Sq REGION at point"  ar-triplequotesq-region-atpt
   :help " ‘ar-triplequotesq-region-atpt’
   Triplequote-Sq REGION at point if any, nil otherwise. "]

  ["Triplequote-Sq SENTENCE at point"  ar-triplequotesq-sentence-atpt
   :help " ‘ar-triplequotesq-sentence-atpt’
   Triplequote-Sq SENTENCE at point if any, nil otherwise. "]

  ["Triplequote-Sq SEXP at point"  ar-triplequotesq-sexp-atpt
   :help " ‘ar-triplequotesq-sexp-atpt’
   Triplequote-Sq SEXP at point if any, nil otherwise. "]

  ["Triplequote-Sq STRING at point"  ar-triplequotesq-string-atpt
   :help " ‘ar-triplequotesq-string-atpt’
   Triplequote-Sq STRING at point if any, nil otherwise. "]

  ["Triplequote-Sq SH-STRUCT at point"  ar-triplequotesq-shstruct-atpt
   :help " ‘ar-triplequotesq-shstruct-atpt’
   Triplequote-Sq SH-STRUCT at point if any, nil otherwise. "]

  ["Triplequote-Sq SYMBOL at point"  ar-triplequotesq-symbol-atpt
   :help " ‘ar-triplequotesq-symbol-atpt’
   Triplequote-Sq SYMBOL at point if any, nil otherwise. "]

  ["Triplequote-Sq URL at point"  ar-triplequotesq-url-atpt
   :help " ‘ar-triplequotesq-url-atpt’
   Triplequote-Sq URL at point if any, nil otherwise. "]

  ["Triplequote-Sq WORD at point"  ar-triplequotesq-word-atpt
   :help " ‘ar-triplequotesq-word-atpt’
   Triplequote-Sq WORD at point if any, nil otherwise. "]

  ["Triplequote-Sq WORD-ALPHA-ONLY at point"  ar-triplequotesq-wordalphaonly-atpt
   :help " ‘ar-triplequotesq-wordalphaonly-atpt’
   Triplequote-Sq WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Underscore"

 ("Character classes"

  ["Underscore [:alnum:] char class at point"  ar-underscore-alnum-atpt
   :help " ‘ar-underscore-alnum-atpt’
   Underscore ALNUM at point if any, nil otherwise. "]

  ["Underscore [:alpha:] char class at point"  ar-underscore-alpha-atpt
   :help " ‘ar-underscore-alpha-atpt’
   Underscore ALPHA at point if any, nil otherwise. "]

  ["Underscore [:ascii:] char class at point"  ar-underscore-ascii-atpt
   :help " ‘ar-underscore-ascii-atpt’
   Underscore ASCII at point if any, nil otherwise. "]

  ["Underscore [:blank:] char class at point"  ar-underscore-blank-atpt
   :help " ‘ar-underscore-blank-atpt’
   Underscore BLANK at point if any, nil otherwise. "]

  ["Underscore [:cntrl:] char class at point"  ar-underscore-cntrl-atpt
   :help " ‘ar-underscore-cntrl-atpt’
   Underscore CNTRL at point if any, nil otherwise. "]

  ["Underscore [:digit:] char class at point"  ar-underscore-digit-atpt
   :help " ‘ar-underscore-digit-atpt’
   Underscore DIGIT at point if any, nil otherwise. "]

  ["Underscore [:graph:] char class at point"  ar-underscore-graph-atpt
   :help " ‘ar-underscore-graph-atpt’
   Underscore GRAPH at point if any, nil otherwise. "]

  ["Underscore [:lower:] char class at point"  ar-underscore-lower-atpt
   :help " ‘ar-underscore-lower-atpt’
   Underscore LOWER at point if any, nil otherwise. "]

  ["Underscore [:nonascii:] char class at point"  ar-underscore-nonascii-atpt
   :help " ‘ar-underscore-nonascii-atpt’
   Underscore NONASCII at point if any, nil otherwise. "]

  ["Underscore [:print:] char class at point"  ar-underscore-print-atpt
   :help " ‘ar-underscore-print-atpt’
   Underscore PRINT at point if any, nil otherwise. "]

  ["Underscore [:punct:] char class at point"  ar-underscore-punct-atpt
   :help " ‘ar-underscore-punct-atpt’
   Underscore PUNCT at point if any, nil otherwise. "]

  ["Underscore [:space:] char class at point"  ar-underscore-space-atpt
   :help " ‘ar-underscore-space-atpt’
   Underscore SPACE at point if any, nil otherwise. "]

  ["Underscore [:upper:] char class at point"  ar-underscore-upper-atpt
   :help " ‘ar-underscore-upper-atpt’
   Underscore UPPER at point if any, nil otherwise. "]

  ["Underscore [:xdigit:] char class at point"  ar-underscore-xdigit-atpt
   :help " ‘ar-underscore-xdigit-atpt’
   Underscore XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Underscore BRACED at point"  ar-underscore-braced-atpt
   :help " ‘ar-underscore-braced-atpt’
   Underscore BRACED at point if any, nil otherwise. "]

  ["Underscore BRACKETED at point"  ar-underscore-bracketed-atpt
   :help " ‘ar-underscore-bracketed-atpt’
   Underscore BRACKETED at point if any, nil otherwise. "]

  ["Underscore LESSER-ANGLED at point"  ar-underscore-lesserangled-atpt
   :help " ‘ar-underscore-lesserangled-atpt’
   Underscore LESSER-ANGLED at point if any, nil otherwise. "]

  ["Underscore GREATER-ANGLED at point"  ar-underscore-greaterangled-atpt
   :help " ‘ar-underscore-greaterangled-atpt’
   Underscore GREATER-ANGLED at point if any, nil otherwise. "]

  ["Underscore LEFT-RIGHT-SINGLEQUOTED at point"  ar-underscore-curvedsinglequoted-atpt
   :help " ‘ar-underscore-curvedsinglequoted-atpt’
   Underscore LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Underscore PARENTIZED at point"  ar-underscore-parentized-atpt
   :help " ‘ar-underscore-parentized-atpt’
   Underscore PARENTIZED at point if any, nil otherwise. "]

  ["Underscore BACKSLASHED at point"  ar-underscore-backslashed-atpt
   :help " ‘ar-underscore-backslashed-atpt’
   Underscore BACKSLASHED at point if any, nil otherwise. "]

  ["Underscore DOLLARED at point"  ar-underscore-dollared-atpt
   :help " ‘ar-underscore-dollared-atpt’
   Underscore DOLLARED at point if any, nil otherwise. "]

  ["Underscore DOUBLEQUOTED at point"  ar-underscore-doublequoted-atpt
   :help " ‘ar-underscore-doublequoted-atpt’
   Underscore DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Underscore EQUALIZED at point"  ar-underscore-equalized-atpt
   :help " ‘ar-underscore-equalized-atpt’
   Underscore EQUALIZED at point if any, nil otherwise. "]

  ["Underscore HYPHENED at point"  ar-underscore-hyphened-atpt
   :help " ‘ar-underscore-hyphened-atpt’
   Underscore HYPHENED at point if any, nil otherwise. "]

  ["Underscore QUOTED at point"  ar-underscore-quoted-atpt
   :help " ‘ar-underscore-quoted-atpt’
   Underscore QUOTED at point if any, nil otherwise. "]

  ["Underscore SINGLEQUOTED at point"  ar-underscore-singlequoted-atpt
   :help " ‘ar-underscore-singlequoted-atpt’
   Underscore SINGLEQUOTED at point if any, nil otherwise. "]

  ["Underscore SLASHED at point"  ar-underscore-slashed-atpt
   :help " ‘ar-underscore-slashed-atpt’
   Underscore SLASHED at point if any, nil otherwise. "]

  ["Underscore UNDERSCORED at point"  ar-underscore-underscored-atpt
   :help " ‘ar-underscore-underscored-atpt’
   Underscore UNDERSCORED at point if any, nil otherwise. "]

  ["Underscore WHITESPACED at point"  ar-underscore-whitespaced-atpt
   :help " ‘ar-underscore-whitespaced-atpt’
   Underscore WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Underscore ANGLED-NO-NEST at point"  ar-underscoreanglednonest-atpt
   :help " ‘ar-underscoreanglednonest-atpt’
   Underscore ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Underscore GREATER-ANGLED-NESTED at point"  ar-underscore-greateranglednested-atpt
   :help " ‘ar-underscore-greateranglednested-atpt’
   Underscore GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Underscore LESSER-ANGLED-NESTED at point"  ar-underscore-lesseranglednested-atpt
   :help " ‘ar-underscore-lesseranglednested-atpt’
   Underscore LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Underscore BUFFER at point"  ar-underscore-buffer-atpt
   :help " ‘ar-underscore-buffer-atpt’
   Underscore BUFFER at point if any, nil otherwise. "]

  ["Underscore COMMENT at point"  ar-underscore-comment-atpt
   :help " ‘ar-underscore-comment-atpt’
   Underscore COMMENT at point if any, nil otherwise. "]

  ["Underscore CSV at point"  ar-underscore-csv-atpt
   :help " ‘ar-underscore-csv-atpt’
   Underscore CSV at point if any, nil otherwise. "]

  ["Underscore DATE at point"  ar-underscore-date-atpt
   :help " ‘ar-underscore-date-atpt’
   Underscore DATE at point if any, nil otherwise. "]

  ["Underscore DEFUN at point"  ar-underscore-defun-atpt
   :help " ‘ar-underscore-defun-atpt’
   Underscore DEFUN at point if any, nil otherwise. "]

  ["Underscore DELIMITED at point"  ar-underscore-delimited-atpt
   :help " ‘ar-underscore-delimited-atpt’
   Underscore DELIMITED at point if any, nil otherwise. "]

  ["Underscore EMAIL at point"  ar-underscore-email-atpt
   :help " ‘ar-underscore-email-atpt’
   Underscore EMAIL at point if any, nil otherwise. "]

  ["Underscore FILENAME at point"  ar-underscore-filename-atpt
   :help " ‘ar-underscore-filename-atpt’
   Underscore FILENAME at point if any, nil otherwise. "]

  ["Underscore FLOAT at point"  ar-underscore-float-atpt
   :help " ‘ar-underscore-float-atpt’
   Underscore FLOAT at point if any, nil otherwise. "]

  ["Underscore FUNCTION at point"  ar-underscore-function-atpt
   :help " ‘ar-underscore-function-atpt’
   Underscore FUNCTION at point if any, nil otherwise. "]

  ["Underscore IP at point"  ar-underscore-ip-atpt
   :help " ‘ar-underscore-ip-atpt’
   Underscore IP at point if any, nil otherwise. "]

  ["Underscore ISBN at point"  ar-underscore-isbn-atpt
   :help " ‘ar-underscore-isbn-atpt’
   Underscore ISBN at point if any, nil otherwise. "]

  ["Underscore LINE at point"  ar-underscore-line-atpt
   :help " ‘ar-underscore-line-atpt’
   Underscore LINE at point if any, nil otherwise. "]

  ["Underscore NAME at point"  ar-underscore-name-atpt
   :help " ‘ar-underscore-name-atpt’
   Underscore NAME at point if any, nil otherwise. "]

  ["Underscore NUMBER at point"  ar-underscore-number-atpt
   :help " ‘ar-underscore-number-atpt’
   Underscore NUMBER at point if any, nil otherwise. "]

  ["Underscore PAGE at point"  ar-underscore-page-atpt
   :help " ‘ar-underscore-page-atpt’
   Underscore PAGE at point if any, nil otherwise. "]

  ["Underscore PARAGRAPH at point"  ar-underscore-paragraph-atpt
   :help " ‘ar-underscore-paragraph-atpt’
   Underscore PARAGRAPH at point if any, nil otherwise. "]

  ["Underscore PAREN at point"  ar-underscore-paren-atpt
   :help " ‘ar-underscore-paren-atpt’
   Underscore PAREN at point if any, nil otherwise. "]

  ["Underscore PHONE at point"  ar-underscore-phone-atpt
   :help " ‘ar-underscore-phone-atpt’
   Underscore PHONE at point if any, nil otherwise. "]

  ["Underscore REGION at point"  ar-underscore-region-atpt
   :help " ‘ar-underscore-region-atpt’
   Underscore REGION at point if any, nil otherwise. "]

  ["Underscore SENTENCE at point"  ar-underscore-sentence-atpt
   :help " ‘ar-underscore-sentence-atpt’
   Underscore SENTENCE at point if any, nil otherwise. "]

  ["Underscore SEXP at point"  ar-underscore-sexp-atpt
   :help " ‘ar-underscore-sexp-atpt’
   Underscore SEXP at point if any, nil otherwise. "]

  ["Underscore STRING at point"  ar-underscore-string-atpt
   :help " ‘ar-underscore-string-atpt’
   Underscore STRING at point if any, nil otherwise. "]

  ["Underscore SH-STRUCT at point"  ar-underscore-shstruct-atpt
   :help " ‘ar-underscore-shstruct-atpt’
   Underscore SH-STRUCT at point if any, nil otherwise. "]

  ["Underscore SYMBOL at point"  ar-underscore-symbol-atpt
   :help " ‘ar-underscore-symbol-atpt’
   Underscore SYMBOL at point if any, nil otherwise. "]

  ["Underscore URL at point"  ar-underscore-url-atpt
   :help " ‘ar-underscore-url-atpt’
   Underscore URL at point if any, nil otherwise. "]

  ["Underscore WORD at point"  ar-underscore-word-atpt
   :help " ‘ar-underscore-word-atpt’
   Underscore WORD at point if any, nil otherwise. "]

  ["Underscore WORD-ALPHA-ONLY at point"  ar-underscore-wordalphaonly-atpt
   :help " ‘ar-underscore-wordalphaonly-atpt’
   Underscore WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)
  ("Whitespace"

 ("Character classes"

  ["Whitespace [:alnum:] char class at point"  ar-whitespace-alnum-atpt
   :help " ‘ar-whitespace-alnum-atpt’
   Whitespace ALNUM at point if any, nil otherwise. "]

  ["Whitespace [:alpha:] char class at point"  ar-whitespace-alpha-atpt
   :help " ‘ar-whitespace-alpha-atpt’
   Whitespace ALPHA at point if any, nil otherwise. "]

  ["Whitespace [:ascii:] char class at point"  ar-whitespace-ascii-atpt
   :help " ‘ar-whitespace-ascii-atpt’
   Whitespace ASCII at point if any, nil otherwise. "]

  ["Whitespace [:blank:] char class at point"  ar-whitespace-blank-atpt
   :help " ‘ar-whitespace-blank-atpt’
   Whitespace BLANK at point if any, nil otherwise. "]

  ["Whitespace [:cntrl:] char class at point"  ar-whitespace-cntrl-atpt
   :help " ‘ar-whitespace-cntrl-atpt’
   Whitespace CNTRL at point if any, nil otherwise. "]

  ["Whitespace [:digit:] char class at point"  ar-whitespace-digit-atpt
   :help " ‘ar-whitespace-digit-atpt’
   Whitespace DIGIT at point if any, nil otherwise. "]

  ["Whitespace [:graph:] char class at point"  ar-whitespace-graph-atpt
   :help " ‘ar-whitespace-graph-atpt’
   Whitespace GRAPH at point if any, nil otherwise. "]

  ["Whitespace [:lower:] char class at point"  ar-whitespace-lower-atpt
   :help " ‘ar-whitespace-lower-atpt’
   Whitespace LOWER at point if any, nil otherwise. "]

  ["Whitespace [:nonascii:] char class at point"  ar-whitespace-nonascii-atpt
   :help " ‘ar-whitespace-nonascii-atpt’
   Whitespace NONASCII at point if any, nil otherwise. "]

  ["Whitespace [:print:] char class at point"  ar-whitespace-print-atpt
   :help " ‘ar-whitespace-print-atpt’
   Whitespace PRINT at point if any, nil otherwise. "]

  ["Whitespace [:punct:] char class at point"  ar-whitespace-punct-atpt
   :help " ‘ar-whitespace-punct-atpt’
   Whitespace PUNCT at point if any, nil otherwise. "]

  ["Whitespace [:space:] char class at point"  ar-whitespace-space-atpt
   :help " ‘ar-whitespace-space-atpt’
   Whitespace SPACE at point if any, nil otherwise. "]

  ["Whitespace [:upper:] char class at point"  ar-whitespace-upper-atpt
   :help " ‘ar-whitespace-upper-atpt’
   Whitespace UPPER at point if any, nil otherwise. "]

  ["Whitespace [:xdigit:] char class at point"  ar-whitespace-xdigit-atpt
   :help " ‘ar-whitespace-xdigit-atpt’
   Whitespace XDIGIT at point if any, nil otherwise. "]

)
 ("Delimited"

  ["Whitespace BRACED at point"  ar-whitespace-braced-atpt
   :help " ‘ar-whitespace-braced-atpt’
   Whitespace BRACED at point if any, nil otherwise. "]

  ["Whitespace BRACKETED at point"  ar-whitespace-bracketed-atpt
   :help " ‘ar-whitespace-bracketed-atpt’
   Whitespace BRACKETED at point if any, nil otherwise. "]

  ["Whitespace LESSER-ANGLED at point"  ar-whitespace-lesserangled-atpt
   :help " ‘ar-whitespace-lesserangled-atpt’
   Whitespace LESSER-ANGLED at point if any, nil otherwise. "]

  ["Whitespace GREATER-ANGLED at point"  ar-whitespace-greaterangled-atpt
   :help " ‘ar-whitespace-greaterangled-atpt’
   Whitespace GREATER-ANGLED at point if any, nil otherwise. "]

  ["Whitespace LEFT-RIGHT-SINGLEQUOTED at point"  ar-whitespace-curvedsinglequoted-atpt
   :help " ‘ar-whitespace-curvedsinglequoted-atpt’
   Whitespace LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Whitespace PARENTIZED at point"  ar-whitespace-parentized-atpt
   :help " ‘ar-whitespace-parentized-atpt’
   Whitespace PARENTIZED at point if any, nil otherwise. "]

  ["Whitespace BACKSLASHED at point"  ar-whitespace-backslashed-atpt
   :help " ‘ar-whitespace-backslashed-atpt’
   Whitespace BACKSLASHED at point if any, nil otherwise. "]

  ["Whitespace DOLLARED at point"  ar-whitespace-dollared-atpt
   :help " ‘ar-whitespace-dollared-atpt’
   Whitespace DOLLARED at point if any, nil otherwise. "]

  ["Whitespace DOUBLEQUOTED at point"  ar-whitespace-doublequoted-atpt
   :help " ‘ar-whitespace-doublequoted-atpt’
   Whitespace DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Whitespace EQUALIZED at point"  ar-whitespace-equalized-atpt
   :help " ‘ar-whitespace-equalized-atpt’
   Whitespace EQUALIZED at point if any, nil otherwise. "]

  ["Whitespace HYPHENED at point"  ar-whitespace-hyphened-atpt
   :help " ‘ar-whitespace-hyphened-atpt’
   Whitespace HYPHENED at point if any, nil otherwise. "]

  ["Whitespace QUOTED at point"  ar-whitespace-quoted-atpt
   :help " ‘ar-whitespace-quoted-atpt’
   Whitespace QUOTED at point if any, nil otherwise. "]

  ["Whitespace SINGLEQUOTED at point"  ar-whitespace-singlequoted-atpt
   :help " ‘ar-whitespace-singlequoted-atpt’
   Whitespace SINGLEQUOTED at point if any, nil otherwise. "]

  ["Whitespace SLASHED at point"  ar-whitespace-slashed-atpt
   :help " ‘ar-whitespace-slashed-atpt’
   Whitespace SLASHED at point if any, nil otherwise. "]

  ["Whitespace UNDERSCORED at point"  ar-whitespace-underscored-atpt
   :help " ‘ar-whitespace-underscored-atpt’
   Whitespace UNDERSCORED at point if any, nil otherwise. "]

  ["Whitespace WHITESPACED at point"  ar-whitespace-whitespaced-atpt
   :help " ‘ar-whitespace-whitespaced-atpt’
   Whitespace WHITESPACED at point if any, nil otherwise. "]

)
 ("Other"

  ["Whitespace ANGLED-NO-NEST at point"  ar-whitespaceanglednonest-atpt
   :help " ‘ar-whitespaceanglednonest-atpt’
   Whitespace ANGLED-NO-NEST at point if any, nil otherwise. "]

  ["Whitespace GREATER-ANGLED-NESTED at point"  ar-whitespace-greateranglednested-atpt
   :help " ‘ar-whitespace-greateranglednested-atpt’
   Whitespace GREATER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Whitespace LESSER-ANGLED-NESTED at point"  ar-whitespace-lesseranglednested-atpt
   :help " ‘ar-whitespace-lesseranglednested-atpt’
   Whitespace LESSER-ANGLED-NESTED at point if any, nil otherwise. "]

  ["Whitespace BUFFER at point"  ar-whitespace-buffer-atpt
   :help " ‘ar-whitespace-buffer-atpt’
   Whitespace BUFFER at point if any, nil otherwise. "]

  ["Whitespace COMMENT at point"  ar-whitespace-comment-atpt
   :help " ‘ar-whitespace-comment-atpt’
   Whitespace COMMENT at point if any, nil otherwise. "]

  ["Whitespace CSV at point"  ar-whitespace-csv-atpt
   :help " ‘ar-whitespace-csv-atpt’
   Whitespace CSV at point if any, nil otherwise. "]

  ["Whitespace DATE at point"  ar-whitespace-date-atpt
   :help " ‘ar-whitespace-date-atpt’
   Whitespace DATE at point if any, nil otherwise. "]

  ["Whitespace DEFUN at point"  ar-whitespace-defun-atpt
   :help " ‘ar-whitespace-defun-atpt’
   Whitespace DEFUN at point if any, nil otherwise. "]

  ["Whitespace DELIMITED at point"  ar-whitespace-delimited-atpt
   :help " ‘ar-whitespace-delimited-atpt’
   Whitespace DELIMITED at point if any, nil otherwise. "]

  ["Whitespace EMAIL at point"  ar-whitespace-email-atpt
   :help " ‘ar-whitespace-email-atpt’
   Whitespace EMAIL at point if any, nil otherwise. "]

  ["Whitespace FILENAME at point"  ar-whitespace-filename-atpt
   :help " ‘ar-whitespace-filename-atpt’
   Whitespace FILENAME at point if any, nil otherwise. "]

  ["Whitespace FLOAT at point"  ar-whitespace-float-atpt
   :help " ‘ar-whitespace-float-atpt’
   Whitespace FLOAT at point if any, nil otherwise. "]

  ["Whitespace FUNCTION at point"  ar-whitespace-function-atpt
   :help " ‘ar-whitespace-function-atpt’
   Whitespace FUNCTION at point if any, nil otherwise. "]

  ["Whitespace IP at point"  ar-whitespace-ip-atpt
   :help " ‘ar-whitespace-ip-atpt’
   Whitespace IP at point if any, nil otherwise. "]

  ["Whitespace ISBN at point"  ar-whitespace-isbn-atpt
   :help " ‘ar-whitespace-isbn-atpt’
   Whitespace ISBN at point if any, nil otherwise. "]

  ["Whitespace LINE at point"  ar-whitespace-line-atpt
   :help " ‘ar-whitespace-line-atpt’
   Whitespace LINE at point if any, nil otherwise. "]

  ["Whitespace NAME at point"  ar-whitespace-name-atpt
   :help " ‘ar-whitespace-name-atpt’
   Whitespace NAME at point if any, nil otherwise. "]

  ["Whitespace NUMBER at point"  ar-whitespace-number-atpt
   :help " ‘ar-whitespace-number-atpt’
   Whitespace NUMBER at point if any, nil otherwise. "]

  ["Whitespace PAGE at point"  ar-whitespace-page-atpt
   :help " ‘ar-whitespace-page-atpt’
   Whitespace PAGE at point if any, nil otherwise. "]

  ["Whitespace PARAGRAPH at point"  ar-whitespace-paragraph-atpt
   :help " ‘ar-whitespace-paragraph-atpt’
   Whitespace PARAGRAPH at point if any, nil otherwise. "]

  ["Whitespace PAREN at point"  ar-whitespace-paren-atpt
   :help " ‘ar-whitespace-paren-atpt’
   Whitespace PAREN at point if any, nil otherwise. "]

  ["Whitespace PHONE at point"  ar-whitespace-phone-atpt
   :help " ‘ar-whitespace-phone-atpt’
   Whitespace PHONE at point if any, nil otherwise. "]

  ["Whitespace REGION at point"  ar-whitespace-region-atpt
   :help " ‘ar-whitespace-region-atpt’
   Whitespace REGION at point if any, nil otherwise. "]

  ["Whitespace SENTENCE at point"  ar-whitespace-sentence-atpt
   :help " ‘ar-whitespace-sentence-atpt’
   Whitespace SENTENCE at point if any, nil otherwise. "]

  ["Whitespace SEXP at point"  ar-whitespace-sexp-atpt
   :help " ‘ar-whitespace-sexp-atpt’
   Whitespace SEXP at point if any, nil otherwise. "]

  ["Whitespace STRING at point"  ar-whitespace-string-atpt
   :help " ‘ar-whitespace-string-atpt’
   Whitespace STRING at point if any, nil otherwise. "]

  ["Whitespace SH-STRUCT at point"  ar-whitespace-shstruct-atpt
   :help " ‘ar-whitespace-shstruct-atpt’
   Whitespace SH-STRUCT at point if any, nil otherwise. "]

  ["Whitespace SYMBOL at point"  ar-whitespace-symbol-atpt
   :help " ‘ar-whitespace-symbol-atpt’
   Whitespace SYMBOL at point if any, nil otherwise. "]

  ["Whitespace URL at point"  ar-whitespace-url-atpt
   :help " ‘ar-whitespace-url-atpt’
   Whitespace URL at point if any, nil otherwise. "]

  ["Whitespace WORD at point"  ar-whitespace-word-atpt
   :help " ‘ar-whitespace-word-atpt’
   Whitespace WORD at point if any, nil otherwise. "]

  ["Whitespace WORD-ALPHA-ONLY at point"  ar-whitespace-wordalphaonly-atpt
   :help " ‘ar-whitespace-wordalphaonly-atpt’
   Whitespace WORD-ALPHA-ONLY at point if any, nil otherwise. "]

)

)

  )
 ("Move"
("Forward"
 ("Delimited"

  ["Forward BRACED at point"  ar-forward-braced-atpt
   :help " ‘ar-forward-braced-atpt’
   Forward BRACED at point if any, nil otherwise. "]

  ["Forward BRACKETED at point"  ar-forward-bracketed-atpt
   :help " ‘ar-forward-bracketed-atpt’
   Forward BRACKETED at point if any, nil otherwise. "]

  ["Forward PARENTIZED at point"  ar-forward-parentized-atpt
   :help " ‘ar-forward-parentized-atpt’
   Forward PARENTIZED at point if any, nil otherwise. "]

  ["Forward DOUBLEQUOTED at point"  ar-forward-doublequoted-atpt
   :help " ‘ar-forward-doublequoted-atpt’
   Forward DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Forward SINGLEQUOTED at point"  ar-forward-singlequoted-atpt
   :help " ‘ar-forward-singlequoted-atpt’
   Forward SINGLEQUOTED at point if any, nil otherwise. "]

  ["Forward TRIPLEQUOTED at point"  ar-forward-triplequoted-atpt
   :help " ‘ar-forward-triplequoted-atpt’
   Forward TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Forward LESSER-ANGLED at point"  ar-forward-lesserangled-atpt
   :help " ‘ar-forward-lesserangled-atpt’
   Forward LESSER-ANGLED at point if any, nil otherwise. "]

  "-"

  ["Forward TRIPLEQUOTED-DQ at point"  ar-forward-triplequoteddq-atpt
   :help " ‘ar-forward-triplequoteddq-atpt’
   Forward TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Forward TRIPLEQUOTE-SQ at point"  ar-forward-triplequotesq-atpt
   :help " ‘ar-forward-triplequotesq-atpt’
   Forward TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Forward GREATER-ANGLED at point"  ar-forward-greaterangled-atpt
   :help " ‘ar-forward-greaterangled-atpt’
   Forward GREATER-ANGLED at point if any, nil otherwise. "]

  ["Forward LEFT-RIGHT-SINGLEQUOTED at point"  ar-forward-curvedsinglequoted-atpt
   :help " ‘ar-forward-curvedsinglequoted-atpt’
   Forward LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Forward BACKSLASHED at point"  ar-forward-backslashed-atpt
   :help " ‘ar-forward-backslashed-atpt’
   Forward BACKSLASHED at point if any, nil otherwise. "]

  ["Forward DOLLARED at point"  ar-forward-dollared-atpt
   :help " ‘ar-forward-dollared-atpt’
   Forward DOLLARED at point if any, nil otherwise. "]

  ["Forward DOUBLEQUOTED at point"  ar-forward-doublequoted-atpt
   :help " ‘ar-forward-doublequoted-atpt’
   Forward DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Forward EQUALIZED at point"  ar-forward-equalized-atpt
   :help " ‘ar-forward-equalized-atpt’
   Forward EQUALIZED at point if any, nil otherwise. "]

  ["Forward HYPHENED at point"  ar-forward-hyphened-atpt
   :help " ‘ar-forward-hyphened-atpt’
   Forward HYPHENED at point if any, nil otherwise. "]

  ["Forward SINGLEQUOTED at point"  ar-forward-singlequoted-atpt
   :help " ‘ar-forward-singlequoted-atpt’
   Forward SINGLEQUOTED at point if any, nil otherwise. "]

  ["Forward SLASHED at point"  ar-forward-slashed-atpt
   :help " ‘ar-forward-slashed-atpt’
   Forward SLASHED at point if any, nil otherwise. "]

  ["Forward UNDERSCORED at point"  ar-forward-underscored-atpt
   :help " ‘ar-forward-underscored-atpt’
   Forward UNDERSCORED at point if any, nil otherwise. "]

  ["Forward WHITESPACED at point"  ar-forward-whitespaced-atpt
   :help " ‘ar-forward-whitespaced-atpt’
   Forward WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Forward [:alnum:] char class at point"  ar-forward-alnum-atpt
   :help " ‘ar-forward-alnum-atpt’
   Return end position of ALNUM at point if succesful, nil otherwise. "]

  ["Forward [:alpha:] char class at point"  ar-forward-alpha-atpt
   :help " ‘ar-forward-alpha-atpt’
   Return end position of ALPHA at point if succesful, nil otherwise. "]

  ["Forward [:ascii:] char class at point"  ar-forward-ascii-atpt
   :help " ‘ar-forward-ascii-atpt’
   Return end position of ASCII at point if succesful, nil otherwise. "]

  ["Forward [:blank:] char class at point"  ar-forward-blank-atpt
   :help " ‘ar-forward-blank-atpt’
   Return end position of BLANK at point if succesful, nil otherwise. "]

  ["Forward [:cntrl:] char class at point"  ar-forward-cntrl-atpt
   :help " ‘ar-forward-cntrl-atpt’
   Return end position of CNTRL at point if succesful, nil otherwise. "]

  ["Forward [:digit:] char class at point"  ar-forward-digit-atpt
   :help " ‘ar-forward-digit-atpt’
   Return end position of DIGIT at point if succesful, nil otherwise. "]

  ["Forward [:graph:] char class at point"  ar-forward-graph-atpt
   :help " ‘ar-forward-graph-atpt’
   Return end position of GRAPH at point if succesful, nil otherwise. "]

  ["Forward [:lower:] char class at point"  ar-forward-lower-atpt
   :help " ‘ar-forward-lower-atpt’
   Return end position of LOWER at point if succesful, nil otherwise. "]

  ["Forward [:nonascii:] char class at point"  ar-forward-nonascii-atpt
   :help " ‘ar-forward-nonascii-atpt’
   Return end position of NONASCII at point if succesful, nil otherwise. "]

  ["Forward [:print:] char class at point"  ar-forward-print-atpt
   :help " ‘ar-forward-print-atpt’
   Return end position of PRINT at point if succesful, nil otherwise. "]

  ["Forward [:punct:] char class at point"  ar-forward-punct-atpt
   :help " ‘ar-forward-punct-atpt’
   Return end position of PUNCT at point if succesful, nil otherwise. "]

  ["Forward [:space:] char class at point"  ar-forward-space-atpt
   :help " ‘ar-forward-space-atpt’
   Return end position of SPACE at point if succesful, nil otherwise. "]

  ["Forward [:upper:] char class at point"  ar-forward-upper-atpt
   :help " ‘ar-forward-upper-atpt’
   Return end position of UPPER at point if succesful, nil otherwise. "]

  ["Forward [:xdigit:] char class at point"  ar-forward-xdigit-atpt
   :help " ‘ar-forward-xdigit-atpt’
   Return end position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Forward ANGLED-NO-NEST at point"  ar-forwardanglednonest-atpt
   :help " ‘ar-forwardanglednonest-atpt’
   Return end position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Forward GREATER-ANGLED-NESTED at point"  ar-forward-greateranglednested-atpt
   :help " ‘ar-forward-greateranglednested-atpt’
   Return end position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Forward LESSER-ANGLED-NESTED at point"  ar-forward-lesseranglednested-atpt
   :help " ‘ar-forward-lesseranglednested-atpt’
   Return end position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Forward BUFFER at point"  ar-forward-buffer-atpt
   :help " ‘ar-forward-buffer-atpt’
   Return end position of BUFFER at point if succesful, nil otherwise. "]

  ["Forward COMMENT at point"  ar-forward-comment-atpt
   :help " ‘ar-forward-comment-atpt’
   Return end position of COMMENT at point if succesful, nil otherwise. "]

  ["Forward CSV at point"  ar-forward-csv-atpt
   :help " ‘ar-forward-csv-atpt’
   Return end position of CSV at point if succesful, nil otherwise. "]

  ["Forward DATE at point"  ar-forward-date-atpt
   :help " ‘ar-forward-date-atpt’
   Return end position of DATE at point if succesful, nil otherwise. "]

  ["Forward DELIMITED at point"  ar-forward-delimited-atpt
   :help " ‘ar-forward-delimited-atpt’
   Return end position of DELIMITED at point if succesful, nil otherwise. "]

  ["Forward EMAIL at point"  ar-forward-email-atpt
   :help " ‘ar-forward-email-atpt’
   Return end position of EMAIL at point if succesful, nil otherwise. "]

  ["Forward FILENAME at point"  ar-forward-filename-atpt
   :help " ‘ar-forward-filename-atpt’
   Return end position of FILENAME at point if succesful, nil otherwise. "]

  ["Forward FLOAT at point"  ar-forward-float-atpt
   :help " ‘ar-forward-float-atpt’
   Return end position of FLOAT at point if succesful, nil otherwise. "]

  ["Forward FUNCTION at point"  ar-forward-function-atpt
   :help " ‘ar-forward-function-atpt’
   Return end position of FUNCTION at point if succesful, nil otherwise. "]

  ["Forward IP at point"  ar-forward-ip-atpt
   :help " ‘ar-forward-ip-atpt’
   Return end position of IP at point if succesful, nil otherwise. "]

  ["Forward ISBN at point"  ar-forward-isbn-atpt
   :help " ‘ar-forward-isbn-atpt’
   Return end position of ISBN at point if succesful, nil otherwise. "]

  ["Forward LINE at point"  ar-forward-line-atpt
   :help " ‘ar-forward-line-atpt’
   Return end position of LINE at point if succesful, nil otherwise. "]

  ["Forward NAME at point"  ar-forward-name-atpt
   :help " ‘ar-forward-name-atpt’
   Return end position of NAME at point if succesful, nil otherwise. "]

  ["Forward NUMBER at point"  ar-forward-number-atpt
   :help " ‘ar-forward-number-atpt’
   Return end position of NUMBER at point if succesful, nil otherwise. "]

  ["Forward PAGE at point"  ar-forward-page-atpt
   :help " ‘ar-forward-page-atpt’
   Return end position of PAGE at point if succesful, nil otherwise. "]

  ["Forward PARAGRAPH at point"  ar-forward-paragraph-atpt
   :help " ‘ar-forward-paragraph-atpt’
   Return end position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Forward PAREN at point"  ar-forward-paren-atpt
   :help " ‘ar-forward-paren-atpt’
   Return end position of PAREN at point if succesful, nil otherwise. "]

  ["Forward PHONE at point"  ar-forward-phone-atpt
   :help " ‘ar-forward-phone-atpt’
   Return end position of PHONE at point if succesful, nil otherwise. "]

  ["Forward REGION at point"  ar-forward-region-atpt
   :help " ‘ar-forward-region-atpt’
   Return end position of REGION at point if succesful, nil otherwise. "]

  ["Forward SENTENCE at point"  ar-forward-sentence-atpt
   :help " ‘ar-forward-sentence-atpt’
   Return end position of SENTENCE at point if succesful, nil otherwise. "]

  ["Forward SEXP at point"  ar-forward-sexp-atpt
   :help " ‘ar-forward-sexp-atpt’
   Return end position of SEXP at point if succesful, nil otherwise. "]

  ["Forward STRING at point"  ar-forward-string-atpt
   :help " ‘ar-forward-string-atpt’
   Return end position of STRING at point if succesful, nil otherwise. "]

  ["Forward SH-STRUCT at point"  ar-forward-shstruct-atpt
   :help " ‘ar-forward-shstruct-atpt’
   Return end position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Forward SYMBOL at point"  ar-forward-symbol-atpt
   :help " ‘ar-forward-symbol-atpt’
   Return end position of SYMBOL at point if succesful, nil otherwise. "]

  ["Forward URL at point"  ar-forward-url-atpt
   :help " ‘ar-forward-url-atpt’
   Return end position of URL at point if succesful, nil otherwise. "]

  ["Forward WORD at point"  ar-forward-word-atpt
   :help " ‘ar-forward-word-atpt’
   Return end position of WORD at point if succesful, nil otherwise. "]

  ["Forward WORD-ALPHA-ONLY at point"  ar-forward-wordalphaonly-atpt
   :help " ‘ar-forward-wordalphaonly-atpt’
   Return end position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

  )
 ("Backward"
 ("Delimited-1"

  ["Backward BRACED at point"  ar-backward-braced-atpt
   :help " ‘ar-backward-braced-atpt’
   Backward BRACED at point if any, nil otherwise. "]

  ["Backward BRACKETED at point"  ar-backward-bracketed-atpt
   :help " ‘ar-backward-bracketed-atpt’
   Backward BRACKETED at point if any, nil otherwise. "]

  ["Backward PARENTIZED at point"  ar-backward-parentized-atpt
   :help " ‘ar-backward-parentized-atpt’
   Backward PARENTIZED at point if any, nil otherwise. "]

  ["Backward DOUBLEQUOTED at point"  ar-backward-doublequoted-atpt
   :help " ‘ar-backward-doublequoted-atpt’
   Backward DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Backward SINGLEQUOTED at point"  ar-backward-singlequoted-atpt
   :help " ‘ar-backward-singlequoted-atpt’
   Backward SINGLEQUOTED at point if any, nil otherwise. "]

  ["Backward TRIPLEQUOTED at point"  ar-backward-triplequoted-atpt
   :help " ‘ar-backward-triplequoted-atpt’
   Backward TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Backward LESSER-ANGLED at point"  ar-backward-lesserangled-atpt
   :help " ‘ar-backward-lesserangled-atpt’
   Backward LESSER-ANGLED at point if any, nil otherwise. "]

)
 ("Delimited-2"

  ["Backward TRIPLEQUOTED-DQ at point"  ar-backward-triplequoteddq-atpt
   :help " ‘ar-backward-triplequoteddq-atpt’
   Backward TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Backward TRIPLEQUOTE-SQ at point"  ar-backward-triplequotesq-atpt
   :help " ‘ar-backward-triplequotesq-atpt’
   Backward TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Backward GREATER-ANGLED at point"  ar-backward-greaterangled-atpt
   :help " ‘ar-backward-greaterangled-atpt’
   Backward GREATER-ANGLED at point if any, nil otherwise. "]

  ["Backward LEFT-RIGHT-SINGLEQUOTED at point"  ar-backward-curvedsinglequoted-atpt
   :help " ‘ar-backward-curvedsinglequoted-atpt’
   Backward LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Backward BACKSLASHED at point"  ar-backward-backslashed-atpt
   :help " ‘ar-backward-backslashed-atpt’
   Backward BACKSLASHED at point if any, nil otherwise. "]

  ["Backward DOLLARED at point"  ar-backward-dollared-atpt
   :help " ‘ar-backward-dollared-atpt’
   Backward DOLLARED at point if any, nil otherwise. "]

  ["Backward DOUBLEQUOTED at point"  ar-backward-doublequoted-atpt
   :help " ‘ar-backward-doublequoted-atpt’
   Backward DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Backward EQUALIZED at point"  ar-backward-equalized-atpt
   :help " ‘ar-backward-equalized-atpt’
   Backward EQUALIZED at point if any, nil otherwise. "]

  ["Backward HYPHENED at point"  ar-backward-hyphened-atpt
   :help " ‘ar-backward-hyphened-atpt’
   Backward HYPHENED at point if any, nil otherwise. "]

  ["Backward SINGLEQUOTED at point"  ar-backward-singlequoted-atpt
   :help " ‘ar-backward-singlequoted-atpt’
   Backward SINGLEQUOTED at point if any, nil otherwise. "]

  ["Backward SLASHED at point"  ar-backward-slashed-atpt
   :help " ‘ar-backward-slashed-atpt’
   Backward SLASHED at point if any, nil otherwise. "]

  ["Backward UNDERSCORED at point"  ar-backward-underscored-atpt
   :help " ‘ar-backward-underscored-atpt’
   Backward UNDERSCORED at point if any, nil otherwise. "]

  ["Backward WHITESPACED at point"  ar-backward-whitespaced-atpt
   :help " ‘ar-backward-whitespaced-atpt’
   Backward WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Backward [:alnum:] char class at point"  ar-backward-alnum-atpt
   :help " ‘ar-backward-alnum-atpt’
   Return beginning position of ALNUM at point if succesful, nil otherwise. "]

  ["Backward [:alpha:] char class at point"  ar-backward-alpha-atpt
   :help " ‘ar-backward-alpha-atpt’
   Return beginning position of ALPHA at point if succesful, nil otherwise. "]

  ["Backward [:ascii:] char class at point"  ar-backward-ascii-atpt
   :help " ‘ar-backward-ascii-atpt’
   Return beginning position of ASCII at point if succesful, nil otherwise. "]

  ["Backward [:blank:] char class at point"  ar-backward-blank-atpt
   :help " ‘ar-backward-blank-atpt’
   Return beginning position of BLANK at point if succesful, nil otherwise. "]

  ["Backward [:cntrl:] char class at point"  ar-backward-cntrl-atpt
   :help " ‘ar-backward-cntrl-atpt’
   Return beginning position of CNTRL at point if succesful, nil otherwise. "]

  ["Backward [:digit:] char class at point"  ar-backward-digit-atpt
   :help " ‘ar-backward-digit-atpt’
   Return beginning position of DIGIT at point if succesful, nil otherwise. "]

  ["Backward [:graph:] char class at point"  ar-backward-graph-atpt
   :help " ‘ar-backward-graph-atpt’
   Return beginning position of GRAPH at point if succesful, nil otherwise. "]

  ["Backward [:lower:] char class at point"  ar-backward-lower-atpt
   :help " ‘ar-backward-lower-atpt’
   Return beginning position of LOWER at point if succesful, nil otherwise. "]

  ["Backward [:nonascii:] char class at point"  ar-backward-nonascii-atpt
   :help " ‘ar-backward-nonascii-atpt’
   Return beginning position of NONASCII at point if succesful, nil otherwise. "]

  ["Backward [:print:] char class at point"  ar-backward-print-atpt
   :help " ‘ar-backward-print-atpt’
   Return beginning position of PRINT at point if succesful, nil otherwise. "]

  ["Backward [:punct:] char class at point"  ar-backward-punct-atpt
   :help " ‘ar-backward-punct-atpt’
   Return beginning position of PUNCT at point if succesful, nil otherwise. "]

  ["Backward [:space:] char class at point"  ar-backward-space-atpt
   :help " ‘ar-backward-space-atpt’
   Return beginning position of SPACE at point if succesful, nil otherwise. "]

  ["Backward [:upper:] char class at point"  ar-backward-upper-atpt
   :help " ‘ar-backward-upper-atpt’
   Return beginning position of UPPER at point if succesful, nil otherwise. "]

  ["Backward [:xdigit:] char class at point"  ar-backward-xdigit-atpt
   :help " ‘ar-backward-xdigit-atpt’
   Return beginning position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Backward ANGLED-NO-NEST at point"  ar-backwardanglednonest-atpt
   :help " ‘ar-backwardanglednonest-atpt’
   Return beginning position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Backward GREATER-ANGLED-NESTED at point"  ar-backward-greateranglednested-atpt
   :help " ‘ar-backward-greateranglednested-atpt’
   Return beginning position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Backward LESSER-ANGLED-NESTED at point"  ar-backward-lesseranglednested-atpt
   :help " ‘ar-backward-lesseranglednested-atpt’
   Return beginning position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Backward BUFFER at point"  ar-backward-buffer-atpt
   :help " ‘ar-backward-buffer-atpt’
   Return beginning position of BUFFER at point if succesful, nil otherwise. "]

  ["Backward COMMENT at point"  ar-backward-comment-atpt
   :help " ‘ar-backward-comment-atpt’
   Return beginning position of COMMENT at point if succesful, nil otherwise. "]

  ["Backward CSV at point"  ar-backward-csv-atpt
   :help " ‘ar-backward-csv-atpt’
   Return beginning position of CSV at point if succesful, nil otherwise. "]

  ["Backward DATE at point"  ar-backward-date-atpt
   :help " ‘ar-backward-date-atpt’
   Return beginning position of DATE at point if succesful, nil otherwise. "]

  ["Backward DEFUN at point"  ar-backward-defun-atpt
   :help " ‘ar-backward-defun-atpt’
   Return beginning position of DEFUN at point if succesful, nil otherwise. "]

  ["Backward DELIMITED at point"  ar-backward-delimited-atpt
   :help " ‘ar-backward-delimited-atpt’
   Return beginning position of DELIMITED at point if succesful, nil otherwise. "]

  ["Backward EMAIL at point"  ar-backward-email-atpt
   :help " ‘ar-backward-email-atpt’
   Return beginning position of EMAIL at point if succesful, nil otherwise. "]

  ["Backward FILENAME at point"  ar-backward-filename-atpt
   :help " ‘ar-backward-filename-atpt’
   Return beginning position of FILENAME at point if succesful, nil otherwise. "]

  ["Backward FLOAT at point"  ar-backward-float-atpt
   :help " ‘ar-backward-float-atpt’
   Return beginning position of FLOAT at point if succesful, nil otherwise. "]

  ["Backward FUNCTION at point"  ar-backward-function-atpt
   :help " ‘ar-backward-function-atpt’
   Return beginning position of FUNCTION at point if succesful, nil otherwise. "]

  ["Backward IP at point"  ar-backward-ip-atpt
   :help " ‘ar-backward-ip-atpt’
   Return beginning position of IP at point if succesful, nil otherwise. "]

  ["Backward ISBN at point"  ar-backward-isbn-atpt
   :help " ‘ar-backward-isbn-atpt’
   Return beginning position of ISBN at point if succesful, nil otherwise. "]

  ["Backward LINE at point"  ar-backward-line-atpt
   :help " ‘ar-backward-line-atpt’
   Return beginning position of LINE at point if succesful, nil otherwise. "]

  ["Backward NAME at point"  ar-backward-name-atpt
   :help " ‘ar-backward-name-atpt’
   Return beginning position of NAME at point if succesful, nil otherwise. "]

  ["Backward NUMBER at point"  ar-backward-number-atpt
   :help " ‘ar-backward-number-atpt’
   Return beginning position of NUMBER at point if succesful, nil otherwise. "]

  ["Backward PAGE at point"  ar-backward-page-atpt
   :help " ‘ar-backward-page-atpt’
   Return beginning position of PAGE at point if succesful, nil otherwise. "]

  ["Backward PARAGRAPH at point"  ar-backward-paragraph-atpt
   :help " ‘ar-backward-paragraph-atpt’
   Return beginning position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Backward PAREN at point"  ar-backward-paren-atpt
   :help " ‘ar-backward-paren-atpt’
   Return beginning position of PAREN at point if succesful, nil otherwise. "]

  ["Backward PHONE at point"  ar-backward-phone-atpt
   :help " ‘ar-backward-phone-atpt’
   Return beginning position of PHONE at point if succesful, nil otherwise. "]

  ["Backward REGION at point"  ar-backward-region-atpt
   :help " ‘ar-backward-region-atpt’
   Return beginning position of REGION at point if succesful, nil otherwise. "]

  ["Backward SENTENCE at point"  ar-backward-sentence-atpt
   :help " ‘ar-backward-sentence-atpt’
   Return beginning position of SENTENCE at point if succesful, nil otherwise. "]

  ["Backward SEXP at point"  ar-backward-sexp-atpt
   :help " ‘ar-backward-sexp-atpt’
   Return beginning position of SEXP at point if succesful, nil otherwise. "]

  ["Backward STRING at point"  ar-backward-string-atpt
   :help " ‘ar-backward-string-atpt’
   Return beginning position of STRING at point if succesful, nil otherwise. "]

  ["Backward SH-STRUCT at point"  ar-backward-shstruct-atpt
   :help " ‘ar-backward-shstruct-atpt’
   Return beginning position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Backward SYMBOL at point"  ar-backward-symbol-atpt
   :help " ‘ar-backward-symbol-atpt’
   Return beginning position of SYMBOL at point if succesful, nil otherwise. "]

  ["Backward URL at point"  ar-backward-url-atpt
   :help " ‘ar-backward-url-atpt’
   Return beginning position of URL at point if succesful, nil otherwise. "]

  ["Backward WORD at point"  ar-backward-word-atpt
   :help " ‘ar-backward-word-atpt’
   Return beginning position of WORD at point if succesful, nil otherwise. "]

  ["Backward WORD-ALPHA-ONLY at point"  ar-backward-wordalphaonly-atpt
   :help " ‘ar-backward-wordalphaonly-atpt’
   Return beginning position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

  )
 ("End"
 ("Delimited-1"

  ["End of BRACED at point"  ar-end-of-braced-atpt
   :help " ‘ar-end-of-braced-atpt’
  End of BRACED at point if any, nil otherwise. "]

  ["End of BRACKETED at point"  ar-end-of-bracketed-atpt
   :help " ‘ar-end-of-bracketed-atpt’
  End of BRACKETED at point if any, nil otherwise. "]

  ["End of PARENTIZED at point"  ar-end-of-parentized-atpt
   :help " ‘ar-end-of-parentized-atpt’
  End of PARENTIZED at point if any, nil otherwise. "]

  ["End of DOUBLEQUOTED at point"  ar-end-of-doublequoted-atpt
   :help " ‘ar-end-of-doublequoted-atpt’
  End of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["End of SINGLEQUOTED at point"  ar-end-of-singlequoted-atpt
   :help " ‘ar-end-of-singlequoted-atpt’
  End of SINGLEQUOTED at point if any, nil otherwise. "]

  ["End of TRIPLEQUOTED at point"  ar-end-of-triplequoted-atpt
   :help " ‘ar-end-of-triplequoted-atpt’
  End of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["End of LESSER-ANGLED at point"  ar-end-of-lesserangled-atpt
   :help " ‘ar-end-of-lesserangled-atpt’
  End of LESSER-ANGLED at point if any, nil otherwise. "]

)
 ("Delimited-2"

  ["End of TRIPLEQUOTED-DQ at point"  ar-end-of-triplequoteddq-atpt
   :help " ‘ar-end-of-triplequoteddq-atpt’
  End of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["End of TRIPLEQUOTE-SQ at point"  ar-end-of-triplequotesq-atpt
   :help " ‘ar-end-of-triplequotesq-atpt’
  End of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["End of GREATER-ANGLED at point"  ar-end-of-greaterangled-atpt
   :help " ‘ar-end-of-greaterangled-atpt’
  End of GREATER-ANGLED at point if any, nil otherwise. "]

  ["End of LEFT-RIGHT-SINGLEQUOTED at point"  ar-end-of-curvedsinglequoted-atpt
   :help " ‘ar-end-of-curvedsinglequoted-atpt’
  End of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["End of BACKSLASHED at point"  ar-end-of-backslashed-atpt
   :help " ‘ar-end-of-backslashed-atpt’
  End of BACKSLASHED at point if any, nil otherwise. "]

  ["End of DOLLARED at point"  ar-end-of-dollared-atpt
   :help " ‘ar-end-of-dollared-atpt’
  End of DOLLARED at point if any, nil otherwise. "]

  ["End of DOUBLEQUOTED at point"  ar-end-of-doublequoted-atpt
   :help " ‘ar-end-of-doublequoted-atpt’
  End of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["End of EQUALIZED at point"  ar-end-of-equalized-atpt
   :help " ‘ar-end-of-equalized-atpt’
  End of EQUALIZED at point if any, nil otherwise. "]

  ["End of HYPHENED at point"  ar-end-of-hyphened-atpt
   :help " ‘ar-end-of-hyphened-atpt’
  End of HYPHENED at point if any, nil otherwise. "]

  ["End of SINGLEQUOTED at point"  ar-end-of-singlequoted-atpt
   :help " ‘ar-end-of-singlequoted-atpt’
  End of SINGLEQUOTED at point if any, nil otherwise. "]

  ["End of SLASHED at point"  ar-end-of-slashed-atpt
   :help " ‘ar-end-of-slashed-atpt’
  End of SLASHED at point if any, nil otherwise. "]

  ["End of UNDERSCORED at point"  ar-end-of-underscored-atpt
   :help " ‘ar-end-of-underscored-atpt’
  End of UNDERSCORED at point if any, nil otherwise. "]

  ["End of WHITESPACED at point"  ar-end-of-whitespaced-atpt
   :help " ‘ar-end-of-whitespaced-atpt’
  End of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["End of [:alnum:] char class at point"  ar-end-of-alnum-atpt
   :help " ‘ar-end-of-alnum-atpt’
   Return end position of ALNUM at point if succesful, nil otherwise. "]

  ["End of [:alpha:] char class at point"  ar-end-of-alpha-atpt
   :help " ‘ar-end-of-alpha-atpt’
   Return end position of ALPHA at point if succesful, nil otherwise. "]

  ["End of [:ascii:] char class at point"  ar-end-of-ascii-atpt
   :help " ‘ar-end-of-ascii-atpt’
   Return end position of ASCII at point if succesful, nil otherwise. "]

  ["End of [:blank:] char class at point"  ar-end-of-blank-atpt
   :help " ‘ar-end-of-blank-atpt’
   Return end position of BLANK at point if succesful, nil otherwise. "]

  ["End of [:cntrl:] char class at point"  ar-end-of-cntrl-atpt
   :help " ‘ar-end-of-cntrl-atpt’
   Return end position of CNTRL at point if succesful, nil otherwise. "]

  ["End of [:digit:] char class at point"  ar-end-of-digit-atpt
   :help " ‘ar-end-of-digit-atpt’
   Return end position of DIGIT at point if succesful, nil otherwise. "]

  ["End of [:graph:] char class at point"  ar-end-of-graph-atpt
   :help " ‘ar-end-of-graph-atpt’
   Return end position of GRAPH at point if succesful, nil otherwise. "]

  ["End of [:lower:] char class at point"  ar-end-of-lower-atpt
   :help " ‘ar-end-of-lower-atpt’
   Return end position of LOWER at point if succesful, nil otherwise. "]

  ["End of [:nonascii:] char class at point"  ar-end-of-nonascii-atpt
   :help " ‘ar-end-of-nonascii-atpt’
   Return end position of NONASCII at point if succesful, nil otherwise. "]

  ["End of [:print:] char class at point"  ar-end-of-print-atpt
   :help " ‘ar-end-of-print-atpt’
   Return end position of PRINT at point if succesful, nil otherwise. "]

  ["End of [:punct:] char class at point"  ar-end-of-punct-atpt
   :help " ‘ar-end-of-punct-atpt’
   Return end position of PUNCT at point if succesful, nil otherwise. "]

  ["End of [:space:] char class at point"  ar-end-of-space-atpt
   :help " ‘ar-end-of-space-atpt’
   Return end position of SPACE at point if succesful, nil otherwise. "]

  ["End of [:upper:] char class at point"  ar-end-of-upper-atpt
   :help " ‘ar-end-of-upper-atpt’
   Return end position of UPPER at point if succesful, nil otherwise. "]

  ["End of [:xdigit:] char class at point"  ar-end-of-xdigit-atpt
   :help " ‘ar-end-of-xdigit-atpt’
   Return end position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Forward ANGLED-NO-NEST at point"  ar-end-ofanglednonest-atpt
   :help " ‘ar-end-ofanglednonest-atpt’
   Return end position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Forward GREATER-ANGLED-NESTED at point"  ar-end-of-greateranglednested-atpt
   :help " ‘ar-end-of-greateranglednested-atpt’
   Return end position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Forward LESSER-ANGLED-NESTED at point"  ar-end-of-lesseranglednested-atpt
   :help " ‘ar-end-of-lesseranglednested-atpt’
   Return end position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Forward BUFFER at point"  ar-end-of-buffer-atpt
   :help " ‘ar-end-of-buffer-atpt’
   Return end position of BUFFER at point if succesful, nil otherwise. "]

  ["Forward COMMENT at point"  ar-end-of-comment-atpt
   :help " ‘ar-end-of-comment-atpt’
   Return end position of COMMENT at point if succesful, nil otherwise. "]

  ["Forward CSV at point"  ar-end-of-csv-atpt
   :help " ‘ar-end-of-csv-atpt’
   Return end position of CSV at point if succesful, nil otherwise. "]

  ["Forward DATE at point"  ar-end-of-date-atpt
   :help " ‘ar-end-of-date-atpt’
   Return end position of DATE at point if succesful, nil otherwise. "]

  ["Forward DEFUN at point"  ar-end-of-defun-atpt
   :help " ‘ar-end-of-defun-atpt’
   Return end position of DEFUN at point if succesful, nil otherwise. "]

  ["Forward DELIMITED at point"  ar-end-of-delimited-atpt
   :help " ‘ar-end-of-delimited-atpt’
   Return end position of DELIMITED at point if succesful, nil otherwise. "]

  ["Forward EMAIL at point"  ar-end-of-email-atpt
   :help " ‘ar-end-of-email-atpt’
   Return end position of EMAIL at point if succesful, nil otherwise. "]

  ["Forward FILENAME at point"  ar-end-of-filename-atpt
   :help " ‘ar-end-of-filename-atpt’
   Return end position of FILENAME at point if succesful, nil otherwise. "]

  ["Forward FLOAT at point"  ar-end-of-float-atpt
   :help " ‘ar-end-of-float-atpt’
   Return end position of FLOAT at point if succesful, nil otherwise. "]

  ["Forward FUNCTION at point"  ar-end-of-function-atpt
   :help " ‘ar-end-of-function-atpt’
   Return end position of FUNCTION at point if succesful, nil otherwise. "]

  ["Forward IP at point"  ar-end-of-ip-atpt
   :help " ‘ar-end-of-ip-atpt’
   Return end position of IP at point if succesful, nil otherwise. "]

  ["Forward ISBN at point"  ar-end-of-isbn-atpt
   :help " ‘ar-end-of-isbn-atpt’
   Return end position of ISBN at point if succesful, nil otherwise. "]

  ["Forward LINE at point"  ar-end-of-line-atpt
   :help " ‘ar-end-of-line-atpt’
   Return end position of LINE at point if succesful, nil otherwise. "]

  ["Forward NAME at point"  ar-end-of-name-atpt
   :help " ‘ar-end-of-name-atpt’
   Return end position of NAME at point if succesful, nil otherwise. "]

  ["Forward NUMBER at point"  ar-end-of-number-atpt
   :help " ‘ar-end-of-number-atpt’
   Return end position of NUMBER at point if succesful, nil otherwise. "]

  ["Forward PAGE at point"  ar-end-of-page-atpt
   :help " ‘ar-end-of-page-atpt’
   Return end position of PAGE at point if succesful, nil otherwise. "]

  ["Forward PARAGRAPH at point"  ar-end-of-paragraph-atpt
   :help " ‘ar-end-of-paragraph-atpt’
   Return end position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Forward PAREN at point"  ar-end-of-paren-atpt
   :help " ‘ar-end-of-paren-atpt’
   Return end position of PAREN at point if succesful, nil otherwise. "]

  ["Forward PHONE at point"  ar-end-of-phone-atpt
   :help " ‘ar-end-of-phone-atpt’
   Return end position of PHONE at point if succesful, nil otherwise. "]

  ["Forward REGION at point"  ar-end-of-region-atpt
   :help " ‘ar-end-of-region-atpt’
   Return end position of REGION at point if succesful, nil otherwise. "]

  ["Forward SENTENCE at point"  ar-end-of-sentence-atpt
   :help " ‘ar-end-of-sentence-atpt’
   Return end position of SENTENCE at point if succesful, nil otherwise. "]

  ["Forward SEXP at point"  ar-end-of-sexp-atpt
   :help " ‘ar-end-of-sexp-atpt’
   Return end position of SEXP at point if succesful, nil otherwise. "]

  ["Forward STRING at point"  ar-end-of-string-atpt
   :help " ‘ar-end-of-string-atpt’
   Return end position of STRING at point if succesful, nil otherwise. "]

  ["Forward SH-STRUCT at point"  ar-end-of-shstruct-atpt
   :help " ‘ar-end-of-shstruct-atpt’
   Return end position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Forward SYMBOL at point"  ar-end-of-symbol-atpt
   :help " ‘ar-end-of-symbol-atpt’
   Return end position of SYMBOL at point if succesful, nil otherwise. "]

  ["Forward URL at point"  ar-end-of-url-atpt
   :help " ‘ar-end-of-url-atpt’
   Return end position of URL at point if succesful, nil otherwise. "]

  ["Forward WORD at point"  ar-end-of-word-atpt
   :help " ‘ar-end-of-word-atpt’
   Return end position of WORD at point if succesful, nil otherwise. "]

  ["Forward WORD-ALPHA-ONLY at point"  ar-end-of-wordalphaonly-atpt
   :help " ‘ar-end-of-wordalphaonly-atpt’
   Return end position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

)
 ("Beginning"
 ("Delimited-1"

  ["Beginning of BRACED at point"  ar-beginning-of-braced-atpt
   :help " ‘ar-beginning-of-braced-atpt’
  Beginning of BRACED at point if any, nil otherwise. "]

  ["Beginning of BRACKETED at point"  ar-beginning-of-bracketed-atpt
   :help " ‘ar-beginning-of-bracketed-atpt’
  Beginning of BRACKETED at point if any, nil otherwise. "]

  ["Beginning of PARENTIZED at point"  ar-beginning-of-parentized-atpt
   :help " ‘ar-beginning-of-parentized-atpt’
  Beginning of PARENTIZED at point if any, nil otherwise. "]

  ["Beginning of DOUBLEQUOTED at point"  ar-beginning-of-doublequoted-atpt
   :help " ‘ar-beginning-of-doublequoted-atpt’
  Beginning of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of SINGLEQUOTED at point"  ar-beginning-of-singlequoted-atpt
   :help " ‘ar-beginning-of-singlequoted-atpt’
  Beginning of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of TRIPLEQUOTED at point"  ar-beginning-of-triplequoted-atpt
   :help " ‘ar-beginning-of-triplequoted-atpt’
  Beginning of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of LESSER-ANGLED at point"  ar-beginning-of-lesserangled-atpt
   :help " ‘ar-beginning-of-lesserangled-atpt’
  Beginning of LESSER-ANGLED at point if any, nil otherwise. "]

)
 ("Delimited-2"

  ["Beginning of TRIPLEQUOTED-DQ at point"  ar-beginning-of-triplequoteddq-atpt
   :help " ‘ar-beginning-of-triplequoteddq-atpt’
  Beginning of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Beginning of TRIPLEQUOTE-SQ at point"  ar-beginning-of-triplequotesq-atpt
   :help " ‘ar-beginning-of-triplequotesq-atpt’
  Beginning of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Beginning of GREATER-ANGLED at point"  ar-beginning-of-greaterangled-atpt
   :help " ‘ar-beginning-of-greaterangled-atpt’
  Beginning of GREATER-ANGLED at point if any, nil otherwise. "]

  ["Beginning of LEFT-RIGHT-SINGLEQUOTED at point"  ar-beginning-of-curvedsinglequoted-atpt
   :help " ‘ar-beginning-of-curvedsinglequoted-atpt’
  Beginning of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of BACKSLASHED at point"  ar-beginning-of-backslashed-atpt
   :help " ‘ar-beginning-of-backslashed-atpt’
  Beginning of BACKSLASHED at point if any, nil otherwise. "]

  ["Beginning of DOLLARED at point"  ar-beginning-of-dollared-atpt
   :help " ‘ar-beginning-of-dollared-atpt’
  Beginning of DOLLARED at point if any, nil otherwise. "]

  ["Beginning of DOUBLEQUOTED at point"  ar-beginning-of-doublequoted-atpt
   :help " ‘ar-beginning-of-doublequoted-atpt’
  Beginning of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of EQUALIZED at point"  ar-beginning-of-equalized-atpt
   :help " ‘ar-beginning-of-equalized-atpt’
  Beginning of EQUALIZED at point if any, nil otherwise. "]

  ["Beginning of HYPHENED at point"  ar-beginning-of-hyphened-atpt
   :help " ‘ar-beginning-of-hyphened-atpt’
  Beginning of HYPHENED at point if any, nil otherwise. "]

  ["Beginning of SINGLEQUOTED at point"  ar-beginning-of-singlequoted-atpt
   :help " ‘ar-beginning-of-singlequoted-atpt’
  Beginning of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning of SLASHED at point"  ar-beginning-of-slashed-atpt
   :help " ‘ar-beginning-of-slashed-atpt’
  Beginning of SLASHED at point if any, nil otherwise. "]

  ["Beginning of UNDERSCORED at point"  ar-beginning-of-underscored-atpt
   :help " ‘ar-beginning-of-underscored-atpt’
  Beginning of UNDERSCORED at point if any, nil otherwise. "]

  ["Beginning of WHITESPACED at point"  ar-beginning-of-whitespaced-atpt
   :help " ‘ar-beginning-of-whitespaced-atpt’
  Beginning of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Beginning of [:alnum:] char class at point"  ar-beginning-of-alnum-atpt
   :help " ‘ar-beginning-of-alnum-atpt’
   Return beginning position of ALNUM at point if succesful, nil otherwise. "]

  ["Beginning of [:alpha:] char class at point"  ar-beginning-of-alpha-atpt
   :help " ‘ar-beginning-of-alpha-atpt’
   Return beginning position of ALPHA at point if succesful, nil otherwise. "]

  ["Beginning of [:ascii:] char class at point"  ar-beginning-of-ascii-atpt
   :help " ‘ar-beginning-of-ascii-atpt’
   Return beginning position of ASCII at point if succesful, nil otherwise. "]

  ["Beginning of [:blank:] char class at point"  ar-beginning-of-blank-atpt
   :help " ‘ar-beginning-of-blank-atpt’
   Return beginning position of BLANK at point if succesful, nil otherwise. "]

  ["Beginning of [:cntrl:] char class at point"  ar-beginning-of-cntrl-atpt
   :help " ‘ar-beginning-of-cntrl-atpt’
   Return beginning position of CNTRL at point if succesful, nil otherwise. "]

  ["Beginning of [:digit:] char class at point"  ar-beginning-of-digit-atpt
   :help " ‘ar-beginning-of-digit-atpt’
   Return beginning position of DIGIT at point if succesful, nil otherwise. "]

  ["Beginning of [:graph:] char class at point"  ar-beginning-of-graph-atpt
   :help " ‘ar-beginning-of-graph-atpt’
   Return beginning position of GRAPH at point if succesful, nil otherwise. "]

  ["Beginning of [:lower:] char class at point"  ar-beginning-of-lower-atpt
   :help " ‘ar-beginning-of-lower-atpt’
   Return beginning position of LOWER at point if succesful, nil otherwise. "]

  ["Beginning of [:nonascii:] char class at point"  ar-beginning-of-nonascii-atpt
   :help " ‘ar-beginning-of-nonascii-atpt’
   Return beginning position of NONASCII at point if succesful, nil otherwise. "]

  ["Beginning of [:print:] char class at point"  ar-beginning-of-print-atpt
   :help " ‘ar-beginning-of-print-atpt’
   Return beginning position of PRINT at point if succesful, nil otherwise. "]

  ["Beginning of [:punct:] char class at point"  ar-beginning-of-punct-atpt
   :help " ‘ar-beginning-of-punct-atpt’
   Return beginning position of PUNCT at point if succesful, nil otherwise. "]

  ["Beginning of [:space:] char class at point"  ar-beginning-of-space-atpt
   :help " ‘ar-beginning-of-space-atpt’
   Return beginning position of SPACE at point if succesful, nil otherwise. "]

  ["Beginning of [:upper:] char class at point"  ar-beginning-of-upper-atpt
   :help " ‘ar-beginning-of-upper-atpt’
   Return beginning position of UPPER at point if succesful, nil otherwise. "]

  ["Beginning of [:xdigit:] char class at point"  ar-beginning-of-xdigit-atpt
   :help " ‘ar-beginning-of-xdigit-atpt’
   Return beginning position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Backward ANGLED-NO-NEST at point"  ar-beginning-ofanglednonest-atpt
   :help " ‘ar-beginning-ofanglednonest-atpt’
   Return beginning position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Backward GREATER-ANGLED-NESTED at point"  ar-beginning-of-greateranglednested-atpt
   :help " ‘ar-beginning-of-greateranglednested-atpt’
   Return beginning position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Backward LESSER-ANGLED-NESTED at point"  ar-beginning-of-lesseranglednested-atpt
   :help " ‘ar-beginning-of-lesseranglednested-atpt’
   Return beginning position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Backward BUFFER at point"  ar-beginning-of-buffer-atpt
   :help " ‘ar-beginning-of-buffer-atpt’
   Return beginning position of BUFFER at point if succesful, nil otherwise. "]

  ["Backward COMMENT at point"  ar-beginning-of-comment-atpt
   :help " ‘ar-beginning-of-comment-atpt’
   Return beginning position of COMMENT at point if succesful, nil otherwise. "]

  ["Backward CSV at point"  ar-beginning-of-csv-atpt
   :help " ‘ar-beginning-of-csv-atpt’
   Return beginning position of CSV at point if succesful, nil otherwise. "]

  ["Backward DATE at point"  ar-beginning-of-date-atpt
   :help " ‘ar-beginning-of-date-atpt’
   Return beginning position of DATE at point if succesful, nil otherwise. "]

  ["Backward DEFUN at point"  ar-beginning-of-defun-atpt
   :help " ‘ar-beginning-of-defun-atpt’
   Return beginning position of DEFUN at point if succesful, nil otherwise. "]

  ["Backward DELIMITED at point"  ar-beginning-of-delimited-atpt
   :help " ‘ar-beginning-of-delimited-atpt’
   Return beginning position of DELIMITED at point if succesful, nil otherwise. "]

  ["Backward EMAIL at point"  ar-beginning-of-email-atpt
   :help " ‘ar-beginning-of-email-atpt’
   Return beginning position of EMAIL at point if succesful, nil otherwise. "]

  ["Backward FILENAME at point"  ar-beginning-of-filename-atpt
   :help " ‘ar-beginning-of-filename-atpt’
   Return beginning position of FILENAME at point if succesful, nil otherwise. "]

  ["Backward FLOAT at point"  ar-beginning-of-float-atpt
   :help " ‘ar-beginning-of-float-atpt’
   Return beginning position of FLOAT at point if succesful, nil otherwise. "]

  ["Backward FUNCTION at point"  ar-beginning-of-function-atpt
   :help " ‘ar-beginning-of-function-atpt’
   Return beginning position of FUNCTION at point if succesful, nil otherwise. "]

  ["Backward IP at point"  ar-beginning-of-ip-atpt
   :help " ‘ar-beginning-of-ip-atpt’
   Return beginning position of IP at point if succesful, nil otherwise. "]

  ["Backward ISBN at point"  ar-beginning-of-isbn-atpt
   :help " ‘ar-beginning-of-isbn-atpt’
   Return beginning position of ISBN at point if succesful, nil otherwise. "]

  ["Backward LINE at point"  ar-beginning-of-line-atpt
   :help " ‘ar-beginning-of-line-atpt’
   Return beginning position of LINE at point if succesful, nil otherwise. "]

  ["Backward NAME at point"  ar-beginning-of-name-atpt
   :help " ‘ar-beginning-of-name-atpt’
   Return beginning position of NAME at point if succesful, nil otherwise. "]

  ["Backward NUMBER at point"  ar-beginning-of-number-atpt
   :help " ‘ar-beginning-of-number-atpt’
   Return beginning position of NUMBER at point if succesful, nil otherwise. "]

  ["Backward PAGE at point"  ar-beginning-of-page-atpt
   :help " ‘ar-beginning-of-page-atpt’
   Return beginning position of PAGE at point if succesful, nil otherwise. "]

  ["Backward PARAGRAPH at point"  ar-beginning-of-paragraph-atpt
   :help " ‘ar-beginning-of-paragraph-atpt’
   Return beginning position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Backward PAREN at point"  ar-beginning-of-paren-atpt
   :help " ‘ar-beginning-of-paren-atpt’
   Return beginning position of PAREN at point if succesful, nil otherwise. "]

  ["Backward PHONE at point"  ar-beginning-of-phone-atpt
   :help " ‘ar-beginning-of-phone-atpt’
   Return beginning position of PHONE at point if succesful, nil otherwise. "]

  ["Backward REGION at point"  ar-beginning-of-region-atpt
   :help " ‘ar-beginning-of-region-atpt’
   Return beginning position of REGION at point if succesful, nil otherwise. "]

  ["Backward SENTENCE at point"  ar-beginning-of-sentence-atpt
   :help " ‘ar-beginning-of-sentence-atpt’
   Return beginning position of SENTENCE at point if succesful, nil otherwise. "]

  ["Backward SEXP at point"  ar-beginning-of-sexp-atpt
   :help " ‘ar-beginning-of-sexp-atpt’
   Return beginning position of SEXP at point if succesful, nil otherwise. "]

  ["Backward STRING at point"  ar-beginning-of-string-atpt
   :help " ‘ar-beginning-of-string-atpt’
   Return beginning position of STRING at point if succesful, nil otherwise. "]

  ["Backward SH-STRUCT at point"  ar-beginning-of-shstruct-atpt
   :help " ‘ar-beginning-of-shstruct-atpt’
   Return beginning position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Backward SYMBOL at point"  ar-beginning-of-symbol-atpt
   :help " ‘ar-beginning-of-symbol-atpt’
   Return beginning position of SYMBOL at point if succesful, nil otherwise. "]

  ["Backward URL at point"  ar-beginning-of-url-atpt
   :help " ‘ar-beginning-of-url-atpt’
   Return beginning position of URL at point if succesful, nil otherwise. "]

  ["Backward WORD at point"  ar-beginning-of-word-atpt
   :help " ‘ar-beginning-of-word-atpt’
   Return beginning position of WORD at point if succesful, nil otherwise. "]

  ["Backward WORD-ALPHA-ONLY at point"  ar-beginning-of-wordalphaonly-atpt
   :help " ‘ar-beginning-of-wordalphaonly-atpt’
   Return beginning position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

)

  )
("Positions"
("Bounds"
 ("Delimited"

  ["Bounds of BRACED at point"  ar-bounds-of-braced-atpt
   :help " ‘ar-bounds-of-braced-atpt’
   Bounds of BRACED at point if any, nil otherwise. "]

  ["Bounds of BRACKETED at point"  ar-bounds-of-bracketed-atpt
   :help " ‘ar-bounds-of-bracketed-atpt’
   Bounds of BRACKETED at point if any, nil otherwise. "]

  ["Bounds of PARENTIZED at point"  ar-bounds-of-parentized-atpt
   :help " ‘ar-bounds-of-parentized-atpt’
   Bounds of PARENTIZED at point if any, nil otherwise. "]

  ["Bounds of DOUBLEQUOTED at point"  ar-bounds-of-doublequoted-atpt
   :help " ‘ar-bounds-of-doublequoted-atpt’
   Bounds of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of SINGLEQUOTED at point"  ar-bounds-of-singlequoted-atpt
   :help " ‘ar-bounds-of-singlequoted-atpt’
   Bounds of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of TRIPLEQUOTED at point"  ar-bounds-of-triplequoted-atpt
   :help " ‘ar-bounds-of-triplequoted-atpt’
   Bounds of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of LESSER-ANGLED at point"  ar-bounds-of-lesserangled-atpt
   :help " ‘ar-bounds-of-lesserangled-atpt’
   Bounds of LESSER-ANGLED at point if any, nil otherwise. "]

  "-"

  ["Bounds of TRIPLEQUOTED-DQ at point"  ar-bounds-of-triplequoteddq-atpt
   :help " ‘ar-bounds-of-triplequoteddq-atpt’
   Bounds of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Bounds of TRIPLEQUOTE-SQ at point"  ar-bounds-of-triplequotesq-atpt
   :help " ‘ar-bounds-of-triplequotesq-atpt’
   Bounds of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Bounds of GREATER-ANGLED at point"  ar-bounds-of-greaterangled-atpt
   :help " ‘ar-bounds-of-greaterangled-atpt’
   Bounds of GREATER-ANGLED at point if any, nil otherwise. "]

  ["Bounds of LEFT-RIGHT-SINGLEQUOTED at point"  ar-bounds-of-curvedsinglequoted-atpt
   :help " ‘ar-bounds-of-curvedsinglequoted-atpt’
   Bounds of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of BACKSLASHED at point"  ar-bounds-of-backslashed-atpt
   :help " ‘ar-bounds-of-backslashed-atpt’
   Bounds of BACKSLASHED at point if any, nil otherwise. "]

  ["Bounds of DOLLARED at point"  ar-bounds-of-dollared-atpt
   :help " ‘ar-bounds-of-dollared-atpt’
   Bounds of DOLLARED at point if any, nil otherwise. "]

  ["Bounds of DOUBLEQUOTED at point"  ar-bounds-of-doublequoted-atpt
   :help " ‘ar-bounds-of-doublequoted-atpt’
   Bounds of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of EQUALIZED at point"  ar-bounds-of-equalized-atpt
   :help " ‘ar-bounds-of-equalized-atpt’
   Bounds of EQUALIZED at point if any, nil otherwise. "]

  ["Bounds of HYPHENED at point"  ar-bounds-of-hyphened-atpt
   :help " ‘ar-bounds-of-hyphened-atpt’
   Bounds of HYPHENED at point if any, nil otherwise. "]

  ["Bounds of SINGLEQUOTED at point"  ar-bounds-of-singlequoted-atpt
   :help " ‘ar-bounds-of-singlequoted-atpt’
   Bounds of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Bounds of SLASHED at point"  ar-bounds-of-slashed-atpt
   :help " ‘ar-bounds-of-slashed-atpt’
   Bounds of SLASHED at point if any, nil otherwise. "]

  ["Bounds of UNDERSCORED at point"  ar-bounds-of-underscored-atpt
   :help " ‘ar-bounds-of-underscored-atpt’
   Bounds of UNDERSCORED at point if any, nil otherwise. "]

  ["Bounds of WHITESPACED at point"  ar-bounds-of-whitespaced-atpt
   :help " ‘ar-bounds-of-whitespaced-atpt’
   Bounds of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Bounds of [:alnum:] char class at point"  ar-bounds-of-alnum-atpt
   :help " ‘ar-bounds-of-alnum-atpt’
   Return bounds of ALNUM at point if succesful, nil otherwise. "]

  ["Bounds of [:alpha:] char class at point"  ar-bounds-of-alpha-atpt
   :help " ‘ar-bounds-of-alpha-atpt’
   Return bounds of ALPHA at point if succesful, nil otherwise. "]

  ["Bounds of [:ascii:] char class at point"  ar-bounds-of-ascii-atpt
   :help " ‘ar-bounds-of-ascii-atpt’
   Return bounds of ASCII at point if succesful, nil otherwise. "]

  ["Bounds of [:blank:] char class at point"  ar-bounds-of-blank-atpt
   :help " ‘ar-bounds-of-blank-atpt’
   Return bounds of BLANK at point if succesful, nil otherwise. "]

  ["Bounds of [:cntrl:] char class at point"  ar-bounds-of-cntrl-atpt
   :help " ‘ar-bounds-of-cntrl-atpt’
   Return bounds of CNTRL at point if succesful, nil otherwise. "]

  ["Bounds of [:digit:] char class at point"  ar-bounds-of-digit-atpt
   :help " ‘ar-bounds-of-digit-atpt’
   Return bounds of DIGIT at point if succesful, nil otherwise. "]

  ["Bounds of [:graph:] char class at point"  ar-bounds-of-graph-atpt
   :help " ‘ar-bounds-of-graph-atpt’
   Return bounds of GRAPH at point if succesful, nil otherwise. "]

  ["Bounds of [:lower:] char class at point"  ar-bounds-of-lower-atpt
   :help " ‘ar-bounds-of-lower-atpt’
   Return bounds of LOWER at point if succesful, nil otherwise. "]

  ["Bounds of [:nonascii:] char class at point"  ar-bounds-of-nonascii-atpt
   :help " ‘ar-bounds-of-nonascii-atpt’
   Return bounds of NONASCII at point if succesful, nil otherwise. "]

  ["Bounds of [:print:] char class at point"  ar-bounds-of-print-atpt
   :help " ‘ar-bounds-of-print-atpt’
   Return bounds of PRINT at point if succesful, nil otherwise. "]

  ["Bounds of [:punct:] char class at point"  ar-bounds-of-punct-atpt
   :help " ‘ar-bounds-of-punct-atpt’
   Return bounds of PUNCT at point if succesful, nil otherwise. "]

  ["Bounds of [:space:] char class at point"  ar-bounds-of-space-atpt
   :help " ‘ar-bounds-of-space-atpt’
   Return bounds of SPACE at point if succesful, nil otherwise. "]

  ["Bounds of [:upper:] char class at point"  ar-bounds-of-upper-atpt
   :help " ‘ar-bounds-of-upper-atpt’
   Return bounds of UPPER at point if succesful, nil otherwise. "]

  ["Bounds of [:xdigit:] char class at point"  ar-bounds-of-xdigit-atpt
   :help " ‘ar-bounds-of-xdigit-atpt’
   Return bounds of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Bounds of ANGLED-NO-NEST at point"  ar-bounds-ofanglednonest-atpt
   :help " ‘ar-bounds-ofanglednonest-atpt’
   Return bounds of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Bounds of GREATER-ANGLED-NESTED at point"  ar-bounds-of-greateranglednested-atpt
   :help " ‘ar-bounds-of-greateranglednested-atpt’
   Return bounds of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Bounds of LESSER-ANGLED-NESTED at point"  ar-bounds-of-lesseranglednested-atpt
   :help " ‘ar-bounds-of-lesseranglednested-atpt’
   Return bounds of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Bounds of BUFFER at point"  ar-bounds-of-buffer-atpt
   :help " ‘ar-bounds-of-buffer-atpt’
   Return bounds of BUFFER at point if succesful, nil otherwise. "]

  ["Bounds of COMMENT at point"  ar-bounds-of-comment-atpt
   :help " ‘ar-bounds-of-comment-atpt’
   Return bounds of COMMENT at point if succesful, nil otherwise. "]

  ["Bounds of CSV at point"  ar-bounds-of-csv-atpt
   :help " ‘ar-bounds-of-csv-atpt’
   Return bounds of CSV at point if succesful, nil otherwise. "]

  ["Bounds of DATE at point"  ar-bounds-of-date-atpt
   :help " ‘ar-bounds-of-date-atpt’
   Return bounds of DATE at point if succesful, nil otherwise. "]

  ["Bounds of DEFUN at point"  ar-bounds-of-defun-atpt
   :help " ‘ar-bounds-of-defun-atpt’
   Return bounds of DEFUN at point if succesful, nil otherwise. "]

  ["Bounds of DELIMITED at point"  ar-bounds-of-delimited-atpt
   :help " ‘ar-bounds-of-delimited-atpt’
   Return bounds of DELIMITED at point if succesful, nil otherwise. "]

  ["Bounds of EMAIL at point"  ar-bounds-of-email-atpt
   :help " ‘ar-bounds-of-email-atpt’
   Return bounds of EMAIL at point if succesful, nil otherwise. "]

  ["Bounds of FILENAME at point"  ar-bounds-of-filename-atpt
   :help " ‘ar-bounds-of-filename-atpt’
   Return bounds of FILENAME at point if succesful, nil otherwise. "]

  ["Bounds of FLOAT at point"  ar-bounds-of-float-atpt
   :help " ‘ar-bounds-of-float-atpt’
   Return bounds of FLOAT at point if succesful, nil otherwise. "]

  ["Bounds of FUNCTION at point"  ar-bounds-of-function-atpt
   :help " ‘ar-bounds-of-function-atpt’
   Return bounds of FUNCTION at point if succesful, nil otherwise. "]

  ["Bounds of IP at point"  ar-bounds-of-ip-atpt
   :help " ‘ar-bounds-of-ip-atpt’
   Return bounds of IP at point if succesful, nil otherwise. "]

  ["Bounds of ISBN at point"  ar-bounds-of-isbn-atpt
   :help " ‘ar-bounds-of-isbn-atpt’
   Return bounds of ISBN at point if succesful, nil otherwise. "]

  ["Bounds of LINE at point"  ar-bounds-of-line-atpt
   :help " ‘ar-bounds-of-line-atpt’
   Return bounds of LINE at point if succesful, nil otherwise. "]

  ["Bounds of NAME at point"  ar-bounds-of-name-atpt
   :help " ‘ar-bounds-of-name-atpt’
   Return bounds of NAME at point if succesful, nil otherwise. "]

  ["Bounds of NUMBER at point"  ar-bounds-of-number-atpt
   :help " ‘ar-bounds-of-number-atpt’
   Return bounds of NUMBER at point if succesful, nil otherwise. "]

  ["Bounds of PAGE at point"  ar-bounds-of-page-atpt
   :help " ‘ar-bounds-of-page-atpt’
   Return bounds of PAGE at point if succesful, nil otherwise. "]

  ["Bounds of PARAGRAPH at point"  ar-bounds-of-paragraph-atpt
   :help " ‘ar-bounds-of-paragraph-atpt’
   Return bounds of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Bounds of PAREN at point"  ar-bounds-of-paren-atpt
   :help " ‘ar-bounds-of-paren-atpt’
   Return bounds of PAREN at point if succesful, nil otherwise. "]

  ["Bounds of PHONE at point"  ar-bounds-of-phone-atpt
   :help " ‘ar-bounds-of-phone-atpt’
   Return bounds of PHONE at point if succesful, nil otherwise. "]

  ["Bounds of REGION at point"  ar-bounds-of-region-atpt
   :help " ‘ar-bounds-of-region-atpt’
   Return bounds of REGION at point if succesful, nil otherwise. "]

  ["Bounds of SENTENCE at point"  ar-bounds-of-sentence-atpt
   :help " ‘ar-bounds-of-sentence-atpt’
   Return bounds of SENTENCE at point if succesful, nil otherwise. "]

  ["Bounds of SEXP at point"  ar-bounds-of-sexp-atpt
   :help " ‘ar-bounds-of-sexp-atpt’
   Return bounds of SEXP at point if succesful, nil otherwise. "]

  ["Bounds of STRING at point"  ar-bounds-of-string-atpt
   :help " ‘ar-bounds-of-string-atpt’
   Return bounds of STRING at point if succesful, nil otherwise. "]

  ["Bounds of SH-STRUCT at point"  ar-bounds-of-shstruct-atpt
   :help " ‘ar-bounds-of-shstruct-atpt’
   Return bounds of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Bounds of SYMBOL at point"  ar-bounds-of-symbol-atpt
   :help " ‘ar-bounds-of-symbol-atpt’
   Return bounds of SYMBOL at point if succesful, nil otherwise. "]

  ["Bounds of URL at point"  ar-bounds-of-url-atpt
   :help " ‘ar-bounds-of-url-atpt’
   Return bounds of URL at point if succesful, nil otherwise. "]

  ["Bounds of WORD at point"  ar-bounds-of-word-atpt
   :help " ‘ar-bounds-of-word-atpt’
   Return bounds of WORD at point if succesful, nil otherwise. "]

  ["Bounds of WORD-ALPHA-ONLY at point"  ar-bounds-of-wordalphaonly-atpt
   :help " ‘ar-bounds-of-wordalphaonly-atpt’
   Return bounds of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

  )
 ("Beginning position"
 ("Delimited"

  ["Beginning position of BRACED at point"  ar-braced-beginning-position-atpt
   :help " ‘ar-braced-beginning-position-atpt’
   Beginning position of BRACED at point if any, nil otherwise. "]

  ["Beginning position of BRACKETED at point"  ar-bracketed-beginning-position-atpt
   :help " ‘ar-bracketed-beginning-position-atpt’
   Beginning position of BRACKETED at point if any, nil otherwise. "]

  ["Beginning position of PARENTIZED at point"  ar-parentized-beginning-position-atpt
   :help " ‘ar-parentized-beginning-position-atpt’
   Beginning position of PARENTIZED at point if any, nil otherwise. "]

  ["Beginning position of DOUBLEQUOTED at point"  ar-doublequoted-beginning-position-atpt
   :help " ‘ar-doublequoted-beginning-position-atpt’
   Beginning position of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of SINGLEQUOTED at point"  ar-singlequoted-beginning-position-atpt
   :help " ‘ar-singlequoted-beginning-position-atpt’
   Beginning position of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of TRIPLEQUOTED at point"  ar-triplequoted-beginning-position-atpt
   :help " ‘ar-triplequoted-beginning-position-atpt’
   Beginning position of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of LESSER-ANGLED at point"  ar-lesserangled-beginning-position-atpt
   :help " ‘ar-lesserangled-beginning-position-atpt’
   Beginning position of LESSER-ANGLED at point if any, nil otherwise. "]

 "-"

  ["Beginning position of TRIPLEQUOTED-DQ at point"  ar-triplequoteddq-beginning-position-atpt
   :help " ‘ar-triplequoteddq-beginning-position-atpt’
   Beginning position of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Beginning position of TRIPLEQUOTE-SQ at point"  ar-triplequotesq-beginning-position-atpt
   :help " ‘ar-triplequotesq-beginning-position-atpt’
   Beginning position of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Beginning position of GREATER-ANGLED at point"  ar-greaterangled-beginning-position-atpt
   :help " ‘ar-greaterangled-beginning-position-atpt’
   Beginning position of GREATER-ANGLED at point if any, nil otherwise. "]

  ["Beginning position of LEFT-RIGHT-SINGLEQUOTED at point"  ar-curvedsinglequoted-beginning-position-atpt
   :help " ‘ar-curvedsinglequoted-beginning-position-atpt’
   Beginning position of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of BACKSLASHED at point"  ar-backslashed-beginning-position-atpt
   :help " ‘ar-backslashed-beginning-position-atpt’
   Beginning position of BACKSLASHED at point if any, nil otherwise. "]

  ["Beginning position of DOLLARED at point"  ar-dollared-beginning-position-atpt
   :help " ‘ar-dollared-beginning-position-atpt’
   Beginning position of DOLLARED at point if any, nil otherwise. "]

  ["Beginning position of DOUBLEQUOTED at point"  ar-doublequoted-beginning-position-atpt
   :help " ‘ar-doublequoted-beginning-position-atpt’
   Beginning position of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of EQUALIZED at point"  ar-equalized-beginning-position-atpt
   :help " ‘ar-equalized-beginning-position-atpt’
   Beginning position of EQUALIZED at point if any, nil otherwise. "]

  ["Beginning position of HYPHENED at point"  ar-hyphened-beginning-position-atpt
   :help " ‘ar-hyphened-beginning-position-atpt’
   Beginning position of HYPHENED at point if any, nil otherwise. "]

  ["Beginning position of SINGLEQUOTED at point"  ar-singlequoted-beginning-position-atpt
   :help " ‘ar-singlequoted-beginning-position-atpt’
   Beginning position of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Beginning position of SLASHED at point"  ar-slashed-beginning-position-atpt
   :help " ‘ar-slashed-beginning-position-atpt’
   Beginning position of SLASHED at point if any, nil otherwise. "]

  ["Beginning position of UNDERSCORED at point"  ar-underscored-beginning-position-atpt
   :help " ‘ar-underscored-beginning-position-atpt’
   Beginning position of UNDERSCORED at point if any, nil otherwise. "]

  ["Beginning position of WHITESPACED at point"  ar-whitespaced-beginning-position-atpt
   :help " ‘ar-whitespaced-beginning-position-atpt’
   Beginning position of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Beginning position of [:alnum:] char class at point"  ar-alnum-beginning-position-atpt
   :help " ‘ar-alnum-beginning-position-atpt’
   Return beginning position of ALNUM at point if succesful, nil otherwise. "]

  ["Beginning position of [:alpha:] char class at point"  ar-alpha-beginning-position-atpt
   :help " ‘ar-alpha-beginning-position-atpt’
   Return beginning position of ALPHA at point if succesful, nil otherwise. "]

  ["Beginning position of [:ascii:] char class at point"  ar-ascii-beginning-position-atpt
   :help " ‘ar-ascii-beginning-position-atpt’
   Return beginning position of ASCII at point if succesful, nil otherwise. "]

  ["Beginning position of [:blank:] char class at point"  ar-blank-beginning-position-atpt
   :help " ‘ar-blank-beginning-position-atpt’
   Return beginning position of BLANK at point if succesful, nil otherwise. "]

  ["Beginning position of [:cntrl:] char class at point"  ar-cntrl-beginning-position-atpt
   :help " ‘ar-cntrl-beginning-position-atpt’
   Return beginning position of CNTRL at point if succesful, nil otherwise. "]

  ["Beginning position of [:digit:] char class at point"  ar-digit-beginning-position-atpt
   :help " ‘ar-digit-beginning-position-atpt’
   Return beginning position of DIGIT at point if succesful, nil otherwise. "]

  ["Beginning position of [:graph:] char class at point"  ar-graph-beginning-position-atpt
   :help " ‘ar-graph-beginning-position-atpt’
   Return beginning position of GRAPH at point if succesful, nil otherwise. "]

  ["Beginning position of [:lower:] char class at point"  ar-lower-beginning-position-atpt
   :help " ‘ar-lower-beginning-position-atpt’
   Return beginning position of LOWER at point if succesful, nil otherwise. "]

  ["Beginning position of [:nonascii:] char class at point"  ar-nonascii-beginning-position-atpt
   :help " ‘ar-nonascii-beginning-position-atpt’
   Return beginning position of NONASCII at point if succesful, nil otherwise. "]

  ["Beginning position of [:print:] char class at point"  ar-print-beginning-position-atpt
   :help " ‘ar-print-beginning-position-atpt’
   Return beginning position of PRINT at point if succesful, nil otherwise. "]

  ["Beginning position of [:punct:] char class at point"  ar-punct-beginning-position-atpt
   :help " ‘ar-punct-beginning-position-atpt’
   Return beginning position of PUNCT at point if succesful, nil otherwise. "]

  ["Beginning position of [:space:] char class at point"  ar-space-beginning-position-atpt
   :help " ‘ar-space-beginning-position-atpt’
   Return beginning position of SPACE at point if succesful, nil otherwise. "]

  ["Beginning position of [:upper:] char class at point"  ar-upper-beginning-position-atpt
   :help " ‘ar-upper-beginning-position-atpt’
   Return beginning position of UPPER at point if succesful, nil otherwise. "]

  ["Beginning position of [:xdigit:] char class at point"  ar-xdigit-beginning-position-atpt
   :help " ‘ar-xdigit-beginning-position-atpt’
   Return beginning position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Beginning position of ANGLED-NO-NEST at point"  ar-anglednonest-beginning-position-atpt
   :help " ‘ar-anglednonest-beginning-position-atpt’
   Return beginning position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Beginning position of GREATER-ANGLED-NESTED at point"  ar-greateranglednested-beginning-position-atpt
   :help " ‘ar-greateranglednested-beginning-position-atpt’
   Return beginning position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Beginning position of LESSER-ANGLED-NESTED at point"  ar-lesseranglednested-beginning-position-atpt
   :help " ‘ar-lesseranglednested-beginning-position-atpt’
   Return beginning position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Beginning position of BUFFER at point"  ar-buffer-beginning-position-atpt
   :help " ‘ar-buffer-beginning-position-atpt’
   Return beginning position of BUFFER at point if succesful, nil otherwise. "]

  ["Beginning position of COMMENT at point"  ar-comment-beginning-position-atpt
   :help " ‘ar-comment-beginning-position-atpt’
   Return beginning position of COMMENT at point if succesful, nil otherwise. "]

  ["Beginning position of CSV at point"  ar-csv-beginning-position-atpt
   :help " ‘ar-csv-beginning-position-atpt’
   Return beginning position of CSV at point if succesful, nil otherwise. "]

  ["Beginning position of DATE at point"  ar-date-beginning-position-atpt
   :help " ‘ar-date-beginning-position-atpt’
   Return beginning position of DATE at point if succesful, nil otherwise. "]

  ["Beginning position of DEFUN at point"  ar-defun-beginning-position-atpt
   :help " ‘ar-defun-beginning-position-atpt’
   Return beginning position of DEFUN at point if succesful, nil otherwise. "]

  ["Beginning position of DELIMITED at point"  ar-delimited-beginning-position-atpt
   :help " ‘ar-delimited-beginning-position-atpt’
   Return beginning position of DELIMITED at point if succesful, nil otherwise. "]

  ["Beginning position of EMAIL at point"  ar-email-beginning-position-atpt
   :help " ‘ar-email-beginning-position-atpt’
   Return beginning position of EMAIL at point if succesful, nil otherwise. "]

  ["Beginning position of FILENAME at point"  ar-filename-beginning-position-atpt
   :help " ‘ar-filename-beginning-position-atpt’
   Return beginning position of FILENAME at point if succesful, nil otherwise. "]

  ["Beginning position of FLOAT at point"  ar-float-beginning-position-atpt
   :help " ‘ar-float-beginning-position-atpt’
   Return beginning position of FLOAT at point if succesful, nil otherwise. "]

  ["Beginning position of FUNCTION at point"  ar-function-beginning-position-atpt
   :help " ‘ar-function-beginning-position-atpt’
   Return beginning position of FUNCTION at point if succesful, nil otherwise. "]

  ["Beginning position of IP at point"  ar-ip-beginning-position-atpt
   :help " ‘ar-ip-beginning-position-atpt’
   Return beginning position of IP at point if succesful, nil otherwise. "]

  ["Beginning position of ISBN at point"  ar-isbn-beginning-position-atpt
   :help " ‘ar-isbn-beginning-position-atpt’
   Return beginning position of ISBN at point if succesful, nil otherwise. "]

  ["Beginning position of LINE at point"  ar-line-beginning-position-atpt
   :help " ‘ar-line-beginning-position-atpt’
   Return beginning position of LINE at point if succesful, nil otherwise. "]

  ["Beginning position of NAME at point"  ar-name-beginning-position-atpt
   :help " ‘ar-name-beginning-position-atpt’
   Return beginning position of NAME at point if succesful, nil otherwise. "]

  ["Beginning position of NUMBER at point"  ar-number-beginning-position-atpt
   :help " ‘ar-number-beginning-position-atpt’
   Return beginning position of NUMBER at point if succesful, nil otherwise. "]

  ["Beginning position of PAGE at point"  ar-page-beginning-position-atpt
   :help " ‘ar-page-beginning-position-atpt’
   Return beginning position of PAGE at point if succesful, nil otherwise. "]

  ["Beginning position of PARAGRAPH at point"  ar-paragraph-beginning-position-atpt
   :help " ‘ar-paragraph-beginning-position-atpt’
   Return beginning position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Beginning position of PAREN at point"  ar-paren-beginning-position-atpt
   :help " ‘ar-paren-beginning-position-atpt’
   Return beginning position of PAREN at point if succesful, nil otherwise. "]

  ["Beginning position of PHONE at point"  ar-phone-beginning-position-atpt
   :help " ‘ar-phone-beginning-position-atpt’
   Return beginning position of PHONE at point if succesful, nil otherwise. "]

  ["Beginning position of REGION at point"  ar-region-beginning-position-atpt
   :help " ‘ar-region-beginning-position-atpt’
   Return beginning position of REGION at point if succesful, nil otherwise. "]

  ["Beginning position of SENTENCE at point"  ar-sentence-beginning-position-atpt
   :help " ‘ar-sentence-beginning-position-atpt’
   Return beginning position of SENTENCE at point if succesful, nil otherwise. "]

  ["Beginning position of SEXP at point"  ar-sexp-beginning-position-atpt
   :help " ‘ar-sexp-beginning-position-atpt’
   Return beginning position of SEXP at point if succesful, nil otherwise. "]

  ["Beginning position of STRING at point"  ar-string-beginning-position-atpt
   :help " ‘ar-string-beginning-position-atpt’
   Return beginning position of STRING at point if succesful, nil otherwise. "]

  ["Beginning position of SH-STRUCT at point"  ar-shstruct-beginning-position-atpt
   :help " ‘ar-shstruct-beginning-position-atpt’
   Return beginning position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Beginning position of SYMBOL at point"  ar-symbol-beginning-position-atpt
   :help " ‘ar-symbol-beginning-position-atpt’
   Return beginning position of SYMBOL at point if succesful, nil otherwise. "]

  ["Beginning position of URL at point"  ar-url-beginning-position-atpt
   :help " ‘ar-url-beginning-position-atpt’
   Return beginning position of URL at point if succesful, nil otherwise. "]

  ["Beginning position of WORD at point"  ar-word-beginning-position-atpt
   :help " ‘ar-word-beginning-position-atpt’
   Return beginning position of WORD at point if succesful, nil otherwise. "]

  ["Beginning position of WORD-ALPHA-ONLY at point"  ar-wordalphaonly-beginning-position-atpt
   :help " ‘ar-wordalphaonly-beginning-position-atpt’
   Return beginning position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

  )
 ("End position"
 ("Delimited"

  ["End position of BRACED at point"  ar-braced-end-position-atpt
   :help " ‘ar-braced-end-position-atpt’
  End position of BRACED at point if any, nil otherwise. "]

  ["End position of BRACKETED at point"  ar-bracketed-end-position-atpt
   :help " ‘ar-bracketed-end-position-atpt’
  End position of BRACKETED at point if any, nil otherwise. "]

  ["End position of PARENTIZED at point"  ar-parentized-end-position-atpt
   :help " ‘ar-parentized-end-position-atpt’
  End position of PARENTIZED at point if any, nil otherwise. "]

  ["End position of DOUBLEQUOTED at point"  ar-doublequoted-end-position-atpt
   :help " ‘ar-doublequoted-end-position-atpt’
  End position of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["End position of SINGLEQUOTED at point"  ar-singlequoted-end-position-atpt
   :help " ‘ar-singlequoted-end-position-atpt’
  End position of SINGLEQUOTED at point if any, nil otherwise. "]

  ["End position of TRIPLEQUOTED at point"  ar-triplequoted-end-position-atpt
   :help " ‘ar-triplequoted-end-position-atpt’
  End position of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["End position of LESSER-ANGLED at point"  ar-lesserangled-end-position-atpt
   :help " ‘ar-lesserangled-end-position-atpt’
  End position of LESSER-ANGLED at point if any, nil otherwise. "]

 "-"

  ["End position of TRIPLEQUOTED-DQ at point"  ar-triplequoteddq-end-position-atpt
   :help " ‘ar-triplequoteddq-end-position-atpt’
  End position of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["End position of TRIPLEQUOTE-SQ at point"  ar-triplequotesq-end-position-atpt
   :help " ‘ar-triplequotesq-end-position-atpt’
  End position of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["End position of GREATER-ANGLED at point"  ar-greaterangled-end-position-atpt
   :help " ‘ar-greaterangled-end-position-atpt’
  End position of GREATER-ANGLED at point if any, nil otherwise. "]

  ["End position of LEFT-RIGHT-SINGLEQUOTED at point"  ar-curvedsinglequoted-end-position-atpt
   :help " ‘ar-curvedsinglequoted-end-position-atpt’
  End position of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["End position of BACKSLASHED at point"  ar-backslashed-end-position-atpt
   :help " ‘ar-backslashed-end-position-atpt’
  End position of BACKSLASHED at point if any, nil otherwise. "]

  ["End position of DOLLARED at point"  ar-dollared-end-position-atpt
   :help " ‘ar-dollared-end-position-atpt’
  End position of DOLLARED at point if any, nil otherwise. "]

  ["End position of DOUBLEQUOTED at point"  ar-doublequoted-end-position-atpt
   :help " ‘ar-doublequoted-end-position-atpt’
  End position of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["End position of EQUALIZED at point"  ar-equalized-end-position-atpt
   :help " ‘ar-equalized-end-position-atpt’
  End position of EQUALIZED at point if any, nil otherwise. "]

  ["End position of HYPHENED at point"  ar-hyphened-end-position-atpt
   :help " ‘ar-hyphened-end-position-atpt’
  End position of HYPHENED at point if any, nil otherwise. "]

  ["End position of SINGLEQUOTED at point"  ar-singlequoted-end-position-atpt
   :help " ‘ar-singlequoted-end-position-atpt’
  End position of SINGLEQUOTED at point if any, nil otherwise. "]

  ["End position of SLASHED at point"  ar-slashed-end-position-atpt
   :help " ‘ar-slashed-end-position-atpt’
  End position of SLASHED at point if any, nil otherwise. "]

  ["End position of UNDERSCORED at point"  ar-underscored-end-position-atpt
   :help " ‘ar-underscored-end-position-atpt’
  End position of UNDERSCORED at point if any, nil otherwise. "]

  ["End position of WHITESPACED at point"  ar-whitespaced-end-position-atpt
   :help " ‘ar-whitespaced-end-position-atpt’
  End position of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["End position of [:alnum:] char class at point"  ar-alnum-end-position-atpt
   :help " ‘ar-alnum-end-position-atpt’
   Return end position of ALNUM at point if succesful, nil otherwise. "]

  ["End position of [:alpha:] char class at point"  ar-alpha-end-position-atpt
   :help " ‘ar-alpha-end-position-atpt’
   Return end position of ALPHA at point if succesful, nil otherwise. "]

  ["End position of [:ascii:] char class at point"  ar-ascii-end-position-atpt
   :help " ‘ar-ascii-end-position-atpt’
   Return end position of ASCII at point if succesful, nil otherwise. "]

  ["End position of [:blank:] char class at point"  ar-blank-end-position-atpt
   :help " ‘ar-blank-end-position-atpt’
   Return end position of BLANK at point if succesful, nil otherwise. "]

  ["End position of [:cntrl:] char class at point"  ar-cntrl-end-position-atpt
   :help " ‘ar-cntrl-end-position-atpt’
   Return end position of CNTRL at point if succesful, nil otherwise. "]

  ["End position of [:digit:] char class at point"  ar-digit-end-position-atpt
   :help " ‘ar-digit-end-position-atpt’
   Return end position of DIGIT at point if succesful, nil otherwise. "]

  ["End position of [:graph:] char class at point"  ar-graph-end-position-atpt
   :help " ‘ar-graph-end-position-atpt’
   Return end position of GRAPH at point if succesful, nil otherwise. "]

  ["End position of [:lower:] char class at point"  ar-lower-end-position-atpt
   :help " ‘ar-lower-end-position-atpt’
   Return end position of LOWER at point if succesful, nil otherwise. "]

  ["End position of [:nonascii:] char class at point"  ar-nonascii-end-position-atpt
   :help " ‘ar-nonascii-end-position-atpt’
   Return end position of NONASCII at point if succesful, nil otherwise. "]

  ["End position of [:print:] char class at point"  ar-print-end-position-atpt
   :help " ‘ar-print-end-position-atpt’
   Return end position of PRINT at point if succesful, nil otherwise. "]

  ["End position of [:punct:] char class at point"  ar-punct-end-position-atpt
   :help " ‘ar-punct-end-position-atpt’
   Return end position of PUNCT at point if succesful, nil otherwise. "]

  ["End position of [:space:] char class at point"  ar-space-end-position-atpt
   :help " ‘ar-space-end-position-atpt’
   Return end position of SPACE at point if succesful, nil otherwise. "]

  ["End position of [:upper:] char class at point"  ar-upper-end-position-atpt
   :help " ‘ar-upper-end-position-atpt’
   Return end position of UPPER at point if succesful, nil otherwise. "]

  ["End position of [:xdigit:] char class at point"  ar-xdigit-end-position-atpt
   :help " ‘ar-xdigit-end-position-atpt’
   Return end position of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["End position of ANGLED-NO-NEST at point"  ar-anglednonest-end-position-atpt
   :help " ‘ar-anglednonest-end-position-atpt’
   Return end position of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["End position of GREATER-ANGLED-NESTED at point"  ar-greateranglednested-end-position-atpt
   :help " ‘ar-greateranglednested-end-position-atpt’
   Return end position of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["End position of LESSER-ANGLED-NESTED at point"  ar-lesseranglednested-end-position-atpt
   :help " ‘ar-lesseranglednested-end-position-atpt’
   Return end position of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["End position of BUFFER at point"  ar-buffer-end-position-atpt
   :help " ‘ar-buffer-end-position-atpt’
   Return end position of BUFFER at point if succesful, nil otherwise. "]

  ["End position of COMMENT at point"  ar-comment-end-position-atpt
   :help " ‘ar-comment-end-position-atpt’
   Return end position of COMMENT at point if succesful, nil otherwise. "]

  ["End position of CSV at point"  ar-csv-end-position-atpt
   :help " ‘ar-csv-end-position-atpt’
   Return end position of CSV at point if succesful, nil otherwise. "]

  ["End position of DATE at point"  ar-date-end-position-atpt
   :help " ‘ar-date-end-position-atpt’
   Return end position of DATE at point if succesful, nil otherwise. "]

  ["End position of DEFUN at point"  ar-defun-end-position-atpt
   :help " ‘ar-defun-end-position-atpt’
   Return end position of DEFUN at point if succesful, nil otherwise. "]

  ["End position of DELIMITED at point"  ar-delimited-end-position-atpt
   :help " ‘ar-delimited-end-position-atpt’
   Return end position of DELIMITED at point if succesful, nil otherwise. "]

  ["End position of EMAIL at point"  ar-email-end-position-atpt
   :help " ‘ar-email-end-position-atpt’
   Return end position of EMAIL at point if succesful, nil otherwise. "]

  ["End position of FILENAME at point"  ar-filename-end-position-atpt
   :help " ‘ar-filename-end-position-atpt’
   Return end position of FILENAME at point if succesful, nil otherwise. "]

  ["End position of FLOAT at point"  ar-float-end-position-atpt
   :help " ‘ar-float-end-position-atpt’
   Return end position of FLOAT at point if succesful, nil otherwise. "]

  ["End position of FUNCTION at point"  ar-function-end-position-atpt
   :help " ‘ar-function-end-position-atpt’
   Return end position of FUNCTION at point if succesful, nil otherwise. "]

  ["End position of IP at point"  ar-ip-end-position-atpt
   :help " ‘ar-ip-end-position-atpt’
   Return end position of IP at point if succesful, nil otherwise. "]

  ["End position of ISBN at point"  ar-isbn-end-position-atpt
   :help " ‘ar-isbn-end-position-atpt’
   Return end position of ISBN at point if succesful, nil otherwise. "]

  ["End position of LINE at point"  ar-line-end-position-atpt
   :help " ‘ar-line-end-position-atpt’
   Return end position of LINE at point if succesful, nil otherwise. "]

  ["End position of NAME at point"  ar-name-end-position-atpt
   :help " ‘ar-name-end-position-atpt’
   Return end position of NAME at point if succesful, nil otherwise. "]

  ["End position of NUMBER at point"  ar-number-end-position-atpt
   :help " ‘ar-number-end-position-atpt’
   Return end position of NUMBER at point if succesful, nil otherwise. "]

  ["End position of PAGE at point"  ar-page-end-position-atpt
   :help " ‘ar-page-end-position-atpt’
   Return end position of PAGE at point if succesful, nil otherwise. "]

  ["End position of PARAGRAPH at point"  ar-paragraph-end-position-atpt
   :help " ‘ar-paragraph-end-position-atpt’
   Return end position of PARAGRAPH at point if succesful, nil otherwise. "]

  ["End position of PAREN at point"  ar-paren-end-position-atpt
   :help " ‘ar-paren-end-position-atpt’
   Return end position of PAREN at point if succesful, nil otherwise. "]

  ["End position of PHONE at point"  ar-phone-end-position-atpt
   :help " ‘ar-phone-end-position-atpt’
   Return end position of PHONE at point if succesful, nil otherwise. "]

  ["End position of REGION at point"  ar-region-end-position-atpt
   :help " ‘ar-region-end-position-atpt’
   Return end position of REGION at point if succesful, nil otherwise. "]

  ["End position of SENTENCE at point"  ar-sentence-end-position-atpt
   :help " ‘ar-sentence-end-position-atpt’
   Return end position of SENTENCE at point if succesful, nil otherwise. "]

  ["End position of SEXP at point"  ar-sexp-end-position-atpt
   :help " ‘ar-sexp-end-position-atpt’
   Return end position of SEXP at point if succesful, nil otherwise. "]

  ["End position of STRING at point"  ar-string-end-position-atpt
   :help " ‘ar-string-end-position-atpt’
   Return end position of STRING at point if succesful, nil otherwise. "]

  ["End position of SH-STRUCT at point"  ar-shstruct-end-position-atpt
   :help " ‘ar-shstruct-end-position-atpt’
   Return end position of SH-STRUCT at point if succesful, nil otherwise. "]

  ["End position of SYMBOL at point"  ar-symbol-end-position-atpt
   :help " ‘ar-symbol-end-position-atpt’
   Return end position of SYMBOL at point if succesful, nil otherwise. "]

  ["End position of URL at point"  ar-url-end-position-atpt
   :help " ‘ar-url-end-position-atpt’
   Return end position of URL at point if succesful, nil otherwise. "]

  ["End position of WORD at point"  ar-word-end-position-atpt
   :help " ‘ar-word-end-position-atpt’
   Return end position of WORD at point if succesful, nil otherwise. "]

  ["End position of WORD-ALPHA-ONLY at point"  ar-wordalphaonly-end-position-atpt
   :help " ‘ar-wordalphaonly-end-position-atpt’
   Return end position of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

)

  )
("Length"
 ("Delimited"

  ["Length of BRACED at point"  ar-length-of-braced-atpt
   :help " ‘ar-length-of-braced-atpt’
   Length of BRACED at point if any, nil otherwise. "]

  ["Length of BRACKETED at point"  ar-length-of-bracketed-atpt
   :help " ‘ar-length-of-bracketed-atpt’
   Length of BRACKETED at point if any, nil otherwise. "]

  ["Length of PARENTIZED at point"  ar-length-of-parentized-atpt
   :help " ‘ar-length-of-parentized-atpt’
   Length of PARENTIZED at point if any, nil otherwise. "]

  ["Length of DOUBLEQUOTED at point"  ar-length-of-doublequoted-atpt
   :help " ‘ar-length-of-doublequoted-atpt’
   Length of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Length of SINGLEQUOTED at point"  ar-length-of-singlequoted-atpt
   :help " ‘ar-length-of-singlequoted-atpt’
   Length of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Length of TRIPLEQUOTED at point"  ar-length-of-triplequoted-atpt
   :help " ‘ar-length-of-triplequoted-atpt’
   Length of TRIPLEQUOTED at point if any, nil otherwise. "]

  ["Length of LESSER-ANGLED at point"  ar-length-of-lesserangled-atpt
   :help " ‘ar-length-of-lesserangled-atpt’
   Length of LESSER-ANGLED at point if any, nil otherwise. "]

  "-"

  ["Length of TRIPLEQUOTED-DQ at point"  ar-length-of-triplequoteddq-atpt
   :help " ‘ar-length-of-triplequoteddq-atpt’
   Length of TRIPLEQUOTED-DQ at point if any, nil otherwise. "]

  ["Length of TRIPLEQUOTE-SQ at point"  ar-length-of-triplequotesq-atpt
   :help " ‘ar-length-of-triplequotesq-atpt’
   Length of TRIPLEQUOTE-SQ at point if any, nil otherwise. "]

  ["Length of GREATER-ANGLED at point"  ar-length-of-greaterangled-atpt
   :help " ‘ar-length-of-greaterangled-atpt’
   Length of GREATER-ANGLED at point if any, nil otherwise. "]

  ["Length of LEFT-RIGHT-SINGLEQUOTED at point"  ar-length-of-curvedsinglequoted-atpt
   :help " ‘ar-length-of-curvedsinglequoted-atpt’
   Length of LEFT-RIGHT-SINGLEQUOTED at point if any, nil otherwise. "]

  ["Length of BACKSLASHED at point"  ar-length-of-backslashed-atpt
   :help " ‘ar-length-of-backslashed-atpt’
   Length of BACKSLASHED at point if any, nil otherwise. "]

  ["Length of DOLLARED at point"  ar-length-of-dollared-atpt
   :help " ‘ar-length-of-dollared-atpt’
   Length of DOLLARED at point if any, nil otherwise. "]

  ["Length of DOUBLEQUOTED at point"  ar-length-of-doublequoted-atpt
   :help " ‘ar-length-of-doublequoted-atpt’
   Length of DOUBLEQUOTED at point if any, nil otherwise. "]

  ["Length of EQUALIZED at point"  ar-length-of-equalized-atpt
   :help " ‘ar-length-of-equalized-atpt’
   Length of EQUALIZED at point if any, nil otherwise. "]

  ["Length of HYPHENED at point"  ar-length-of-hyphened-atpt
   :help " ‘ar-length-of-hyphened-atpt’
   Length of HYPHENED at point if any, nil otherwise. "]

  ["Length of SINGLEQUOTED at point"  ar-length-of-singlequoted-atpt
   :help " ‘ar-length-of-singlequoted-atpt’
   Length of SINGLEQUOTED at point if any, nil otherwise. "]

  ["Length of SLASHED at point"  ar-length-of-slashed-atpt
   :help " ‘ar-length-of-slashed-atpt’
   Length of SLASHED at point if any, nil otherwise. "]

  ["Length of UNDERSCORED at point"  ar-length-of-underscored-atpt
   :help " ‘ar-length-of-underscored-atpt’
   Length of UNDERSCORED at point if any, nil otherwise. "]

  ["Length of WHITESPACED at point"  ar-length-of-whitespaced-atpt
   :help " ‘ar-length-of-whitespaced-atpt’
   Length of WHITESPACED at point if any, nil otherwise. "]

)
 ("Character classes"

  ["Length of [:alnum:] char class at point"  ar-length-of-alnum-atpt
   :help " ‘ar-length-of-alnum-atpt’
   Return length of ALNUM at point if succesful, nil otherwise. "]

  ["Length of [:alpha:] char class at point"  ar-length-of-alpha-atpt
   :help " ‘ar-length-of-alpha-atpt’
   Return length of ALPHA at point if succesful, nil otherwise. "]

  ["Length of [:ascii:] char class at point"  ar-length-of-ascii-atpt
   :help " ‘ar-length-of-ascii-atpt’
   Return length of ASCII at point if succesful, nil otherwise. "]

  ["Length of [:blank:] char class at point"  ar-length-of-blank-atpt
   :help " ‘ar-length-of-blank-atpt’
   Return length of BLANK at point if succesful, nil otherwise. "]

  ["Length of [:cntrl:] char class at point"  ar-length-of-cntrl-atpt
   :help " ‘ar-length-of-cntrl-atpt’
   Return length of CNTRL at point if succesful, nil otherwise. "]

  ["Length of [:digit:] char class at point"  ar-length-of-digit-atpt
   :help " ‘ar-length-of-digit-atpt’
   Return length of DIGIT at point if succesful, nil otherwise. "]

  ["Length of [:graph:] char class at point"  ar-length-of-graph-atpt
   :help " ‘ar-length-of-graph-atpt’
   Return length of GRAPH at point if succesful, nil otherwise. "]

  ["Length of [:lower:] char class at point"  ar-length-of-lower-atpt
   :help " ‘ar-length-of-lower-atpt’
   Return length of LOWER at point if succesful, nil otherwise. "]

  ["Length of [:nonascii:] char class at point"  ar-length-of-nonascii-atpt
   :help " ‘ar-length-of-nonascii-atpt’
   Return length of NONASCII at point if succesful, nil otherwise. "]

  ["Length of [:print:] char class at point"  ar-length-of-print-atpt
   :help " ‘ar-length-of-print-atpt’
   Return length of PRINT at point if succesful, nil otherwise. "]

  ["Length of [:punct:] char class at point"  ar-length-of-punct-atpt
   :help " ‘ar-length-of-punct-atpt’
   Return length of PUNCT at point if succesful, nil otherwise. "]

  ["Length of [:space:] char class at point"  ar-length-of-space-atpt
   :help " ‘ar-length-of-space-atpt’
   Return length of SPACE at point if succesful, nil otherwise. "]

  ["Length of [:upper:] char class at point"  ar-length-of-upper-atpt
   :help " ‘ar-length-of-upper-atpt’
   Return length of UPPER at point if succesful, nil otherwise. "]

  ["Length of [:xdigit:] char class at point"  ar-length-of-xdigit-atpt
   :help " ‘ar-length-of-xdigit-atpt’
   Return length of XDIGIT at point if succesful, nil otherwise. "]

)
 ("Other"

  ["Length of ANGLED-NO-NEST at point"  ar-length-ofanglednonest-atpt
   :help " ‘ar-length-ofanglednonest-atpt’
   Return length of ANGLED-NO-NEST at point if succesful, nil otherwise. "]

  ["Length of GREATER-ANGLED-NESTED at point"  ar-length-of-greateranglednested-atpt
   :help " ‘ar-length-of-greateranglednested-atpt’
   Return length of GREATER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Length of LESSER-ANGLED-NESTED at point"  ar-length-of-lesseranglednested-atpt
   :help " ‘ar-length-of-lesseranglednested-atpt’
   Return length of LESSER-ANGLED-NESTED at point if succesful, nil otherwise. "]

  ["Length of BUFFER at point"  ar-length-of-buffer-atpt
   :help " ‘ar-length-of-buffer-atpt’
   Return length of BUFFER at point if succesful, nil otherwise. "]

  ["Length of COMMENT at point"  ar-length-of-comment-atpt
   :help " ‘ar-length-of-comment-atpt’
   Return length of COMMENT at point if succesful, nil otherwise. "]

  ["Length of CSV at point"  ar-length-of-csv-atpt
   :help " ‘ar-length-of-csv-atpt’
   Return length of CSV at point if succesful, nil otherwise. "]

  ["Length of DATE at point"  ar-length-of-date-atpt
   :help " ‘ar-length-of-date-atpt’
   Return length of DATE at point if succesful, nil otherwise. "]

  ["Length of DEFUN at point"  ar-length-of-defun-atpt
   :help " ‘ar-length-of-defun-atpt’
   Return length of DEFUN at point if succesful, nil otherwise. "]

  ["Length of DELIMITED at point"  ar-length-of-delimited-atpt
   :help " ‘ar-length-of-delimited-atpt’
   Return length of DELIMITED at point if succesful, nil otherwise. "]

  ["Length of EMAIL at point"  ar-length-of-email-atpt
   :help " ‘ar-length-of-email-atpt’
   Return length of EMAIL at point if succesful, nil otherwise. "]

  ["Length of FILENAME at point"  ar-length-of-filename-atpt
   :help " ‘ar-length-of-filename-atpt’
   Return length of FILENAME at point if succesful, nil otherwise. "]

  ["Length of FLOAT at point"  ar-length-of-float-atpt
   :help " ‘ar-length-of-float-atpt’
   Return length of FLOAT at point if succesful, nil otherwise. "]

  ["Length of FUNCTION at point"  ar-length-of-function-atpt
   :help " ‘ar-length-of-function-atpt’
   Return length of FUNCTION at point if succesful, nil otherwise. "]

  ["Length of IP at point"  ar-length-of-ip-atpt
   :help " ‘ar-length-of-ip-atpt’
   Return length of IP at point if succesful, nil otherwise. "]

  ["Length of ISBN at point"  ar-length-of-isbn-atpt
   :help " ‘ar-length-of-isbn-atpt’
   Return length of ISBN at point if succesful, nil otherwise. "]

  ["Length of LINE at point"  ar-length-of-line-atpt
   :help " ‘ar-length-of-line-atpt’
   Return length of LINE at point if succesful, nil otherwise. "]

  ["Length of NAME at point"  ar-length-of-name-atpt
   :help " ‘ar-length-of-name-atpt’
   Return length of NAME at point if succesful, nil otherwise. "]

  ["Length of NUMBER at point"  ar-length-of-number-atpt
   :help " ‘ar-length-of-number-atpt’
   Return length of NUMBER at point if succesful, nil otherwise. "]

  ["Length of PAGE at point"  ar-length-of-page-atpt
   :help " ‘ar-length-of-page-atpt’
   Return length of PAGE at point if succesful, nil otherwise. "]

  ["Length of PARAGRAPH at point"  ar-length-of-paragraph-atpt
   :help " ‘ar-length-of-paragraph-atpt’
   Return length of PARAGRAPH at point if succesful, nil otherwise. "]

  ["Length of PAREN at point"  ar-length-of-paren-atpt
   :help " ‘ar-length-of-paren-atpt’
   Return length of PAREN at point if succesful, nil otherwise. "]

  ["Length of PHONE at point"  ar-length-of-phone-atpt
   :help " ‘ar-length-of-phone-atpt’
   Return length of PHONE at point if succesful, nil otherwise. "]

  ["Length of REGION at point"  ar-length-of-region-atpt
   :help " ‘ar-length-of-region-atpt’
   Return length of REGION at point if succesful, nil otherwise. "]

  ["Length of SENTENCE at point"  ar-length-of-sentence-atpt
   :help " ‘ar-length-of-sentence-atpt’
   Return length of SENTENCE at point if succesful, nil otherwise. "]

  ["Length of SEXP at point"  ar-length-of-sexp-atpt
   :help " ‘ar-length-of-sexp-atpt’
   Return length of SEXP at point if succesful, nil otherwise. "]

  ["Length of STRING at point"  ar-length-of-string-atpt
   :help " ‘ar-length-of-string-atpt’
   Return length of STRING at point if succesful, nil otherwise. "]

  ["Length of SH-STRUCT at point"  ar-length-of-shstruct-atpt
   :help " ‘ar-length-of-shstruct-atpt’
   Return length of SH-STRUCT at point if succesful, nil otherwise. "]

  ["Length of SYMBOL at point"  ar-length-of-symbol-atpt
   :help " ‘ar-length-of-symbol-atpt’
   Return length of SYMBOL at point if succesful, nil otherwise. "]

  ["Length of URL at point"  ar-length-of-url-atpt
   :help " ‘ar-length-of-url-atpt’
   Return length of URL at point if succesful, nil otherwise. "]

  ["Length of WORD at point"  ar-length-of-word-atpt
   :help " ‘ar-length-of-word-atpt’
   Return length of WORD at point if succesful, nil otherwise. "]

  ["Length of WORD-ALPHA-ONLY at point"  ar-length-of-wordalphaonly-atpt
   :help " ‘ar-length-of-wordalphaonly-atpt’
   Return length of WORD-ALPHA-ONLY at point if succesful, nil otherwise. "]

)

  )

;; ar-thing-at-point-utils-nodelim-core-menu: ar-atpt-classes end

)))

    map))
(provide 'thingatpt-utils-map)
;;; thingatpt-utils-map.el ends here
