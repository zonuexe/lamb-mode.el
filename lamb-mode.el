;;; lamb-mode.el --- Major mode for editing Lamb language  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  USAMI Kenta

;; Author: USAMI Kenta
;; URL: https://github.com/zonuexe/lamb-mode.el
;; Keywords: languages
;; Created: 27 Aug 2018
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.3"))
;; License: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; "Lamb" is a Lambda Calculus based programming language.
;; https://github.com/wejournal/lamb

;;; Code:
(require 'regexp-opt)

(defvar lamb-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?-  ". 12" table)
    (modify-syntax-entry ?.  "."    table)
    (modify-syntax-entry ?:  "."    table)
    (modify-syntax-entry ?<  "."    table)
    (modify-syntax-entry ?=  "."    table)
    (modify-syntax-entry ?>  "."    table)
    (modify-syntax-entry ?\  " "    table)
    (modify-syntax-entry ?\" "\""   table)
    (modify-syntax-entry ?\' "\""   table)
    (modify-syntax-entry ?\( "()"   table)
    (modify-syntax-entry ?\) ")("   table)
    (modify-syntax-entry ?\\ "\\"   table)
    (modify-syntax-entry ?\n ">"    table)
    (modify-syntax-entry ?\r ">"    table)
    (modify-syntax-entry ?\t " "    table)
    (modify-syntax-entry ?^  "."    table)
    (modify-syntax-entry ?_  "_"    table)
    table))

(defconst lamb-mode-keywords
  '("def"
    "in"
    "let"
    "type"
    "val"))

(defconst lamb-mode-builtin-functions
  '(;; defined in `typed.lam'
    "B"
    "C"
    "I"
    "K"
    "S"
    "W"
    "false"
    "true"
    "not"
    "and"
    "or"
    "xor"
    "if"
    "pair"
    "fst"
    "snd"
    "succ"
    "pred"
    "plus"
    "odd"
    "even"
    "is_zero"
    "odd_copy"
    "even_copy"
    "nil"
    "cons"
    "hd"
    "tl"
    "map"
    "append"
    "concat"
    "filter"
    "filter_map"
    "reverse"
    "reverse_range"
    "range"
    "zero8"
    "sup8"
    "not8"
    "and8"
    "or8"
    "xor8"
    "is_not_zero8"
    "is_zero8"
    "not_eq8"
    "lt8"
    "lte8"
    "eq8"
    "odd8"
    "even8"
    "ntz8"
    "nlz8"
    "sl1_8"
    "sl2_8"
    "sl3_8"
    "sl4_8"
    "sl5_8"
    "sl6_8"
    "sl7_8"
    "sr1_8"
    "sr2_8"
    "sr3_8"
    "sr4_8"
    "sr5_8"
    "sr6_8"
    "sr7_8"
    "add8"
    "nat8"
    "inc8"
    "dec8"
    "sub8"
    "mul8"
    "div8"
    "bin8"
    "is_multiple8"
    "is_not_multiple8"
    "one8"
    "two8"
    "three8"
    "four8"
    "five8"
    "six8"
    "seven8"
    "eight8"
    "nine8"
    "show_nat"
    "show_bin8"
    ;; defined in `untyped.lam'
    "U"
    "Theta"
    "Y"
    "omega"
    "Omega"
    ;; defined in `decimal.lam'
    "decimal_show"
    "d0"
    "d1"
    "d2"
    "d3"
    "d4"
    "d5"
    "d6"
    "d7"
    "d8"
    "d9"
    "dinc"
    "ddec"
    "decimal_zero"
    "decimal_inc"
    "decimal"
    ;; defined in `prim.c'
    "seq"
    "zero64"
    "succ64"
    "eq64"
    "lt64"
    "add64"
    "sub64"
    "mul64"
    "div64"
    "mod64"
    "print64"
    "newline"
    "puts"
    ))

(defvar lamb-mode-font-lock-keywords
  `(("^\\(--|\\s-+.+?\\)$" (1 font-lock-doc-face t))
    (,(regexp-opt lamb-mode-keywords 'symbols) (1 font-lock-keyword-face))
    ("\\_<def\\s-+\\(\\_<.+?\\_>\\)\\s-?" (1 font-lock-function-name-face))
    (,(regexp-opt lamb-mode-builtin-functions 'symbols) (1 font-lock-builtin-face))
    ("\\_<let\\s-+\\(\\_<.+?\\_>\\)\\s-+:=" (1 font-lock-variable-name-face))
    ("\\_<type\\s-+\\(\\_<.+?\\_>\\)" (1 font-lock-type-face))
    ("\\_<val\\s-+\\(\\_<.+?\\_>\\)\\s-+:" (1 font-lock-variable-name-face))
    ("\\s-+:\\s-+\\(\\_<.+?\\_>\\)\\s-*" (1 font-lock-type-face))
    (":\\s-*\\(\\_<.+?\\_>\\)" (1 font-lock-type-face))
    ("\\(\\_<[^(-:)]+?\\_>\\)\\s-*->\\s-*" (1 font-lock-type-face))
    ("\\s-*->\\s-*\\(\\_<[^(-:)]+?\\_>\\)\\(?:)\\|\\s-\\|\\.\\|$\\)" (1 font-lock-type-face))
    ("\\(?:^\\|\\s-+\\|(\\)\\^\\(\\_<.+?\\_>\\)" (1 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode lamb-mode prog-mode "Lamb"
  "Major mode for editing Lamb language"
  (set-syntax-table lamb-mode-syntax-table)
  (setq font-lock-defaults '(lamb-mode-font-lock-keywords nil nil))
  (setq comment-start "--")
  (setq tab-width 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lam?\\'" . lamb-mode))

(provide 'lamb-mode)
;;; lamb-mode.el ends here
