;;; lamb-mode.el --- Major mode for editing Lamb language  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  USAMI Kenta

;; Author: USAMI Kenta
;; URL: https://github.com/zonuexe/lamb-mode.el
;; Keywords: languages
;; Created: 27 Aug 2018
;; Version: 0.0.1
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

(defvar lamb-mode-font-lock-keywords
  `((,(regexp-opt lamb-mode-keywords 'symbols) (1 font-lock-keyword-face))
    ("\\_<def\\s-+\\(\\_<.+?\\_>\\)\\s-?" (1 font-lock-function-name-face))
    ("\\_<let\\s-+\\(\\_<.+?\\_>\\)\\s-+:=" (1 font-lock-variable-name-face))
    ("\\_<type\\s-+\\(\\_<.+?\\_>\\)" (1 font-lock-type-face))
    ("\\_<val\\s-+\\(\\_<.+?\\_>\\)\\s-+:" (1 font-lock-variable-name-face))
    ("\\s-+:\\s-+\\(\\_<.+?\\_>\\)\\s-*" (1 font-lock-type-face))
    (":\\s-*\\(\\_<.+?\\_>\\)" (1 font-lock-type-face))
    ("\\(\\_<[^(-:)]+?\\_>\\)\\s-*->\\s-*" (1 font-lock-type-face))
    ("\\s-*->\\s-*\\(\\_<[^(-:)]+?\\_>\\)\\(?:)\\|\\s-\\)" (1 font-lock-type-face))
    ("\\(?:^\\|\\s-+\\|(\\)\\^\\(\\_<.+?\\_>\\)" (1 font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode lamb-mode prog-mode "Lamb"
  "Major mode for editing Lamb language"
  (set-syntax-table lamb-mode-syntax-table)
  (setq font-lock-defaults '(lamb-mode-font-lock-keywords nil nil))
  (setq comment-start "//")
  (setq tab-width 2))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lam?\\'" . lamb-mode))

(provide 'lamb-mode)
;;; lamb-mode.el ends here
