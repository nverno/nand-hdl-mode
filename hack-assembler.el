;;; hack-assembler.el --- mode for editing Hack assembler code -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nand-hdl-mode
;; Package-Requires: 
;; Created: 19 May 2021

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;; Description:
;;
;; Syntax and highlighting for Hack assembler code.
;;
;;; Code:
(require 'nand-hdl-mode)                ; nand-hdl-directory

(defgroup hack-assembler nil
  "Major mode for editing Hack assembler files."
  :group 'languages
  :prefix "hack-assembler-")

(defcustom hack-assembler-binary
  (expand-file-name (concat "tools/Assembler" nand-hdl-ext) nand-hdl-directory)
  "Location of Assembler.[sh|bat]"
  :group 'hack-assembler
  :type 'file)

;;; Font-locking
(defmacro hack--opt (kws)
  `(eval-when-compile
     (regexp-opt ,kws 'words)))

(eval-and-compile
  (defconst hack-assembler-builtins
   '("SP" "LCL" "ARG" "THIS" "THAT" "SCREEN" "KBD"
     "R0" "R1" "R2" "R3" "R4" "R5" "R6" "R7" "R8"
     "R9" "R10" "R11" "R12" "R13" "R14" "R15")
   "Builtin RAM variables.")

  (defconst hack-assembler-dests '("A" "M" "D" "AD" "ADM" "MD" "AM"))

  (defconst hack-assembler-instructions '("JLT" "JEQ" "JGT" "JLE" "JNE" "JGE" "JMP")))

(defconst hack-assembler-variable-regexp "[a-zA-Z_.$-][a-zA-Z0-9_.$-]*")

(defconst hack-assembler-label-regexp
  (concat "(\\(" hack-assembler-variable-regexp "\\))"))

(defconst hack-assembler-font-lock-keywords
  `(("^\\s-*\\(@\\)\\([0-9]+\\)"
     (1 font-lock-preprocessor-face)
     (2 font-lock-constant-face))
    (,(concat "^\\s-*\\(@\\)\\(" hack-assembler-variable-regexp "\\)")
     (1 font-lock-preprocessor-face)
     (2 font-lock-variable-name-face))
    (,(concat "^\\s-*" hack-assembler-label-regexp) (1 font-lock-function-name-face))
    (,(concat "^\\s-*" (hack--opt hack-assembler-dests)) . font-lock-keyword-face)
    (,(hack--opt hack-assembler-instructions) . font-lock-builtin-face)))

(defvar hack-assembler-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?@ "'" st)
    st))

;;;###autoload
(define-derived-mode hack-assembler-mode prog-mode "Hack"
  "Major mode for editing Hack assembly files.\n
\\{hack-assembler-mode-map}"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local compile-command
              (concat hack-assembler-binary " "
                      (if buffer-file-name
			  (shell-quote-argument buffer-file-name))))
  (setq-local font-lock-defaults
              `(hack-assembler-font-lock-keywords nil nil nil))
  ;; (setq-local syntax-propertize-function hack-assembler-syntax-propertize)
  (setq-local imenu-generic-expression
              `((nil ,(concat "^\\s-*" hack-assembler-label-regexp) 1))))

(provide 'hack-assembler)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; hack-assembler.el ends here
