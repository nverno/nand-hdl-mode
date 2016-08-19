;;; nand-hdl-mode --- Major mode for NAND hardward description language files (.hdl)

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/hdl-mode
;; Package-Requires: 
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 18 August 2016

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

;; Emac major mode for NAND hardward description language files (.hdl).

;;; Code:

(defgroup nand-hdl nil
  "Major mode for editing NAND hardware description language files."
  :group 'languages
  :prefix "nand-hdl-")

(defcustom nand-hdl-indent 4
  "Default indent level to use in `nand-hdl-mode'"
  :group 'nand-hdl
  :type 'integer)
;;;###autoload(put 'nand-hdl-indent 'safe-local-variable 'integerp)

(defcustom nand-hdl-indent-parts t
  "If non-nil, indent 'PARTS:', otherwise align with 'CHIP'."
  :group 'nand-hdl
  :type 'boolean)

(defcustom nand-hdl-highlight-doc t
  "If non-nil, highlight documentation stubs."
  :group 'nand-hdl
  :type 'boolean)

(defface nand-hdl-doc-face
  '((nil (:foreground "SaddleBrown" :background "#f7f7f7")))
  "Special face to highlight documentation (after '/**')."
  :group 'nand-hdl)


;; ------------------------------------------------------------
;;* User Functions
(defun nand-hdl-run ()
  (interactive))

(defun nand-hdl-truth ()
  (interactive))


;; ------------------------------------------------------------
;; Internal

;;* Font-lock
(defconst nand-hdl-keywords
  '("CHIP" "IN" "OUT" "PARTS" "BUILTIN" "CLOCKED"))

(defvar nand-hdl-font-lock-keywords
  `(("\\(?:CHIP\\|BUILTIN\\)\\s-*\\([^ {]+\\)" 1 font-lock-function-name-face)
    ("\\(?:IN\\|OUT\\)\\s-*\\(.*\\)" 1 font-lock-variable-name-face)
    (,(regexp-opt nand-hdl-keywords) . font-lock-builtin-face)
    ))

(defun nand-hdl-syntax-propertize-function (start end)
  (goto-char start)
  ;; ("\\(?:/\\*\\*\\([^\\(?:*/)]+\\)\\)" 1 'nand-hdl-doc-face)
  )
;; (defconst nand-hdl-syntax-propertize nil)
;; (eval-and-compile
;;   (defconst nand-hdl-doc-re
;;     "\\(?:/\\*\\*\\([^\\(?:*/)]+)"

;;* Indentation
(require 'smie)

(defconst nand-hdl-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ",") (assoc " ") (nonassoc ";")))))

(defun nand-hdl-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) nand-hdl-indent)
    (`(:elem . args) 0)
    ;; (`(:before . "{")
    ;;  (smie-rule-parent))
    (`(:list-intro . ,(or `"\n" `"" `";")) t)))


;; ------------------------------------------------------------
;;* Major mode

;; Syntax (c/c++ style comments)
(defvar nand-hdl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Sytax for `nand-hdl-mode'")

;; Menu
(defvar nand-hdl-menu
  '("NandHDL"
    ["Run" nand-hdl-run :help "Run script"]
    ["Show truth table" nand-hdl-truth :help "Show truth table in other window"]))

;; Map
(defvar nand-hdl-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil nand-hdl-menu)
    (define-key map (kbd "C-c C-c") #'nand-hdl-run)
    (define-key map (kbd "C-c C-t") #'nand-hdl-truth)
    map))

;;;###autoload
(define-derived-mode nand-hdl-mode prog-mode "NandHDL"
  "Major mode for editing NAND hardware description files (.hdl).\n
\\{nand-hdl-mode-map}"
  (setq-local comment-start "/* ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end " */")
  (setq-local font-lock-defaults
              `(nand-hdl-font-lock-keywords nil nil nil))
  ;; (setq-local syntax-propertize-function nand-hdl-syntax-propertize)
  (setq-local imenu-generic-expression
              '((nil "^\\(?:CHIP\\|BUILTIN\\)\\s-*\\([^ {]+\\)" 1)))
  (setq-local outline-regexp "^\\(?:CHIP\\|BUILTIN\\)")
  (smie-setup nand-hdl-grammar #'nand-hdl-rules
              :forward-token #'smie-default-forward-token
              :backward-token #'smie-default-backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hdl\\'" . nand-hdl-mode))

(provide 'nand-hdl-mode)

;;; nand-hdl-mode.el ends here
