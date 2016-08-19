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

(defcustom nand-hdl-directory "nand2tetris"
  "Location of base directory 'nand2tetris', it should contain
the 'tools' directory."
  :group 'nand-hdl
  :type 'file)

(defcustom nand-hdl-emulator
  (and nand-hdl-directory
       (concat (expand-file-name
                (concat "tools/HardwareSimulator"
                        (if (eq system-type 'windows-nt)
                            ".bat" ".sh"))
                nand-hdl-directory)))
  "Location of 'HardwardSimulator'.  Used to run script and output
to compilation buffer."
  :group 'nand-hdl
  :type 'file)

(defcustom nand-hdl-indent 4
  "Default indent level to use inside blocks."
  :group 'nand-hdl
  :type 'integer)

(defcustom nand-hdl-indent-parts 2
  "Indentation of 'PARTS' with respect to containing block."
  :group 'nand-hdl
  :type 'integer)

(defcustom nand-hdl-indent-declarations 2
  "Indentation of declarations with respect to containing block."
  :group 'nand-hdl
  :type 'integer)

(defcustom nand-hdl-auto-newline t
  "If non-nil, automatically newline after semicolons."
  :group 'nand-hdl
  :type 'boolean)

;;;###autoload(put 'nand-hdl-indent 'safe-local-variable 'integerp)
;;;###autoload(put 'nand-hdl-indent-parts 'safe-local-variable 'integerp)
;;;###autoload(put 'nand-hdl-indent-declarations 'safe-local-variable 'integerp)

(defcustom nand-hdl-highlight-doc t
  "If non-nil, highlight documentation stubs."
  :group 'nand-hdl
  :type 'boolean)

(defface nand-hdl-doc-face
  '((nil (:foreground "SaddleBrown" :background "#f7f7f7")))
  "Special face to highlight documentation (after '/**')."
  :group 'nand-hdl)


;; ------------------------------------------------------------
;;* Compilation
(require 'compile)
;; compilation-error-regexp-alist-alist

(defvar nand-hdl-error-regexp-alist
  '((nand-hdl-1
     "\\(In HDL file \\)\\([^,]+\\),\\s-*Line\\s-*\\([0-9]+\\)" 2 3)
    (nand-hdl-2
     "\\(Chip\\)\\s-*\\([^ ]+\\)"
     (1
      (progn (concat (file-name-sans-extension buffer-file-name)
                     ".tst")))
     nil nil 2)))

(defun nand-hdl-add-compile-regexp ()
  (interactive)
  (when (not (assoc 'nand-hdl-1 compilation-error-regexp-alist-alist))
    (mapcar (lambda (item)
              (push (car item) compilation-error-regexp-alist)
              (push item compilation-error-regexp-alist-alist))
            nand-hdl-error-regexp-alist)))
(add-hook 'compilation-mode-hook 'nand-hdl-add-compile-regexp)

(defun nand-hdl-compile ()
  (interactive)
  (save-buffer)
  (when (not (file-exists-p nand-hdl-emulator))
    (user-error "Can't find Hardware emulator: %s"
                (or nand-hdl-emulator "undefined")))
  (let* ((test-file
          (concat
           (file-name-sans-extension (buffer-file-name)) ".tst"))
         (compile-command
          (concat
           (if (eq system-type 'windows-nt)
               (concat "cmd.exe /c " nand-hdl-emulator)
             (concat "bash -c " nand-hdl-emulator))
           " " test-file))
         (compilation-read-command))
   (compile compile-command)))

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

;;* Abbrevs
(defun nand-hdl-define-abbrev (table name expansion &optional hook)
  (condition-case nil
      (define-abbrev-table name expansion hook 0 t)
    (error
     (define-abbrev-table table name expansion hook))))



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
    ["Compile" nand-hdl-compile :help "Compile script"]
    ["Show truth table" nand-hdl-truth :help "Show truth table in other window"]))

;; Map
(defvar nand-hdl-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil nand-hdl-menu)
    (define-key map (kbd "C-c C-c") #'nand-hdl-compile)
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
