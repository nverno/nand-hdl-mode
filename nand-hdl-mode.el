;;; nand-hdl-mode --- Major mode for NAND hardward description language files (.hdl) -*- lexical-binding: t -*-

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

;; Emac major mode for NAND hardward description language files (.hdl) as 
;; defined by the coursera class nand2tetris.

;; The mode provides:
;;
;; - syntax / font-locking
;; - customizable indentation: choose indentation for IN/OUT declarations,
;;   PARTS, and general offset.
;; - Compilation support, running the chip in simulator, and jumping to mismatches
;;   in .out file.
;; - Comparison buffer for expected vs. output. (`C-c C-c`)
;; - imenu, other user functions, etc.
;; - Autocompletion / dropdown help using `company-nand' with
;;   `company-mode' and `company-quickhelp'
;; - snippets to fill in chip components

;; Tools in build directory:
;; - Autogenerate snippets from the HDL survival website (for use with yasnippet)
;; - Autogenerate docs.txt from builtin chips.
;;
;; Todo:
;; - Completion-at-point functions.
;; - Jump to definitions
;;
;;; Installation:
;;
;; Require this file or autoload mode / compile.
;; Snippets can be enabled by simply enabling `yas-minor-mode' in
;; `nand-hdl-mode-hook' and loading the `nand-hdl-snippet-dir' with
;; `yas-load-directory'.
;;
;; The variable `nand-hdl-directory' needs to be set to point to
;; the installation directory of nand2tetris, ie the directory
;; containing 'tools' and 'projects' directories.
;;
;; ```lisp
;; ;; Example
;; (require 'nand-hdl-mode)
;; (add-hook 'nand-hdl-mode-hook 'yas-minor-mode)
;; ```
;;
;; See `company-nand' for autocompletion setup.
;;
;; ![example](example.png)

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(defgroup nand-hdl nil
  "Major mode for editing NAND hardware description language files."
  :group 'languages
  :prefix "nand-hdl-")

(defcustom nand-hdl-directory "nand2tetris"
  "Location of base directory 'nand2tetris', it should contain
the 'tools' directory with the hardware simulator, etc."
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

(defcustom nand-hdl-shell
  (if (eq system-type 'windows-nt) "cmd.exe" "bash")
  "Shell used to call NAND tools."
  :group 'nand-hdl
  :type 'file)

(defcustom nand-hdl-shell-switches
  (if (eq system-type 'windows-nt) '("/c") '(""))
  "Switches used with `nand-hdl-shell'."
  :group 'nand-hdl
  :type 'listp)

(defvar nand-hdl-snippet-dir nil)
(setq nand-hdl-snippet-dir
      (when load-file-name
        (expand-file-name "snippets" (file-name-directory
                                      load-file-name))))

(defvar nand-hdl-output-buffer "*Nand Output*")


;; ------------------------------------------------------------
;; Internal
;; silence byte-compiler
(defvar yas-load-directory)

(defvar nand-hdl-ext
  (if (eval-when-compile (eq system-type 'windows-nt))
      ".bat" ".sh"))

(defun nand-hdl-call (tool &optional call file dest display)
  (when (not (file-exists-p nand-hdl-directory))
    (user-error "Can't find NAND root directory: %s"
                (or nand-hdl-directory "")))
  (if nand-hdl-directory
      (let ((tp (expand-file-name (concat "tools/" tool nand-hdl-ext)
                                  nand-hdl-directory))
            (switches (mapconcat 'identity nand-hdl-shell-switches " ")))
        (when (not (file-exists-p tp))
          (user-error "%s not found at %s" tool tp))
        (if (and call file)
            (call-process nand-hdl-shell nil dest display switches tp file)
         (concat nand-hdl-shell " " switches " " tp)))))

;; ------------------------------------------------------------
;;* Compilation

(require 'compile)

(defvar nand-hdl-error-regexp-alist
  '((nand-hdl-1
     "In HDL file \\([^,]+\\),\\s-*Line\\s-*\\([0-9]+\\),\\([^:]+\\)" 1 2)
    (nand-hdl-2
     "\\(Chip\\)\\s-*\\([^ ]+\\).* load \\(.*\\)" 3 0 0 2 2 (2 compilation-error-face))
    (nand-hdl-3
      "\\(Comparison\\) failure at [lL]ine \\([0-9]+\\)" 1 2 nil 2)
    ))

;; debug
(defun nand-hdl-replace-compile-regexp ()
  (interactive)
  (mapcar (lambda (item)
            (cl-delete item compilation-error-regexp-alist-alist
                       :test (lambda (x y) (eq (car x) (car y))))
            (push item compilation-error-regexp-alist-alist))
          nand-hdl-error-regexp-alist))

;; Hack to find associated files from compilation buffer when the emulator
;; doesn't specify a filename
(defun nand-hdl-process-setup ()
  (let ((file (file-name-sans-extension (buffer-file-name))))
    (dolist (x '("tst" "out" "hdl" "cmp"))
      (setenv (concat x "-file") (concat file "." x)))))

(defun nand-hdl-parse-errors-filename (filename)
  (cond
   ((string= filename "Comparison")
    (getenv "out-file"))
   (t (getenv "hdl-file"))))

(defun nand-hdl-add-compile-regexp ()
  (interactive)
  (when (not (assoc 'nand-hdl-3 compilation-error-regexp-alist-alist))
    (mapcar (lambda (item)
              (push (car item) compilation-error-regexp-alist)
              (push item compilation-error-regexp-alist-alist))
            nand-hdl-error-regexp-alist)))
(add-hook 'compilation-mode-hook 'nand-hdl-add-compile-regexp)

;; @@FIXME: Jump to missing line of chip? Line number not given in output
;; (defun nand-hdl-error-line ()
;;   (compilation-parse-errors
;;    (match-beginning 0)
;;    (line-end-position)
;;    ("Chip\\s-*\\([^]+\\)")
;;    (let ((buff (find-file-noselect (expand-file-name file dir))))
;;      (with-current-buffer buff
;;        (goto-char (point-min))
;;        (re-search-forward (regexp-quote (match-string-no-properties 1)) nil t 1)
;;        (match-beginning 0)))))

;; ------------------------------------------------------------
;;* User Functions

(defun nand-hdl-run (&optional silent compile wait)
  "Run chip in simulator and display output:
1. By default in `nand-hdl-output-buffer'
2. If SILENT is non-nil just run without displaying output, if WAIT is 0
run asynchronously.
3. If COMPILE is non-nil in compilation buffer."
  (interactive)
  (save-buffer)
  (let ((sim "HardwareSimulator"))
    (nand-hdl-process-setup)
    (let* ((file (file-name-sans-extension (buffer-file-name)))
           (test-file (concat file ".tst")))
      (if compile
          (let ((compilation-read-command)
                (compile-command (concat (nand-hdl-call sim) " " test-file))
                ;; Allow handling jumping to .out and .cmp files when the emulator
                ;; doesn't specify any filenames in the output
                (compilation-process-setup-function
                 #'(lambda () (setq-local compilation-parse-errors-filename-function
                                     'nand-hdl-parse-errors-filename))))
            (compile compile-command))
        (if silent (nand-hdl-call sim t test-file wait)
          (nand-hdl-call sim t test-file nand-hdl-output-buffer t)
          (pop-to-buffer nand-hdl-output-buffer))))))

(defun nand-hdl-compile ()
  "Run chip in simulator and output to compilation buffer."
  (interactive)
  (nand-hdl-run nil t))

(defun nand-hdl-expected ()
  "Show the truth table (.cmp) file for this chip in another window."
  (interactive)
  (let ((file (file-name-sans-extension (buffer-file-name))))
    (find-file-other-window (expand-file-name (concat file ".cmp") file))))

(defun nand-hdl-output ()
  "Show the output of previous run for this chip."
  (interactive)
  (let* ((file (file-name-sans-extension (buffer-file-name)))
         (out (expand-file-name (concat file ".out") file)))
    (unless (file-exists-p out)
      (user-error "File %s doesn't exist, has it been run?" out))
    (find-file-other-window out)))

(defun nand-hdl-compare (&optional run-first)
  "Show comparison between output and expected results in other window."
  (interactive "P")
  (let* ((buff (get-buffer-create nand-hdl-output-buffer))
         (inhibit-read-only t)
         (file (file-name-sans-extension (buffer-file-name)))
         (cmp (expand-file-name (concat file ".cmp") file))
         (out (expand-file-name (concat file ".out") file))
         (run-first (or run-first
                        current-prefix-arg
                        (not (file-exists-p out)))))
    (when run-first (nand-hdl-run t))
    (if (and (file-exists-p cmp) (file-exists-p out))
        (progn
          (with-current-buffer buff
           (erase-buffer)
           (insert "Expected:\n")
           (insert-file-contents cmp)
           (goto-char (point-max))
           (insert "\n\nOutput:\n")
           (insert-file-contents out))
          (pop-to-buffer buff))
      (message "Output not created... compiling")
      (nand-hdl-run nil t))))

;; or just diff .out and .cmp file, but for compatibility with
;; systems with no diff, could add elisp version,
;; (defun nand-hdl-highlight-diffs ()
;;   (interactive)
;;   (let* ((file (file-name-sans-extension (buffer-file-name)))
;;          (out (expand-file-name (concat file ".out") file))
;;          (cmp (expand-file-name (concat file ".cmp") file)))
;;     (shell-command-to-string
;;      (format "diff --unchanged-line-format=\"\" --new-line-format= %s %s"
;;              "" "%dn" out cmp))))


;; ------------------------------------------------------------
;;* Completion

;; 
(defun nand-hdl-vars-before-point ()
  (save-excursion
    (let (vin vout)
      (goto-char (point-min))
      (forward-comment (point-max))
      (let* ((in (progn
                   (re-search-forward "IN" nil t 1)
                   (match-end 0)))
             (out (progn
                    (re-search-forward "OUT" nil t 1)
                    (match-beginning 0)))
             (parts (progn
                      (re-search-forward "PARTS:" nil t 1)
                      (match-beginning 0))))
        (goto-char in)
        (while (re-search-forward "[a-zA-Z]+" out t)
          (push (match-string-no-properties 0) vin))
        (goto-char out)
        (forward-char 3)
        (while (re-search-forward "[a-zA-Z]+" parts t)
          (push (match-string-no-properties 0) vout)))
      `((,vin ,vout)))))


;; ------------------------------------------------------------
;;* Font-lock 
(defconst nand-hdl-keywords
  '("CHIP" "IN" "OUT" "PARTS" "BUILTIN" "CLOCKED"))

(defvar nand-hdl-font-lock-keywords
  `(("\\(?:CHIP\\|BUILTIN\\)\\s-*\\([A-Za-z0-9]+\\)" 1 font-lock-function-name-face)
    ("\\(?:IN\\|OUT\\)\\([^][ ,]+\\)" 1 font-lock-variable-name-face)
    (,(regexp-opt nand-hdl-keywords) . font-lock-builtin-face)
    ("\\([a-zA-Z0-9]+\\)\\s-*(" 1 font-lock-function-name-face)
    ))

;; (defun nand-hdl-syntax-propertize-function (start end)
;;   (goto-char start)
;; ("\\(?:/\\*\\*\\([^\\(?:*/)]+\\)\\)" 1 'nand-hdl-doc-face)
;; )
;; (defconst nand-hdl-syntax-propertize nil)
;; (eval-and-compile
;;   (defconst nand-hdl-doc-re
;;     "\\(?:/\\*\\*\\([^\\(?:*/)]+)"

;;* Indentation
(require 'smie)

(defconst nand-hdl-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc "PARTS" ":") (nonassoc "IN" "OUT") (assoc ",") (assoc ";" "\n")))))

(defun nand-hdl-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) nand-hdl-indent)
    (`(:before . ";")
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . ,(or `"IN" `"OUT")) nand-hdl-indent-declarations)
    (`(:after . ,(or `"IN" `"OUT")) nand-hdl-indent-declarations)
    (`(:after . "PARTS") nand-hdl-indent-parts)
    (`(:before . "PARTS") nand-hdl-indent-parts)
    (`(:before . ":")
     (if (smie-rule-hanging-p)
         (- nand-hdl-indent nand-hdl-indent-parts)))
    (`(:list-intro . ,(or `"\n" `"")) t)))

;; ------------------------------------------------------------
;;* Abbrevs
(defun nand-hdl-define-abbrev (table name expansion &optional hook)
  (condition-case nil
      (define-abbrev-table name expansion hook 0 t)
    (error
     (define-abbrev-table table name expansion hook))))

;;* snippets
(defun nand-hdl-maybe-load-snippets ()
  (when (and (bound-and-true-p yas-minor-mode)
             (file-exists-p nand-hdl-snippet-dir))
    (yas-load-directory nand-hdl-snippet-dir)))
(eval-after-load "yas-minor-mode" '(nand-hdl-maybe-load-snippets))


;; ------------------------------------------------------------
;;* Major mode

;; Syntax (c/c++ style comments)
(defvar nand-hdl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\* ". 23" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?= "." st)
    st)
  "Sytax for `nand-hdl-mode'")

;; Menu
(defvar nand-hdl-menu
  '("NandHDL"
    ["Compile" nand-hdl-compile :help "Run with output to compilation buffer"]
    ["Compare" nand-hdl-compare
     :help "Compare output to expected (run first if required)"]
    ["Run" nand-hdl-run :help "Run with output to buffer"]
    ["Show Expected" nand-hdl-expected :help "Show truth table in other window"]
    ["Show Output" nand-hdl-output :help "Show output from run"]))

;; Map
(defvar nand-hdl-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define nil map nil nand-hdl-menu)
    (define-key map (kbd "<f5>")    #'nand-hdl-compile)
    (define-key map (kbd "C-c C-e") #'nand-hdl-expected)
    (define-key map (kbd "C-c C-c") #'nand-hdl-compare)
    (define-key map (kbd "C-c C-o") #'nand-hdl-output)
    (define-key map (kbd "C-c C-r") #'nand-hdl-run)
    map))

;;;###autoload
(define-derived-mode nand-hdl-mode prog-mode "NandHDL"
  "Major mode for editing NAND hardware description files (.hdl).\n
\\{nand-hdl-mode-map}"
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local font-lock-defaults
              `(nand-hdl-font-lock-keywords nil nil nil))
  ;; (setq-local syntax-propertize-function nand-hdl-syntax-propertize)
  (setq-local imenu-generic-expression
              '((nil "^\\(?:CHIP\\|BUILTIN\\)\\s-*\\([^ {]+\\)" 1)
                (nil "\\(PARTS\\):" 1)))
  (setq-local outline-regexp "^\\(?:CHIP\\|BUILTIN\\)")
  (smie-setup nand-hdl-grammar #'nand-hdl-rules
              :forward-token #'smie-default-forward-token
              :backward-token #'smie-default-backward-token))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hdl\\'" . nand-hdl-mode))

(provide 'nand-hdl-mode)

;;; nand-hdl-mode.el ends here
