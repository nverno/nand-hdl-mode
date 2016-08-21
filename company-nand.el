;;; company-nand --- Emacs completion support for NAND hdl files.

;; Author: Noah Peart <noah.v.peart@gmail.com>
;; URL: https://github.com/nverno/nand-hdl-mode
;; Package-Requires: ((company "0.8.0") (cl-lib "0.5.0"))
;; Copyright (C) 2016, Noah Peart, all rights reserved.
;; Created: 21 August 2016

;;; Commentary:

;;  Emacs autocompletion backend for `nand-hdl-mode' using `company-mode'.

;;; Installation:

;; Install `company-mode' and add this file to `load-path'.
;; Then either compile/create autoloads and load autoloads files,
;; or require the file in your init file:

;; ```lisp
;; (require 'company-nand) ; or autoload
;;
;; ;; Example hook
;; (add-hook 'nand-hdl-mode-hook
;;           #'(lambda ()
;;               (set (make-local-variable 'company-backends)
;;                    '((company-nand company-dabbrev-code)
;;                      company-dabbrev))))
;; ```

;;; Code:
(eval-when-compile
  (require 'cl-lib))
(require 'company)

(defgroup company-nand nil
  "Company completion backend for `nand-hdl-mode'."
  :group 'company :group 'nand-hdl-mode
  :prefix "company-nand-")

(defcustom company-nand-modes '(nand-hdl-mode)
  "Modes to activate `company-nand'."
  :group 'company-nand
  :type 'sexp)

(defvar company-nand-info--dir nil)
(setq company-nand-info--dir
      (when load-file-name
        (expand-file-name "info" (file-name-directory load-file-name))))

(defcustom company-nand-info-dir company-nand-info--dir
  "Location of info directory from `company-nand' files."
  :group 'company-nand
  :type 'file)

(defcustom company-nand-annotation t
  "If non-nil and boolean, show signature in company annotation, as 
well as in minibuffer.  If string, use that as 
annotation instead (default '<Builtin>')."
  :group 'company-nand
  :type '(choice string boolean))

;; ------------------------------------------------------------
;; Internal
(defconst company-nand-keywords
  '("CHIP" "IN" "OUT" "PARTS" "BUILTIN" "CLOCKED"))

;;* completion candidates
(defvar company-nand-candidates-list nil
  "Cache completion candidates.")

(defun company-nand-read (file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name file company-nand-info-dir))
    (let ((data (car (read-from-string
                      (buffer-substring-no-properties
                       (point-min) (point-max))))))
      data)))

(defun company-nand-build-list ()
  "Build candidate list."
  (let ((data (company-nand-read "docs.txt"))
        (sigs (company-nand-read "signatures.txt")))
    (setq company-nand-candidates-list
          (sort
           (cl-remove-duplicates
            (append
             (cl-loop for (k . v) in data
                collect (propertize
                         k
                         'annot (or (and company-nand-annotation
                                         (booleanp company-nand-annotation)
                                         (cdr (assoc k sigs)))
                                    company-nand-annotation
                                    "<Builtin Chip>")
                         'meta (cdr (assoc k sigs))
                         'doc v))
             (cl-loop for i in company-nand-keywords
                collect (propertize i
                                    'meta "keyword"
                                    'annot "<Keyword>"
                                    'doc "")))
            :test 'string=)
           #'(lambda (x y) (string> y x)))))
  company-nand-candidates-list)

(defun company-nand-prefix ()
  (and (derived-mode-p major-mode company-nand-modes)
       (not (company-in-string-or-comment))
       (company-grab-symbol)))

(defun company-nand-candidates (arg)
  (all-completions arg (or company-nand-candidates-list
                           (company-nand-build-list))))

;; ------------------------------------------------------------
;;* Info
(defun company-nand-meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-nand-doc (candidate)
  (with-temp-buffer
    (insert (get-text-property 0 'doc candidate))
    (goto-char (point-min))
    (company-doc-buffer
     (buffer-substring-no-properties (line-beginning-position)
                                     (point-max)))))

(defun company-nand-annotation (candidate)
  (get-text-property 0 'annot candidate))


;;;###autoload
(defun company-nand (command &optional arg &rest _args)
  "Company backend for NAND hardware description language files."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nand))
    (prefix (company-nand-prefix))
    (annotation (company-nand-annotation arg))
    (meta (company-nand-meta arg))
    (doc-buffer (company-nand-doc arg))
    (sorted t)
    (duplicates nil)
    (candidates (company-nand-candidates arg))))

(provide 'company-nand)

;;; company-nand.el ends here
