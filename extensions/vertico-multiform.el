;;; vertico-multiform.el --- Configure Vertico in different forms per command -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.18"))
;; Homepage: https://github.com/minad/vertico

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a Vertico extension for fine tuning the Vertico
;; display and other minibuffer modes per command or completion
;; category. For some commands you may want to use the `vertico-buffer'
;; display and for completion categories like file you prefer the
;; `vertico-grid-mode'.
;;
;; Example:
;;
;;    (setq vertico-multiform-command-modes
;;          '((consult-line buffer)
;;            (consult-imenu reverse buffer)
;;            (execute-extended-command flat)))
;;
;;    (setq vertico-multiform-category-modes
;;          '((file buffer grid)))
;;
;;    (vertico-multiform-mode)

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib))

(defcustom vertico-multiform-command-modes nil
  "Alist of commands/regexps and list of modes to turn on per command.
Takes precedence over `vertico-multiform-category-modes'."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp) :value-type (repeat symbol)))

(defcustom vertico-multiform-category-modes nil
  "Alist of categories/regexps and list of modes to turn on per category.
Has lower precedence than `vertico-multiform-command-modes'."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp) :value-type (repeat symbol)))

(defcustom vertico-multiform-command-settings nil
  "Alist of commands/regexps and alist of variables to set per command.
Takes precedence over `vertico-multiform-category-settings'."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp)
                :value-type (alist :key-type symbol :value-type sexp)))

(defcustom vertico-multiform-category-settings nil
  "Alist of categories/regexps and alist of variables to set per category.
Has lower precedence than `vertico-multiform-command-settings'."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp)
                :value-type (alist :key-type symbol :value-type sexp)))

(defvar vertico-multiform--stack nil)

(defun vertico-multiform--toggle (arg)
  "Toggle modes from stack depending on ARG."
  (when-let (win (active-minibuffer-window))
    (with-selected-window win
      (dolist (f (car vertico-multiform--stack))
        (funcall f arg)))))

(defun vertico-multiform--lookup (key list)
  "Lookup symbolic KEY in LIST.
The keys in LIST can be symbols or regexps."
  (and (symbolp key)
       (cl-loop for x in list
                if (if (symbolp (car x))
                       (eq key (car x))
                     (string-match-p (car x) (symbol-name key)))
                return x)))

(defun vertico-multiform--setup ()
  "Enable modes at minibuffer setup."
  (let ((cat (completion-metadata-get
              (completion-metadata
               (buffer-substring (minibuffer-prompt-end)
                                 (max (minibuffer-prompt-end) (point)))
               minibuffer-completion-table
               minibuffer-completion-predicate)
              'category))
        (exit (make-symbol "vertico-multiform--exit"))
        (depth (recursion-depth)))
    (fset exit (lambda ()
                 (when (= depth (recursion-depth))
                   (remove-hook 'minibuffer-exit-hook exit)
                   (vertico-multiform--toggle -1)
                   (pop vertico-multiform--stack))))
    (add-hook 'minibuffer-exit-hook exit)
    (dolist (x (cdr (or (vertico-multiform--lookup this-command vertico-multiform-command-settings)
                        (and cat (vertico-multiform--lookup cat vertico-multiform-category-settings)))))
      (set (make-local-variable (car x)) (cdr x)))
    (push (mapcar (lambda (m)
                    (let ((v (intern (format "vertico-%s-mode" m))))
                      (if (fboundp v) v m)))
                  (cdr (or (vertico-multiform--lookup this-command vertico-multiform-command-modes)
                           (and cat (vertico-multiform--lookup cat vertico-multiform-category-modes)))))
          vertico-multiform--stack)
    (vertico-multiform--toggle 1)
    (vertico--setup)))

(defun vertico-multiform--advice (&rest app)
  "Override advice for `vertico--advice' switching modes on and off.
APP is the original function call."
  (unwind-protect
      (progn
        (vertico-multiform--toggle -1)
        (minibuffer-with-setup-hook #'vertico-multiform--setup
          (apply app)))
    (vertico-multiform--toggle 1)))

;;;###autoload
(define-minor-mode vertico-multiform-mode
  "Configure Vertico in various forms per command."
  :global t :group 'vertico
  (if vertico-multiform-mode
      (advice-add #'vertico--advice :override #'vertico-multiform--advice)
    (advice-remove #'vertico--advice #'vertico-multiform--advice)))

(provide 'vertico-multiform)
;;; vertico-multiform.el ends here
