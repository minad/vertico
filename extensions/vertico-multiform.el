;;; vertico-multiform.el --- Configure Vertico in different forms per command -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.17"))
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

(defcustom vertico-multiform-command-modes nil
  "Alist of commands and list of modes to turn on per command.
Takes precedence over `vertico-multiform-category-modes'."
  :group 'vertico
  :type '(alist :key-type symbol :value-type (repeat symbol)))

(defcustom vertico-multiform-category-modes nil
  "Alist of categories and list of modes to turn on per category.
Has lower precedence than `vertico-multiform-command-modes'."
  :group 'vertico
  :type '(alist :key-type symbol :value-type (repeat symbol)))

(defcustom vertico-multiform-command-settings nil
  "Alist of commands and alist of variables to set per command.
Takes precedence over `vertico-multiform-category-settings'."
  :group 'vertico
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type sexp)))

(defcustom vertico-multiform-category-settings nil
  "Alist of categories and alist of variables to set per category.
Has lower precedence than `vertico-multiform-command-settings'."
  :group 'vertico
  :type '(alist :key-type symbol
                :value-type (alist :key-type symbol :value-type sexp)))

(defun vertico-multiform--advice (&rest app)
  "Advice for `vertico--advice' switching modes on and off.
APP is the original function call."
  (let ((modes 'init)
        (setup (make-symbol "vertico-multiform--setup"))
        (exit (make-symbol "vertico-multiform--exit"))
        (depth (1+ (recursion-depth))))
    (fset setup
          (lambda ()
            (when (eq modes 'init)
              (let ((cat (completion-metadata-get
                          (completion-metadata
                           (buffer-substring (minibuffer-prompt-end)
                                             (max (minibuffer-prompt-end) (point)))
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
                          'category)))
                (dolist (setting (or (and cat (alist-get cat vertico-multiform-category-settings))
                                     (alist-get this-command vertico-multiform-command-settings)))
                  (set (make-local-variable (car setting)) (cdr setting)))
                (setq modes
                      (mapcar (lambda (m)
                                (let ((v (intern (format "vertico-%s-mode" m))))
                                  (if (fboundp v) v m)))
                              (or (and cat (alist-get cat vertico-multiform-category-modes))
                                  (alist-get this-command vertico-multiform-command-modes))))))
            (cond
             ((= depth (recursion-depth))
              (mapc (lambda (f) (funcall f 1)) modes))
             ((= (1+ depth) (recursion-depth))
              (mapc (lambda (f) (funcall f -1)) modes)))))
    (fset exit
          (lambda ()
            (cond
             ((= depth (recursion-depth))
              (mapc (lambda (f) (funcall f -1)) modes))
             ((= (1+ depth) (recursion-depth))
              (mapc (lambda (f) (funcall f 1)) modes)))))
    ;; NOTE: The setup/exit nesting is only correct for shallow recursions.
    ;; Hopefully nobody is crazy enough to work at recursion level 99.
    (add-hook 'minibuffer-setup-hook setup (+ -99 depth))
    (add-hook 'minibuffer-exit-hook exit (- 99 depth))
    (unwind-protect
        (apply app)
      (remove-hook 'minibuffer-setup-hook setup)
      (remove-hook 'minibuffer-exit-hook exit))))

;;;###autoload
(define-minor-mode vertico-multiform-mode
  "Configure Vertico in various forms per command."
  :global t :group 'vertico
  (if vertico-multiform-mode
      (advice-add #'vertico--advice :around #'vertico-multiform--advice)
    (advice-remove #'vertico--advice #'vertico-multiform--advice)))

(provide 'vertico-multiform)
;;; vertico-multiform.el ends here
