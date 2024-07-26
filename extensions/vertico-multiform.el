;;; vertico-multiform.el --- Configure Vertico in different forms per command -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 1.9
;; Package-Requires: ((emacs "27.1") (compat "30") (vertico "1.9"))
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a Vertico extension for fine tuning the Vertico
;; display and other minibuffer modes per command or completion
;; category.  For some commands you may want to use the
;; `vertico-buffer' display and for completion categories like file
;; you prefer the `vertico-grid-mode'.
;;
;; Example:
;;
;;    (setq vertico-multiform-commands
;;          '((consult-line buffer)
;;            (consult-imenu reverse buffer)
;;            (execute-extended-command flat)))
;;
;;    (setq vertico-multiform-categories
;;          '((file buffer grid)
;;            (imenu (:not indexed mouse))
;;            (symbol (vertico-sort-function . vertico-sort-alpha))))
;;
;;    (vertico-multiform-mode)
;;
;; Temporary toggling between the different display modes is possible.
;; The following keys are bound in the `vertico-multiform-map'.
;;
;;   M-B -> `vertico-multiform-buffer'
;;   M-F -> `vertico-multiform-flat'
;;   M-G -> `vertico-multiform-grid'
;;   M-R -> `vertico-multiform-reverse'
;;   M-U -> `vertico-multiform-unobtrusive'
;;   M-V -> `vertico-multiform-vertical'
;;
;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defcustom vertico-multiform-commands nil
  "Alist of commands/regexps and list of settings to turn on per command.
Takes precedence over `vertico-multiform-categories'.  A setting
can either be a mode symbol, a function, an inverted mode symbol
or function, or a cons cell of variable name and value.  The key
t can be used to specify catch all/default settings.  The value
of `this-command' is used as key for the lookup."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp (const t)) :value-type (repeat sexp)))

(defcustom vertico-multiform-categories nil
  "Alist of categories/regexps and list of settings to turn on per category.
See `vertico-multiform-commands' on details about the settings.  The
category settings have lower precedence than
`vertico-multiform-commands'."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp (const t)) :value-type (repeat sexp)))

(defvar vertico-multiform--stack nil)

(defun vertico-multiform--toggle (arg)
  "Toggle modes from stack depending on ARG."
  (when-let ((win (active-minibuffer-window))
             (modes (car vertico-multiform--stack)))
    (when (> arg 0) (setq modes (reverse modes)))
    (with-selected-window win
      (dolist (m modes)
        (if (eq (car-safe m) :not)
            (funcall (cdr m) (- arg))
          (funcall m arg))))))

(defun vertico-multiform--lookup (key list)
  "Lookup symbolic KEY in LIST.
The keys in LIST can be symbols or regexps."
  (and (symbolp key)
       (let (case-fold-search)
         (seq-find (pcase-lambda (`(,x . ,_))
                     (cond
                      ((eq x t))
                      ((symbolp x) (eq key x))
                      ((string-match-p x (symbol-name key)))))
                   list))))

(defun vertico-multiform--setup ()
  "Enable modes at minibuffer setup."
  (let ((cat (compat-call completion-metadata-get
              (completion-metadata (buffer-substring-no-properties
                                    (minibuffer-prompt-end)
                                    (max (minibuffer-prompt-end) (point)))
                                   minibuffer-completion-table
                                   minibuffer-completion-predicate)
              'category))
        (exit (make-symbol "vertico-multiform--exit"))
        (depth (recursion-depth))
        (modes nil))
    (fset exit (lambda ()
                 (when (= depth (recursion-depth))
                   (remove-hook 'minibuffer-exit-hook exit)
                   (vertico-multiform--toggle -1)
                   (pop vertico-multiform--stack))))
    (add-hook 'minibuffer-exit-hook exit)
    (dolist (x (cdr (or (vertico-multiform--lookup this-command vertico-multiform-commands)
                        (vertico-multiform--lookup cat vertico-multiform-categories))))
      (pcase x
        (`(:not . ,fs)
         (dolist (f fs)
           (let ((sym (and (symbolp f) (intern-soft (format "vertico-%s-mode" f)))))
             (push (cons :not (if (and sym (fboundp sym)) sym f)) modes))))
        ((or (pred functionp) (pred symbolp))
         (let ((sym (and (symbolp x) (intern-soft (format "vertico-%s-mode" x)))))
           (push (if (and sym (fboundp sym)) sym x) modes)))
        (`(,k . ,v) (set (make-local-variable k) v))
        (_ (error "Invalid multiform setting %S" x))))
    (push modes vertico-multiform--stack)
    (vertico-multiform--toggle 1)
    (vertico--setup)))

(defvar-keymap vertico-multiform-map
  :doc "Additional keymap activated in multiform mode.")

;;;###autoload
(define-minor-mode vertico-multiform-mode
  "Configure Vertico in various forms per command."
  :global t :group 'vertico
  (when (/= (recursion-depth) 0)
    (warn "vertico-multiform must not be toggled from recursive minibuffers"))
  (when vertico-multiform--stack
    (warn "vertico-multiform state is inconsistent")
    (setq vertico-multiform--stack nil))
  (cl-callf2 rassq-delete-all vertico-multiform-map minor-mode-map-alist)
  (when vertico-multiform-mode
    (push `(vertico--input . ,vertico-multiform-map) minor-mode-map-alist)))

(cl-defmethod vertico--advice (&context (vertico-multiform-mode (eql t)) &rest app)
  (unwind-protect
      (progn
        (vertico-multiform--toggle -1)
        (minibuffer-with-setup-hook #'vertico-multiform--setup
          (apply app)))
    (vertico-multiform--toggle 1)))

(defun vertico-multiform--temporary-mode (mode arg)
  "Enable or disable MODE temporarily in minibuffer given ARG.
ARG can be nil, t, -1, 1 or toggle."
  (unless (minibufferp)
    (user-error "`%s' must be called inside the minibuffer" this-command))
  (unless vertico-multiform-mode
    (user-error "`vertico-multiform-mode' is not enabled"))
  (setq arg (pcase arg
              ('toggle (not (and (boundp mode) (symbol-value mode))))
              ((or 'nil 't) arg)
              (_ (> arg 0))))
  (unless (eq arg (and (boundp mode) (symbol-value mode)))
    (funcall mode (if arg 1 -1))
    (let ((modes (car vertico-multiform--stack))
          (not-mode (cons :not mode)))
      (when arg
        (cl-rotatef not-mode mode))
      (if (member mode modes)
          (setcar vertico-multiform--stack (remove mode modes))
        (push not-mode (car vertico-multiform--stack))))))

(defvar vertico-multiform--display-modes nil)
(defvar-local vertico-multiform--display-last nil)

(defun vertico-multiform-vertical (&optional mode)
  "Toggle to display MODE temporarily in minibuffer.
MODE defaults to the vertical display."
  (interactive)
  (let (last)
    (dolist (m vertico-multiform--display-modes)
      (when (and (boundp m) (symbol-value m))
        (setq last m)
        (vertico-multiform--temporary-mode m -1)))
    (when (eq last mode)
      (setq mode vertico-multiform--display-last))
    (when mode
      (vertico-multiform--temporary-mode mode 1))
    (setq vertico-multiform--display-last last)))

(pcase-dolist (`(,key ,name) '(("M-B" buffer)
                               ("M-F" flat)
                               ("M-G" grid)
                               ("M-R" reverse)
                               ("M-U" unobtrusive) ;; must come after flat
                               ("M-V" vertical)))
  (let ((toggle (intern (format "vertico-multiform-%s" name))))
    (unless (eq name 'vertical)
      (let ((mode (intern (format "vertico-%s-mode" name))))
        (defalias toggle
          (lambda () (interactive) (vertico-multiform-vertical mode))
          (format "Toggle the %s display." name))
        (push mode vertico-multiform--display-modes)))
    (put toggle 'completion-predicate #'vertico--command-p)
    (keymap-set vertico-multiform-map key toggle)))

(provide 'vertico-multiform)
;;; vertico-multiform.el ends here
