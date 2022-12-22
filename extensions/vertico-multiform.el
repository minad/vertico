;;; vertico-multiform.el --- Configure Vertico in different forms per command -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "1.0"))
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
;; category. For some commands you may want to use the `vertico-buffer'
;; display and for completion categories like file you prefer the
;; `vertico-grid-mode'.
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
;; Temporary toggling between the different display modes is
;; possible. Bind the following commands:
;;
;; (define-key vertico-map "\M-V" #'vertico-multiform-vertical)
;; (define-key vertico-map "\M-G" #'vertico-multiform-grid)
;; (define-key vertico-map "\M-F" #'vertico-multiform-flat)
;; (define-key vertico-map "\M-R" #'vertico-multiform-reverse)
;; (define-key vertico-map "\M-U" #'vertico-multiform-unobtrusive)
;;
;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defcustom vertico-multiform-commands nil
  "Alist of commands/regexps and list of settings to turn on per command.
Takes precedence over `vertico-multiform-categories'. A setting can
either be a mode symbol, a function, an inverted mode symbol or
function, or a cons cell of variable name and value. The key t can be
used to specify catch all/default settings."
  :group 'vertico
  :type '(alist :key-type (choice symbol regexp (const t)) :value-type (repeat sexp)))

(defcustom vertico-multiform-categories nil
  "Alist of categories/regexps and list of settings to turn on per category.
See `vertico-multiform-commands' on details about the settings. The
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
       (seq-find (lambda (x)
                   (cond
                    ((eq (car x) t))
                    ((symbolp (car x)) (eq key (car x)))
                    ((string-match-p (car x) (symbol-name key)))))
                 list)))

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
  (when (/= (recursion-depth) 0)
    (warn "vertico-multiform must not be toggled from recursive minibuffers"))
  (when vertico-multiform--stack
    (warn "vertico-multiform state is inconsistent")
    (setq vertico-multiform--stack nil))
  (if vertico-multiform-mode
      (advice-add #'vertico--advice :override #'vertico-multiform--advice)
    (advice-remove #'vertico--advice #'vertico-multiform--advice)))

(defun vertico-multiform--ensure ()
  "Ensure that multiform mode is enabled."
  (unless (minibufferp)
    (user-error "`%s' must be called inside the minibuffer" this-command))
  (unless vertico-multiform-mode
    (user-error "`vertico-multiform-mode' is not enabled")))

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

(defvar-local vertico-multiform--display-last nil)

(defun vertico-multiform-vertical (&optional mode)
  "Toggle to display MODE temporarily in minibuffer.
MODE defaults to the vertical display."
  (interactive)
  (let (last)
    (dolist (m '(vertico-unobtrusive-mode vertico-flat-mode
                 vertico-grid-mode vertico-reverse-mode))
      (when (and (boundp m) (symbol-value m))
        (setq last m)
        (vertico-multiform--temporary-mode m -1)))
    (when (eq last mode)
      (setq mode vertico-multiform--display-last))
    (when mode
      (vertico-multiform--temporary-mode mode 1))
    (setq vertico-multiform--display-last last)))

(put #'vertico-multiform-vertical 'completion-predicate #'vertico--command-p)

(defmacro vertico-multiform--define-display-toggle (name)
  "Define toggle for display mode NAME."
  (let ((sym (intern (format "vertico-multiform-%s" name))))
    `(progn
       (defun ,sym ()
         ,(format "Toggle the %s display." name)
         (interactive)
         (vertico-multiform-vertical ',(intern (format "vertico-%s-mode" name))))
       (put ',sym 'completion-predicate #'vertico--command-p))))

(vertico-multiform--define-display-toggle grid)
(vertico-multiform--define-display-toggle flat)
(vertico-multiform--define-display-toggle reverse)
(vertico-multiform--define-display-toggle unobtrusive)

(provide 'vertico-multiform)
;;; vertico-multiform.el ends here
