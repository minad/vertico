;;; vertico-repeat.el --- Repeat Vertico sessions -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which enables repetition of
;; Vertico sessions via the `vertico-repeat', `vertico-repeat-previous'
;; and `vertico-repeat-select' commands.  If the repeat commands are
;; called from an existing Vertico minibuffer session, only sessions
;; corresponding to the current minibuffer command are offered via
;; completion.
;;
;; It is necessary to register a minibuffer setup hook, which saves
;; the Vertico state for repetition.  In order to save the history
;; across Emacs sessions, enable `savehist-mode' and add
;; `vertico-repeat-history' to `savehist-additional-variables'.
;;
;; (keymap-global-set "M-R" #'vertico-repeat)
;; (keymap-set vertico-map "M-P" #'vertico-repeat-previous)
;; (keymap-set vertico-map "M-N" #'vertico-repeat-next)
;; (keymap-set vertico-map "S-<prior>" #'vertico-repeat-previous)
;; (keymap-set vertico-map "S-<next>" #'vertico-repeat-next)
;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
;;
;; See also the related extension `vertico-suspend', which uses a
;; different technique, relying on recursive minibuffers to suspend
;; the current completion session temporarily while preserving the
;; entire state.

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defcustom vertico-repeat-filter
  '(vertico-repeat
    vertico-repeat-select
    execute-extended-command
    execute-extended-command-for-buffer)
  "List of commands to filter out from the history."
  :type '(repeat symbol)
  :group 'vertico)

(defcustom vertico-repeat-transformers
  (list #'vertico-repeat--filter-empty
        #'vertico-repeat--filter-commands)
  "List of functions to apply to history element before saving."
  :type '(repeat function)
  :group 'vertico)

(declare-function vertico-multiform-vertical "ext:vertico-multiform")
(defvar vertico-multiform--display-modes)
(defvar vertico-repeat-history nil)
(defvar-local vertico-repeat--command nil)
(defvar-local vertico-repeat--input nil)
(defvar-local vertico-repeat--step nil)
(defvar-local vertico-repeat--pos 0)

(defun vertico-repeat--filter-commands (session)
  "Filter SESSION if command is listed in `vertico-repeat-filter'."
  (and (not (memq (car session) vertico-repeat-filter)) session))

(defun vertico-repeat--filter-empty (session)
  "Filter SESSION if input is empty."
  (and (cadr session) (not (equal (cadr session) "")) session))

(defun vertico-repeat--save-input ()
  "Save current minibuffer input."
  (setq vertico-repeat--input (minibuffer-contents-no-properties)))

(defun vertico-repeat--current ()
  "Return the current session datum."
  `(,vertico-repeat--command
    ,vertico-repeat--input
    ,@(and vertico--lock-candidate
           (>= vertico--index 0)
           (list (substring-no-properties
                  (nth vertico--index vertico--candidates))))
    ,@(and (bound-and-true-p vertico-multiform-mode)
           (ensure-list
            (seq-find (lambda (x) (and (boundp x) (symbol-value x)))
                      vertico-multiform--display-modes)))))

(defun vertico-repeat--save-exit ()
  "Save command session in `vertico-repeat-history'."
  (let ((session (vertico-repeat--current))
        (transform vertico-repeat-transformers))
    (while (and transform (setq session (funcall (pop transform) session))))
    (when session
      (add-to-history 'vertico-repeat-history session))))

(defun vertico-repeat--restore (session)
  "Restore Vertico SESSION for `vertico-repeat'."
  (delete-minibuffer-contents)
  (insert (cadr session))
  (setq vertico--lock-candidate
        (when-let ((cand (seq-find #'stringp (cddr session))))
          (vertico--update)
          (when-let ((idx (seq-position vertico--candidates cand)))
            (setq vertico--index idx)
            t)))
  ;; Restore display modes if not modifying the current session
  (when-let (((not (and vertico-repeat--command
                        (eq vertico-repeat--command (car session)))))
             (mode (seq-find #'symbolp (cddr session)))
             ((bound-and-true-p vertico-multiform-mode))
             ((not (and (boundp mode) (symbol-value mode)))))
    (vertico-multiform-vertical mode))
  (vertico--exhibit))

(defun vertico-repeat--run (session)
  "Run Vertico completion SESSION."
  (unless session
    (user-error "No repeatable session"))
  (if (and vertico-repeat--command (eq vertico-repeat--command (car session)))
      (vertico-repeat--restore session)
    (minibuffer-with-setup-hook
        (apply-partially #'vertico-repeat--restore session)
      (command-execute (setq this-command (car session))))))

;;;###autoload
(defun vertico-repeat-save ()
  "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'."
  (when (and vertico--input (symbolp this-command))
    (setq vertico-repeat--command this-command)
    (add-hook 'post-command-hook #'vertico-repeat--save-input nil 'local)
    (add-hook 'minibuffer-exit-hook #'vertico-repeat--save-exit nil 'local)))

;;;###autoload
(defun vertico-repeat-next (n)
  "Repeat Nth next Vertico completion session.
This command must be called from an existing Vertico session
after `vertico-repeat-previous'."
  (interactive "p")
  (vertico-repeat-previous (- n)))

;;;###autoload
(defun vertico-repeat-previous (n)
  "Repeat Nth previous Vertico completion session.
If called from an existing Vertico session, restore the input and
selected candidate for the current command."
  (interactive "p")
  (vertico-repeat--run
   (if (not vertico-repeat--command)
       (and (> n 0) (nth (1- n) vertico-repeat-history))
     (cond
      ((not vertico-repeat--step)
       (setq vertico-repeat--step
             (cons (vertico-repeat--current)
                   (cl-loop for h in vertico-repeat-history
                            if (eq (car h) vertico-repeat--command) collect h))))
      ((= vertico-repeat--pos 0)
       (setcar vertico-repeat--step (vertico-repeat--current))))
     (cl-incf n vertico-repeat--pos)
     (when-let (((>= n 0)) (session (nth n vertico-repeat--step)))
       (setq vertico-repeat--pos n)
       session))))

(define-obsolete-function-alias
  'vertico-repeat-last 'vertico-repeat-previous "1.4")

;;;###autoload
(defun vertico-repeat-select ()
  "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command."
  (interactive)
  (vertico-repeat--run
   (let* ((current-cmd vertico-repeat--command)
          (trimmed
           (delete-dups
            (or
             (cl-loop
              for session in vertico-repeat-history
              if (or (not current-cmd) (eq (car session) current-cmd))
              collect
              (list
               (symbol-name (car session))
               (replace-regexp-in-string
                "\\s-+" " "
                (string-trim (cadr session)))
               session))
             (user-error "No repeatable session"))))
          (max-cmd (cl-loop for (cmd . _) in trimmed
                            maximize (string-width cmd)))
          (formatted (cl-loop
                      for (cmd input session) in trimmed collect
                      (cons
                       (concat
                        (and (not current-cmd)
                             (propertize cmd 'face 'font-lock-function-name-face))
                        (and (not current-cmd)
                             (make-string (- max-cmd (string-width cmd) -4) ?\s))
                        input)
                       session)))
          (enable-recursive-minibuffers t))
     (cdr (assoc (completing-read
                  (if current-cmd
                      (format "History of %s: " current-cmd)
                    "Completion history: ")
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        '(metadata (display-sort-function . identity)
                                   (cycle-sort-function . identity))
                      (complete-with-action action formatted str pred)))
                  nil t nil t)
                 formatted)))))

;;;###autoload
(defun vertico-repeat (&optional arg)
  "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history."
  (interactive "P")
  (if arg (vertico-repeat-select) (vertico-repeat-previous 1)))

(provide 'vertico-repeat)
;;; vertico-repeat.el ends here
