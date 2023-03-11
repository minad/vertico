;;; vertico-repeat.el --- Repeat Vertico sessions -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "1.2"))
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
;; Vertico sessions via the `vertico-repeat', `vertico-repeat-last' and
;; `vertico-repeat-select' commands.  If the repeat commands are called
;; from an existing Vertico minibuffer session, only sessions
;; corresponding to the current minibuffer command are offered via
;; completion.  It is necessary to register a minibuffer setup hook,
;; which saves the Vertico state for repetition.  In order to save the
;; history across Emacs sessions, enable `savehist-mode' and add
;; `vertico-repeat-history' to `savehist-additional-variables'.
;;
;; (keymap-global-set "M-R" #'vertico-repeat)
;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

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

(defvar vertico-repeat-history nil)
(defvar-local vertico-repeat--command nil)
(defvar-local vertico-repeat--input nil)

(defun vertico-repeat--filter-commands (session)
  "Filter SESSION if command is listed in `vertico-repeat-filter'."
  (and (not (memq (car session) vertico-repeat-filter)) session))

(defun vertico-repeat--filter-empty (session)
  "Filter SESSION if input is empty."
  (and (cadr session) (not (equal (cadr session) "")) session))

(defun vertico-repeat--save-input ()
  "Save current minibuffer input."
  (setq vertico-repeat--input (minibuffer-contents-no-properties)))

(defun vertico-repeat--save-exit ()
  "Save command session in `vertico-repeat-history'."
  (let ((session `(,vertico-repeat--command
                   ,vertico-repeat--input
                   ,@(and vertico--lock-candidate
                          (>= vertico--index 0)
                          (list (substring-no-properties
                                 (nth vertico--index vertico--candidates))))))
        (transform vertico-repeat-transformers))
    (while (and transform (setq session (funcall (pop transform) session))))
    (when session
      (add-to-history 'vertico-repeat-history session))))

(defun vertico-repeat--restore (session)
  "Restore Vertico SESSION for `vertico-repeat'."
  (delete-minibuffer-contents)
  (insert (cadr session))
  (when (caddr session)
    (vertico--update)
    (when-let (idx (seq-position vertico--candidates (caddr session)))
      (setq vertico--index idx
            vertico--lock-candidate t)))
  (vertico--exhibit))

;;;###autoload
(defun vertico-repeat-save ()
  "Save Vertico session for `vertico-repeat'.
This function must be registered as `minibuffer-setup-hook'."
  (when (and vertico--input (symbolp this-command))
    (setq vertico-repeat--command this-command)
    (add-hook 'post-command-hook #'vertico-repeat--save-input nil 'local)
    (add-hook 'minibuffer-exit-hook #'vertico-repeat--save-exit nil 'local)))

;;;###autoload
(defun vertico-repeat-last (&optional session)
  "Repeat last Vertico completion SESSION.
If called interactively from an existing Vertico session,
`vertico-repeat-last' will restore the last input and
last selected candidate for the current command."
  (interactive
   (list (or (if vertico-repeat--command
                 (seq-find (lambda (x) (eq (car x) vertico-repeat--command))
                           vertico-repeat-history)
               (car vertico-repeat-history))
             (user-error "No repeatable Vertico session"))))
  (if (and vertico-repeat--command (eq vertico-repeat--command (car session)))
      (vertico-repeat--restore session)
    (minibuffer-with-setup-hook
        (apply-partially #'vertico-repeat--restore session)
      (command-execute (setq this-command (car session))))))

;;;###autoload
(defun vertico-repeat-select ()
  "Select a Vertico session from the session history and repeat it.
If called from an existing Vertico session, you can select among
previous sessions for the current command."
  (interactive)
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
              (if (caddr session)
                  (replace-regexp-in-string
                   "\\s-+" " "
                   (string-trim (caddr session)))
                "")
              session))
            (user-error "No repeatable Vertico session"))))
         (max-cmd (cl-loop for (cmd . _) in trimmed
                           maximize (string-width cmd)))
         (max-input (cl-loop for (_cmd input . _) in trimmed
                             maximize (string-width input)))
         (formatted (cl-loop
                     for (cmd input cand session) in trimmed collect
                     (cons
                      (concat
                       (and (not current-cmd)
                            (propertize cmd 'face 'font-lock-function-name-face))
                       (and (not current-cmd)
                            (make-string (- max-cmd (string-width cmd) -4) ?\s))
                       input
                       (make-string (- max-input (string-width input) -4) ?\s)
                       (and cand (propertize cand 'face 'font-lock-comment-face)))
                      session)))
         (enable-recursive-minibuffers t)
         (selected (or (cdr (assoc (completing-read
                                    (if current-cmd
                                        (format "History of %s: " current-cmd)
                                      "Completion history: ")
                                    (lambda (str pred action)
                                      (if (eq action 'metadata)
                                          '(metadata (display-sort-function . identity)
                                                     (cycle-sort-function . identity))
                                        (complete-with-action action formatted str pred)))
                                    nil t nil t)
                                   formatted))
                       (user-error "No session selected"))))
    (vertico-repeat-last selected)))

;;;###autoload
(defun vertico-repeat (&optional arg)
  "Repeat last Vertico session.
If prefix ARG is non-nil, offer completion menu to select from session history."
  (interactive "P")
  (call-interactively
   (if arg #'vertico-repeat-select #'vertico-repeat-last)))

(provide 'vertico-repeat)
;;; vertico-repeat.el ends here
