;;; vertico-repeat.el --- Repeat the last Vertico session -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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

;; This package is a Vertico extension, which allows to repeat the last
;; Vertico session via the `vertico-repeat' command.
;;
;; (global-set-key "\M-r" #'vertico-repeat)
;; (global-set-key "\M-R" #'vertico-repeat-history)

;;; Code:

(require 'vertico)

(defcustom vertico-repeat-filter
  '("^vertico-repeat-"
    execute-extended-command)
  "Filter commands from the `vertico-repeat' command history."
  :type '(repeat regexp)
  :group 'vertico)

(defvar-local vertico-repeat--restore nil)
(defvar vertico-repeat--input nil)
(defvar vertico-repeat--command nil)
(defvar vertico-repeat--candidate nil)
(defvar vertico-repeat--history nil)

(defun vertico-repeat--save-input ()
  "Save current minibuffer content for `vertico-repeat'."
  (setq vertico-repeat--input (minibuffer-contents-no-properties)))

(defun vertico-repeat--save-candidate ()
  "Save currently selected candidate for `vertico-repeat'."
  (setq vertico-repeat--candidate
        (and vertico--lock-candidate
             (>= vertico--index 0)
             (substring-no-properties (nth vertico--index vertico--candidates))))
  (unless (string-match-p (string-join
                           (mapcar (lambda (x)
                                     (if (stringp x) x (format "\\`%s\\'" x)))
                                   vertico-repeat-filter)
                           "\\|")
                          (symbol-name vertico-repeat--command))
    (let ((elem (list vertico-repeat--candidate vertico-repeat--command vertico-repeat--input)))
      (cond
       ((not (equal (cdr elem) (cdar vertico-repeat--history)))
        (setq vertico-repeat--history (delete elem vertico-repeat--history))
        (add-to-history 'vertico-repeat--history elem))
       ((and (car vertico-repeat--history) vertico-repeat--candidate)
        (setf (caar vertico-repeat--history) vertico-repeat--candidate))))))

(defun vertico-repeat--restore ()
  "Restore Vertico status for `vertico-repeat'."
  (setq vertico-repeat--restore t)
  (delete-minibuffer-contents)
  (insert vertico-repeat--input)
  (when vertico-repeat--candidate
    (run-at-time 0 nil
                 (lambda ()
                   (when-let (idx (seq-position vertico--candidates vertico-repeat--candidate))
                     (setq vertico--index idx
                           vertico--lock-candidate t)
                     (vertico--exhibit))))))

;;;###autoload
(defun vertico-repeat--save ()
  "Save Vertico status for `vertico-repeat'."
  (when vertico--input
    (unless vertico-repeat--restore
      (setq vertico-repeat--command (if (boundp 'minibuffer-current-command)
                                        minibuffer-current-command
                                      this-command)
            vertico-repeat--input ""
            vertico-repeat--candidate nil
            vertico-repeat--restore nil))
    (add-hook 'post-command-hook #'vertico-repeat--save-input nil 'local)
    (add-hook 'minibuffer-exit-hook #'vertico-repeat--save-candidate nil 'local)))

;;;###autoload
(defun vertico-repeat ()
  "Repeat last Vertico completion session."
  (interactive)
  (unless vertico-repeat--command
    (user-error "No repeatable Vertico session"))
  (minibuffer-with-setup-hook
      #'vertico-repeat--restore
    (command-execute (setq this-command vertico-repeat--command))))

;;;###autoload
(add-hook 'minibuffer-setup-hook #'vertico-repeat--save)

(defun vertico-repeat-history ()
  "Select from command history and call `vertico-repeat'."
  (interactive)
  (let* ((cands (or
                 (delete-dups
                  (mapcar (lambda (elem)
                            (cons (concat
                                   (symbol-name (cadr elem))
                                   (propertize " " 'display '(space :align-to (+ left 40))) ;; TODO compute aligment
                                   (propertize (string-trim (caddr elem))
                                               'face 'font-lock-string-face)
                                   (propertize " " 'display '(space :align-to (+ left 80))) ;; TODO compute aligment
                                   (when (car elem)
                                     (propertize (string-trim
                                                  (replace-regexp-in-string "[\x100000-\x10FFFD]*" "" (car elem))) ;; TODO consult hack
                                                 'face 'font-lock-comment-face)))
                                  elem))
                          vertico-repeat--history))
                 (user-error "Command history is empty")))
         (elem (cdr (assoc (completing-read
                            "Repeat: "
                            (lambda (str pred action)
                              (if (eq action 'metadata)
                                  '(metadata (display-sort-function . identity)
                                             (cycle-sort-function . identity))
                                (complete-with-action action cands str pred)))
                            nil t nil t)
                           cands))))
    (when elem
      (setq vertico-repeat--candidate (car elem)
            vertico-repeat--command (cadr elem)
            vertico-repeat--input (caddr elem))
      (vertico-repeat))))

(provide 'vertico-repeat)
;;; vertico-repeat.el ends here
