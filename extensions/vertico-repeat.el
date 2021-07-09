;;; vertico-repeat.el --- Repeat the last Vertico session -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

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

;;; Code:

(require 'vertico)

(defvar vertico-repeat--input nil)
(defvar vertico-repeat--command nil)
(defvar vertico-repeat--candidate nil)

(defun vertico-repeat--save-input ()
  "Save current minibuffer content for `vertico-repeat'."
  (setq vertico-repeat--input (minibuffer-contents)))

(defun vertico-repeat--save-candidate ()
  "Save currently selected candidate for `vertico-repeat'."
  (setq vertico-repeat--candidate
        (and vertico--lock-candidate
             (>= vertico--index 0)
             (nth vertico--index vertico--candidates))))

(defun vertico-repeat--save ()
  "Save Vertico status for `vertico-repeat'."
  (unless (eq real-this-command #'vertico-repeat)
    (setq vertico-repeat--command (if (boundp 'minibuffer-current-command)
                                      minibuffer-current-command
                                    this-command)
          vertico-repeat--input ""
          vertico-repeat--candidate nil))
  (add-hook 'post-command-hook #'vertico-repeat--save-input nil 'local)
  (add-hook 'minibuffer-exit-hook #'vertico-repeat--save-candidate nil 'local))

(defun vertico-repeat--restore ()
  "Restore Vertico status for `vertico-repeat'."
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
(defun vertico-repeat ()
  "Repeat last Vertico completion session."
  (interactive)
  (unless vertico-repeat--command
    (user-error "No repeatable Vertico session"))
  (minibuffer-with-setup-hook
      #'vertico-repeat--restore
    (command-execute (setq this-command vertico-repeat--command))))

;;;###autoload
(advice-add #'vertico--setup :after #'vertico-repeat--save)

(provide 'vertico-repeat)
;;; vertico-repeat.el ends here
