;;; vertico-directory.el --- Ido-like direction navigation for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.27"))
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

;; This package is a Vertico extension, which provides Ido-like
;; directory navigation commands. The commands can be bound in the
;; `vertico-map'. Furthermore a cleanup function for shadowed file paths
;; is provided.
;;
;; (define-key vertico-map "\r" #'vertico-directory-enter)
;; (define-key vertico-map "\d" #'vertico-directory-delete-char)
;; (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;;; Code:

(require 'vertico)

(defun vertico-directory--completing-file-p ()
  "Return non-nil when completing file names."
  (eq 'file
      (completion-metadata-get
       (completion-metadata
        (buffer-substring (minibuffer-prompt-end)
                          (max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category)))

;;;###autoload
(defun vertico-directory-enter ()
  "Enter directory or exit completion with current candidate."
  (interactive)
  (if (and (>= vertico--index 0)
           (let ((cand (vertico--candidate)))
             (or (string-suffix-p "/" cand)
                 (and (vertico--remote-p cand)
                      (string-suffix-p ":" cand))))
           ;; Check vertico--base for stepwise file path completion
           (not (equal vertico--base ""))
           (vertico-directory--completing-file-p))
      (vertico-insert)
    (vertico-exit)))

;;;###autoload
(defun vertico-directory-up (&optional n)
  "Delete N directories before point."
  (interactive "p")
  (when (and (> (point) (minibuffer-prompt-end))
             (eq (char-before) ?/)
             (vertico-directory--completing-file-p))
    (let ((path (buffer-substring (minibuffer-prompt-end) (point))) found)
      (when (string-match-p "\\`~[^/]*/\\'" path)
        (delete-minibuffer-contents)
        (insert (expand-file-name path)))
      (dotimes (_ n found)
        (save-excursion
          (let ((end (point)))
            (goto-char (1- end))
            (when (search-backward "/" (minibuffer-prompt-end) t)
              (delete-region (1+ (point)) end)
              (setq found t))))))))

;;;###autoload
(defun vertico-directory-delete-char (&optional n)
  "Delete N directories or chars before point."
  (interactive "p")
  (unless (vertico-directory-up n)
    (backward-delete-char n)))

;;;###autoload
(defun vertico-directory-delete-word (&optional n)
  "Delete N directories or words before point."
  (interactive "p")
  (unless (vertico-directory-up n)
    (let ((pt (point)))
      (backward-word n)
      (delete-region pt (point)))))

;;;###autoload
(defun vertico-directory-tidy ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (eq this-command #'self-insert-command)
    (dolist (ov '(tramp-rfn-eshadow-overlay rfn-eshadow-overlay))
      (when (and (boundp ov)
                 (setq ov (symbol-value ov))
                 (overlay-buffer ov)
                 (= (point) (point-max))
                 (or (>= (- (point) (overlay-end ov)) 2)
                     (eq ?/ (char-before (- (point) 2)))))
        (delete-region (overlay-start ov) (overlay-end ov))))))

;; Emacs 28: Do not show Vertico commands in M-X
(dolist (sym '(vertico-directory-up vertico-directory-enter
               vertico-directory-delete-char vertico-directory-delete-word))
  (put sym 'completion-predicate #'vertico--command-p))

(provide 'vertico-directory)
;;; vertico-directory.el ends here
