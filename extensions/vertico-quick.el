;;; vertico-quick.el --- Quick keys for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which provides quick keys.
;; (define-key vertico-map "\M-q" #'vertico-quick-insert)
;; (define-key vertico-map "\C-q" #'vertico-quick-exit)

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defface vertico-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#7042a2" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#d5baff" :foreground "black")
    (t :background "magenta" :foreground "white"))
  "Face used for the first quick key."
  :group 'vertico-faces)

(defface vertico-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#004065" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#8ae4f2" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used for the second quick key."
  :group 'vertico-faces)

(defcustom vertico-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'vertico)

(defcustom vertico-quick2 "jkl"
  "Two level quick keys."
  :type 'string
  :group 'vertico)

(defvar-local vertico-quick--list nil)
(defvar-local vertico-quick--first nil)

(defun vertico-quick--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `vertico--format-candidate' for arguments."
  (let* ((fst (length vertico-quick1))
         (snd (length vertico-quick2))
         (len (+ fst snd))
         (idx (- index start))
         (keys (if (>= idx fst)
                   (let ((first (elt vertico-quick2 (mod (/ (- idx fst) len) snd)))
                         (second (elt (concat vertico-quick1 vertico-quick2) (mod (- idx fst) len))))
                     (cond
                      ((eq first vertico-quick--first)
                       (push (cons second index) vertico-quick--list)
                       (concat " " (propertize (char-to-string second) 'face 'vertico-quick1)))
                      (vertico-quick--first "  ")
                      (t
                       (push (cons first (list first)) vertico-quick--list)
                       (concat (propertize (char-to-string first) 'face 'vertico-quick1)
                               (propertize (char-to-string second) 'face 'vertico-quick2)))))
                 (let ((first (elt vertico-quick1 (mod idx fst))))
                   (if vertico-quick--first
                       "  "
                     (push (cons first index) vertico-quick--list)
                     (concat (propertize (char-to-string first) 'face 'vertico-quick1) " "))))))
    (if (bound-and-true-p vertico-flat-mode)
        (setq keys (replace-regexp-in-string " " "" keys)
              cand (string-trim cand)
              cand (substring cand (min (length cand) (length keys))))
      (setq keys (concat keys (make-string (max 1 (- (length prefix) 2)) ?\s))))
    (funcall orig cand keys suffix index start)))

(defun vertico-quick--read (&optional first)
  "Read quick key given FIRST pressed key."
  (cl-letf (((symbol-function #'vertico--format-candidate)
             (apply-partially #'vertico-quick--format-candidate
                              (symbol-function #'vertico--format-candidate)))
            (vertico-quick--first first)
            (vertico-quick--list))
    (vertico--exhibit)
    (alist-get (read-key) vertico-quick--list)))

;;;###autoload
(defun vertico-quick-jump ()
  "Jump to candidate using quick keys."
  (interactive)
  (if (= vertico--total 0)
      (and (minibuffer-message "No match") nil)
    (let ((idx (vertico-quick--read)))
      (when (consp idx) (setq idx (vertico-quick--read (car idx))))
      (when idx (setq vertico--index idx)))))

;;;###autoload
(defun vertico-quick-exit ()
  "Exit with candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (vertico-exit)))

;;;###autoload
(defun vertico-quick-insert ()
  "Insert candidate using quick keys."
  (interactive)
  (when (vertico-quick-jump)
    (vertico-insert)))

;; Emacs 28: Do not show Vertico commands in M-X
(dolist (sym '(vertico-quick-jump vertico-quick-exit vertico-quick-insert))
  (put sym 'completion-predicate #'vertico--command-p))

(provide 'vertico-quick)
;;; vertico-quick.el ends here
