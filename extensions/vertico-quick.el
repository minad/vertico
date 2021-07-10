;;; vertico-quick.el --- Quick keys for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which provides quick keys.
;; (define-key vertico-map "\M-q" #'vertico-quick-insert)
;; (define-key vertico-map "\C-q" #'vertico-quick-exit)

;;; Code:

(require 'vertico)

(defface vertico-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#7042a2" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#d5baff" :foreground "black")
    (t :background "magenta" :foreground "white"))
  "Face used for the first quick key."
  :group 'vertico
  :group 'faces)

(defface vertico-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#004065" :weight bold :foreground "white")
    (((class color) (min-colors 88) (background light))
     :weight bold :background "#8ae4f2" :foreground "black")
    (t :background "blue" :foreground "white"))
  "Face used for the second quick key."
  :group 'vertico
  :group 'faces)

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
                     (push (cons first t) vertico-quick--list)
                     (push (cons (+ first (lsh second 16)) index) vertico-quick--list)
                     (cond
                      ((eq first vertico-quick--first)
                       (concat " " (propertize (char-to-string second) 'face 'vertico-quick1)))
                      (vertico-quick--first "  ")
                      (t
                       (concat (propertize (char-to-string first) 'face 'vertico-quick1)
                               (propertize (char-to-string second) 'face 'vertico-quick2)))))
                 (let ((first (elt vertico-quick1 (mod idx fst))))
                   (push (cons first index) vertico-quick--list)
                   (if vertico-quick--first
                       "  "
                     (concat (propertize (char-to-string first) 'face 'vertico-quick1) " "))))))
    (if (bound-and-true-p vertico-flat-mode)
        (setq keys (replace-regexp-in-string " " "" keys)
              cand (string-trim cand)
              cand (substring cand (min (length cand) (length keys))))
      (setq keys (concat keys (make-string (max 1 (- (length prefix) 2)) ?\s))))
    (funcall orig cand keys suffix index start)))

;;;###autoload
(defun vertico-quick-jump ()
  "Jump to candidate using quick keys."
  (interactive)
  (cl-letf ((vertico-quick--list nil) (key nil)
            ((symbol-function #'vertico--format-candidate)
             (apply-partially #'vertico-quick--format-candidate
                              (symbol-function #'vertico--format-candidate))))
    (vertico--exhibit)
    (setq key (read-key))
    (when (and (alist-get key vertico-quick--list)
               (seq-position vertico-quick2 key))
      (let ((vertico-quick--first key)
            (vertico-quick--list))
        (vertico--exhibit))
      (setq key (+ key (lsh (read-key) 16))))
    (when-let (idx (alist-get key vertico-quick--list))
      (setq vertico--index idx))))

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

(provide 'vertico-quick)
;;; vertico-quick.el ends here
