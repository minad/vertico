;;; vertico-quick.el --- Quick keys for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4") (vertico "1.5"))
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

;; This package is a Vertico extension, which prefixes candidates with
;; quick keys.  Typing these quick keys allows you to select the
;; candidate in front of them.  This is designed to be a faster
;; alternative to selecting a candidate with `vertico-next' and
;; `vertico-previous'.
;; (keymap-set vertico-map "M-q" #'vertico-quick-insert)
;; (keymap-set vertico-map "C-q" #'vertico-quick-exit)

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defface vertico-quick1
  '((((class color) (min-colors 88) (background dark))
     :background "#0050af" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#7feaff" :foreground "black" :inherit bold)
    (t :background "blue" :foreground "white" :inherit bold))
  "Face used for the first quick key."
  :group 'vertico-faces)

(defface vertico-quick2
  '((((class color) (min-colors 88) (background dark))
     :background "#7f1f7f" :foreground "white" :inherit bold)
    (((class color) (min-colors 88) (background light))
     :background "#ffaaff" :foreground "black" :inherit bold)
    (t :background "magenta" :foreground "white" :inherit bold))
  "Face used for the second quick key."
  :group 'vertico-faces)

(defcustom vertico-quick1 "asdfgh"
  "Single level quick keys."
  :type 'string
  :group 'vertico)

(defcustom vertico-quick2 "jkluionm"
  "Two level quick keys."
  :type 'string
  :group 'vertico)

(defun vertico-quick--keys (two index start)
  "Format quick keys prefix.
INDEX is the current candidate index.
START is the index of the first displayed candidate.
TWO is non-nil if two keys should be displayed."
  (let ((fst (length vertico-quick1))
        (snd (length vertico-quick2))
        (idx (- index start)))
    (if (>= idx fst)
        (let ((first (elt vertico-quick2 (mod (/ (- idx fst) fst) snd)))
              (second (elt vertico-quick1 (mod (- idx fst) fst))))
          (cond
           ((eq first two)
            (list
             (concat " " (propertize (char-to-string second) 'face 'vertico-quick1))
             (cons second index)))
           (two
            (list "  "))
           (t
            (list
             (concat (propertize (char-to-string first) 'face 'vertico-quick1)
                     (propertize (char-to-string second) 'face 'vertico-quick2))
             (cons first (list first))))))
      (let ((first (elt vertico-quick1 (mod idx fst))))
        (if two
            (list "  ")
          (list
           (concat (propertize (char-to-string first) 'face 'vertico-quick1) " ")
           (cons first index)))))))

(defun vertico-quick--read (&optional first)
  "Read quick key given FIRST pressed key."
  (cl-letf* ((list nil)
             (orig (symbol-function #'vertico--format-candidate))
             ((symbol-function #'vertico--format-candidate)
              (lambda (cand prefix suffix index start)
                (pcase-let ((`(,keys . ,events) (vertico-quick--keys first index start)))
                  (setq list (nconc events list))
                  (if (bound-and-true-p vertico-flat-mode)
                      (setq keys (string-replace " " "" keys)
                            cand (string-trim cand)
                            cand (substring cand (min (length cand) (length keys))))
                    (setq keys (concat keys (make-string (max 1 (- (length prefix) 2)) ?\s))))
                  (funcall orig cand keys suffix index start)))))
    (vertico--exhibit)
    (alist-get (read-key) list)))

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
