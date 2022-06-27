;;; vertico-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.25"))
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

;; This package is a Vertico extension, which prefixes candidates with indices
;; if enabled via `vertico-indexed-mode'. It allows you to select candidates
;; with prefix arguments. This is designed to be a faster alternative to
;; selecting a candidate with `vertico-next' and `vertico-previous'.

;;; Code:

(require 'vertico)

(defface vertico-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'vertico-faces)

(defvar vertico-indexed--commands
  '(vertico-insert vertico-exit vertico-directory-enter))
(defvar-local vertico-indexed--min 0)
(defvar-local vertico-indexed--max 0)

(defun vertico-indexed--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `vertico--format-candidate' for arguments."
  (setq vertico-indexed--min start vertico-indexed--max index)
  (funcall orig cand
           (concat (propertize (format
                                (format "%%%ds " (if (> vertico-count 10) 2 1))
                                (- index start))
                               'face 'vertico-indexed)
                   prefix)
           suffix index start))

(defun vertico-indexed--handle-prefix (orig &rest args)
  "Handle prefix argument before calling ORIG function with ARGS."
  (if (and current-prefix-arg (called-interactively-p t))
      (let ((vertico--index (+ vertico-indexed--min (prefix-numeric-value current-prefix-arg))))
        (if (or (< vertico--index vertico-indexed--min)
                (> vertico--index vertico-indexed--max)
                (= vertico--total 0))
            (minibuffer-message "Out of range")
          (funcall orig)))
    (apply orig args)))

;;;###autoload
(define-minor-mode vertico-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'vertico
  (cond
   (vertico-indexed-mode
    (advice-add #'vertico--format-candidate :around #'vertico-indexed--format-candidate)
    (dolist (cmd vertico-indexed--commands)
      (advice-add cmd :around #'vertico-indexed--handle-prefix)))
   (t
    (advice-remove #'vertico--format-candidate #'vertico-indexed--format-candidate)
    (dolist (cmd vertico-indexed--commands)
      (advice-remove cmd #'vertico-indexed--handle-prefix)))))

(provide 'vertico-indexed)
;;; vertico-indexed.el ends here
