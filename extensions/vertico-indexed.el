;;; vertico-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which prefixes candidates with
;; indices and allows selection using prefix arguments.

;;; Code:

(require 'vertico)

(defface vertico-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'vertico
  :group 'faces)

(defvar-local vertico-indexed--start 0)

(defun vertico-indexed--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `vertico--format-candidate' for arguments."
  (setq-local vertico-indexed--start start)
  (funcall orig cand
           (concat (propertize (format "%-2s " (- index start))
                               'face 'vertico-indexed)
                   prefix)
           suffix index start))

(defun vertico-indexed--goto ()
  "Goto candidate given by `current-prefix-arg'."
  (when current-prefix-arg
    (vertico--goto (+ vertico-indexed--start (prefix-numeric-value current-prefix-arg)))))

;;;###autoload
(define-minor-mode vertico-indexed-mode
  "Prefix candidates with indices."
  :global t
  (cond
   (vertico-indexed-mode
    (advice-add #'vertico--format-candidate :around #'vertico-indexed--format-candidate)
    (advice-add #'vertico-insert :before #'vertico-indexed--goto)
    (advice-add #'vertico-exit :before #'vertico-indexed--goto))
   (t
    (advice-remove #'vertico--format-candidate #'vertico-indexed--format-candidate)
    (advice-remove #'vertico-insert #'vertico-indexed--goto)
    (advice-remove #'vertico-exit #'vertico-indexed--goto))))

(provide 'vertico-indexed)
;;; vertico-indexed.el ends here
