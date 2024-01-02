;;; vertico-indexed.el --- Select indexed candidates -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 1.6
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4") (vertico "1.6"))
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

;; This package is a Vertico extension, which prefixes candidates with indices
;; if enabled via `vertico-indexed-mode'.  It allows you to select candidates
;; with prefix arguments.  This is designed to be a faster alternative to
;; selecting a candidate with `vertico-next' and `vertico-previous'.

;;; Code:

(require 'vertico)

(defface vertico-indexed
  '((t :height 0.75 :inherit font-lock-comment-face))
  "Face used for the candidate index prefix."
  :group 'vertico-faces)

(defcustom vertico-indexed-start 0
  "Start of the indexing."
  :group 'vertico
  :type 'natnum)

(defvar vertico-indexed--commands
  '(vertico-insert vertico-exit vertico-directory-enter))
(defvar-local vertico-indexed--min 0)
(defvar-local vertico-indexed--max 0)

;;;###autoload
(define-minor-mode vertico-indexed-mode
  "Prefix candidates with indices."
  :global t :group 'vertico)

(cl-defmethod vertico--prepare :before (&context (vertico-indexed-mode (eql t)))
  (when (and prefix-arg (memq this-command vertico-indexed--commands))
    (let ((index (+ vertico-indexed--min
                    (- (prefix-numeric-value prefix-arg)
                       vertico-indexed-start))))
      (if (and (>= index vertico-indexed--min)
               (<= index vertico-indexed--max)
               (/= vertico--total 0))
          (setq vertico--index index prefix-arg nil)
        (minibuffer-message "Out of range")
        (setq this-command #'ignore)))))

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context (vertico-indexed-mode (eql t)))
  (setq vertico-indexed--min start vertico-indexed--max index)
  (cl-call-next-method
   cand
   (concat (propertize (format
                        (if (> (+ vertico-indexed-start vertico-count) 10)
                            "%2d " "%1d ")
                        (+ (- index start) vertico-indexed-start))
                       'face 'vertico-indexed)
           prefix)
   suffix index start))

(provide 'vertico-indexed)
;;; vertico-indexed.el ends here
