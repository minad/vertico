;;; vertico-flat.el --- Flat, horizontal display for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension providing a horizontal display.

;;; Code:

(require 'vertico)

(defvar vertico-flat--group-format nil)

(defun vertico-flat--display (candidates)
  "Display CANDIDATES horizontally."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (when (>= vertico--index 0)
    (setq candidates
          (seq-drop-while (lambda (cand)
                            (let ((face (get-text-property 0 'face cand)))
                              (not (if (listp face)
                                       (memq 'vertico-current face)
                                     (eq 'vertico-current face)))))
                          candidates)))
  (setq candidates
        (seq-map-indexed (lambda (cand idx)
                           (string-trim
                            (replace-regexp-in-string
                             "[ \t]+" (if (= idx 0) #(" " 0 1 (face vertico-current)) " ")
                             (substring cand 0 -1))))
                         candidates))
  (overlay-put
   vertico--candidates-ov 'after-string
   (concat #(" " 0 1 (cursor t))
           (if candidates
               (concat "{" (string-join candidates " | ") "}")
             "[No match]"))))

(defun vertico-flat--affixate (_ candidates)
  "Return CANDIDATES without adding annotations."
  candidates)

;;;###autoload
(define-minor-mode vertico-flat-mode
  "Flat, horizontal display for Vertico."
  :global t
  (cond
   (vertico-flat-mode
    (setq vertico-flat--group-format vertico-group-format
          vertico-group-format nil)
    (advice-add #'vertico--affixate :override #'vertico-flat--affixate)
    (advice-add #'vertico--display-candidates :override #'vertico-flat--display))
   (t
    (setq vertico-group-format vertico-flat--group-format)
    (advice-remove #'vertico--affixate #'vertico-flat--affixate)
    (advice-remove #'vertico--display-candidates #'vertico-flat--display))))

(provide 'vertico-flat)
;;; vertico-flat.el ends here
