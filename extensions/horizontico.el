;;; horizontico.el --- HORIZONTal Interactive COmpletion -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Version: 0.1

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

(defvar horizontico--orig-group-format nil)
(defvar horizontico--orig-count nil)

(defun horizontico--display (candidates)
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
                           (replace-regexp-in-string
                            "\\` +" ""
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

(defun horizontico--disable-annotations (_ candidates)
  "Return CANDIDATES without adding annotations."
  candidates)

;;;###autoload
(define-minor-mode horizontico-mode
  "HORIZONTal Interactive COmpletion."
  :global t
  (cond
   (horizontico-mode
    (setq horizontico--orig-group-format vertico-group-format
          horizontico--orig-count vertico-count
          vertico-group-format nil
          vertico-count 20)
    (advice-add #'vertico--affixate :override #'horizontico--disable-annotations)
    (advice-add #'vertico--display-candidates :override #'horizontico--display))
   (t
    (setq vertico-group-format horizontico--orig-group-format
          vertico-count horizontico--orig-count)
    (advice-remove #'vertico--affixate #'horizontico--disable-annotations)
    (advice-remove #'vertico--display-candidates #'horizontico--display))))

(provide 'horizontico)
;;; horizontico.el ends here
