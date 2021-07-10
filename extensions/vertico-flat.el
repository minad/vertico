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

(defcustom vertico-flat-separator
  (list (propertize "{" 'face 'minibuffer-prompt)
        (propertize " | " 'face 'minibuffer-prompt)
        (propertize "}" 'face 'minibuffer-prompt)
        (propertize "…" 'face 'minibuffer-prompt))
  "Separator strings."
  :type '(list string string string string)
  :group 'vertico)

(defun vertico-flat--display (candidates)
  "Display CANDIDATES horizontally."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put
   vertico--candidates-ov 'after-string
   (concat #(" " 0 1 (cursor t))
           (if candidates
               (concat (car vertico-flat-separator)
                       (string-join candidates (cadr vertico-flat-separator))
                       (caddr vertico-flat-separator))
             "[No match]"))))

(defun vertico-flat--format-candidates (_metadata)
  "Format candidates."
  (let* ((index vertico--index)
         (count vertico-count)
         (candidates (nthcdr vertico--index vertico--candidates))
         (width (- (window-width) 4
                   (length (car vertico-flat-separator))
                   (length (caddr vertico-flat-separator))
                   (length (cadddr vertico-flat-separator))
                   (car (posn-col-row (posn-at-point (1- (point-max)))))))
         (result))
    (while (and candidates (> width 0) (> count 0))
      (let ((cand (car candidates)))
        (when (string-match-p "\n" cand)
          (setq cand (vertico--truncate-multiline cand width)))
        (setq cand (string-trim
                    (replace-regexp-in-string
                     "[ \t]+" (if (= index vertico--index) #(" " 0 1 (face vertico-current)) " ")
                     (vertico--format-candidate cand "" "" index vertico--index))))
        (setq index (1+ index)
              count (1- count)
              width (- width (string-width cand) (length (cadr vertico-flat-separator))))
        (when (or (not result) (> width 0))
          (push cand result))
        (pop candidates)))
    (unless (or (= vertico--total 0) (= index vertico--total))
      (push (cadddr vertico-flat-separator) result))
    (nreverse result)))

;;;###autoload
(define-minor-mode vertico-flat-mode
  "Flat, horizontal display for Vertico."
  :global t
  (cond
   (vertico-flat-mode
    (advice-add #'vertico--format-candidates :override #'vertico-flat--format-candidates)
    (advice-add #'vertico--display-candidates :override #'vertico-flat--display))
   (t
    (advice-remove #'vertico--format-candidates #'vertico-flat--format-candidates)
    (advice-remove #'vertico--display-candidates #'vertico-flat--display))))

(provide 'vertico-flat)
;;; vertico-flat.el ends here
