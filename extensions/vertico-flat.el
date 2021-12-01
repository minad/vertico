;;; vertico-flat.el --- Flat, horizontal display for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension providing a horizontal display.
;;
;; The mode can be bound to a key to toggle to the horizontal display.
;; (define-key vertico-map "\M-F" #'vertico-flat-mode)

;;; Code:

(require 'vertico)

(defcustom vertico-flat-max-lines 1
  "Maximal number of lines to use."
  :type 'integer
  :group 'vertico)

(defcustom vertico-flat-format
  '(:left      #("{" 0 1 (face minibuffer-prompt))
    :separator #(" | " 0 3 (face minibuffer-prompt))
    :right     #("}" 0 1 (face minibuffer-prompt))
    :ellipsis  #("…" 0 1 (face minibuffer-prompt))
    :no-match  "[No match]")
  "Formatting strings."
  :type 'plist
  :group 'vertico)

(defun vertico-flat--display (candidates)
  "Display CANDIDATES horizontally."
  (setq-local truncate-lines nil)
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put
   vertico--candidates-ov 'after-string
   (concat #(" " 0 1 (cursor t))
           (if candidates
               (concat (plist-get vertico-flat-format :left)
                       (string-join candidates (plist-get vertico-flat-format :separator))
                       (plist-get vertico-flat-format :right))
             (plist-get vertico-flat-format :no-match)))))

(defun vertico-flat--arrange-candidates ()
  "Arrange candidates."
  (let* ((index (max 0 vertico--index)) (count vertico-count)
         (candidates (nthcdr vertico--index vertico--candidates))
         (width (- (* vertico-flat-max-lines (- (window-width) 4))
                   (length (plist-get vertico-flat-format :left))
                   (length (plist-get vertico-flat-format :separator))
                   (length (plist-get vertico-flat-format :right))
                   (length (plist-get vertico-flat-format :ellipsis))
                   (car (posn-col-row (posn-at-point (1- (point-max)))))))
         (result) (wrapped))
    (while (and candidates (not (eq wrapped (car candidates)))
                (> width 0) (> count 0))
      (let ((cand (car candidates)))
        (setq cand (car (funcall vertico--highlight-function (list cand))))
        (when (string-match-p "\n" cand)
          (setq cand (vertico--truncate-multiline cand width)))
        (setq cand (string-trim
                    (replace-regexp-in-string
                     "[ \t]+"
                     (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                     (vertico--format-candidate cand "" "" index vertico--index))))
        (setq index (1+ index)
              count (1- count)
              width (- width (string-width cand) (length (plist-get vertico-flat-format :separator))))
        (when (or (not result) (> width 0))
          (push cand result))
        (pop candidates)
        (when (and vertico-cycle (not candidates))
          (setq candidates vertico--candidates index 0
                wrapped (nth vertico--index vertico--candidates)))))
    (when (if wrapped
              (> vertico--total (- vertico-count count))
            (and (/= vertico--total 0) (/= index vertico--total)))
      (push (plist-get vertico-flat-format :ellipsis) result))
    (nreverse result)))

;;;###autoload
(define-minor-mode vertico-flat-mode
  "Flat, horizontal display for Vertico."
  :global t :group 'vertico
  (cond
   (vertico-flat-mode
    ;; Allow toggling between flat and grid modes
    (when (and (bound-and-true-p vertico-grid-mode) (fboundp #'vertico-grid-mode))
      (vertico-grid-mode -1))
    ;; Shrink current minibuffer window
    (when-let (win (active-minibuffer-window))
      (window-resize win (- (window-pixel-height)) nil nil 'pixelwise))
    (advice-add #'vertico--arrange-candidates :override #'vertico-flat--arrange-candidates)
    (advice-add #'vertico--display-candidates :override #'vertico-flat--display))
   (t
    (advice-remove #'vertico--arrange-candidates #'vertico-flat--arrange-candidates)
    (advice-remove #'vertico--display-candidates #'vertico-flat--display))))

(provide 'vertico-flat)
;;; vertico-flat.el ends here
