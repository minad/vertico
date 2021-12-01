;;; vertico-grid.el --- Grid display for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension providing a grid display.
;;
;; The mode can be bound to a key to toggle to the grid display.
;; (define-key vertico-map "\M-G" #'vertico-grid-mode)

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib))

(defcustom vertico-grid-max-columns 8
  "Maximal number of grid columns."
  :type 'integer
  :group 'vertico)

(defcustom vertico-grid-separator
  #("  |  " 2 3 (display (space :width (1)) face (:inverse-video t)))
  "Separator between columns."
  :type 'string
  :group 'vertico)

(defcustom vertico-grid-rows 6
  "Number of grid rows."
  :type 'integer
  :group 'vertico)

(defcustom vertico-grid-lookahead 200
  "Number of candidates to lookahead for column number computation.
When scrolling beyond this limit, candidates may be truncated."
  :type 'integer
  :group 'vertico)

(defvar-local vertico-grid--columns 1
  "Current number of grid columns.")

(defun vertico-grid--arrange-candidates ()
  "Arrange candidates."
  (when (<= vertico--index 0)
    (let ((cand vertico--candidates) (w 1) (n 0))
      (while (and cand (< n vertico-grid-lookahead))
        (setq w (max w (length (car cand))) n (1+ n))
        (pop cand))
      (setq vertico-grid--columns
            (max 1 (min vertico-grid-max-columns
                        (floor (window-width) (+ w (length vertico-grid-separator))))))))
  (let* ((sep (length vertico-grid-separator))
         (count (* vertico-grid-rows vertico-grid--columns))
         (start (* count (floor (max 0 vertico--index) count)))
         (width (- (/ (window-width) vertico-grid--columns) sep))
         (cands
          (seq-map-indexed (lambda (cand index)
                             (cl-incf index start)
                             (when (string-match-p "\n" cand)
                               (setq cand (vertico--truncate-multiline cand width)))
                             (truncate-string-to-width
                              (string-trim
                               (replace-regexp-in-string
                                "[ \t]+"
                                (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                                (vertico--format-candidate cand "" "" index start)))
                              width))
                           (funcall vertico--highlight-function
                                    (seq-subseq vertico--candidates start
                                                (min (+ start count)
                                                     vertico--total)))))
         (width (make-vector vertico-grid--columns 0)))
    (dotimes (col vertico-grid--columns)
      (dotimes (row vertico-grid-rows)
        (aset width col (max
                         (aref width col)
                         (string-width (or (nth (+ row (* col vertico-grid-rows)) cands) ""))))))
    (dotimes (col (1- vertico-grid--columns))
      (cl-incf (aref width (1+ col)) (+ (aref width col) sep)))
    (cl-loop for row from 0 to (1- vertico-grid-rows) collect
             (let ((line (list "\n")))
               (cl-loop for col from (1- vertico-grid--columns) downto 0 do
                        (when-let (cand (nth (+ row (* col vertico-grid-rows)) cands))
                          (push cand line)
                          (when (> col 0)
                            (push vertico-grid-separator line)
                            (push (propertize " " 'display
                                              `(space :align-to (+ left ,(aref width (1- col))))) line))))
             (string-join line)))))

(defun vertico-grid-left (&optional n)
  "Move N columns to the left in the grid."
  (interactive "p")
  (vertico-grid-right (- (or n 1))))

(defun vertico-grid-right (&optional n)
  "Move N columns to the right in the grid."
  (interactive "p")
  (let* ((page (* vertico-grid-rows vertico-grid--columns))
         (p (/ vertico--index page))
         (q (mod vertico--index page))
         (x (/ q vertico-grid-rows))
         (y (mod q vertico-grid-rows))
         (z (+ (* p page) (* vertico-grid--columns y) x (or n 1))))
    (setq x (mod z vertico-grid--columns)
          y (/ z vertico-grid--columns))
    (vertico--goto (+ (* x vertico-grid-rows) (mod y vertico-grid-rows)
                      (* (/ y vertico-grid-rows) page)))))

;;;###autoload
(define-minor-mode vertico-grid-mode
  "Grid display for Vertico."
  :global t :group 'vertico
  (cond
   (vertico-grid-mode
    ;; Allow toggling between flat and grid modes
    (when (and (bound-and-true-p vertico-flat-mode) (fboundp #'vertico-flat-mode))
      (vertico-flat-mode -1))
    ;; Shrink current minibuffer window
    (when-let (win (active-minibuffer-window))
      (window-resize win (- (window-pixel-height)) nil nil 'pixelwise))
    (define-key vertico-map [remap left-char] #'vertico-grid-left)
    (define-key vertico-map [remap right-char] #'vertico-grid-right)
    (advice-add #'vertico--arrange-candidates :override #'vertico-grid--arrange-candidates))
   (t
    (assq-delete-all 'left-char (assq 'remap vertico-map))
    (assq-delete-all 'right-char (assq 'remap vertico-map))
    (advice-remove #'vertico--arrange-candidates #'vertico-grid--arrange-candidates))))

;; Emacs 28: Do not show Vertico commands in M-X
(dolist (sym '(vertico-grid-left vertico-grid-right))
  (put sym 'completion-predicate #'vertico--command-p))

(provide 'vertico-grid)
;;; vertico-grid.el ends here
