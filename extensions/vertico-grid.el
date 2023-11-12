;;; vertico-grid.el --- Grid display for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "1.4"))
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

;; This package is a Vertico extension providing a grid display.
;;
;; The mode can be enabled globally or via `vertico-multiform-mode'
;; per command or completion category.  Alternatively the grid display
;; can be toggled temporarily with M-G if `vertico-multiform-mode' is
;; enabled.

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defcustom vertico-grid-min-columns 2
  "Minimal number of grid columns."
  :type 'natnum
  :group 'vertico)

(defcustom vertico-grid-max-columns 8
  "Maximal number of grid columns."
  :type 'natnum
  :group 'vertico)

(defcustom vertico-grid-annotate 0
  "Reserved characters for the annotations."
  :type 'natnum
  :group 'vertico)

(defcustom vertico-grid-separator
  #("   |   " 3 4 (display (space :width (1)) face (:inherit shadow :inverse-video t)))
  "Separator between columns."
  :type 'string
  :group 'vertico)

(defcustom vertico-grid-lookahead 100
  "Number of candidates to lookahead for column number computation.
When scrolling beyond this limit, candidates may be truncated."
  :type 'natnum
  :group 'vertico)

(defvar-keymap vertico-grid-map
  :doc "Additional keymap activated in grid mode."
  "<remap> <left-char>" #'vertico-grid-left
  "<remap> <right-char>" #'vertico-grid-right
  "<remap> <scroll-down-command>" #'vertico-grid-scroll-down
  "<remap> <scroll-up-command>" #'vertico-grid-scroll-up)

(defvar-local vertico-grid--columns vertico-grid-min-columns
  "Current number of grid columns.")

(defun vertico-grid-left (&optional n)
  "Move N columns to the left in the grid."
  (interactive "p")
  (vertico-grid-right (- (or n 1))))

(defun vertico-grid-right (&optional n)
  "Move N columns to the right in the grid."
  (interactive "p")
  (let* ((page (* vertico-count vertico-grid--columns))
         (x1 (/ (% vertico--index page) vertico-count))
         (cols (min (1- vertico-grid--columns)
                    (+ x1 (/ (- vertico--total vertico--index 1) vertico-count))))
         (x2 (if vertico-cycle
                 (mod (+ x1 (or n 1)) (1+ cols))
               (min cols (max 0 (+ x1 (or n 1)))))))
    (vertico--goto (+ vertico--index (* vertico-count (- x2 x1))))))

(defun vertico-grid-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (vertico--goto (max 0 (- vertico--index (* (or n 1) vertico-grid--columns vertico-count)))))

(defun vertico-grid-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (vertico-grid-scroll-down (- (or n 1))))

;;;###autoload
(define-minor-mode vertico-grid-mode
  "Grid display for Vertico."
  :global t :group 'vertico
  ;; Shrink current minibuffer window
  (when-let ((win (active-minibuffer-window)))
    (unless (frame-root-window-p win)
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise)))
  (setq minor-mode-map-alist (rassq-delete-all vertico-grid-map minor-mode-map-alist))
  (when vertico-grid-mode
    (push `(vertico--input . ,vertico-grid-map) minor-mode-map-alist)))

(cl-defmethod vertico--arrange-candidates (&context (vertico-grid-mode (eql t)))
  (when (<= vertico--index 0)
    (let ((w 1))
      (cl-loop repeat vertico-grid-lookahead for cand in vertico--candidates do
               (setq w (max w (+ vertico-grid-annotate (length cand)))))
      (setq vertico-grid--columns
            (max vertico-grid-min-columns
                 (min vertico-grid-max-columns
                      (floor (vertico--window-width) (+ w (length vertico-grid-separator))))))))
  (let* ((sep (length vertico-grid-separator))
         (count (* vertico-count vertico-grid--columns))
         (start (* count (floor (max 0 vertico--index) count)))
         (width (- (/ (vertico--window-width) vertico-grid--columns) sep))
         (cands (funcall (if (> vertico-grid-annotate 0) #'vertico--affixate #'identity)
                         (cl-loop repeat count for c in (nthcdr start vertico--candidates)
                                  collect (funcall vertico--hilit (substring c)))))
         (cands (cl-loop
                 for cand in cands for index from 0 collect
                 (let (prefix suffix)
                   (when (consp cand)
                     (setq prefix (cadr cand) suffix (caddr cand) cand (car cand)))
                   (when (string-search "\n" cand)
                     (setq cand (vertico--truncate-multiline cand width)))
                   (truncate-string-to-width
                    (string-trim
                     (replace-regexp-in-string
                      "[ \t]+"
                      (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                      (vertico--format-candidate cand prefix suffix (+ index start) start)))
                    width))))
         (width (make-vector vertico-grid--columns 0)))
    (dotimes (col vertico-grid--columns)
      (dotimes (row vertico-count)
        (aset width col (max
                         (aref width col)
                         (string-width (or (nth (+ row (* col vertico-count)) cands) ""))))))
    (dotimes (col (1- vertico-grid--columns))
      (cl-incf (aref width (1+ col)) (+ (aref width col) sep)))
    (cl-loop for row from 0 to (1- (min vertico-count vertico--total)) collect
             (let ((line (list "\n")))
               (cl-loop for col from (1- vertico-grid--columns) downto 0 do
                        (when-let ((cand (nth (+ row (* col vertico-count)) cands)))
                          (push cand line)
                          (when (> col 0)
                            (push vertico-grid-separator line)
                            (push (propertize " " 'display
                                              `(space :align-to (+ left ,(aref width (1- col))))) line))))
             (string-join line)))))

;; Emacs 28: Do not show Vertico commands in M-X
(dolist (sym '(vertico-grid-left vertico-grid-right
               vertico-grid-scroll-up vertico-grid-scroll-down))
  (put sym 'completion-predicate #'vertico--command-p))

(provide 'vertico-grid)
;;; vertico-grid.el ends here
