;;; vertico-mouse.el --- Mouse support for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.29"))
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

;; This package is a Vertico extension, which adds mouse support.

;;; Code:

(require 'vertico)

(defface vertico-mouse
  '((t :inherit highlight))
  "Face used for mouse highlighting."
  :group 'vertico-faces)

(defun vertico-mouse--candidate-map (index)
  "Return keymap for candidate with INDEX."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda ()
                                (interactive)
                                (with-selected-window (active-minibuffer-window)
                                  (let ((vertico--index index))
                                    (vertico-exit)))))
    (define-key map [mouse-3] (lambda ()
                                (interactive)
                                (with-selected-window (active-minibuffer-window)
                                  (let ((vertico--index index))
                                    (vertico-insert)))))
    map))

(defun vertico-mouse--format-candidate (orig cand prefix suffix index start)
  "Format candidate, see `vertico--format-candidate' for arguments."
  (setq cand (funcall orig cand prefix suffix index start))
  (when (equal suffix "")
    (setq cand (concat (substring cand 0 -1)
                       (propertize " " 'display '(space :align-to right))
                       "\n"))
    (when (= index vertico--index)
      (add-face-text-property 0 (length cand) 'vertico-current 'append cand)))
  (add-text-properties 0 (1- (length cand))
                       `(mouse-face vertico-mouse keymap ,(vertico-mouse--candidate-map index))
                       cand)
  cand)

(defun vertico-mouse--scroll-up (n)
  "Scroll up by N lines."
  (vertico--goto (max 0 (+ vertico--index n))))

(defun vertico-mouse--scroll-down (n)
  "Scroll down by N lines."
  (vertico-mouse--scroll-up (- n)))

(defun vertico-mouse--setup ()
  "Setup mouse scrolling."
  (setq-local mwheel-scroll-up-function #'vertico-mouse--scroll-up
              mwheel-scroll-down-function #'vertico-mouse--scroll-down))

;;;###autoload
(define-minor-mode vertico-mouse-mode
  "Mouse support for Vertico."
  :global t :group 'vertico
  (cond
   (vertico-mouse-mode
    (advice-add #'vertico--format-candidate :around #'vertico-mouse--format-candidate)
    (advice-add #'vertico--setup :after #'vertico-mouse--setup))
   (t
    (advice-remove #'vertico--format-candidate #'vertico-mouse--format-candidate)
    (advice-remove #'vertico--setup #'vertico-mouse--setup))))

(provide 'vertico-mouse)
;;; vertico-mouse.el ends here
