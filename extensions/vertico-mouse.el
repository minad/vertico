;;; vertico-mouse.el --- Mouse support for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which adds mouse support.

;;; Code:

(require 'vertico)

(defface vertico-mouse
  '((t :inherit highlight))
  "Face used for mouse highlighting."
  :group 'vertico-faces)

(defun vertico-mouse--candidate-click (index key)
  "Return command handling click on candidate with INDEX.
The command will behave like KEY."
  (when-let ((cmd (keymap-lookup vertico-map key)))
    (lambda ()
      (interactive)
      ;; Ensure that the command is always executed in the minibuffer.
      ;; Mouse clicks can also happen if another window is selected.
      (with-selected-window (active-minibuffer-window)
        (let ((vertico--index index))
          (funcall cmd))))))

(defun vertico-mouse--candidate-map (index)
  "Return keymap for candidate with INDEX."
  (define-keymap
    "<mouse-1>" (vertico-mouse--candidate-click index "RET")
    "<mouse-3>" (vertico-mouse--candidate-click index "TAB")))

(defun vertico-mouse--scroll-up (n)
  "Scroll up by N lines."
  (vertico--goto (max 0 (+ vertico--index n))))

(defun vertico-mouse--scroll-down (n)
  "Scroll down by N lines."
  (vertico-mouse--scroll-up (- n)))

;;;###autoload
(define-minor-mode vertico-mouse-mode
  "Mouse support for Vertico."
  :global t :group 'vertico)

(cl-defmethod vertico--format-candidate
  :around (cand prefix suffix index start &context (vertico-mouse-mode (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
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

(cl-defmethod vertico--setup :after (&context (vertico-mouse-mode (eql t)))
  (setq-local mwheel-scroll-up-function #'vertico-mouse--scroll-up
              mwheel-scroll-down-function #'vertico-mouse--scroll-down))

(provide 'vertico-mouse)
;;; vertico-mouse.el ends here
