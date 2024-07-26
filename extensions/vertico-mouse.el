;;; vertico-mouse.el --- Mouse support for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 1.9
;; Package-Requires: ((emacs "27.1") (compat "30") (vertico "1.9"))
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

(defun vertico-mouse--index (event)
  "Return candidate index at EVENT."
  (when-let ((object (posn-object (event-end event)))
             ((consp object)))
    (get-text-property (cdr object) 'vertico-mouse--index (car object))))

(defun vertico-mouse--click (key)
  "Create command handling mouse click, behave like KEY press."
  (lambda (event)
    (interactive "e")
    ;; Mouse clicks can even happen if another window is selected.
    (with-selected-window (active-minibuffer-window)
      (when-let ((vertico--index (vertico-mouse--index event))
                 (cmd (keymap-local-lookup key)))
        (funcall cmd)))))

(defvar-keymap vertico-mouse-map
  :doc "Additional keymap activated in mouse mode."
  "<mouse-1>" (vertico-mouse--click "RET")
  "<mouse-3>" (vertico-mouse--click "TAB"))
(fset 'vertico-mouse-map vertico-mouse-map)

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
  (setq cand (cl-call-next-method cand prefix
                                  (concat suffix #(" " 0 1 (display (space :align-to right))))
                                  index start))
  (add-text-properties 0 (1- (length cand))
                       `(vertico-mouse--index ,index
                         mouse-face vertico-mouse keymap vertico-mouse-map)
                       cand)
  cand)

(cl-defmethod vertico--setup :after (&context (vertico-mouse-mode (eql t)))
  (when (boundp 'mwheel-coalesce-scroll-events)
    (setq-local mwheel-coalesce-scroll-events t))
  (when (boundp 'pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))
  (setq-local mwheel-scroll-up-function #'vertico-mouse--scroll-up
              mwheel-scroll-down-function #'vertico-mouse--scroll-down))

(provide 'vertico-mouse)
;;; vertico-mouse.el ends here
