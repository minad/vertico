;;; vertico-buffer.el --- Display Vertico in a buffer instead of the minibuffer -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which allows to display Vertico
;; in a buffer instead of the minibuffer. The buffer display can be enabled
;; by the `vertico-buffer-mode'.

;;; Code:

(require 'vertico)

(defvar-local vertico-buffer--overlay nil)

(defcustom vertico-buffer-hide-prompt nil
  "Hide the prompt in the minibuffer."
  :type 'boolean
  :group 'vertico)

(defvar vertico-buffer-action
  `(display-buffer-in-side-window
    (window-parameters
     (mode-line-format . mode-line-buffer-identification)
     (header-line-format . (:eval (vertico-buffer--prompt))))
    (window-height . ,(+ 3 vertico-count))
    (side . bottom)
    (slot . -1))
  "Display action for the Vertico buffer.")

(defun vertico-buffer--prompt ()
  "Return minibuffer prompt string, to be used as `header-line-format'."
  (with-selected-window (active-minibuffer-window)
    (let ((str (buffer-string))
          (pt (point)))
      (if (= pt (point-max))
          (setq str (concat str #(" " 0 1 (face (:inverse-video t)))))
        (setq pt (max (minibuffer-prompt-end) pt))
        (put-text-property (1- pt) pt 'face '(:inverse-video t) str))
      (when vertico-count-format
        (setq str (concat (propertize (vertico--format-count) 'face 'minibuffer-prompt) str)))
      str)))

(defun vertico-buffer--display (lines)
  "Display LINES in buffer."
  (let ((buffer (get-buffer-create
                 (if (= 1 (recursion-depth))
                     " *Vertico*"
                   (format " *Vertico [%s]*" (1- (recursion-depth)))))))
    (with-current-buffer buffer
      (setq-local display-line-numbers nil
                  truncate-lines nil
                  show-trailing-whitespace nil
                  inhibit-modification-hooks t)
      (erase-buffer)
      (insert (string-join lines))
      (goto-char (point-min)))
    (display-buffer buffer vertico-buffer-action)))

(defun vertico-buffer--setup ()
  "Setup minibuffer overlay, which pushes the minibuffer content down."
  (when vertico-buffer-hide-prompt
    (setq vertico-buffer--overlay (make-overlay (point-max) (point-max) nil t t))
    (overlay-put vertico-buffer--overlay 'window (selected-window))
    (overlay-put vertico-buffer--overlay 'priority 1000)
    (overlay-put vertico-buffer--overlay 'after-string "\n\n")
    (set-window-vscroll nil 100)
    (window-resize nil (- (window-height)))))

;;;###autoload
(define-minor-mode vertico-buffer-mode
  "Display Vertico in a buffer instead of the minibuffer."
  :global t
  (cond
   (vertico-buffer-mode
    (advice-add #'vertico--display-candidates :override #'vertico-buffer--display)
    (advice-add #'vertico--setup :after #'vertico-buffer--setup))
   (t
    (advice-remove #'vertico--display-candidates #'vertico-buffer--display)
    (advice-remove #'vertico--setup #'vertico-buffer--setup))))

(provide 'vertico-buffer)
;;; vertico-buffer.el ends here
