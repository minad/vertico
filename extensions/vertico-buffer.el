;;; vertico-buffer.el --- Display Vertico in a buffer instead of the minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.25"))
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

;; This package is a Vertico extension, which displays Vertico in a
;; buffer instead of the minibuffer. The buffer display can be enabled
;; by the `vertico-buffer-mode'.

;;; Code:

(require 'vertico)

(defcustom vertico-buffer-hide-prompt t
  "Hide prompt in the minibuffer."
  :group 'vertico
  :type 'boolean)

(defcustom vertico-buffer-display-action
  '(display-buffer-reuse-window)
  "Display action for the Vertico buffer."
  :group 'vertico
  :type `(choice
          (const :tag "Reuse some window"
                 (display-buffer-reuse-window))
          (const :tag "Below target buffer"
                 (display-buffer-below-selected
                  (window-height . ,(+ 3 vertico-count))))
          (const :tag "Bottom of frame"
                 (display-buffer-at-bottom
                  (window-height . ,(+ 3 vertico-count))))
          (const :tag "Side window on the right"
                 (display-buffer-in-side-window
                  (side . right)
                  (window-width . 0.3)))
          (const :tag "Side window on the left"
                 (display-buffer-in-side-window
                  (side . left)
                  (window-width . 0.3)))
          (const :tag "Side window at the top"
                 (display-buffer-in-side-window
                  (window-height . ,(+ 3 vertico-count))
                  (side . top)))
          (const :tag "Side window at the bottom"
                 (display-buffer-in-side-window
                  (window-height . ,(+ 3 vertico-count))
                  (side . bottom)))
          (sexp :tag "Other")))

(defun vertico-buffer--redisplay (win)
  "Redisplay window WIN."
  (when-let (mbwin (active-minibuffer-window))
    (when (eq (window-buffer mbwin) (current-buffer))
      (let ((old cursor-in-non-selected-windows)
            (new (and (eq (selected-window) mbwin) 'box)))
        (unless (eq new old)
          (setq-local cursor-in-non-selected-windows new)
          (force-mode-line-update t)))
      (unless (eq win mbwin)
        (setq-local truncate-lines (< (window-point win)
                                      (* 0.8 (window-width win))))
        (set-window-point win (point)))
      (when vertico-buffer-hide-prompt
        (window-resize mbwin (- (window-pixel-height mbwin)) nil nil 'pixelwise)
        (set-window-vscroll mbwin 100)))))

(defun vertico-buffer--setup ()
  "Setup buffer display."
  (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
  (let* ((action vertico-buffer-display-action) tmp win
         (_ (unwind-protect
                (progn
                  (setf tmp (generate-new-buffer "*vertico-buffer*")
                        ;; Set a fake major mode such that `display-buffer-reuse-mode-window'
                        ;; does not take over!
                        (buffer-local-value 'major-mode tmp) 'vertico-buffer-mode
                        ;; Temporarily select the original window such
                        ;; that `display-buffer-same-window' works.
                        win (with-minibuffer-selected-window (display-buffer tmp action)))
                  (set-window-buffer win (current-buffer)))
              (kill-buffer tmp)))
         (sym (make-symbol "vertico-buffer--destroy"))
         (depth (recursion-depth))
         (now (window-parameter win 'no-other-window))
         (ndow (window-parameter win 'no-delete-other-windows)))
    (fset sym (lambda ()
                (when (= depth (recursion-depth))
                  (with-selected-window (active-minibuffer-window)
                    (when (window-live-p win)
                      (set-window-parameter win 'no-other-window now)
                      (set-window-parameter win 'no-delete-other-windows ndow))
                    (when vertico-buffer-hide-prompt
                      (set-window-vscroll nil 0))
                    (remove-hook 'minibuffer-exit-hook sym)))))
    ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
    ;; The hook will not be called when abnormally exiting the minibuffer
    ;; from another buffer via `keyboard-escape-quit'.
    (add-hook 'minibuffer-exit-hook sym)
    (set-window-parameter win 'no-other-window t)
    (set-window-parameter win 'no-delete-other-windows t)
    (overlay-put vertico--candidates-ov 'window win)
    (when (and vertico-buffer-hide-prompt vertico--count-ov)
      (overlay-put vertico--count-ov 'window win))
    (setq-local show-trailing-whitespace nil
                truncate-lines t
                face-remapping-alist
                (copy-tree `((mode-line-inactive mode-line)
                             ,@face-remapping-alist))
                mode-line-format
                (list (format " %s "
                              (propertize
                               (format (if (< depth 2) "*%s*" "*%s [%s]*")
                                       (replace-regexp-in-string
                                        ":? *\\'" ""
                                        (minibuffer-prompt))
                                       depth)
                               'face 'mode-line-buffer-id)))
                cursor-in-non-selected-windows 'box
                vertico-count (- (/ (window-pixel-height win)
                                    (default-line-height)) 2))))

;;;###autoload
(define-minor-mode vertico-buffer-mode
  "Display Vertico in a buffer instead of the minibuffer."
  :global t :group 'vertico
  (cond
   (vertico-buffer-mode
    (advice-add #'vertico--setup :after #'vertico-buffer--setup)
    (advice-add #'vertico--resize-window :override #'ignore))
   (t
    (advice-remove #'vertico--setup #'vertico-buffer--setup)
    (advice-remove #'vertico--resize-window #'ignore))))

(provide 'vertico-buffer)
;;; vertico-buffer.el ends here
