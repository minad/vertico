;;; vertico-buffer.el --- Display Vertico in a buffer instead of the minibuffer -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which displays Vertico in a
;; buffer instead of the minibuffer.  The buffer display can be enabled
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
          (const :tag "Least recently used window"
                 (,'display-buffer-use-least-recent-window))
          (const :tag "Left of current window"
                 (display-buffer-in-direction
                  (direction . left)
                  (window-width . 0.3)))
          (const :tag "Right of current window"
                 (display-buffer-in-direction
                  (direction . right)
                  (window-height . 0.3)))
          (const :tag "Above current window"
                 (display-buffer-in-direction
                  (direction . above)
                  (window-height . ,(+ 3 vertico-count))))
          (const :tag "Below current window"
                 (display-buffer-in-direction
                  (direction . below)
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
      (unless (eq win mbwin)
        (setq-local truncate-lines (< (window-point win)
                                      (* 0.8 (window-width win))))
        (set-window-point win (point))
        (set-window-hscroll win 0))
      (when vertico-buffer-hide-prompt
        (window-resize mbwin (- (window-pixel-height mbwin)) nil nil 'pixelwise)
        (set-window-vscroll mbwin 100))
      (let ((old cursor-in-non-selected-windows)
            (new (and (eq (selected-window) mbwin)
                      (if (memq cursor-type '(nil t)) 'box cursor-type))))
        (unless (eq new old)
          (setq-local cursor-in-non-selected-windows new)
          (force-mode-line-update t))))))

;;;###autoload
(define-minor-mode vertico-buffer-mode
  "Display Vertico in a buffer instead of the minibuffer."
  :global t :group 'vertico)

(cl-defmethod vertico--resize-window (_height &context (vertico-buffer-mode (eql t))))

(cl-defmethod vertico--setup :after (&context (vertico-buffer-mode (eql t)))
  (add-hook 'pre-redisplay-functions #'vertico-buffer--redisplay nil 'local)
  (let* ((action vertico-buffer-display-action) tmp win old-buf
         (_ (unwind-protect
                (progn
                  (with-current-buffer (setq tmp (generate-new-buffer "*vertico-buffer*"))
                    ;; Set a fake major mode such that `display-buffer-reuse-mode-window'
                    ;; does not take over!
                    (setq major-mode 'vertico-buffer-mode))
                  ;; Temporarily select the original window such
                  ;; that `display-buffer-same-window' works.
                  (setq old-buf (mapcar (lambda (win) (cons win (window-buffer win))) (window-list))
                        win (with-minibuffer-selected-window (display-buffer tmp action))
                        old-buf (alist-get win old-buf))
                  (set-window-buffer win (current-buffer)))
              (kill-buffer tmp)))
         (sym (make-symbol "vertico-buffer--destroy"))
         (depth (recursion-depth))
         (now (window-parameter win 'no-other-window))
         (ndow (window-parameter win 'no-delete-other-windows)))
    (fset sym (lambda ()
                (when (= depth (recursion-depth))
                  (with-selected-window (active-minibuffer-window)
                    (cond
                     ((and (window-live-p win) (buffer-live-p old-buf))
                      (set-window-parameter win 'no-other-window now)
                      (set-window-parameter win 'no-delete-other-windows ndow)
                      (set-window-dedicated-p win nil)
                      (set-window-buffer win old-buf))
                     ((window-live-p win)
                      (delete-window win)))
                    (when vertico-buffer-hide-prompt
                      (set-window-vscroll nil 0))
                    (remove-hook 'minibuffer-exit-hook sym)))))
    ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
    ;; The hook will not be called when abnormally exiting the minibuffer
    ;; from another buffer via `keyboard-escape-quit'.
    (add-hook 'minibuffer-exit-hook sym)
    (set-window-parameter win 'no-other-window t)
    (set-window-parameter win 'no-delete-other-windows t)
    (set-window-dedicated-p win t)
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
                vertico-count (- (/ (window-pixel-height win)
                                    (default-line-height)) 2))))

(provide 'vertico-buffer)
;;; vertico-buffer.el ends here
