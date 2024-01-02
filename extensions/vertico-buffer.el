;;; vertico-buffer.el --- Display Vertico like a regular buffer -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 1.6
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4") (vertico "1.6"))
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

;; This package is a Vertico extension, which displays Vertico like a
;; regular buffer in a large window instead of the miniwindow.  The
;; buffer display can be enabled by the `vertico-buffer-mode'.

;; The mode can be enabled globally or via `vertico-multiform-mode'
;; per command or completion category.  Alternatively the buffer
;; display can be toggled temporarily with M-B if
;; `vertico-multiform-mode' is enabled.

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

(defvar-local vertico-buffer--restore nil)

(defun vertico-buffer--redisplay (win)
  "Redisplay window WIN."
  (when-let ((mbwin (active-minibuffer-window)))
    (when (eq (window-buffer mbwin) (current-buffer))
      (unless (eq win mbwin)
        (setq-local truncate-lines (< (window-point win)
                                      (* 0.8 (window-width win))))
        (set-window-point win (point))
        (set-window-hscroll win 0))
      (when vertico-buffer-hide-prompt
        (window-resize mbwin (- (window-pixel-height mbwin)) nil nil 'pixelwise)
        (set-window-vscroll mbwin 3))
      (when transient-mark-mode
        (with-silent-modifications
          (vertico--remove-face (point-min) (point-max) 'region)
          (when (use-region-p)
            (add-face-text-property
             (max (minibuffer-prompt-end) (region-beginning))
             (region-end) 'region))))
      (let ((old cursor-in-non-selected-windows)
            (new (and (eq (selected-window) mbwin)
                      (if (memq cursor-type '(nil t)) 'box cursor-type))))
        (unless (eq new old)
          (setq-local cursor-in-non-selected-windows new)
          (force-mode-line-update t))))))

(defun vertico-buffer--setup ()
  "Setup buffer display."
  (let* ((action vertico-buffer-display-action)
         (old-wins (mapcar (lambda (w) (cons w (window-buffer w))) (window-list)))
         win old-buf tmp-buf
         (_ (unwind-protect
                (progn
                  (with-current-buffer
                      (setq tmp-buf (generate-new-buffer "*vertico-buffer*"))
                    ;; Set a fake major mode such that
                    ;; `display-buffer-reuse-mode-window' does not take over!
                    (setq major-mode 'vertico-buffer-mode))
                  ;; Temporarily select the original window such that
                  ;; `display-buffer-same-window' works.
                  (setq win (with-minibuffer-selected-window
                              (display-buffer tmp-buf action))
                        old-buf (alist-get win old-wins))
                  (set-window-buffer win (current-buffer)))
              (kill-buffer tmp-buf)))
         (old-no-other (window-parameter win 'no-other-window))
         (old-no-delete (window-parameter win 'no-delete-other-windows))
         (old-state (buffer-local-set-state
                     cursor-in-non-selected-windows cursor-in-non-selected-windows
                     show-trailing-whitespace nil
                     truncate-lines t
                     face-remapping-alist (copy-tree `((mode-line-inactive mode-line)
                                                       ,@face-remapping-alist))
                     mode-line-format
                     (list (format  #(" %s%s " 1 3 (face mode-line-buffer-id))
                                    (replace-regexp-in-string ":? *\\'" ""
                                                              (minibuffer-prompt))
                                    (let ((depth (recursion-depth)))
                                      (if (< depth 2) "" (format " [%s]" depth)))))
                     vertico-count (- (/ (window-pixel-height win)
                                         (default-line-height)) 2))))
    (set-window-parameter win 'no-other-window t)
    (set-window-parameter win 'no-delete-other-windows t)
    (set-window-dedicated-p win t)
    (overlay-put vertico--candidates-ov 'window win)
    (when (and vertico-buffer-hide-prompt vertico--count-ov)
      (overlay-put vertico--count-ov 'window win))
    (setq-local vertico-buffer--restore (make-symbol "vertico-buffer--restore"))
    (fset vertico-buffer--restore
          (lambda ()
            (with-selected-window (active-minibuffer-window)
              (when vertico-buffer--restore
                (when transient-mark-mode
                  (with-silent-modifications
                    (vertico--remove-face (point-min) (point-max) 'region)))
                (remove-hook 'pre-redisplay-functions #'vertico-buffer--redisplay 'local)
                (remove-hook 'minibuffer-exit-hook vertico-buffer--restore)
                (fset vertico-buffer--restore nil)
                (kill-local-variable 'vertico-buffer--restore)
                (buffer-local-restore-state old-state)
                (overlay-put vertico--candidates-ov 'window nil)
                (when vertico--count-ov (overlay-put vertico--count-ov 'window nil))
                (cond
                 ((and (window-live-p win) (buffer-live-p old-buf))
                  (set-window-parameter win 'no-other-window old-no-other)
                  (set-window-parameter win 'no-delete-other-windows old-no-delete)
                  (set-window-dedicated-p win nil)
                  (set-window-buffer win old-buf))
                 ((window-live-p win)
                  (delete-window win)))
                (when vertico-buffer-hide-prompt
                  (set-window-vscroll nil 0))))))
    ;; We cannot use a buffer-local minibuffer-exit-hook here.  The hook will
    ;; not be called when abnormally exiting the minibuffer from another buffer
    ;; via `keyboard-escape-quit'.
    (add-hook 'minibuffer-exit-hook vertico-buffer--restore)
    (add-hook 'pre-redisplay-functions #'vertico-buffer--redisplay nil 'local)))

;;;###autoload
(define-minor-mode vertico-buffer-mode
  "Display Vertico like a regular buffer in a large window."
  :global t :group 'vertico
  ;; Shrink current minibuffer window
  (when-let ((win (active-minibuffer-window)))
    (unless (frame-root-window-p win)
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise))
    (with-selected-window win
      (cond
       ((and vertico-buffer-mode vertico--input (not vertico-buffer--restore))
        (vertico-buffer--setup))
       ((and (not vertico-buffer-mode) vertico-buffer--restore)
        (funcall vertico-buffer--restore))))))

(cl-defmethod vertico--resize-window (_height &context (vertico-buffer-mode (eql t))))

(cl-defmethod vertico--setup :after (&context (vertico-buffer-mode (eql t)))
  (vertico-buffer--setup))

(provide 'vertico-buffer)
;;; vertico-buffer.el ends here
