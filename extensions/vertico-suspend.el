;;; vertico-suspend.el --- Suspend the current Vertico session -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4") (vertico "1.5"))
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

;; This package is a Vertico extension, which provides the
;; `vertico-suspend' command to suspend the current Vertico completion
;; session.  If `vertico-suspend' is called from within the currently
;; active Vertico completion minibuffer, the completion session is
;; suspended.  Otherwise the last session is restored.  It is possible
;; to suspend multiple nested Vertico sessions.  Note that
;; `vertico-suspend' requires you to enable recursive minibuffers, see
;; the variable `enable-recursive-minibuffers'.
;;
;; (keymap-global-set "M-S" #'vertico-suspend)
;;
;; See also the related extension `vertico-repeat', which uses a
;; different technique, storing a completion session history.
;;
;; The extension has the following known issues:
;;
;; * `vertico-suspend' restores the window configuration when resuming
;;   and when `vertico-buffer' is used.  This can be seen as a
;;   disturbance, however minibuffer exiting also changes the window
;;   configuration by default.
;;
;; * `echo-keystrokes' does not work in recursive minibuffers.  This
;;   issue cannot be fixed without modifying the C source of Emacs,
;;   since Emacs disables echo in recursive minibuffers.

;;; Code:

(require 'vertico)

(defvar-local vertico-suspend--wc nil)
(defvar-local vertico-suspend--ov nil)

;;;###autoload
(defun vertico-suspend ()
  "Suspend the current completion session.
If the command is invoked from within the Vertico minibuffer, the
current session is suspended.  If the command is invoked from
outside the minibuffer, the active minibuffer is either selected
or the latest completion session is restored."
  (interactive)
  (unless enable-recursive-minibuffers
    (user-error "Recursive minibuffers must be enabled"))
  (if-let ((win (active-minibuffer-window))
           (buf (window-buffer win))
           ((buffer-local-value 'vertico--input buf)))
      (cond
       ((minibufferp)
        (setq vertico-suspend--ov (make-overlay (point-min) (point-max) nil t t))
        (overlay-put vertico-suspend--ov 'invisible t)
        (overlay-put vertico-suspend--ov 'priority 1000)
        (overlay-put vertico--candidates-ov 'before-string nil)
        (overlay-put vertico--candidates-ov 'after-string nil)
        (set-window-parameter win 'no-other-window t)
        ;; vertico-buffer handling
        (when (memq 'vertico-buffer--redisplay pre-redisplay-functions)
          (remove-hook 'pre-redisplay-functions 'vertico-buffer--redisplay 'local)
          (setq-local cursor-in-non-selected-windows nil
                      vertico-suspend--wc (current-window-configuration))
          (dolist (w (get-buffer-window-list buf))
            (unless (eq w win)
              (delete-window w)))
          (set-window-vscroll nil 0))
        (unless (frame-root-window-p win)
          (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise))
        (other-window 1))
       (t
        (select-window win)
        (set-window-parameter win 'no-other-window nil)
        (when vertico-suspend--ov
          (delete-overlay vertico-suspend--ov)
          (setq vertico-suspend--ov nil))
        ;; vertico-buffer handling
        (when vertico-suspend--wc
          (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
          (set-window-configuration vertico-suspend--wc nil t)
          (setq vertico-suspend--wc nil))))
    (user-error "No Vertico session to suspend or resume")))

(defun vertico-suspend--message (&rest app)
  "Apply APP in non-suspended minibuffers, otherwise bail out."
  (when-let ((win (active-minibuffer-window))
             ((not (buffer-local-value 'vertico-suspend--ov (window-buffer win)))))
    (apply app)))

(advice-add #'set-minibuffer-message :around #'vertico-suspend--message)

(provide 'vertico-suspend)
;;; vertico-suspend.el ends here
