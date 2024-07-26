;;; vertico-suspend.el --- Suspend the current Vertico session -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2023
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

;; This package is a Vertico extension providing the `vertico-suspend'
;; command to suspend the current Vertico completion session.  If
;; `vertico-suspend' is called from within the currently active
;; Vertico minibuffer, the completion session is suspended.  Otherwise
;; the last session is restored.  It is possible to suspend multiple
;; nested Vertico sessions.  Note that `vertico-suspend' requires that
;; recursive minibuffers are enabled by setting the customizable
;; variable `enable-recursive-minibuffers' to t.
;;
;; (keymap-global-set "M-S" #'vertico-suspend)
;;
;; See also the related extension `vertico-repeat', which uses a
;; different technique, storing a completion session history.
;;
;; There exists a small issue with `vertico-suspend'.  The setting
;; `echo-keystrokes' does not work.  Unfortunately this cannot be
;; fixed without modifying the C source of Emacs, since Emacs forcibly
;; disables echo if a minibuffer is active.

;;; Code:

(require 'vertico)

(defvar vertico-buffer--restore)
(declare-function vertico-buffer-mode "ext:vertico-buffer")
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
  (advice-add #'set-minibuffer-message :around #'vertico-suspend--message)
  (if-let ((win (active-minibuffer-window))
           (buf (window-buffer win))
           ((buffer-local-value 'vertico--input buf)))
      (cond
       ((minibufferp)
        (add-hook 'pre-redisplay-functions #'vertico-suspend--unselect nil 'local)
        (setq vertico-suspend--ov (make-overlay (point-min) (point-max) nil t t))
        (overlay-put vertico-suspend--ov 'invisible t)
        (overlay-put vertico-suspend--ov 'priority 1000)
        (overlay-put vertico--candidates-ov 'before-string nil)
        (overlay-put vertico--candidates-ov 'after-string nil)
        (set-window-parameter win 'no-other-window t)
        (when (bound-and-true-p vertico-buffer-mode)
          (vertico-buffer-mode -1)
          (setq vertico-buffer--restore #'ignore))
        (vertico-suspend--unselect))
       (t
        (select-window win)
        (set-window-parameter win 'no-other-window nil)
        (remove-hook 'pre-redisplay-functions #'vertico-suspend--unselect 'local)
        (when vertico-suspend--ov
          (delete-overlay vertico-suspend--ov)
          (setq vertico-suspend--ov nil))
        (when (eq #'ignore (bound-and-true-p vertico-buffer--restore))
          (setq vertico-buffer--restore nil)
          (vertico-buffer-mode 1))))
    (user-error "No Vertico session to suspend or resume")))

(defun vertico-suspend--unselect (&rest _)
  "Ensure that suspended minibuffer is not selected."
  (let ((win (get-buffer-window)))
    (when (eq win (selected-window))
      (unless (frame-root-window-p win)
        (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise))
      (select-window (minibuffer-selected-window) t))))

(defun vertico-suspend--message (&rest app)
  "Apply APP in non-suspended minibuffers, otherwise bail out."
  (when-let ((win (active-minibuffer-window))
             ((not (buffer-local-value 'vertico-suspend--ov (window-buffer win)))))
    (apply app)))

(provide 'vertico-suspend)
;;; vertico-suspend.el ends here
