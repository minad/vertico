;;; vertico-bottom.el --- Display the prompt at the bottom -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.22"))
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

;; This package is a Vertico extension, which displays the prompt at the bottom.
;;
;; (define-key vertico-map "\M-B" #'vertico-multiform-bottom)

;;; Code:

(require 'vertico)

(defun vertico-bottom--display-candidates (lines)
  "Display LINES above prompt.."
  (move-overlay vertico--candidates-ov (point-min) (point-min))
  (unless (eq vertico-resize t)
    (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put vertico--candidates-ov 'before-string string)
    (overlay-put vertico--candidates-ov 'after-string nil))
  (vertico--resize-window (length lines)))

;;;###autoload
(define-minor-mode vertico-bottom-mode
  "Display prompt at the bottom."
  :global t :group 'vertico
  ;; Reset overlays
  (dolist (buf (buffer-list))
    (when-let (ov (buffer-local-value 'vertico--candidates-ov buf))
      (overlay-put ov 'before-string nil)))
  (if vertico-bottom-mode
      (advice-add #'vertico--display-candidates :override #'vertico-bottom--display-candidates)
    (advice-remove #'vertico--display-candidates #'vertico-bottom--display-candidates)))

(provide 'vertico-bottom)
;;; vertico-bottom.el ends here
