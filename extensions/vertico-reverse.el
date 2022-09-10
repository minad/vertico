;;; vertico-reverse.el --- Reverse the Vertico display -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.27"))
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

;; This package is a Vertico extension, which reverses the list of candidates.
;;
;; The mode can be enabled globally or via `vertico-multiform-mode' per
;; command or completion category. Alternatively the reverse display can be
;; toggled temporarily if `vertico-multiform-mode' is enabled:
;;
;; (define-key vertico-map "\M-R" #'vertico-multiform-reverse)

;;; Code:

(require 'vertico)

(defvar vertico-reverse-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap beginning-of-buffer] #'vertico-last)
    (define-key map [remap minibuffer-beginning-of-buffer] #'vertico-last)
    (define-key map [remap end-of-buffer] #'vertico-first)
    (define-key map [remap scroll-down-command] #'vertico-scroll-up)
    (define-key map [remap scroll-up-command] #'vertico-scroll-down)
    (define-key map [remap next-line] #'vertico-previous)
    (define-key map [remap previous-line] #'vertico-next)
    (define-key map [remap next-line-or-history-element] #'vertico-previous)
    (define-key map [remap previous-line-or-history-element] #'vertico-next)
    (define-key map [remap backward-paragraph] #'vertico-next-group)
    (define-key map [remap forward-paragraph] #'vertico-previous-group)
    map)
  "Additional keymap activated in reverse mode.")

(defun vertico-reverse--display-candidates (lines)
  "Display LINES in reverse."
  (move-overlay vertico--candidates-ov (point-min) (point-min))
  (setq lines (nreverse lines))
  (unless (eq vertico-resize t)
    (setq lines (nconc (make-list (max 0 (- vertico-count (length lines))) "\n") lines)))
  (let ((string (apply #'concat lines)))
    (add-face-text-property 0 (length string) 'default 'append string)
    (overlay-put vertico--candidates-ov 'before-string string)
    (overlay-put vertico--candidates-ov 'after-string nil))
  (vertico--resize-window (length lines)))

;;;###autoload
(define-minor-mode vertico-reverse-mode
  "Reverse the Vertico display."
  :global t :group 'vertico
  ;; Reset overlays
  (dolist (buf (buffer-list))
    (when-let (ov (buffer-local-value 'vertico--candidates-ov buf))
      (overlay-put ov 'before-string nil)))
  (cond
   (vertico-reverse-mode
    (add-to-list 'minor-mode-map-alist `(vertico--input . ,vertico-reverse-map))
    (advice-add #'vertico--display-candidates :override #'vertico-reverse--display-candidates))
   (t
    (setq minor-mode-map-alist (delete `(vertico--input . ,vertico-reverse-map) minor-mode-map-alist))
    (advice-remove #'vertico--display-candidates #'vertico-reverse--display-candidates))))

(provide 'vertico-reverse)
;;; vertico-reverse.el ends here
