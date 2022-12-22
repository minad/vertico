;;; vertico-unobtrusive.el --- Unobtrusive display for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "1.0"))
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

;; This package is a Vertico extension providing a unobtrusive display.
;; The unobtrusive display only shows the topmost candidate and nothing
;; else, it is a simple derivative of `vertico-flat-mode'.
;;
;; The mode can be enabled globally or via `vertico-multiform-mode' per
;; command or completion category. Alternatively the unobtrusive display
;; can be toggled temporarily if `vertico-multiform-mode' is enabled:
;;
;; (define-key vertico-map "\M-U" #'vertico-multiform-unobtrusive)

;;; Code:

(require 'vertico-flat)

(defvar vertico-unobtrusive--orig-count nil)
(defvar vertico-unobtrusive--orig-count-format nil)

;;;###autoload
(define-minor-mode vertico-unobtrusive-mode
  "Unobtrusive display for Vertico."
  :global t :group 'vertico
  (cond
   (vertico-unobtrusive-mode
    (unless vertico-unobtrusive--orig-count
      (push '(vertico-current . default) (default-value 'face-remapping-alist))
      (setq vertico-unobtrusive--orig-count vertico-count
            vertico-unobtrusive--orig-count-format vertico-count-format
            vertico-count 1
            vertico-count-format nil
            vertico-flat-format `(:separator nil :ellipsis nil ,@vertico-flat-format)))
    (advice-add #'vertico--setup :before #'redisplay)
    (vertico-flat-mode 1))
   (t
    (when vertico-unobtrusive--orig-count
      (setq-default face-remapping-alist
                    (remove '(vertico-current . default)
                            (default-value 'face-remapping-alist)))
      (setq vertico-count vertico-unobtrusive--orig-count
            vertico-count-format vertico-unobtrusive--orig-count-format
            vertico-flat-format (nthcdr 4 vertico-flat-format)
            vertico-unobtrusive--orig-count nil))
    (advice-remove #'vertico--setup #'redisplay)
    (vertico-flat-mode -1)))
  (setq vertico-flat-mode nil))

(provide 'vertico-unobtrusive)
;;; vertico-unobtrusive.el ends here
