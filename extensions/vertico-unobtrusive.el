;;; vertico-unobtrusive.el --- Unobtrusive display for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 2.7
;; Package-Requires: ((emacs "29.1") (compat "30") (vertico "2.7"))
;; URL: https://github.com/minad/vertico

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

;; This package is a Vertico extension providing a unobtrusive
;; display.  The unobtrusive display only shows the topmost candidate
;; and nothing else, it is a simple derivative of `vertico-flat-mode'.
;;
;; The mode `vertico-unobtrusive-mode' can be enabled globally or
;; via `vertico-multiform-mode' per command or completion category.
;; Alternatively the unobtrusive display can be toggled temporarily
;; with M-U if `vertico-multiform-mode' is enabled.

;;; Code:

(require 'vertico-flat)

(defvar vertico-unobtrusive--restore nil)

;;;###autoload
(define-minor-mode vertico-unobtrusive-mode
  "Unobtrusive display for Vertico."
  :global t :group 'vertico
  (cond
   ((and vertico-unobtrusive-mode (not vertico-unobtrusive--restore))
    (push '(vertico-current . default) (default-value 'face-remapping-alist))
    (setq vertico-unobtrusive--restore (cons vertico-count vertico-count-format)
          vertico-count 1
          vertico-count-format nil
          vertico-flat-format `(:separator nil :ellipsis nil ,@vertico-flat-format)))
   ((and (not vertico-unobtrusive-mode) vertico-unobtrusive--restore)
    (cl-callf2 delete '(vertico-current . default)
               (default-value 'face-remapping-alist))
    (setq vertico-count (car vertico-unobtrusive--restore)
          vertico-count-format (cdr vertico-unobtrusive--restore)
          vertico-flat-format (nthcdr 4 vertico-flat-format)
          vertico-unobtrusive--restore nil)))
  (vertico-flat-mode (if vertico-unobtrusive-mode 1 -1)))

(cl-defmethod vertico--setup :before (&context (vertico-unobtrusive-mode (eql t)))
  (redisplay))

(provide 'vertico-unobtrusive)
;;; vertico-unobtrusive.el ends here
