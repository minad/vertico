;;; vertico-resize.el --- Resizeable minibuffer for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: J.D. Smith 
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.13"))
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

;; This package is a Vertico extension enables resizing the vertico
;; mini-buffer temporarily during a given vertico session. 

;;; Code:

(require 'vertico)

(defun vertico-resize--minibuffer ()
  "Advice to enable a resizable vertico mini-buffer.
Note that a full minibuffer can only be grown in size."
  (if-let ((orig (get 'vertico-count 'original)))
      (setq vertico-count orig)
    (put 'vertico-count 'original vertico-count))
  (add-hook 'window-size-change-functions
	    (lambda (win)
	      (let ((height (window-height win)))
		(when (/= (1- height) vertico-count)
		  (setq-local vertico-count (1- height))
		  (vertico--exhibit))))
	    t t))

;;;###autoload
(define-minor-mode vertico-resize-mode
  "Resizeable minibuffer for Vertico."
  :global t
  (cond
   (vertico-resize-mode
    (advice-add #'vertico--setup :before #'vertico-resize--minibuffer))
   (t
    (advice-remove #'vertico--setup #'vertico-resize--minibuffer))))

(provide 'vertico-resize)
;;; vertico-resize.el ends here
