;;; vertico-demand.el --- Show Vertico on demand -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (vertico "0.14"))
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

;; This package is a Vertico extension, which shows the UI on demand.

;;; Code:

(require 'vertico)
(eval-when-compile
  (require 'cl-lib))

(defvar vertico-demand-map
  (let ((map (make-composed-keymap nil minibuffer-local-map)))
    (define-key map [remap scroll-down-command] #'vertico-demand-show)
    (define-key map [remap scroll-up-command] #'vertico-demand-show)
    (define-key map [remap next-line] #'vertico-demand-show)
    (define-key map [remap previous-line] #'vertico-demand-show)
    (define-key map [remap next-line-or-history-element] #'vertico-demand-show)
    (define-key map [remap previous-line-or-history-element] #'vertico-demand-show)
    (define-key map [remap exit-minibuffer] #'vertico-demand-complete-and-exit)
    (define-key map "\t" #'vertico-demand-complete)
    (define-key map [C-return] #'vertico-exit-input)
    map)
  "Vertico demand minibuffer keymap derived from `minibuffer-local-map'.")

(defun vertico-demand-complete ()
  "Complete minibuffer input or open Vertico UI."
  (interactive)
  (cl-letf (((symbol-function #'minibuffer-completion-help) #'vertico-demand-show))
    (minibuffer-complete)))

(defun vertico-demand-complete-and-exit ()
  "Complete minibuffer input and exit or open Vertico UI."
  (interactive)
  (cl-letf (((symbol-function #'minibuffer-completion-help) #'vertico-demand-show)
            (minibuffer-completion-confirm nil)
            (minibuffer--require-match t)
            (completion-cycle-threshold nil)) ;; disable cycling; ensure unique match!
    (minibuffer-complete-and-exit)))

(defun vertico-demand-show (&rest _)
  "Show Vertico UI."
  (interactive)
  (vertico--setup))

(defun vertico-demand--setup ()
  "Setup Vertico demand mode in the minibuffer."
  (setq-local completion-show-inline-help t
              completion-auto-help t)
  (use-local-map vertico-demand-map))

(defun vertico-demand--advice (&rest args)
  "Advice for completion function, receiving ARGS."
  (minibuffer-with-setup-hook #'vertico-demand--setup (apply args)))

;;;###autoload
(define-minor-mode vertico-demand-mode
  "Open Vertico on demand."
  :global t :group 'vertico
  (if vertico-demand-mode
      (advice-add #'vertico--advice :override #'vertico-demand--advice)
    (advice-remove #'vertico--advice #'vertico-demand--advice)))

(provide 'vertico-demand)
;;; vertico-demand.el ends here
