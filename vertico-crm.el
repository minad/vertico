;;; vertico-crm.el --- Enhanced `completing-read-multiple' support for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

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

;; This package provides `vertico-crm-mode', which sets up an enhanced
;; `completing-read-multiple' UI for Vertico.

;;; Code:

(require 'vertico)
(require 'crm)

(defvar-local vertico-crm--table nil)
(defvar-local vertico-crm--selected nil)
(defvar-local vertico-crm--count-ov nil)

(defcustom vertico-crm-count-format " (%s selected): "
  "Format string used for the selection count."
  :type '(choice (const nil) string)
  :group 'vertico)

(defface vertico-crm-selected
  '((t :inherit secondary-selection))
  "Face used to highlight selected items."
  :group 'vertico)

(defvar vertico-crm-map
  (let ((map (make-composed-keymap nil vertico-map)))
    (define-key map [remap vertico-insert] #'vertico-crm-select)
    (define-key map [backtab] #'vertico-crm-select-erase)
    map)
  "Minibuffer keymap derived from `vertico-map'.")

(defun vertico-crm--update-count ()
  "Update the count overlay."
  (when vertico-crm--count-ov
    (overlay-put vertico-crm--count-ov 'display
                 (and vertico-crm--selected
                      (format vertico-crm-count-format
                              (length vertico-crm--selected))))))

(defun vertico-crm--format (cand)
  "Format selected candidate CAND."
  ;; Restore original candidate in order to preserve formatting
  (setq cand (substring (or (car (all-completions cand vertico-crm--table nil)) cand)))
  (add-face-text-property 0 (length cand) 'vertico-crm-selected 'append cand)
  (put-text-property 0 (length cand) 'vertico-crm--selected t cand)
  cand)

(defun vertico-crm--collection (str pred action)
  "Programmable completion table for `vertico-crm--completing-read-multiple'.
See `completing-read' for the arguments STR, PRED and ACTION."
  (pcase action
    ('metadata
     (let* ((md (and (functionp vertico-crm--table)
                     (cdr (funcall vertico-crm--table str pred action))))
            (group-fun (alist-get 'group-function md))
            (title (substitute-command-keys "Select multiple [\\[vertico-crm-select]]")))
       `(metadata
         (group-function
          . ,(lambda (cand transform)
               (if (get-text-property 0 'vertico-crm--selected cand)
                   (if transform cand "Selected")
                 (or (and group-fun (funcall group-fun cand transform))
                     (if transform cand title)))))
         ,@md)))
    ('t
     (nconc
      (all-completions str vertico-crm--selected nil)
      (cl-delete-if (lambda (x) (member x vertico-crm--selected))
                    (all-completions str vertico-crm--table pred))))
    (_ (complete-with-action action vertico-crm--table str pred))))

(defun vertico-crm--completing-read-multiple (prompt table &optional
                                                     pred require-match initial-input
                                                     hist def inherit-input-method)
  "Enhanced replacement for `completing-read-multiple'.
See `completing-read-multiple' for the arguments."
  ;; TODO maybe it is better to ignore initial-input or to pass it to completing-read?
  ;; It depends on if initial-input is used to preselect candidates or if initial-input
  ;; is used as a filter string. It is hard or impossible to determine this.
  (let ((selected))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'minibuffer-exit-hook (lambda () (setq selected vertico-crm--selected)) nil 'local)
          (when-let (pos (and vertico-crm-count-format
                              (string-match-p "\\(?: (default[^)]+)\\)?: \\'"
                                              (minibuffer-prompt))))
            (setq vertico-crm--count-ov (make-overlay (+ (point-min) pos)
                                                      (minibuffer-prompt-end))))
          (setq vertico-crm--table table
                vertico-crm--selected
                (and initial-input
                     (mapcar #'vertico-crm--format
                             (split-string initial-input crm-separator 'omit-nulls))))
          (vertico-crm--update-count)
          (use-local-map vertico-crm-map))
      (let* ((hist-sym (pcase hist
                         ('nil 'minibuffer-history)
                         ('t nil)
                         (`(,sym . ,_) sym) ;; ignore history position
                         (_ hist)))
             (hist-val (symbol-value hist-sym))
             (result
              (completing-read prompt
                               #'vertico-crm--collection
                               pred
                               require-match
                               nil ;; initial-input
                               hist
                               "" ;; default
                               inherit-input-method)))
        (setq selected (mapcar #'substring-no-properties selected))
        (unless (or (equal result "") selected)
          (setq selected (list result)))
        (set hist-sym (append selected hist-val))
        (when (consp def)
          (setq def (car def)))
        (if (and def (not (equal "" def)) (not selected))
            (split-string def crm-separator 'omit-nulls)
          selected)))))

(defun vertico-crm-select ()
  "Select/deselect current candidate."
  (interactive)
  (let ((cand (vertico--candidate)))
    (when (and (not (equal cand "")) (vertico--match-p cand))
      (when (>= vertico--index 0)
        (when (> vertico--total 1)
          (vertico--goto (if (= (1+ vertico--index) vertico--total)
                             -1
                           (1+ vertico--index))))
        (setq vertico--input t))
      (if (member cand vertico-crm--selected)
          ;; Multi selections are not possible.
          ;; This is probably no problem, since this is rarely desired.
          (setq vertico-crm--selected (delete cand vertico-crm--selected))
        (setq vertico--lock-groups t
              vertico--all-groups '("Selected")
              vertico-crm--selected
              (nconc vertico-crm--selected (list (vertico-crm--format cand)))))
      (vertico-crm--update-count))))

(defun vertico-crm-select-erase ()
  "Select/deselect current candidate and erase input."
  (interactive)
  (vertico-crm-select)
  (delete-minibuffer-contents)
  (setq vertico--lock-candidate nil))

;;;###autoload
(define-minor-mode vertico-crm-mode
  "Enhanced `completing-read-multiple' support for Vertico."
  :global t
  (if vertico-crm-mode
      (add-hook 'vertico-mode-hook #'vertico-crm--setup)
    (remove-hook 'vertico-mode-hook #'vertico-crm--setup))
  (vertico-crm--setup))

(defun vertico-crm--setup ()
  "Setup enhanced `completing-read-multiple'."
  (if (and vertico-crm-mode vertico-mode)
      (progn
        (advice-remove #'completing-read-multiple #'vertico--advice)
        (advice-add #'completing-read-multiple :override #'vertico-crm--completing-read-multiple))
    (advice-remove #'completing-read-multiple #'vertico-crm--completing-read-multiple)
    (when vertico-mode
      (advice-add #'completing-read-multiple :around #'vertico--advice))))

(provide 'vertico-crm)
;;; vertico-crm.el ends here
