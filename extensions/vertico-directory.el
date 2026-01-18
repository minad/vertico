;;; vertico-directory.el --- Ido-like directory navigation for Vertico -*- lexical-binding: t -*-

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

;; This package is a Vertico extension, which provides Ido-like
;; directory navigation commands.  The commands can be bound in the
;; `vertico-map'.
;;
;; (keymap-set vertico-map "RET" #'vertico-directory-enter)
;; (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
;; (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
;;
;; Alternatively use `vertico-directory-map' together with
;; `vertico-multiform-mode'.
;;
;; (setq vertico-multiform-categories
;;       '((file (:keymap . vertico-directory-map))))
;; (vertico-multiform-mode)
;;
;; Furthermore a cleanup function for shadowed file paths is provided.
;;
;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;;; Code:

(require 'vertico)
(eval-when-compile (require 'subr-x))

;;;###autoload
(defun vertico-directory-enter (&optional arg)
  "Enter directory or exit completion with current candidate.
Exit with current input if prefix ARG is given."
  (interactive "P")
  (if-let* (((not arg))
            ((>= vertico--index 0))
            ((eq 'file (vertico--metadata-get 'category)))
            ;; Check vertico--base for stepwise file path completion
            ((not (equal vertico--base "")))
            (cand (vertico--candidate))
            ((or (string-suffix-p "/" cand)
                 (and (vertico--remote-p cand)
                      (string-suffix-p ":" cand))))
            ;; Handle /./ and /../ manually instead of via `expand-file-name'
            ;; and `abbreviate-file-name', such that we don't accidentally
            ;; perform unwanted substitutions in the existing completion.
            ((progn
               (setq cand (string-replace "/./" "/" cand))
               (unless (string-suffix-p "/../../" cand)
                 (setq cand (replace-regexp-in-string "/[^/|:]+/\\.\\./\\'" "/" cand)))
               (not (equal (minibuffer-contents-no-properties) cand)))))
      (progn
        (delete-minibuffer-contents)
        (insert cand))
    (vertico-exit arg)))

;;;###autoload
(defun vertico-directory-up (&optional n)
  "Delete N names before point."
  (interactive "p")
  (when (and (> (point) (minibuffer-prompt-end))
             (eq 'file (vertico--metadata-get 'category)))
    (let ((path (buffer-substring-no-properties (minibuffer-prompt-end) (point)))
          found)
      (when (string-match-p "\\`~[^/]*/\\'" path)
        (delete-minibuffer-contents)
        (insert (expand-file-name path)))
      (dotimes (_ (or n 1) found)
        (save-excursion
          (let ((end (point)))
            (goto-char (1- end))
            (when (search-backward "/" (minibuffer-prompt-end) t)
              (delete-region (1+ (point)) end)
              (setq found t))))))))

;;;###autoload
(defun vertico-directory-delete-char (n)
  "Delete N directories or chars before point."
  (interactive "p")
  (unless (and (not (and (use-region-p) delete-active-region (= n 1)))
               (eq (char-before) ?/) (vertico-directory-up n))
    (with-no-warnings (delete-backward-char n))))

;;;###autoload
(defun vertico-directory-delete-word (n)
  "Delete N directories or words before point."
  (interactive "p")
  (unless (and (eq (char-before) ?/) (vertico-directory-up n))
    (delete-region (prog1 (point) (backward-word n)) (point))))

;;;###autoload
(defun vertico-directory-tidy ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (eq this-command #'self-insert-command)
    (dolist (ov '(tramp-rfn-eshadow-overlay rfn-eshadow-overlay))
      (when (and (boundp ov)
                 (setq ov (symbol-value ov))
                 (overlay-buffer ov)
                 (= (point) (point-max))
                 (> (point) (overlay-end ov)))
        (delete-region (overlay-start ov) (overlay-end ov))))))

(defvar-keymap vertico-directory-map
  :doc "File name editing map."
  "RET" #'vertico-directory-enter
  "DEL" #'vertico-directory-delete-char
  "M-DEL" #'vertico-directory-delete-word)

;;;###autoload (autoload 'vertico-directory-map "vertico-directory" nil t 'keymap)
(defalias 'vertico-directory-map vertico-directory-map)

(provide 'vertico-directory)
;;; vertico-directory.el ends here
