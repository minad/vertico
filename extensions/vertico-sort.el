;;; vertico-sort.el --- Sort functions for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 2.0
;; Package-Requires: ((emacs "28.1") (compat "30") (vertico "2.0"))
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

;; This package provides a set of sort functions for Vertico, which
;; can be used as `vertico-sort-function'.  By default, Vertico uses
;; the `vertico-sort-history-length-alpha' function.

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defvar-local vertico-sort--history nil
  "History hash table and corresponding base string.")

(defun vertico-sort--history ()
  "Recompute history hash table and return it."
  (or (and (equal (car vertico-sort--history) vertico--base) (cdr vertico-sort--history))
      (let* ((base vertico--base)
             (base-len (length base))
             (hist (and (not (eq minibuffer-history-variable t)) ;; Disabled for `t'.
                        (symbol-value minibuffer-history-variable)))
             (hash (make-hash-table :test #'equal :size (length hist)))
             (file-p (and (> base-len 0) ;; Step-wise completion, unlike `project-find-file'
                          (eq minibuffer-history-variable 'file-name-history)))
             (curr-file (when-let ((win (and file-p (minibuffer-selected-window)))
                                   (file (buffer-file-name (window-buffer win))))
                          (abbreviate-file-name file))))
        (cl-loop for elem in hist for index from 0 do
                 (when (and (not (equal curr-file elem)) ;; Deprioritize current file
                            (or (= base-len 0)
                                (and (>= (length elem) base-len)
                                     (eq t (compare-strings base 0 base-len elem 0 base-len)))))
                   (let ((file-sep (and file-p (string-search "/" elem base-len))))
                     ;; Drop base string from history elements & special file handling.
                     (when (or (> base-len 0) file-sep)
                       (setq elem (substring elem base-len (and file-sep (1+ file-sep)))))
                     (unless (gethash elem hash) (puthash elem index hash)))))
        (cdr (setq vertico-sort--history (cons base hash))))))

(defun vertico-sort--length-string< (x y)
  "Sorting predicate which compares X and Y first by length then by `string<'."
  (or (< (length x) (length y)) (and (= (length x) (length y)) (string< x y))))

(defun vertico-sort--decorated (list)
  "Sort decorated LIST and remove decorations."
  (setq list (sort list #'car-less-than-car))
  (cl-loop for item on list do (setcar item (cdar item)))
  list)

(defmacro vertico-sort--define (by bsize bindex bpred pred)
  "Generate optimized sorting function.
The function is configured by BY, BSIZE, BINDEX, BPRED and PRED."
  `(defun ,(intern (mapconcat #'symbol-name `(vertico sort ,@by) "-")) (candidates)
     ,(concat "Sort candidates by " (mapconcat #'symbol-name by ", ") ".")
     (let* ((buckets (make-vector ,bsize nil))
            ,@(and (eq (car by) 'history) '((hhash (vertico-sort--history)) (hcands))))
       (dolist (% candidates)
         ;; Find recent candidate in history or fill bucket
         (,@(if (not (eq (car by) 'history)) `(progn)
              `(if-let ((idx (gethash % hhash))) (push (cons idx %) hcands)))
          (push % (aref buckets (min ,(1- bsize) ,bindex)))))
       (nconc ,@(and (eq (car by) 'history) '((vertico-sort--decorated hcands)))
              (mapcan (lambda (bucket) (sort bucket #',bpred))
                      (nbutlast (append buckets nil)))
              ;; Last bucket needs special treatment
              (sort (aref buckets ,(1- bsize)) #',pred)))))

;;;###autoload (autoload 'vertico-sort-history-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-history-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-alpha "vertico-sort")
(vertico-sort--define (history length alpha) 48 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (history alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)
(vertico-sort--define (length alpha) 48 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)

(provide 'vertico-sort)
;;; vertico-sort.el ends here
