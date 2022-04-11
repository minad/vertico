;;; vertico-sort.el --- Sort functions for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

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

;; This package provides a set of sort functions for Vertico, which can be used
;; as `vertico-sort-function' or `vertico-sort-override-function'. Vertico uses
;; the `vertico-sort-history-length-alpha' function by default if available.

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defvar-local vertico-sort--history-hash nil)

(defun vertico-sort--history-hash ()
  "Recompute history hash table and return it."
  (or (and (equal (car vertico-sort--history-hash) vertico--base)
           (cdr vertico-sort--history-hash))
      (let* ((base vertico--base)
             (base-size (length base))
             ;; History disabled if `minibuffer-history-variable' eq `t'.
             (hist (and (not (eq minibuffer-history-variable t))
                        (symbol-value minibuffer-history-variable)))
             (hash (make-hash-table :test #'equal :size (length hist))))
        (if (= base-size 0)
            ;; Put history elements into the hash
            (cl-loop for elem in hist for index from 0 do
                     (unless (gethash elem hash)
                       (puthash elem index hash)))
          ;; Drop base from history elements, before putting them into the hash
          (cl-loop for elem in hist for index from 0 do
                   (when (and (>= (length elem) base-size)
                              (eq t (compare-strings base 0 base-size elem 0 base-size)))
                     (setq elem (substring elem base-size))
                     (unless (gethash elem hash)
                       (puthash elem index hash)))))
        (cdr (setq vertico-sort--history-hash (cons base hash))))))

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
  `(defun ,(intern (mapconcat #'symbol-name `(vertico sort ,@by) "-")) (cands)
     ,(concat "Sort CANDS by " (mapconcat #'symbol-name by ", ") ".")
     (let* ((buckets (make-vector ,bsize nil))
            ,@(and (eq (car by) 'history)
                   '((hhash (vertico-sort--history-hash)) (hcands))))
       (dolist (% cands)
         ,(if (eq (car by) 'history)
              ;; Find recent candidates or fill buckets
              `(if-let (idx (gethash % hhash))
                   (push (cons idx %) hcands)
                 (let ((idx (min ,(1- bsize) ,bindex)))
                   (aset buckets idx (cons % (aref buckets idx)))))
            ;; Fill buckets
            `(let ((idx (min ,(1- bsize) ,bindex)))
               (aset buckets idx (cons % (aref buckets idx))))))
       (nconc ,@(and (eq (car by) 'history) '((vertico-sort--decorated hcands)))
              (mapcan (lambda (bucket) (sort bucket #',bpred))
                      (nbutlast (append buckets nil)))
              ;; Last bucket needs special treatment
              (sort (aref buckets ,(1- bsize)) #',pred)))))

;;;###autoload (autoload 'vertico-sort-history-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-history-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-alpha "vertico-sort")
(vertico-sort--define (history length alpha) 32 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (history alpha) 32 (if (eq % "") 0 (/ (aref % 0) 4)) string< string<)
(vertico-sort--define (length alpha) 32 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (alpha) 32 (if (eq % "") 0 (/ (aref % 0) 4)) string< string<)

(provide 'vertico-sort)
;;; vertico-sort.el ends here
