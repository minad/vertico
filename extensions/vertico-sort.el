;;; vertico-sort.el --- Sort functions for Vertico -*- lexical-binding: t -*-

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

;; This package provides a set of sort functions for Vertico, which can be used
;; as `vertico-sort-function'.  By default, Vertico uses the
;; `vertico-sort-history-length-alpha' function, which sorts first by history,
;; then by length and finally by character.  If `history-delete-duplicates' is
;; nil, duplicate elements are ranked higher with exponential decay.  In order
;; to save the history across Emacs sessions, enable `savehist-mode'.

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defvar-local vertico-sort--history nil
  "History hash table and corresponding base string.")

(defcustom vertico-sort-history-duplicate 10
  "History position shift for duplicate history elements.
The more often a duplicate element occurs in the history, the earlier it
appears in the completion list.  The shift decays exponentially with
`vertico-sort-history-decay'.  Note that duplicates occur only if
`history-delete-duplicates' is disabled."
  :type 'number
  :group 'vertico)

(defcustom vertico-sort-history-decay 10
  "Exponential decay for the position shift of duplicate elements.
The shift will decay away after `vertico-sort-history-duplicate' times
`vertico-sort-history-decay' history elements."
  :type 'number
  :group 'vertico)

(defun vertico-sort--history ()
  "Recompute history hash table and return it."
  (or (and (equal (car vertico-sort--history) vertico--base) (cdr vertico-sort--history))
      (let* ((base vertico--base)
             (base-len (length base))
             (hist (and (not (eq minibuffer-history-variable t)) ;; Disabled for `t'.
                        (symbol-value minibuffer-history-variable)))
             (ht (make-hash-table :test #'equal :size (length hist)))
             (file-p (and (> base-len 0) ;; Step-wise completion, unlike `project-find-file'
                          (eq minibuffer-history-variable 'file-name-history)))
             (curr-file (when-let* ((win (and file-p (minibuffer-selected-window)))
                                    (file (buffer-file-name (window-buffer win))))
                          (abbreviate-file-name file)))
             (decay (/ -1.0 (* vertico-sort-history-duplicate vertico-sort-history-decay))))
        (cl-loop for elem in hist for idx from 0 do
                 (when (and (not (equal curr-file elem)) ;; Deprioritize current file
                            (or (= base-len 0)
                                (and (>= (length elem) base-len)
                                     (eq t (compare-strings base 0 base-len elem 0 base-len)))))
                   (let ((file-sep (and file-p (string-search "/" elem base-len))))
                     ;; Drop base string from history elements & special file handling.
                     (when (or (> base-len 0) file-sep)
                       (setq elem (substring elem base-len (and file-sep (1+ file-sep)))))
                     (let ((r (if-let* ((r (gethash elem ht)))
                                  ;; Reduce duplicate rank with exponential decay.
                                  (- r (round (* vertico-sort-history-duplicate
                                                 (exp (* decay idx)))))
                                ;; Never outrank the most recent element.
                                (if (= idx 0) (/ most-negative-fixnum 2) idx))))
                       (puthash elem r ht)))))
        (cdr (setq vertico-sort--history (cons base ht))))))

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
     (let ((buckets (make-vector ,bsize nil)) last
           ,@(and (eq (car by) 'history) '((hhash (vertico-sort--history)) hcands)))
       (dolist (% candidates)
         ;; Find recent candidate in history or fill bucket
         (,@(if (not (eq (car by) 'history)) `(progn)
              `(if-let* ((idx (gethash % hhash))) (push (cons idx %) hcands)))
          (let ((i ,bindex)) (if (< i ,bsize) (push % (aref buckets i)) (push % last)))))
       (nconc ,@(and (eq (car by) 'history) '((vertico-sort--decorated hcands)))
              (mapcan (lambda (bucket) (sort bucket #',bpred)) buckets)
              (sort last #',pred)))))

;;;###autoload (autoload 'vertico-sort-history-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-history-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-length-alpha "vertico-sort")
;;;###autoload (autoload 'vertico-sort-alpha "vertico-sort")
(vertico-sort--define (history length alpha) 48 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (history alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)
(vertico-sort--define (length alpha) 48 (length %) string< vertico-sort--length-string<)
(vertico-sort--define (alpha) 32 (if (equal % "") 0 (/ (aref % 0) 4)) string< string<)

;;;###autoload
(defun vertico-sort-directories-first (list)
  "Sort directories before files in LIST."
  (setq list (vertico-sort-history-length-alpha list))
  (nconc (cl-loop for x in list if (string-suffix-p "/" x) collect x)
         (cl-loop for x in list if (not (string-suffix-p "/" x)) collect x)))

(provide 'vertico-sort)
;;; vertico-sort.el ends here
