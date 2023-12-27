;;; vertico-flat.el --- Flat, horizontal display for Vertico -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 1.6
;; Package-Requires: ((emacs "27.1") (compat "29.1.4.4") (vertico "1.6"))
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

;; This package is a Vertico extension providing a horizontal display.
;;
;; The mode can be enabled globally or via `vertico-multiform-mode'
;; per command or completion category.  Alternatively the flat display
;; can be toggled temporarily with M-F if `vertico-multiform-mode' is
;; enabled.
;;
;; The flat display can be made to look like `ido-mode' by setting
;; `vertico-cycle' to t. See also the `vertico-flat-format'
;; configuration variable for further tweaks.

;;; Code:

(require 'vertico)
(eval-when-compile (require 'cl-lib))

(defcustom vertico-flat-max-lines 1
  "Maximal number of lines to use."
  :type 'natnum
  :group 'vertico)

(defcustom vertico-flat-format
  '(:multiple   #("{%s}" 0 1 (face minibuffer-prompt)
                  3 4 (face minibuffer-prompt))
    :single     #("[%s]" 0 1 (face minibuffer-prompt)
                  1 3 (face success) 3 4 (face minibuffer-prompt))
    :prompt     #("(%s)" 0 1 (face minibuffer-prompt)
                  3 4 (face minibuffer-prompt))
    :separator  #(" | " 0 3 (face minibuffer-prompt))
    :ellipsis   #("…" 0 1 (face minibuffer-prompt))
    :no-match   "[No match]")
  "Formatting strings."
  :type 'plist
  :group 'vertico)

(defcustom vertico-flat-annotate nil
  "Annotate candidates."
  :type 'boolean
  :group 'vertico)

(defvar-keymap vertico-flat-map
  :doc "Additional keymap activated in flat mode."
  "<remap> <left-char>" #'vertico-previous
  "<remap> <right-char>" #'vertico-next)

;;;###autoload
(define-minor-mode vertico-flat-mode
  "Flat, horizontal display for Vertico."
  :global t :group 'vertico
  ;; Shrink current minibuffer window
  (when-let ((win (active-minibuffer-window)))
    (unless (frame-root-window-p win)
      (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise)))
  (cl-callf2 rassq-delete-all vertico-flat-map minor-mode-map-alist)
  (when vertico-flat-mode
    (push `(vertico--input . ,vertico-flat-map) minor-mode-map-alist)))

(cl-defmethod vertico--display-candidates (candidates &context (vertico-flat-mode (eql t)))
  (setq-local truncate-lines nil
              resize-mini-windows t)
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put
   vertico--candidates-ov 'after-string
   (concat #(" " 0 1 (cursor t))
           (cond
            ((and (not candidates) (plist-get vertico-flat-format :no-match)))
            ((and (= vertico--total 1) (= vertico--index 0)
                  (when-let ((fmt (plist-get vertico-flat-format :single)))
                    (format fmt (substring-no-properties (car candidates))))))
            (t (format (plist-get vertico-flat-format (if (< vertico--index 0) :prompt :multiple))
                       (string-join candidates (plist-get vertico-flat-format :separator))))))))

(cl-defmethod vertico--arrange-candidates (&context (vertico-flat-mode (eql t)))
  (let* ((index (max 0 vertico--index)) (count vertico-count)
         (candidates (nthcdr vertico--index vertico--candidates))
         (width (- (* vertico-flat-max-lines (- (vertico--window-width) 4))
                   (length (plist-get vertico-flat-format :left))
                   (length (plist-get vertico-flat-format :separator))
                   (length (plist-get vertico-flat-format :right))
                   (length (plist-get vertico-flat-format :ellipsis))
                   (car (posn-col-row (posn-at-point (1- (point-max)))))))
         (result) (wrapped))
    (while (and candidates (not (eq wrapped (car candidates)))
                (> width 0) (> count 0))
      (let ((cand (pop candidates)) (prefix "") (suffix ""))
        (setq cand (funcall vertico--hilit (substring cand)))
        (pcase (and vertico-flat-annotate (vertico--affixate (list cand)))
          (`((,c ,p ,s)) (setq cand c prefix p suffix s)))
        (when (string-search "\n" cand)
          (setq cand (vertico--truncate-multiline cand width)))
        (setq cand (string-trim
                    (replace-regexp-in-string
                     "[ \t]+"
                     (lambda (x) (apply #'propertize " " (text-properties-at 0 x)))
                     (vertico--format-candidate cand prefix suffix index vertico--index)))
              index (1+ index)
              count (1- count)
              width (- width (string-width cand) (length (plist-get vertico-flat-format :separator))))
        (when (or (not result) (> width 0))
          (push cand result))
        (when (and vertico-cycle (not candidates))
          (setq candidates vertico--candidates index 0
                wrapped (nth vertico--index vertico--candidates)))))
    (when (if wrapped
              (> vertico--total (- vertico-count count))
            (and (/= vertico--total 0) (/= index vertico--total)))
      (push (plist-get vertico-flat-format :ellipsis) result))
    (nreverse result)))

(provide 'vertico-flat)
;;; vertico-flat.el ends here
