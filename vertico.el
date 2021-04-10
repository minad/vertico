;;; vertico.el --- VERTical Interactive COmpletion -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler
;; Maintainer: Daniel Mendler
;; Created: 2021
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
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

;; This package provides a minimalistic vertical completion UI, which is based
;; on the default completion system. By reusing the default system, Vertico
;; achieve full compatibility with built-in Emacs commands and completion
;; tables. Vertico is pretty bare-bone and only provides a minimal set of
;; commands.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'subr-x))

(defgroup vertico nil
  "VERTical Interactive COmpletion."
  :group 'convenience
  :prefix "vertico-")

(defcustom vertico-sort-threshold 20000
  "Candidates will only be sorted if there are fewer than this threshold."
  :type 'integer)

(defcustom vertico-count-format (cons "%-6s " "%s/%s")
  "Format string used for the candidate count."
  :type '(choice (const nil) (cons string string)))

(defcustom vertico-group-format
  (concat
   #("    " 0 4 (face vertico-group-separator))
   #(" %s " 0 4 (face vertico-group-title))
   #(" " 0 1 (face vertico-group-separator display (space :align-to right))))
  "Format string used for the group title."
  :type '(choice (const nil) string))

(defcustom vertico-count 10
  "Maximal number of candidates to show."
  :type 'integer)

(defcustom vertico-multiline
  (cons #("⤶" 0 1 (face vertico-multiline))
        #("…" 0 1 (face vertico-multiline)))
  "Replacements for multiline strings."
  :type '(cons string string))

(defgroup vertico-faces nil
  "Faces used by Vertico."
  :group 'vertico
  :group 'faces)

(defface vertico-multiline '((t :inherit shadow))
  "Face used to highlight multiline replacement characters.")

(defface vertico-group-title '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines.")

(defface vertico-group-separator '((t :inherit shadow :strike-through t))
  "Face used for the separator lines of the candidate groups.")

(defface vertico-current '((t :inherit highlight :extend t))
  "Face used to highlight the currently selected candidate.")

(defvar vertico-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [remap beginning-of-buffer] #'vertico-beginning-of-buffer)
    (define-key map [remap minibuffer-beginning-of-buffer] #'vertico-beginning-of-buffer)
    (define-key map [remap end-of-buffer] #'vertico-end-of-buffer)
    (define-key map [remap scroll-down-command] #'vertico-scroll-down)
    (define-key map [remap scroll-up-command] #'vertico-scroll-up)
    (define-key map [remap next-line] #'vertico-next)
    (define-key map [remap previous-line] #'vertico-previous)
    (define-key map [remap next-line-or-history-element] #'vertico-next)
    (define-key map [remap previous-line-or-history-element] #'vertico-previous)
    (define-key map [remap exit-minibuffer] #'vertico-exit)
    (define-key map [remap kill-ring-save] #'vertico-save)
    (define-key map [C-return] #'vertico-exit-input)
    (define-key map "\t" #'vertico-insert)
    map)
  "Minibuffer keymap.")

(defvar-local vertico--history-hash nil
  "History hash table.")

(defvar-local vertico--history-dir nil
  "Directory of `vertico--history-hash'.")

(defvar-local vertico--candidates-ov nil
  "Overlay showing the candidates.")

(defvar-local vertico--count-ov nil
  "Overlay showing the number of candidates.")

(defvar-local vertico--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local vertico--input nil
  "Cons of last minibuffer contents and boundaries or t.")

(defvar-local vertico--candidates nil
  "List of candidates.")

(defvar-local vertico--base 0
  "Size of the base string, which is concatenated with the candidate.")

(defvar-local vertico--total 0
  "Length of the candidate list `vertico--candidates'.")

(defvar-local vertico--keep nil
  "Keep current candidate index `vertico--index'.")

(defun vertico--sort-predicate (x y)
  "Sorting predicate which compares X and Y."
  (or (< (cdr x) (cdr y))
      (and (= (cdr x) (cdr y))
           (string< (car x) (car y)))))

(defun vertico--sort (input candidates)
  "Sort CANDIDATES by history, length and alphabetically, given current INPUT."
  ;; Store the history position first in a hashtable in order to allow O(1)
  ;; history lookup. File names get special treatment. In principle, completion
  ;; tables with boundaries should also get special treatment, but files are
  ;; the most important.
  (cond
   ((eq minibuffer-history-variable 'file-name-history)
    (let ((dir (expand-file-name (substitute-in-file-name
                                  (or (file-name-directory input)
                                      default-directory)))))
      (unless (equal vertico--history-dir dir)
        (setq vertico--history-hash (make-hash-table :test #'equal :size (length file-name-history))
              vertico--history-dir dir)
        (let* ((index 0)
               (adir (abbreviate-file-name dir))
               (dlen (length dir))
               (alen (length adir)))
          (dolist (elem file-name-history)
            (let* ((len (length elem))
                   (file (cond
                          ((and (> len dlen)
                                (eq t (compare-strings dir 0 dlen elem 0 dlen)))
                           (substring elem dlen))
                          ((and (> len alen)
                                (eq t (compare-strings adir 0 alen elem 0 alen)))
                           (substring elem alen)))))
              (when file
                (when-let (slash (string-match-p "/" file))
                  (setq file (substring file 0 (1+ slash))))
                (unless (gethash file vertico--history-hash)
                  (puthash file index vertico--history-hash)))
              (setq index (1+ index))))))))
   ((not vertico--history-hash)
    (let ((index 0)
          ;; History disabled if `minibuffer-history-variable' eq `t'.
          (hist (and (not (eq minibuffer-history-variable t))
                     (symbol-value minibuffer-history-variable))))
      (setq vertico--history-hash (make-hash-table :test #'equal :size (length hist)))
      (dolist (elem hist)
        (unless (gethash elem vertico--history-hash)
          (puthash elem index vertico--history-hash))
        (setq index (1+ index))))))
  ;; Decorate each candidate with (index<<13) + length. This way we sort first by index and then by
  ;; length. We assume that the candidates are shorter than 2**13 characters and that the history is
  ;; shorter than 2**16 entries.
  (let ((cand candidates))
    (while cand
      (setcar cand (cons (car cand)
                         (+ (lsh (gethash (car cand) vertico--history-hash #xFFFF) 13)
                            (length (car cand)))))
      (pop cand)))
  (setq candidates (sort candidates #'vertico--sort-predicate))
  ;; Drop decoration from the candidates
  (let ((cand candidates))
    (while cand
      (setcar cand (caar cand))
      (pop cand)))
  candidates)

(defun vertico--annotate (metadata candidates)
  "Annotate CANDIDATES with annotation function specified by METADATA."
  (if-let (aff (or (completion-metadata-get metadata 'affixation-function)
                   (plist-get completion-extra-properties :affixation-function)))
      (funcall aff candidates)
    (if-let (ann (or (completion-metadata-get metadata 'annotation-function)
                     (plist-get completion-extra-properties :annotation-function)))
        (mapcar (lambda (cand) (list cand (or (funcall ann cand) ""))) candidates)
      candidates)))

(defvar orderless-skip-highlighting)
(defun vertico--highlight (input metadata candidates)
  "Highlight CANDIDATES with INPUT using the completion style specified by METADATA."
  (let* ((orderless-skip-highlighting)
         (highlighted (nconc
                       (completion-all-completions input
                                                   candidates
                                                   nil
                                                   (length input)
                                                   metadata)
                       nil)))
    ;; Check if everything went alright, all the candidates should still be present.
    (if (= (length highlighted) (length candidates))
        highlighted candidates)))

(defun vertico--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (head (car (member elem list)))
      (nconc (list head) (delq head list))
    list))

(defun vertico--file-predicate ()
  "Filter predicate for files."
  (let ((ignore (concat "\\(?:\\`\\|/\\)\\.?\\./\\'"
                        (and completion-ignored-extensions
                             (concat "\\|" (regexp-opt completion-ignored-extensions) "\\'")))))
    (if-let (pred minibuffer-completion-predicate)
        (lambda (x) (and (not (string-match-p ignore x)) (funcall pred x)))
      (lambda (x) (not (string-match-p ignore x))))))

(defun vertico--recompute-candidates (pt content bounds metadata)
  "Recompute candidates given PT, CONTENT, BOUNDS and METADATA."
  (let* ((field (substring content (car bounds) (+ pt (cdr bounds))))
         (all (completion-all-completions
               content
               minibuffer-completion-table
               (if minibuffer-completing-file-name
                   (vertico--file-predicate)
                 minibuffer-completion-predicate)
               pt metadata))
         (base (if-let (last (last all))
                   (prog1 (cdr last)
                     (setcdr last nil))
                 0))
         (def (or (car-safe minibuffer-default) minibuffer-default))
         (total (length all)))
    (when (<= total vertico-sort-threshold)
      (setq all (if-let (sort (completion-metadata-get metadata 'display-sort-function))
                    (funcall sort all)
                  (vertico--sort content all))))
    ;; Move special candidates: "field" appears at the top, before "field/", before default value
    (when (stringp def)
      (setq all (vertico--move-to-front def all)))
    (when (and minibuffer-completing-file-name (not (string-suffix-p "/" field)))
      (setq all (vertico--move-to-front (concat field "/") all)))
    (setq all (vertico--move-to-front field all))
    (when-let (group (completion-metadata-get metadata 'x-group-function))
      (setq all (mapcan #'cdr (funcall group all))))
    (list base total all)))

(defun vertico--update-candidates (pt content bounds metadata)
  "Preprocess candidates given PT, CONTENT, BOUNDS and METADATA."
  (pcase (let ((while-no-input-ignore-events '(selection-request)))
           (while-no-input (vertico--recompute-candidates pt content bounds metadata)))
    ('nil (abort-recursive-edit))
    (`(,base ,total ,candidates)
     (unless (and vertico--keep (< vertico--index 0))
       (if-let* ((old (and candidates
                           vertico--keep
                           (>= vertico--index 0)
                           (nth vertico--index vertico--candidates)))
                 (idx (seq-position candidates old)))
           ;; Update index, when kept candidate is found in new candidates list.
           (setq vertico--index idx)
         ;; Otherwise select the prompt for missing candidates or for matching content, as long as
         ;; the full content after the boundary is empty, including content after point.
         (setq vertico--keep nil
               vertico--index
               (if (or (not candidates)
                       (and (= (car bounds) (length content))
                            (test-completion content minibuffer-completion-table
                                             minibuffer-completion-predicate)))
                   -1 0))))
     (setq vertico--input (cons content pt)
           vertico--base base
           vertico--total total
           vertico--candidates candidates))))

(defun vertico--flatten-string (prop str)
  "Flatten STR with display or invisible PROP."
  (let ((end (length str)) (pos 0) (chunks))
    (while (< pos end)
      (let ((next (next-single-property-change pos prop str end)))
        (if-let (val (get-text-property pos prop str))
            (when (and (eq prop 'display) (stringp val))
              (push val chunks))
          (push (substring str pos next) chunks))
        (setq pos next)))
    (apply #'concat (nreverse chunks))))

(defun vertico--format-candidates (content bounds metadata)
  "Format current candidates with CONTENT string, BOUNDS and METADATA."
  (let* ((group (completion-metadata-get metadata 'x-group-function))
         (group-format (and group vertico-group-format (concat vertico-group-format "\n")))
         (index (min (max 0 (- vertico--index (/ vertico-count 2) (if group-format -1 0)))
                     (max 0 (- vertico--total vertico-count))))
         (candidates
          (thread-last (seq-subseq vertico--candidates index
                                   (min (+ index vertico-count) vertico--total))
            (vertico--highlight (substring content (car bounds)) metadata)
            (vertico--annotate metadata)))
         (max-width (- (window-width) 4))
         (current-line 0) (title) (lines))
    (dolist (ann-cand candidates)
      (let* ((prefix "") (suffix "")
             (cand (pcase ann-cand
                     (`(,c ,s) (setq suffix s) c)
                     (`(,c ,p ,s) (setq prefix p suffix s) c)
                     (c c))))
        (when-let (new-title (and group-format (caar (funcall group (list cand)))))
          (unless (equal title new-title)
            (push (format group-format (setq title new-title)) lines)))
        (when (string-match-p "\n" cand)
          (setq cand (thread-last cand
                       (replace-regexp-in-string "[\t ]+" " ")
                       (replace-regexp-in-string "[\t\n ]*\n[\t\n ]*" (car vertico-multiline))
                       (replace-regexp-in-string "\\`[\t\n ]+\\|[\t\n ]+\\'" ""))
                cand (truncate-string-to-width cand max-width 0 nil (cdr vertico-multiline))))
        (setq cand (vertico--flatten-string 'invisible (vertico--flatten-string 'display cand))
              cand (concat prefix cand
                           (if (text-property-not-all 0 (length suffix) 'face nil suffix)
                               suffix
                             (propertize suffix 'face 'completions-annotations))
                           "\n"))
        (when (= index vertico--index)
          (setq current-line (length lines))
          (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
        (push cand lines)
        (setq index (1+ index))))
    (when (> (length lines) vertico-count)
      (if (< current-line (- vertico-count 1))
          (setq lines (nthcdr (- (length lines) vertico-count) lines))
        (setcdr (nthcdr (- vertico-count 1) lines) nil)))
    (when lines
      (setcar lines (substring (car lines) 0 -1)))
    (nreverse lines)))

(defun vertico--display-candidates (lines)
  "Update candidates overlay `vertico--candidates-ov' with LINES."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put vertico--candidates-ov 'after-string
               (apply #'concat #(" " 0 1 (cursor t)) (and lines "\n") lines))
  (let* ((resize (default-value 'resize-mini-windows))
         (delta (- (max (length lines) (if resize 0 vertico-count)) (window-height) -1)))
    (when (or (> delta 0) (eq resize t))
      (window-resize nil delta))))

(defun vertico--display-count ()
  "Update count overlay `vertico--count-ov'."
  (when vertico-count-format
    (move-overlay vertico--count-ov (point-min) (point-min))
    (overlay-put vertico--count-ov 'before-string
                 (format (car vertico-count-format)
                         (format (cdr vertico-count-format)
                                 (cond
                                  ((>= vertico--index 0) (1+ vertico--index))
                                  ((vertico--require-match) "!")
                                  (t "*"))
                                 vertico--total)))))

(defun vertico--tidy-shadowed-file ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (and minibuffer-completing-file-name
             (eq this-command #'self-insert-command)
             (bound-and-true-p rfn-eshadow-overlay)
             (overlay-buffer rfn-eshadow-overlay)
             (= (point) (point-max))
             (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
                 (eq ?/ (char-before (- (point) 2)))))
    (delete-region (overlay-start rfn-eshadow-overlay) (overlay-end rfn-eshadow-overlay))))

(defun vertico--prompt-selection ()
  "Highlight the prompt if selected."
  (let ((inhibit-modification-hooks t))
    (vertico--add-face 'vertico-current (minibuffer-prompt-end) (point-max)
                       (and (< vertico--index 0) (not (vertico--require-match))))))

(defun vertico--add-face (face beg end add)
  "Add FACE between BEG and END depending if ADD is t, otherwise remove."
  (while (< beg end)
    (let* ((val (get-text-property beg 'face))
           (faces (remq face (if (listp val) val (list val))))
           (next (next-single-property-change beg 'face nil end)))
      (add-text-properties beg next `(face ,(if add (cons face faces) faces)))
      (setq beg next))))

(defun vertico--exhibit ()
  "Exhibit completion UI."
  (vertico--tidy-shadowed-file)
  (let* ((pt (max 0 (- (point) (minibuffer-prompt-end))))
         (metadata (completion-metadata (buffer-substring-no-properties
                                         (minibuffer-prompt-end)
                                         (+ (minibuffer-prompt-end) pt))
                                        minibuffer-completion-table
                                        minibuffer-completion-predicate))
         (content (minibuffer-contents-no-properties))
         (before (substring content 0 pt))
         (after (substring content pt))
         ;; BUG: `completion-boundaries` fails for `partial-completion`
         ;; if the cursor is moved between the slashes of "~//".
         ;; See also marginalia.el which has the same issue.
         ;; Upstream bug: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47678
         (bounds (or (condition-case nil
                         (completion-boundaries before
                                                minibuffer-completion-table
                                                minibuffer-completion-predicate
                                                after)
                       (t (cons 0 (length after)))))))
    (unless (equal vertico--input (cons content pt))
      (vertico--update-candidates pt content bounds metadata))
    (vertico--prompt-selection)
    (vertico--display-count)
    (vertico--display-candidates (vertico--format-candidates content bounds metadata))))

(defun vertico--require-match ()
  "Return t if match is required."
  (not (memq minibuffer--require-match '(nil confirm confirm-after-completion))))

(defun vertico--goto (index)
  "Go to candidate with INDEX."
  (setq vertico--keep t
        vertico--index
        (max (if (and (vertico--require-match) vertico--candidates)
                 0 -1)
             (min index (- vertico--total 1)))))

(defun vertico-beginning-of-buffer ()
  "Go to first candidate."
  (interactive)
  (vertico--goto 0))

(defun vertico-end-of-buffer ()
  "Go to last candidate."
  (interactive)
  (vertico--goto (- vertico--total 1)))

(defun vertico-scroll-down ()
  "Go back by one page."
  (interactive)
  (vertico--goto (max 0 (- vertico--index vertico-count))))

(defun vertico-scroll-up ()
  "Go forward by one page."
  (interactive)
  (vertico--goto (+ vertico--index vertico-count)))

(defun vertico-next ()
  "Go to next candidate."
  (interactive)
  (vertico--goto (1+ vertico--index)))

(defun vertico-previous ()
  "Go to previous candidate."
  (interactive)
  (vertico--goto (- vertico--index 1)))

(defun vertico-exit (&optional arg)
  "Exit minibuffer with current candidate or input if prefix ARG is given."
  (interactive "P")
  (unless arg (vertico-insert))
  (let ((input (minibuffer-contents-no-properties)))
    (if (or (memq minibuffer--require-match '(nil confirm-after-completion))
            (equal "" input) ;; The questionable null completion
            (test-completion input
                             minibuffer-completion-table
                             minibuffer-completion-predicate)
            (and (eq minibuffer--require-match 'confirm)
                 (eq (ignore-errors (read-char "Confirm")) 13)))
        (exit-minibuffer)
      (message "Match required"))))

(defun vertico-exit-input ()
  "Exit minibuffer with input."
  (interactive)
  (vertico-exit t))

(defun vertico-save ()
  "Save current candidate to kill ring."
  (interactive)
  (if (or (use-region-p) (not transient-mark-mode))
      (call-interactively #'kill-ring-save)
    (kill-new (vertico--candidate))))

(defun vertico-insert ()
  "Insert current candidate in minibuffer."
  (interactive)
  (when-let (cand (and (>= vertico--index 0) (vertico--candidate)))
    (delete-minibuffer-contents)
    (insert cand)))

(defun vertico--candidate ()
  "Return current candidate string."
  (let ((content (minibuffer-contents-no-properties)))
    (if (< vertico--index 0)
        content
      (concat (substring content 0 vertico--base)
              (nth vertico--index vertico--candidates)))))

(defun vertico--setup ()
  "Setup completion UI."
  (setq vertico--input t
        vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  (setq-local orderless-skip-highlighting t ;; Orderless optimization
              resize-mini-windows 'grow-only
              truncate-lines t
              max-mini-window-height 1.0)
  (use-local-map vertico-map)
  (add-hook 'post-command-hook #'vertico--exhibit -99 'local))

(defun vertico--advice (orig &rest args)
  "Advice for ORIG completion function, receiving ARGS."
  (minibuffer-with-setup-hook #'vertico--setup (apply orig args)))

;;;###autoload
(define-minor-mode vertico-mode
  "VERTical Interactive COmpletion."
  :global t
  (if vertico-mode
      (progn
        (advice-add #'completing-read-default :around #'vertico--advice)
        (advice-add #'completing-read-multiple :around #'vertico--advice))
    (advice-remove #'completing-read-default #'vertico--advice)
    (advice-remove #'completing-read-multiple #'vertico--advice)))

(provide 'vertico)
;;; vertico.el ends here
