;;; vertico.el --- VERTical Interactive COmpletion -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.10
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

;; Vertico provides a minimalistic vertical completion UI, which is
;; based on the default completion system. By reusing the default
;; system, Vertico achieves full compatibility with built-in Emacs
;; commands and completion tables. Vertico is pretty bare-bone and
;; comes with only a minimal set of commands.

;;; Code:

(require 'seq)
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup vertico nil
  "VERTical Interactive COmpletion."
  :group 'convenience
  :prefix "vertico-")

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

(defcustom vertico-cycle nil
  "Enable cycling for `vertico-next' and `vertico-previous'."
  :type 'boolean)

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
  (let ((map (make-composed-keymap nil minibuffer-local-map)))
    (define-key map [remap beginning-of-buffer] #'vertico-first)
    (define-key map [remap minibuffer-beginning-of-buffer] #'vertico-first)
    (define-key map [remap end-of-buffer] #'vertico-last)
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
  "Vertico minibuffer keymap derived from `minibuffer-local-map'.")

(defvar-local vertico--highlight #'identity
  "Deferred candidate highlighting function.")

(defvar-local vertico--history-hash nil
  "History hash table.")

(defvar-local vertico--history-base nil
  "Base prefix of `vertico--history-hash'.")

(defvar-local vertico--candidates-ov nil
  "Overlay showing the candidates.")

(defvar-local vertico--count-ov nil
  "Overlay showing the number of candidates.")

(defvar-local vertico--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local vertico--input nil
  "Cons of last minibuffer contents and point or t.")

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
  (or (< (length x) (length y))
      (and (= (length x) (length y))
           (string< x y))))

(defun vertico--sort (base candidates)
  "Sort CANDIDATES by history, length and alphabetically, given current BASE prefix string."
  (unless (and vertico--history-hash (equal vertico--history-base base))
    (let* ((index 0)
           (base-size (length base))
           ;; History disabled if `minibuffer-history-variable' eq `t'.
           (hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable)))
           (hash (make-hash-table :test #'equal :size (length hist))))
      (if (= base-size 0)
          ;; Put history elements into the hash
          (dolist (elem hist)
            (unless (gethash elem hash)
              (puthash elem index hash))
            (setq index (1+ index)))
        ;; Drop base string from history elements, before putting them into the hash
        (dolist (elem hist)
          (when (and (>= (length elem) base-size)
                     (eq t (compare-strings base 0 base-size elem 0 base-size)))
            (setq elem (substring elem base-size))
            (unless (gethash elem hash)
              (puthash elem index hash)))
          (setq index (1+ index))))
      (setq vertico--history-hash hash
            vertico--history-base base)))
  ;; Separate history candidates from candidates first.
  ;; Move the remaining candidates into buckets according to length.
  (let* ((max-bucket 40)
         (buckets (make-vector (1+ max-bucket) nil))
         (hcands))
    (dolist (cand candidates)
      (if-let (idx (gethash cand vertico--history-hash))
          (push (cons idx cand) hcands)
        (let ((idx (min max-bucket (length cand))))
          (aset buckets idx (cons cand (aref buckets idx))))))
    ;; Sort history candidates
    (setq hcands (sort hcands #'car-less-than-car))
    (let ((cand hcands))
      (while cand
        (setcar cand (cdar cand))
        (pop cand)))
    (nconc
     ;; Sorted History candidates
     hcands
     ;; Sort bucket candidates
     (mapcan
      (lambda (bucket) (sort bucket #'string<))
      (nbutlast (append buckets nil)))
     ;; Last bucket needs special treatment
     (sort (aref buckets max-bucket) #'vertico--sort-predicate))))

(defun vertico--annotate (metadata candidates)
  "Annotate CANDIDATES with annotation function specified by METADATA."
  (if-let (aff (or (completion-metadata-get metadata 'affixation-function)
                   (plist-get completion-extra-properties :affixation-function)))
      (funcall aff candidates)
    (if-let (ann (or (completion-metadata-get metadata 'annotation-function)
                     (plist-get completion-extra-properties :annotation-function)))
        (mapcar (lambda (cand) (list cand "" (or (funcall ann cand) ""))) candidates)
      candidates)))

(defun vertico--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let (found (member elem list))
      (let ((head (list (car found))))
        (nconc head (delq (setcar found nil) list)))
    list))

;; bug#47711: Deferred highlighting for `completion-all-completions'
;; XXX There is one complication: `completion--twq-all' already adds `completions-common-part'.
;; See below `vertico--candidate'.
(declare-function orderless-highlight-matches "ext:orderless")
(declare-function orderless-pattern-compiler "ext:orderless")
(require 'orderless nil 'noerror)
(defun vertico--all-completions (&rest args)
  "Compute all completions for ARGS with deferred highlighting."
  (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
             (orig-flex (symbol-function #'completion-flex-all-completions))
             ((symbol-function #'completion-flex-all-completions)
              (lambda (&rest args)
                ;; Unfortunately for flex we have to undo the deferred highlighting, since flex uses
                ;; the completion-score for sorting, which is applied during highlighting.
                (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                  (apply orig-flex args))))
             ;; Defer the following highlighting functions
             (hl #'identity)
             ((symbol-function #'completion-hilit-commonality)
              (lambda (cands prefix &optional base)
                (setq hl (lambda (x) (nconc (completion-hilit-commonality x prefix base) nil)))
                (and cands (nconc cands base))))
             ((symbol-function #'completion-pcm--hilit-commonality)
              (lambda (pattern cands)
                (setq hl (lambda (x) (completion-pcm--hilit-commonality pattern x)))
                cands))
             ((symbol-function #'orderless-highlight-matches)
              (lambda (pattern cands)
                (let ((regexps (orderless-pattern-compiler pattern)))
                  (setq hl (lambda (x) (orderless-highlight-matches regexps x))))
                cands)))
    (cons (apply #'completion-all-completions args) hl)))

(defun vertico--recompute-candidates (pt content bounds metadata)
  "Recompute candidates given PT, CONTENT, BOUNDS and METADATA."
  (let* ((field (substring content (car bounds) (+ pt (cdr bounds))))
         ;; `minibuffer-completing-file-name' has been obsoleted by the completion category
         (completing-file (eq 'file (completion-metadata-get metadata 'category)))
         (all-hl (vertico--all-completions content
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate
                                           pt metadata))
         (all (car all-hl))
         (base (or (when-let (z (last all)) (prog1 (cdr z) (setcdr z nil))) 0))
         (def (or (car-safe minibuffer-default) minibuffer-default)))
    ;; Filter the ignored file extensions. We cannot use modified predicate for this filtering,
    ;; since this breaks the special casing in the `completion-file-name-table' for `file-exists-p'
    ;; and `file-directory-p'.
    (when completing-file
      (let ((ignore (concat "\\(?:\\`\\|/\\)\\.?\\./\\'"
                            (and completion-ignored-extensions
                                 (concat "\\|" (regexp-opt completion-ignored-extensions) "\\'")))))
        (setq all (cl-delete-if (lambda (x) (string-match-p ignore x)) all))))
    (setq all (if-let (sort (completion-metadata-get metadata 'display-sort-function))
                  (funcall sort all)
                (vertico--sort (substring content 0 base) all)))
    ;; Move special candidates: "field" appears at the top, before "field/", before default value
    (when (stringp def)
      (setq all (vertico--move-to-front def all)))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (vertico--move-to-front (concat field "/") all)))
    (setq all (vertico--move-to-front field all))
    (when-let (title-fun (completion-metadata-get metadata 'x-title-function))
      (setq all (vertico--group-by title-fun all)))
    (list base (length all) all (cdr all-hl))))

(defun vertico--group-by (fun elems)
  "Group ELEMS by FUN."
  (when elems
    (let ((group-list) (group-hash (make-hash-table :test #'equal)))
      (while elems
        (let* ((key (funcall fun (car elems) nil))
               (group (gethash key group-hash)))
          (if group
              (setcdr group (setcdr (cdr group) elems)) ;; Append to tail of group
            (setq group (cons elems elems)) ;; (head . tail)
            (push group group-list)
            (puthash key group group-hash))
          (setq elems (cdr elems))))
      (setcdr (cdar group-list) nil) ;; Unlink last tail
      (setq group-list (nreverse group-list))
      (prog1 (caar group-list)
        (while (cdr group-list)
          (setcdr (cdar group-list) (caadr group-list)) ;; Link groups
          (setq group-list (cdr group-list)))))))

(defun vertico--update-candidates (pt content bounds metadata)
  "Preprocess candidates given PT, CONTENT, BOUNDS and METADATA."
  ;; bug#38024: Icomplete uses `while-no-input-ignore-events' to repair updating issues
  (pcase (let ((while-no-input-ignore-events '(selection-request)))
           (while-no-input (vertico--recompute-candidates pt content bounds metadata)))
    ('nil (abort-recursive-edit))
    (`(,base ,total ,candidates ,hl)
     ;; Find position of old candidate in the new list.
     (unless (and vertico--keep (< vertico--index 0))
       (let ((old (and candidates
                       vertico--keep
                       (>= vertico--index 0)
                       (nth vertico--index vertico--candidates))))
         (setq vertico--index (and old (seq-position candidates old)))))
     (setq vertico--input (cons content pt)
           vertico--base base
           vertico--total total
           vertico--highlight hl
           vertico--candidates candidates)
     ;; If the current index is nil, compute new index. Select the prompt:
     ;; * If there are no candidates
     ;; * If the default is missing from the candidate list.
     ;; * For matching content, as long as the full content after the boundary is empty,
     ;;   including content after point.
     (unless vertico--index
       (setq vertico--keep nil
             vertico--index
             (if (or (not vertico--candidates)
                     (vertico--default-missing-p)
                     (and (= (car bounds) (length content))
                          (test-completion content minibuffer-completion-table
                                           minibuffer-completion-predicate)))
                 -1 0))))))

(defun vertico--flatten-string (prop str)
  "Flatten STR with display or invisible PROP."
  (let ((end (length str)) (pos 0) (chunks))
    (while (< pos end)
      (let ((next (next-single-property-change pos prop str end))
            (val (get-text-property pos prop str)))
        (cond
         ((and val (eq prop 'display) (stringp val))
          (push val chunks))
         ((not (and val (eq prop 'invisible)))
          (push (substring str pos next) chunks)))
        (setq pos next)))
    (apply #'concat (nreverse chunks))))

(defun vertico--format-candidates (metadata)
  "Format current candidates with METADATA."
  (let* ((title-fun (completion-metadata-get metadata 'x-title-function))
         (group-format (and title-fun vertico-group-format (concat vertico-group-format "\n")))
         (index (min (max 0 (- vertico--index (/ vertico-count 2) (if group-format -1 0)))
                     (max 0 (- vertico--total vertico-count))))
         (candidates
          (thread-last (seq-subseq vertico--candidates index
                                   (min (+ index vertico-count) vertico--total))
            (funcall vertico--highlight)
            (vertico--annotate metadata)))
         (max-width (- (window-width) 4))
         (current-line 0) (title) (lines))
    (dolist (cand candidates)
      (let ((prefix "") (suffix ""))
        (when (consp cand)
          (setq prefix (cadr cand) suffix (caddr cand) cand (car cand)))
        (when-let (new-title (and group-format (funcall title-fun cand nil)))
          (unless (equal title new-title)
            (push (format group-format (setq title new-title)) lines))
          (setq cand (funcall title-fun cand 'transform)))
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
    (setq lines (nreverse lines) index (length lines))
    (while (> index vertico-count)
      (if (< current-line (/ index 2))
          (nbutlast lines)
        (setq current-line (- current-line 1) lines (cdr lines)))
      (setq index (- index 1)))
    lines))

(defun vertico--display-candidates (lines)
  "Update candidates overlay `vertico--candidates-ov' with LINES."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put vertico--candidates-ov 'after-string
               (apply #'concat #(" " 0 1 (cursor t)) (and lines "\n") lines))
  (vertico--resize-window (length lines)))

(defun vertico--resize-window (height)
  "Resize active minibuffer window to HEIGHT."
  (unless (frame-root-window-p (active-minibuffer-window))
    (let* ((resize (default-value 'resize-mini-windows))
           (window-resize-pixelwise t)
           (dp (- (max (cdr (window-text-pixel-size))
                       (* (default-line-height) (1+ (if resize height vertico-count))))
                  (window-pixel-height))))
      (when (and (or (/= height 0) (< dp 0))
                 (or (> dp 0) (eq resize t)))
        (window-resize nil dp nil nil 'pixelwise)))))

(defun vertico--display-count ()
  "Update count overlay `vertico--count-ov'."
  (when vertico-count-format
    (move-overlay vertico--count-ov (point-min) (point-min))
    (overlay-put vertico--count-ov 'before-string
                 (format (car vertico-count-format)
                         (format (cdr vertico-count-format)
                                 (cond ((>= vertico--index 0) (1+ vertico--index))
                                       ((vertico--allow-prompt-selection-p) "*")
                                       (t "!"))
                                 vertico--total)))))

(defun vertico--tidy-shadowed-file ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (and (eq this-command #'self-insert-command)
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
                       (and (< vertico--index 0) (vertico--allow-prompt-selection-p)))))

(defun vertico--add-face (face beg end add &optional obj)
  "Add FACE between BEG and END from OBJ if ADD is t, otherwise remove."
  (while (< beg end)
    (let* ((val (get-text-property beg 'face obj))
           (faces (remq face (if (listp val) val (list val))))
           (next (next-single-property-change beg 'face obj end)))
      (add-text-properties beg next `(face ,(if add (cons face faces) faces)) obj)
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
         ;; bug#47678: `completion-boundaries` fails for `partial-completion`
         ;; if the cursor is moved between the slashes of "~//".
         ;; See also marginalia.el which has the same issue.
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
    (vertico--display-candidates (vertico--format-candidates metadata))))

(defun vertico--allow-prompt-selection-p ()
  "Return t if prompt can be selected."
  (or (memq minibuffer--require-match '(nil confirm confirm-after-completion))
      (vertico--default-missing-p)))

(defun vertico--default-missing-p ()
  "Return t if default is missing from the candidate list."
  (when-let (def (or (car-safe minibuffer-default) minibuffer-default))
    (and (= (minibuffer-prompt-end) (point)) (not (member def vertico--candidates)))))

(defun vertico--goto (index)
  "Go to candidate with INDEX."
  (setq vertico--keep t
        vertico--index
        (max (if (or (vertico--allow-prompt-selection-p) (not vertico--candidates)) -1 0)
             (min index (- vertico--total 1)))))

(defun vertico-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (vertico--goto (if (> vertico--index 0) 0 -1)))

(defun vertico-last ()
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
  (vertico--goto
   (if (and vertico-cycle (= (1+ vertico--index) vertico--total))
       -1
     (1+ vertico--index))))

(defun vertico-previous ()
  "Go to previous candidate."
  (interactive)
  (vertico--goto
   (if (and vertico-cycle (= vertico--index (if (vertico--allow-prompt-selection-p) -1 0)))
       (- vertico--total 1)
     (- vertico--index 1))))

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
  ;; XXX There is a small bug here, depending on interpretation. When
  ;; completing "~/emacs/master/li|/calc" where "|" is the cursor,
  ;; then the returned candidate only includes the prefix
  ;; "~/emacs/master/lisp/", but not the suffix "/calc". Default
  ;; completion has the same problem when selecting in the
  ;; *Completions* buffer. See bug#48356.
  (when-let (cand (and (>= vertico--index 0) (vertico--candidate)))
    (delete-minibuffer-contents)
    (insert cand)))

(defun vertico--candidate (&optional hl)
  "Return current candidate string with optional highlighting if HL is non-nil."
  (let ((content (minibuffer-contents)))
    (if (>= vertico--index 0)
        (let ((cand (nth vertico--index vertico--candidates)))
          ;;; XXX Drop the completions-common-part face which is added by `completion--twq-all'.
          ;; This is a hack in Emacs and should better be fixed in Emacs itself, the corresponding
          ;; code is already marked with a FIXME. Should this be reported as a bug?
          (vertico--add-face 'completions-common-part 0 (length cand) nil cand)
          (concat (substring content 0 vertico--base)
                  (if hl (car (funcall vertico--highlight (list cand))) cand)))
      content)))

(defun vertico--setup ()
  "Setup completion UI."
  (setq vertico--input t
        vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
        vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  (setq-local resize-mini-windows 'grow-only
              truncate-lines (< (minibuffer-prompt-end) (/ (window-width) 2))
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
