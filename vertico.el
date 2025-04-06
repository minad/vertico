;;; vertico.el --- VERTical Interactive COmpletion -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 2.0
;; Package-Requires: ((emacs "28.1") (compat "30"))
;; URL: https://github.com/minad/vertico
;; Keywords: convenience, files, matching, completion

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

;; Vertico provides a performant and minimalistic vertical completion UI
;; based on the default completion system.  By reusing the built-in
;; facilities, Vertico achieves full compatibility with built-in Emacs
;; completion commands and completion tables.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup vertico nil
  "VERTical Interactive COmpletion."
  :link '(info-link :tag "Info Manual" "(vertico)")
  :link '(url-link :tag "Website" "https://github.com/minad/vertico")
  :link '(url-link :tag "Wiki" "https://github.com/minad/vertico/wiki")
  :link '(emacs-library-link :tag "Library Source" "vertico.el")
  :group 'convenience
  :group 'minibuffer
  :prefix "vertico-")

(defcustom vertico-count-format (cons "%-6s " "%s/%s")
  "Format string used for the candidate count."
  :type '(choice (const :tag "No candidate count" nil) (cons string string)))

(defcustom vertico-group-format
  (concat #("    " 0 4 (face vertico-group-separator))
          #(" %s " 0 4 (face vertico-group-title))
          #(" " 0 1 (face vertico-group-separator display (space :align-to right))))
  "Format string used for the group title."
  :type '(choice (const :tag "No group titles" nil) string))

(defcustom vertico-count 10
  "Maximal number of candidates to show."
  :type 'natnum)

(defcustom vertico-preselect 'directory
  "Configure if the prompt or first candidate is preselected.
- prompt: Always select the prompt.
- first: Select the first candidate, allow prompt selection.
- no-prompt: Like first, but forbid selection of the prompt entirely.
- directory: Like first, but select the prompt if it is a directory."
  :type '(choice (const prompt) (const first) (const no-prompt) (const directory)))

(defcustom vertico-scroll-margin 2
  "Number of lines at the top and bottom when scrolling.
The value should lie between 0 and vertico-count/2."
  :type 'natnum)

(defcustom vertico-resize resize-mini-windows
  "How to resize the Vertico minibuffer window, see `resize-mini-windows'."
  :type '(choice (const :tag "Fixed" nil)
                 (const :tag "Shrink and grow" t)
                 (const :tag "Grow-only" grow-only)))

(defcustom vertico-cycle nil
  "Enable cycling for `vertico-next' and `vertico-previous'."
  :type 'boolean)

(defcustom vertico-multiline
  (cons #("↲" 0 1 (face vertico-multiline)) #("…" 0 1 (face vertico-multiline)))
  "Replacements for multiline strings."
  :type '(cons (string :tag "Newline") (string :tag "Truncation")))

(defcustom vertico-sort-function
  (and (fboundp 'vertico-sort-history-length-alpha) 'vertico-sort-history-length-alpha)
  "Default sorting function, used if no `display-sort-function' is specified."
  :type '(choice
          (const :tag "No sorting" nil)
          (const :tag "By history, length and alpha" vertico-sort-history-length-alpha)
          (const :tag "By history and alpha" vertico-sort-history-alpha)
          (const :tag "By length and alpha" vertico-sort-length-alpha)
          (const :tag "Alphabetically" vertico-sort-alpha)
          (function :tag "Custom function")))

(defcustom vertico-sort-override-function nil
  "Override sort function which overrides the `display-sort-function'."
  :type '(choice (const nil) function))

(defgroup vertico-faces nil
  "Faces used by Vertico."
  :group 'vertico
  :group 'faces)

(defface vertico-multiline '((t :inherit shadow))
  "Face used to highlight multiline replacement characters.")

(defface vertico-group-title '((t :inherit shadow :slant italic))
  "Face used for the title text of the candidate group headlines.")

(defface vertico-group-separator '((t :inherit vertico-group-title :strike-through t))
  "Face used for the separator lines of the candidate groups.")

(defface vertico-current '((t :inherit highlight :extend t))
  "Face used to highlight the currently selected candidate.")

(defvar-keymap vertico-map
  :doc "Vertico minibuffer keymap derived from `minibuffer-local-map'."
  :parent minibuffer-local-map
  "<remap> <beginning-of-buffer>" #'vertico-first
  "<remap> <minibuffer-beginning-of-buffer>" #'vertico-first
  "<remap> <end-of-buffer>" #'vertico-last
  "<remap> <scroll-down-command>" #'vertico-scroll-down
  "<remap> <scroll-up-command>" #'vertico-scroll-up
  "<remap> <next-line>" #'vertico-next
  "<remap> <previous-line>" #'vertico-previous
  "<remap> <next-line-or-history-element>" #'vertico-next
  "<remap> <previous-line-or-history-element>" #'vertico-previous
  "<remap> <backward-paragraph>" #'vertico-previous-group
  "<remap> <forward-paragraph>" #'vertico-next-group
  "<remap> <exit-minibuffer>" #'vertico-exit
  "<remap> <kill-ring-save>" #'vertico-save
  "M-RET" #'vertico-exit-input
  "TAB" #'vertico-insert
  "<touchscreen-begin>" #'ignore)

(defvar-local vertico--hilit #'identity
  "Lazy candidate highlighting function.")

(defvar-local vertico--candidates-ov nil
  "Overlay showing the candidates.")

(defvar-local vertico--count-ov nil
  "Overlay showing the number of candidates.")

(defvar-local vertico--index -1
  "Index of current candidate or negative for prompt selection.")

(defvar-local vertico--scroll 0
  "Scroll position.")

(defvar-local vertico--input nil
  "Cons of last minibuffer contents and point or t.")

(defvar-local vertico--candidates nil
  "List of candidates.")

(defvar-local vertico--metadata nil
  "Completion metadata.")

(defvar-local vertico--base ""
  "Base string, which is concatenated with the candidate.")

(defvar-local vertico--total 0
  "Length of the candidate list `vertico--candidates'.")

(defvar-local vertico--lock-candidate nil
  "Lock-in current candidate.")

(defvar-local vertico--lock-groups nil
  "Lock-in current group order.")

(defvar-local vertico--all-groups nil
  "List of all group titles.")

(defvar-local vertico--groups nil
  "List of current group titles.")

(defvar-local vertico--allow-prompt nil
  "Prompt selection is allowed.")

(defun vertico--affixate (cands)
  "Annotate CANDS with annotation function."
  (if-let ((aff (vertico--metadata-get 'affixation-function)))
      (funcall aff cands)
    (if-let ((ann (vertico--metadata-get 'annotation-function)))
        (cl-loop for cand in cands collect
                 (let ((suff (or (funcall ann cand) "")))
                   ;; The default completion UI adds the `completions-annotations'
                   ;; face if no other faces are present.
                   (unless (text-property-not-all 0 (length suff) 'face nil suff)
                     (setq suff (propertize suff 'face 'completions-annotations)))
                   (list cand "" suff)))
      (cl-loop for cand in cands collect (list cand "" "")))))

(defun vertico--move-to-front (elem list)
  "Move ELEM to front of LIST."
  (if-let ((found (member elem list))) ;; No duplicates, compare with Corfu.
      (nconc (list (car found)) (delq (setcar found nil) list))
    list))

(defun vertico--filter-completions (&rest args)
  "Compute all completions for ARGS with lazy highlighting."
  (dlet ((completion-lazy-hilit t) (completion-lazy-hilit-fn nil))
    (static-if (>= emacs-major-version 30)
        (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)
      (cl-letf* ((orig-pcm (symbol-function #'completion-pcm--hilit-commonality))
                 (orig-flex (symbol-function #'completion-flex-all-completions))
                 ((symbol-function #'completion-flex-all-completions)
                  (lambda (&rest args)
                    ;; Unfortunately for flex we have to undo the lazy highlighting, since flex uses
                    ;; the completion-score for sorting, which is applied during highlighting.
                    (cl-letf (((symbol-function #'completion-pcm--hilit-commonality) orig-pcm))
                      (apply orig-flex args))))
                 ((symbol-function #'completion-pcm--hilit-commonality)
                  (lambda (pattern cands)
                    (setq completion-lazy-hilit-fn
                          (lambda (x)
                            ;; `completion-pcm--hilit-commonality' sometimes throws an internal error
                            ;; for example when entering "/sudo:://u".
                            (condition-case nil
                                (car (completion-pcm--hilit-commonality pattern (list x)))
                              (t x))))
                    cands))
                 ((symbol-function #'completion-hilit-commonality)
                  (lambda (cands prefix &optional base)
                    (setq completion-lazy-hilit-fn
                          (lambda (x) (car (completion-hilit-commonality (list x) prefix base))))
                    (and cands (nconc cands base)))))
        (cons (apply #'completion-all-completions args) completion-lazy-hilit-fn)))))

(defun vertico--metadata-get (prop)
  "Return PROP from completion metadata."
  (compat-call completion-metadata-get vertico--metadata prop))

(defun vertico--sort-function ()
  "Return the sorting function."
  (or vertico-sort-override-function
      (vertico--metadata-get 'display-sort-function)
      vertico-sort-function))

(defun vertico--recompute (pt content)
  "Recompute state given PT and CONTENT."
  (pcase-let* ((table minibuffer-completion-table)
               (pred minibuffer-completion-predicate)
               (before (substring content 0 pt))
               (after (substring content pt))
               ;; bug#47678: `completion-boundaries' fails for `partial-completion'
               ;; if the cursor is moved before the slashes of "~//".
               ;; See also corfu.el which has the same issue.
               (bounds (condition-case nil
                           (completion-boundaries before table pred after)
                         (t (cons 0 (length after)))))
               (field (substring content (car bounds) (+ pt (cdr bounds))))
               ;; bug#75910: category instead of `minibuffer-completing-file-name'
               (completing-file (eq 'file (vertico--metadata-get 'category)))
               (`(,all . ,hl) (vertico--filter-completions content table pred pt vertico--metadata))
               (base (or (when-let ((z (last all))) (prog1 (cdr z) (setcdr z nil))) 0))
               (vertico--base (substring content 0 base))
               (def (or (car-safe minibuffer-default) minibuffer-default))
               (groups) (def-missing) (lock))
    ;; Filter the ignored file extensions. We cannot use modified predicate for this filtering,
    ;; since this breaks the special casing in the `completion-file-name-table' for `file-exists-p'
    ;; and `file-directory-p'.
    (when completing-file (setq all (completion-pcm--filename-try-filter all)))
    ;; Sort using the `display-sort-function' or the Vertico sort functions
    (setq all (delete-consecutive-dups (funcall (or (vertico--sort-function) #'identity) all)))
    ;; Move special candidates: "field" appears at the top, before "field/", before default value
    (when (stringp def)
      (setq all (vertico--move-to-front def all)))
    (when (and completing-file (not (string-suffix-p "/" field)))
      (setq all (vertico--move-to-front (concat field "/") all)))
    (setq all (vertico--move-to-front field all))
    (when-let ((fun (and all (vertico--metadata-get 'group-function))))
      (setq groups (vertico--group-by fun all) all (car groups)))
    (setq def-missing (and def (equal content "") (not (member def all)))
          lock (and vertico--lock-candidate ;; Locked position of old candidate.
                    (if (< vertico--index 0) -1
                      (seq-position all (nth vertico--index vertico--candidates)))))
    `((vertico--base . ,vertico--base)
      (vertico--metadata . ,vertico--metadata)
      (vertico--candidates . ,all)
      (vertico--total . ,(length all))
      (vertico--hilit . ,(or hl #'identity))
      (vertico--allow-prompt . ,(and (not (eq vertico-preselect 'no-prompt))
                                     (or def-missing (eq vertico-preselect 'prompt)
                                         (memq minibuffer--require-match
                                               '(nil confirm confirm-after-completion)))))
      (vertico--lock-candidate . ,lock)
      (vertico--groups . ,(cadr groups))
      (vertico--all-groups . ,(or (caddr groups) vertico--all-groups))
      (vertico--index . ,(or lock
                             (if (or def-missing (eq vertico-preselect 'prompt) (not all)
                                     (and completing-file (eq vertico-preselect 'directory)
                                          (= (length vertico--base) (length content))
                                          (test-completion content table pred)))
                                 -1 0))))))

(defun vertico--cycle (list n)
  "Rotate LIST to position N."
  (nconc (copy-sequence (nthcdr n list)) (seq-take list n)))

(defun vertico--group-by (fun elems)
  "Group ELEMS by FUN."
  (let ((ht (make-hash-table :test #'equal)) titles groups)
    ;; Build hash table of groups
    (cl-loop for elem on elems
             for title = (funcall fun (car elem) nil) do
             (if-let ((group (gethash title ht)))
                 (setcdr group (setcdr (cdr group) elem)) ;; Append to tail of group
               (puthash title (cons elem elem) ht) ;; New group element (head . tail)
               (push title titles)))
    (setq titles (nreverse titles))
    ;; Cycle groups if `vertico--lock-groups' is set
    (when-let ((vertico--lock-groups)
               (group (seq-find (lambda (group) (gethash group ht))
                                vertico--all-groups)))
      (setq titles (vertico--cycle titles (seq-position titles group))))
    ;; Build group list
    (dolist (title titles)
      (push (gethash title ht) groups))
    ;; Unlink last tail
    (setcdr (cdar groups) nil)
    (setq groups (nreverse groups))
    ;; Link groups
    (let ((link groups))
      (while (cdr link)
        (setcdr (cdar link) (caadr link))
        (pop link)))
    ;; Check if new groups are found
    (dolist (group vertico--all-groups)
      (remhash group ht))
    (list (caar groups) titles
          (if (hash-table-empty-p ht) vertico--all-groups titles))))

(defun vertico--remote-p (path)
  "Return t if PATH is a remote path."
  (string-match-p "\\`/[^/|:]+:" (substitute-in-file-name path)))

(defun vertico--update (&optional interruptible)
  "Update state, optionally INTERRUPTIBLE."
  (let* ((pt (max 0 (- (point) (minibuffer-prompt-end))))
         (content (minibuffer-contents-no-properties))
         (input (cons content pt)))
    (unless (or (and interruptible (input-pending-p)) (equal vertico--input input))
      ;; Redisplay to make input immediately visible before expensive candidate
      ;; recomputation (gh:minad/vertico#89).  No redisplay during init because
      ;; of flicker.
      (when (and interruptible (consp vertico--input))
        ;; Prevent recursive exhibit from timer (`consult-vertico--refresh').
        (cl-letf (((symbol-function #'vertico--exhibit) #'ignore)) (redisplay)))
      (pcase (let ((vertico--metadata (completion-metadata (substring content 0 pt)
                                                           minibuffer-completion-table
                                                           minibuffer-completion-predicate)))
               ;; If Tramp is used, do not compute the candidates in an
               ;; interruptible fashion, since this will break the Tramp
               ;; password and user name prompts (See gh:minad/vertico#23).
               (if (or (not interruptible)
                       (and (eq 'file (vertico--metadata-get 'category))
                            (or (vertico--remote-p content) (vertico--remote-p default-directory))))
                   (vertico--recompute pt content)
                 (let ((non-essential t))
                   (while-no-input (vertico--recompute pt content)))))
        ('nil (abort-recursive-edit))
        ((and state (pred consp))
         (setq vertico--input input)
         (dolist (s state) (set (car s) (cdr s))))))))

(defun vertico--display-string (str)
  "Return display STR without display and invisible properties."
  (let ((end (length str)) (pos 0) chunks)
    (while (< pos end)
      (let ((nextd (next-single-property-change pos 'display str end))
            (disp (get-text-property pos 'display str)))
        (if (stringp disp)
            (let ((face (get-text-property pos 'face str)))
              (when face
                (add-face-text-property 0 (length disp) face t (setq disp (concat disp))))
              (setq pos nextd chunks (cons disp chunks)))
          (while (< pos nextd)
            (let ((nexti (next-single-property-change pos 'invisible str nextd)))
              (unless (or (get-text-property pos 'invisible str)
                          (and (= pos 0) (= nexti end))) ;; full string -> no allocation
                  (push (substring str pos nexti) chunks))
              (setq pos nexti))))))
    (if chunks (apply #'concat (nreverse chunks)) str)))

(defun vertico--window-width ()
  "Return minimum width of windows, which display the minibuffer."
  (cl-loop for win in (get-buffer-window-list) minimize (window-width win)))

(defun vertico--truncate-multiline (str max)
  "Truncate multiline STR to MAX."
  (let ((pos 0) (res ""))
    (while (and (< (length res) (* 2 max)) (string-match "\\(\\S-+\\)\\|\\s-+" str pos))
      (setq res (concat res (if (match-end 1) (match-string 0 str)
                              (if (string-search "\n" (match-string 0 str))
                                  (car vertico-multiline) " ")))
            pos (match-end 0)))
    (truncate-string-to-width (string-trim res) max 0 nil (cdr vertico-multiline))))

(defun vertico--compute-scroll ()
  "Compute new scroll position."
  (let ((off (max (min vertico-scroll-margin (/ vertico-count 2)) 0))
        (corr (if (= vertico-scroll-margin (/ vertico-count 2)) (1- (mod vertico-count 2)) 0)))
    (setq vertico--scroll (min (max 0 (- vertico--total vertico-count))
                               (max 0 (+ vertico--index off 1 (- vertico-count))
                                    (min (- vertico--index off corr) vertico--scroll))))))

(defun vertico--format-group-title (title cand)
  "Format group TITLE given the current CAND."
  (when (string-prefix-p title cand)
    ;; Highlight title if title is a prefix of the candidate
    (setq cand (propertize cand 'face 'vertico-group-title)
          title (substring (funcall vertico--hilit cand) 0 (length title)))
    (vertico--remove-face 0 (length title) 'completions-first-difference title))
  (format (concat vertico-group-format "\n") title))

(defun vertico--format-count ()
  "Format the count string."
  (format (car vertico-count-format)
          (format (cdr vertico-count-format)
                  (cond ((>= vertico--index 0) (1+ vertico--index))
                        (vertico--allow-prompt "*")
                        (t "!"))
                  vertico--total)))

(defun vertico--display-count ()
  "Update count overlay `vertico--count-ov'."
  (move-overlay vertico--count-ov (point-min) (point-min))
  (overlay-put vertico--count-ov 'before-string
               (if vertico-count-format (vertico--format-count) "")))

(defun vertico--prompt-selection ()
  "Highlight the prompt if selected."
  (let ((inhibit-modification-hooks t))
    (if (and (< vertico--index 0) vertico--allow-prompt)
        (add-face-text-property (minibuffer-prompt-end) (point-max) 'vertico-current 'append)
      (vertico--remove-face (minibuffer-prompt-end) (point-max) 'vertico-current))))

(defun vertico--remove-face (beg end face &optional obj)
  "Remove FACE between BEG and END from OBJ."
  (while (< beg end)
    (let ((next (next-single-property-change beg 'face obj end)))
      (when-let ((val (get-text-property beg 'face obj)))
        (put-text-property beg next 'face (remq face (ensure-list val)) obj))
      (setq beg next))))

(defun vertico--exhibit ()
  "Exhibit completion UI."
  (let ((buffer-undo-list t)) ;; Overlays affect point position and undo list!
    (vertico--update 'interruptible)
    (vertico--prompt-selection)
    (vertico--display-count)
    (vertico--display-candidates (vertico--arrange-candidates))
    (vertico--resize)))

(defun vertico--goto (index)
  "Go to candidate with INDEX."
  (setq vertico--index
        (max (if (or vertico--allow-prompt (= 0 vertico--total)) -1 0)
             (min index (1- vertico--total)))
        vertico--lock-candidate (or (>= vertico--index 0) vertico--allow-prompt)))

(defun vertico--candidate (&optional hl)
  "Return current candidate string with optional highlighting if HL is non-nil."
  (let ((content (substring (or (car-safe vertico--input) (minibuffer-contents-no-properties)))))
    (cond
     ((>= vertico--index 0)
      (let ((cand (substring (nth vertico--index vertico--candidates))))
        ;; XXX Drop the completions-common-part face which is added by the
        ;; `completion--twq-all' hack.  This should better be fixed in Emacs
        ;; itself, the corresponding code is already marked as fixme.
        (vertico--remove-face 0 (length cand) 'completions-common-part cand)
        (concat vertico--base (if hl (funcall vertico--hilit cand) cand))))
     ((and (equal content "") (or (car-safe minibuffer-default) minibuffer-default)))
     (t content))))

(defun vertico--match-p (input)
  "Return t if INPUT is a valid match."
  (let ((rm minibuffer--require-match))
    (or (memq rm '(nil confirm-after-completion))
        (equal "" input) ;; Null completion, returns default value
        (if (functionp rm) (funcall rm input) ;; Emacs 29 supports functions
          (test-completion input minibuffer-completion-table minibuffer-completion-predicate))
        (if (eq rm 'confirm) (eq (ignore-errors (read-char "Confirm")) 13)
          (minibuffer-message "Match required") nil))))

(cl-defgeneric vertico--format-candidate (cand prefix suffix index _start)
  "Format CAND given PREFIX, SUFFIX and INDEX."
  (setq cand (vertico--display-string (concat prefix cand suffix "\n")))
  (when (= index vertico--index)
    (add-face-text-property 0 (length cand) 'vertico-current 'append cand))
  cand)

(cl-defgeneric vertico--arrange-candidates ()
  "Arrange candidates."
  (vertico--compute-scroll)
  (let ((curr-line 0) lines)
    ;; Compute group titles
    (let* (title (index vertico--scroll)
           (group-fun (and vertico-group-format (vertico--metadata-get 'group-function)))
           (candidates
            (vertico--affixate
             (cl-loop repeat vertico-count for c in (nthcdr index vertico--candidates)
                      collect (funcall vertico--hilit (substring c))))))
      (pcase-dolist ((and cand `(,str . ,_)) candidates)
        (when-let ((new-title (and group-fun (funcall group-fun str nil))))
          (unless (equal title new-title)
            (setq title new-title)
            (push (vertico--format-group-title title str) lines))
          (setcar cand (funcall group-fun str 'transform)))
        (when (= index vertico--index)
          (setq curr-line (length lines)))
        (push (cons index cand) lines)
        (cl-incf index)))
    ;; Drop excess lines
    (setq lines (nreverse lines))
    (cl-loop for count from (length lines) above vertico-count do
             (if (< curr-line (/ count 2))
                 (nbutlast lines)
               (setq curr-line (1- curr-line) lines (cdr lines))))
    ;; Format candidates
    (let ((max-width (- (vertico--window-width) 4)) start)
      (cl-loop for line on lines do
               (pcase (car line)
                 (`(,index ,cand ,prefix ,suffix)
                  (setq start (or start index))
                  (when (string-search "\n" cand)
                    (setq cand (vertico--truncate-multiline cand max-width)))
                  (setcar line (vertico--format-candidate cand prefix suffix index start))))))
    lines))

(cl-defgeneric vertico--display-candidates (lines)
  "Update candidates overlay `vertico--candidates-ov' with LINES."
  (move-overlay vertico--candidates-ov (point-max) (point-max))
  (overlay-put vertico--candidates-ov 'after-string
               (apply #'concat #(" " 0 1 (cursor t)) (and lines "\n") lines)))

(cl-defgeneric vertico--resize ()
  "Resize active minibuffer window."
  (setq-local truncate-lines (< (point) (* 0.8 (vertico--window-width)))
              resize-mini-windows vertico-resize
              max-mini-window-height 1.0)
  (unless truncate-lines (set-window-hscroll nil 0))
  (unless (or vertico-resize (frame-root-window-p (active-minibuffer-window)))
    (let ((delta (- (max (cdr (window-text-pixel-size))
                         (* (default-line-height) (1+ vertico-count)))
                 (window-pixel-height))))
      (when (/= 0 delta) (window-resize nil delta nil nil 'pixelwise)))))

(cl-defgeneric vertico--prepare ()
  "Ensure that the state is prepared before running the next command."
  (when-let ((cmd (and (symbolp this-command) (symbol-name this-command)))
             ((string-prefix-p "vertico-" cmd))
             ((not (and vertico--metadata (string-prefix-p "vertico-directory-" cmd)))))
    (vertico--update)))

(cl-defgeneric vertico--setup ()
  "Setup completion UI."
  (when (boundp 'pixel-scroll-precision-mode)
    (setq-local pixel-scroll-precision-mode nil))
  (setq-local scroll-margin 0
              vertico--input t
              completion-auto-help nil
              completion-show-inline-help nil
              vertico--candidates-ov (make-overlay (point-max) (point-max) nil t t)
              vertico--count-ov (make-overlay (point-min) (point-min) nil t t))
  (overlay-put vertico--count-ov 'priority 1) ;; For `minibuffer-depth-indicate-mode'
  (use-local-map vertico-map)
  (add-hook 'pre-command-hook #'vertico--prepare nil 'local)
  (add-hook 'post-command-hook #'vertico--exhibit nil 'local))

(cl-defgeneric vertico--advice (&rest app)
  "Advice for completion function, apply APP."
  (minibuffer-with-setup-hook #'vertico--setup (apply app)))

(defun vertico-first ()
  "Go to first candidate, or to the prompt when the first candidate is selected."
  (interactive)
  (vertico--goto (if (> vertico--index 0) 0 -1)))

(defun vertico-last ()
  "Go to last candidate."
  (interactive)
  (vertico--goto (1- vertico--total)))

(defun vertico-scroll-down (&optional n)
  "Go back by N pages."
  (interactive "p")
  (vertico--goto (max 0 (- vertico--index (* (or n 1) vertico-count)))))

(defun vertico-scroll-up (&optional n)
  "Go forward by N pages."
  (interactive "p")
  (vertico-scroll-down (- (or n 1))))

(defun vertico-next (&optional n)
  "Go forward N candidates."
  (interactive "p")
  (let ((index (+ vertico--index (or n 1))))
    (vertico--goto
     (cond
      ((not vertico-cycle) index)
      ((= vertico--total 0) -1)
      (vertico--allow-prompt (1- (mod (1+ index) (1+ vertico--total))))
      (t (mod index vertico--total))))))

(defun vertico-previous (&optional n)
  "Go backward N candidates."
  (interactive "p")
  (vertico-next (- (or n 1))))

(defun vertico-exit (&optional arg)
  "Exit minibuffer with current candidate or input if prefix ARG is given."
  (interactive "P")
  (when (and (not arg) (>= vertico--index 0))
    (vertico-insert))
  (when (vertico--match-p (minibuffer-contents-no-properties))
    (exit-minibuffer)))

(defun vertico-next-group (&optional n)
  "Cycle N groups forward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (when (cdr vertico--groups)
    (if (setq vertico--lock-groups (not (eq n 0)))
        (setq vertico--groups (vertico--cycle vertico--groups
                                              (let ((len (length vertico--groups)))
                                                (- len (mod (- (or n 1)) len))))
              vertico--all-groups (vertico--cycle vertico--all-groups
                                                  (seq-position vertico--all-groups
                                                                (car vertico--groups))))
      (setq vertico--groups nil
            vertico--all-groups nil))
    (setq vertico--lock-candidate nil
          vertico--input nil)))

(defun vertico-previous-group (&optional n)
  "Cycle N groups backward.
When the prefix argument is 0, the group order is reset."
  (interactive "p")
  (vertico-next-group (- (or n 1))))

(defun vertico-exit-input ()
  "Exit minibuffer with input."
  (interactive)
  (vertico-exit t))

(defun vertico-save ()
  "Save current candidate to kill ring."
  (interactive)
  (if (or (use-region-p) (not transient-mark-mode))
      (call-interactively #'kill-ring-save)
    (kill-new (substring-no-properties (vertico--candidate)))))

(defun vertico-insert ()
  "Insert current candidate in minibuffer."
  (interactive)
  ;; XXX There is a small bug here, depending on interpretation. When completing
  ;; "~/emacs/master/li|/calc" where "|" is the cursor, then the returned
  ;; candidate only includes the prefix "~/emacs/master/lisp/", but not the
  ;; suffix "/calc". Default completion has the same problem when selecting in
  ;; the *Completions* buffer. See bug#48356.
  (when (> vertico--total 0)
    (let ((vertico--index (max 0 vertico--index)))
      (insert (prog1 (vertico--candidate) (delete-minibuffer-contents))))))

;;;###autoload
(define-minor-mode vertico-mode
  "VERTical Interactive COmpletion."
  :global t :group 'vertico
  (dolist (fun '(completing-read-default completing-read-multiple))
    (if vertico-mode
        (advice-add fun :around #'vertico--advice)
      (advice-remove fun #'vertico--advice))))

(defun vertico--command-p (_sym buffer)
  "Return non-nil if Vertico is active in BUFFER."
  (buffer-local-value 'vertico--input buffer))

;; Do not show Vertico commands in M-X
(dolist (sym '( vertico-next vertico-next-group vertico-previous vertico-previous-group
                vertico-scroll-down vertico-scroll-up vertico-exit vertico-insert
                vertico-exit-input vertico-save vertico-first vertico-last
                vertico-repeat-next ;; autoloads in vertico-repeat.el
                vertico-quick-jump vertico-quick-exit vertico-quick-insert ;; autoloads in vertico-quick.el
                vertico-directory-up vertico-directory-enter ;; autoloads in vertico-directory.el
                vertico-directory-delete-char vertico-directory-delete-word))
  (put sym 'completion-predicate #'vertico--command-p))

(provide 'vertico)
;;; vertico.el ends here
