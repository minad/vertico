#+title: vertico.el - VERTical Interactive COmpletion
#+author: Daniel Mendler
#+language: en
#+export_file_name: vertico.texi
#+texinfo_dir_category: Emacs
#+texinfo_dir_title: Vertico: (vertico).
#+texinfo_dir_desc: VERTical Interactive COmpletion.

#+html: <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/75/Vertigomovie_restoration.jpg/800px-Vertigomovie_restoration.jpg" align="right" width="30%">

* Introduction

Vertico provides a minimalistic vertical completion UI, which is based on the
default completion system. By reusing the default system, Vertico achieves full
compatibility with built-in Emacs commands and completion tables. Vertico is
pretty bare-bone and comes with only a minimal set of commands. The code base is
small (~500 lines of code without whitespace and comments). Additional
enhancements can be installed separately via complementary packages.

* Features

- Vertical display with arrow key navigation
- Shows the index of the current candidate and the total number of candidates
- The current candidate is inserted with =TAB= and selected with =RET=
- Non-existing candidates are entered by moving the point to the prompt line
- Candidates sorting by history position, string length and alphabetically
- Long candidates with newlines are formatted to take up less space
- Deferred completion style highlighting for performance
- Support for ~annotation-function~, ~affixation-function~ and ~x-title-function~

[[https://github.com/minad/vertico/blob/main/screenshot.svg?raw=true]]

* Configuration

Vertico is available from [[http://elpa.gnu.org/packages/vertico.html][GNU ELPA]], such that it can be installed directly via
~package-install~. After installation, the global minor mode can be enabled with
=M-x vertico-mode=. In order to configure Vertico and other packages in your
init.el, you may want to use ~use-package~. I recommend to give orderless
completion a try, which is different from the familiar prefix TAB completion.
Here is an example configuration:

#+begin_src emacs-lisp
  ;; Enable vertico
  (use-package vertico
    :init
    (vertico-mode)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    ;; (setq vertico-cycle t)
  )

  ;; Use the `orderless' completion style.
  ;; Enable `partial-completion' for files to allow path expansion.
  ;; You may prefer to use `initials' instead of `partial-completion'.
  (use-package orderless
    :init
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles . (partial-completion))))))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (use-package savehist
    :init
    (savehist-mode))

  ;; A few more useful configurations...
  (use-package emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Grow and shrink minibuffer
    ;;(setq resize-mini-windows t)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t))
#+end_src

* Key bindings

Vertico defines its own local keymap in the minibuffer which is derived from
~minibuffer-local-map~. The keymap mostly keeps the ~fundamental-mode~
keybindings intact and remaps and binds only a few commands. Note in particular
the binding of =TAB= to ~vertico-insert~ and the bindings of
~vertico-exit/exit-input~.

- ~beginning-of-buffer~, ~minibuffer-beginning-of-buffer~ -> ~vertico-first~
- ~end-of-buffer~ -> ~vertico-last~
- ~scroll-down-command~ -> ~vertico-scroll-down~
- ~scroll-up-command~ -> ~vertico-scroll-up~
- ~next-line~, ~next-line-or-history-element~ -> ~vertico-next~
- ~previous-line~, ~previous-line-or-history-element~ -> ~vertico-previous~
- ~exit-minibuffer~ -> ~vertico-exit~
- ~kill-ring-save~ -> ~vertico-save~
- =<C-return>= -> ~vertico-exit-input~
- =TAB= -> ~vertico-insert~

* TAB completion

The bindings of the ~minibuffer-local-completion-map~ are not available in
Vertico by default. This means that TAB works differently from what you may
expect from the default Emacs completion system.

If you prefer to have the default completion commands a key press away you can
add new bindings or even replace the Vertico bindings. Then the default
completion commands will work as usual. For example you can use =M-TAB= to cycle
between candidates if you have set ~completion-cycle-threshold~.

#+begin_src emacs-lisp
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)
#+end_src

The ~orderless~ completion style does not support TAB prefix completion. In
order to enable that you may want to combine ~orderless~ with ~substring~, or
not use ~orderless~ at all.

#+begin_src emacs-lisp
  (setq completion-styles '(substring orderless))
  (setq completion-styles '(basic substring partial-completion flex))
#+end_src

If Vertico is active, it makes sense to disable the automatic =*Completions*=
buffer by setting ~completion-auto-help~ to ~nil~. TAB-completion can be made
less noisy by setting ~completion-show-inline-help~ to ~nil~.

#+begin_src emacs-lisp
  (advice-add #'vertico--setup :after
              (lambda (&rest _)
                (setq-local completion-auto-help nil
                            completion-show-inline-help nil)))
#+end_src

* Complementary packages

Vertico works well together with a few complementary packages, which enrich the
completion UI. These packages are fully supported:

- [[https://github.com/minad/marginalia][Marginalia]]: Rich annotations in the minibuffer
- [[https://github.com/minad/consult][Consult]]: Many useful search and navigation commands
- [[https://github.com/oantolin/embark][Embark]]: Minibuffer actions and context menu
- [[https://github.com/oantolin/orderless][Orderless]]: Advanced completion style

You may also want to look into my [[https://github.com/minad/corfu][Corfu]] package, which provides a minimal
completion system for completion-in-region using overlays. Corfu is developed in
the same spirit as Vertico.

* Alternatives

There are many alternative completion UIs, each UI with its own advantages and
disadvantages. The [[https://github.com/raxod502/selectrum][Selectrum readme]] gives an extensive comparison of many
available completion systems from the perspective of Selectrum.

Vertico aims to be fully compliant with all Emacs commands and achieves that
with a minimal code base, relying purely on ~completing-read~ while avoiding to
invent its own APIs. Inventing a custom API as Helm or Ivy is explicitly avoided
in order to increase flexibility and package reuse.

Since Vertico only provides the UI, you may want to combine it with some of the
complementary packages, to give a full-featured completion experience similar to
Ivy. Vertico is targeted at users interested in crafting their Emacs precisely
to their liking - completion plays an integral part in how the users interacts
with Emacs. There are at least two other interactive completion UIs, which
follow a similar philosophy:

- [[https://github.com/raxod502/selectrum][Selectrum]]: If you are looking for a less minimalistic and more full-featured
  (but also more complex) package, you may be interested in Selectrum, which has
  a similar UI as Vertico. Additionally Selectrum optimizes Tramp file directory
  browsing with caching, supports Avy-style quick keys, a horizontal display and
  a configurable buffer display.
- [[https://github.com/oantolin/icomplete-vertical][Icomplete-vertical]]: This package enhances the Emacs builtin Icomplete with a
  vertical display. In contrast to Vertico, the candidates are rotated such that
  the current candidate always appears at the top. From my perspective,
  candidate rotation feels a bit less intuitive than the UI of Vertico or
  Selectrum. Note that Emacs 28 offers an built-in ~icomplete-vertical-mode~.

* Caveats

Vertico is robust and works well for most use cases, except when navigating
remote directories via Tramp (See [[https://github.com/minad/vertico/issues/20][issue 20]]). My opinion is that the Tramp
performance problems should be resolved on a lower layer within the file
completion table. In case you are a heavy Tramp user, I recommend to give
Selectrum a try. Furthermore there are a few problematic completion commands
described in the next section.

** Problematic completion commands

   A few completion commands make certain assumptions about the completion
   styles and the completion UI. Some of the assumptions may not hold in Vertico
   and as such require minor workarounds.

*** ~org-set-tags-command~

  ~org-set-tags-command~ implements a completion table which relies on the ~basic~
  completion style and TAB completion. This table does not work well with Vertico
  and Icomplete. The issue can be mitigated by deactivating most of the Vertico UI
  and relying purely on TAB completion. The UI is still enhanced by Vertico, since
  Vertico shows the available tags.

  #+begin_src emacs-lisp
    (defun disable-selection ()
      (when (eq minibuffer-completion-table #'org-tags-completion-function)
        (setq-local vertico-map minibuffer-local-completion-map
                    completion-cycle-threshold nil
                    completion-styles '(basic))))
    (advice-add #'vertico--setup :before #'disable-selection)
  #+end_src

  In order to fix the issues properly, ~org-set-tags-command~ should be
  implemented using ~completing-read-multiple~ as discussed on the [[https://lists.gnu.org/archive/html/emacs-orgmode/2020-07/msg00222.html][mailing list]].

*** ~Info-goto-node~

  The command ~Info-goto-node~ uses the ~Info-read-node-name~ completion table,
  which almost works as is with Vertico. However there is the issue that the
  completion table sometimes throws unexpected errors (bug#47771).

*** ~tmm-menubar~

  The text menu bar works well with Vertico but always shows a =*Completions*=
  buffer, which is unwanted if you are using the Vertico UI. This completion
  buffer can be disabled as follows.

  #+begin_src emacs-lisp
    (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  #+end_src

* Contributions

Since this package is part of [[http://elpa.gnu.org/packages/vertico.html][GNU ELPA]] contributions require a copyright
assignment to the FSF.
