#+title: vertico.el - VERTical Interactive COmpletion
#+author: Daniel Mendler
#+language: en
#+export_file_name: vertico.texi
#+texinfo_dir_category: Emacs
#+texinfo_dir_title: Vertico: (vertico).
#+texinfo_dir_desc: VERTical Interactive COmpletion.

#+html: <a href="http://elpa.gnu.org/packages/vertico.html"><img alt="GNU ELPA" src="https://elpa.gnu.org/packages/vertico.svg"/></a>
#+html: <a href="http://elpa.gnu.org/devel/vertico.html"><img alt="GNU-devel ELPA" src="https://elpa.gnu.org/devel/vertico.svg"/></a>
#+html: <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/7/75/Vertigomovie_restoration.jpg/800px-Vertigomovie_restoration.jpg" align="right" width="30%">

* Introduction

  Vertico provides a performant and minimalistic vertical completion UI based on
  the default completion system. The main focus of Vertico is to provide a UI
  which behaves /correctly/ under all circumstances. By reusing the built-in
  facilities system, Vertico achieves /full compatibility/ with built-in Emacs
  completion commands and completion tables. Vertico only provides the
  completion UI but aims to be flexible and extensible. Additional enhancements
  can be installed separately via [[#extensions][extensions]] or [[#complementary-packages][complementary packages]]. The code
  base is small and maintainable (~vertico.el~ is only about 600 lines of code
  without white space and comments).

* Features

  - Vertical display with arrow key navigation
  - Prompt shows the current candidate index and the total number of candidates
  - The current candidate is inserted with =TAB= and selected with =RET=
  - Non-existing candidates can be entered by moving the point to the prompt line
  - Configurable sorting by history position, length and alphabetically
  - Long candidates with newlines are formatted to take up less space
  - Deferred completion style highlighting for performance
  - Support for annotations (~annotation-function~ and ~affixation-function~)
  - Support for grouping and group cycling commands (~group-function~)

  [[https://github.com/minad/vertico/blob/main/screenshot.svg?raw=true]]

* Configuration

  Vertico is available from [[http://elpa.gnu.org/packages/vertico.html][GNU ELPA]], such that it can be installed directly via
  ~package-install~. After installation, the global minor mode can be enabled with
  =M-x vertico-mode=. In order to configure Vertico and other packages in your
  init.el, you may want to use ~use-package~. I recommend to give Orderless
  completion a try, which is different from the prefix TAB completion used by
  the basic default completion system or in shells. Here is an example
  configuration:

  #+begin_src emacs-lisp
    ;; Enable vertico
    (use-package vertico
      :init
      (vertico-mode)

      ;; Grow and shrink the Vertico minibuffer
      ;; (setq vertico-resize t)

      ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
      ;; (setq vertico-cycle t)
      )

    ;; Use the `orderless' completion style. Additionally enable
    ;; `partial-completion' for file path expansion. `partial-completion' is
    ;; important for wildcard support. Multiple files can be opened at once
    ;; with `find-file' if you enter a wildcard. You may also give the
    ;; `initials' completion style a try.
    (use-package orderless
      :init
      (setq completion-styles '(orderless)
            completion-category-defaults nil
            completion-category-overrides '((file (styles partial-completion)))))

    ;; Persist history over Emacs restarts. Vertico sorts by history position.
    (use-package savehist
      :init
      (savehist-mode))

    ;; A few more useful configurations...
    (use-package emacs
      :init
      ;; Add prompt indicator to `completing-read-multiple'.
      ;; Alternatively try `consult-completing-read-multiple'.
      (defun crm-indicator (args)
        (cons (concat "[CRM] " (car args)) (cdr args)))
      (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

      ;; Do not allow the cursor in the minibuffer prompt
      (setq minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt))
      (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

      ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
      ;; Vertico commands are hidden in normal buffers.
      ;; (setq read-extended-command-predicate
      ;;       #'command-completion-default-include-p)

      ;; Enable recursive minibuffers
      (setq enable-recursive-minibuffers t))
  #+end_src

* Key bindings

  Vertico defines its own local keymap in the minibuffer which is derived from
  ~minibuffer-local-map~. The keymap keeps most of the ~fundamental-mode~
  keybindings intact and remaps and binds only a handful of commands. Note in
  particular the binding of =TAB= to ~vertico-insert~ and the bindings of
  ~vertico-exit/exit-input~.

  - ~beginning-of-buffer~, ~minibuffer-beginning-of-buffer~ -> ~vertico-first~
  - ~end-of-buffer~ -> ~vertico-last~
  - ~scroll-down-command~ -> ~vertico-scroll-down~
  - ~scroll-up-command~ -> ~vertico-scroll-up~
  - ~next-line~, ~next-line-or-history-element~ -> ~vertico-next~
  - ~previous-line~, ~previous-line-or-history-element~ -> ~vertico-previous~
  - ~forward-paragraph~ -> ~vertico-next-group~
  - ~backward-paragraph~ -> ~vertico-previous-group~
  - ~exit-minibuffer~ -> ~vertico-exit~
  - ~kill-ring-save~ -> ~vertico-save~
  - =<C-return>= -> ~vertico-exit-input~
  - =TAB= -> ~vertico-insert~

* Completion styles and TAB completion

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

  The ~orderless~ completion style does not support completion of a common prefix
  substring, as you may be familiar with from shells or the basic default
  completion system. The reason is that the Orderless input string is usually
  not a prefix. In order to support completing prefixes you may want to combine
  ~orderless~ with ~substring~ in your =completion-styles= configuration.

  #+begin_src emacs-lisp
    (setq completion-styles '(substring orderless))
  #+end_src

  Alternatively you can experiment with the built-in completion-styles, e.g.,
  adding =partial-completion= or =flex=. The =partial-completion= style is important
  to add if you want to open multiple files at once with ~find-file~ using
  wildcards. In order to open multiple files at once, you have to move to the
  prompt and then press =RET=.

  #+begin_src emacs-lisp
    (setq completion-styles '(basic substring partial-completion flex))
  #+end_src

  Because Vertico is fully compatible with Emacs default completion
  system, further customization of completion behavior can be achieved
  by setting the designated Emacs variables. For example, one may wish
  to disable case-sensitivity for file and buffer matching when built-in
  completion styles are used instead of ~orderless~:

  #+begin_src emacs-lisp
    (setq read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t)
  #+end_src

* Extensions
  :properties:
  :custom_id: extensions
  :end:

  *NOTE*: The extensions currently require manual installation, e.g., via
  ~package-install-file~. Please ensure that you are using the /newest/ Vertico
  commit when using extensions.

  We maintain small extension packages to Vertico in this repository in the
  subdirectory [[https://github.com/minad/vertico/tree/main/extensions][extensions/]]. The extensions can be installed additionally to
  Vertico. Currently these extensions are available:

  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-buffer.el][vertico-buffer.el]]: =vertico-buffer-mode= to display Vertico in a separate buffer
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-directory.el][vertico-directory.el]]: Commands for Ido-like directory navigation
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-flat.el][vertico-flat.el]]: =vertico-flat-mode= to enable a flat, horizontal display
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-indexed.el][vertico-indexed.el]]: =vertico-indexed-mode= to select indexed candidates with prefix arguments
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-mouse.el][vertico-mouse.el]]: =vertico-mouse-mode= to support for scrolling and candidate selection
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-quick.el][vertico-quick.el]]: Commands to select using Avy-style quick keys
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-repeat.el][vertico-repeat.el]]: The command =vertico-repeat= repeats the last completion session
  - [[https://github.com/minad/vertico/blob/main/extensions/vertico-reverse.el][vertico-reverse.el]]: =vertico-reverse-mode= to reverse the display

  With these extensions it is possible to adapt Vertico heavily such that it
  matches your preference or behaves similar to familiar UIs. The combination
  =vertico-flat= plus =vertico-directory= resembles Ido in look and feel.

  Configuration example for =vertico-directory.el=:

  #+begin_src emacs-lisp
    ;; Configure directory extension.
    ;; NOTE: The file `vertico-directory.el' must be installed manually.
    (use-package vertico-directory
      ;; More convenient directory navigation commands
      :bind (:map vertico-map
                  ("RET" . vertico-directory-enter)
                  ("DEL" . vertico-directory-delete-char)
                  ("M-DEL" . vertico-directory-delete-word))
      ;; Tidy shadowed file names
      :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
  #+end_src

* Complementary packages
  :properties:
  :custom_id: complementary-packages
  :end:

  Vertico works well together with complementary packages, which enrich the
  completion UI. These packages are fully supported:

  - [[https://github.com/minad/marginalia][Marginalia]]: Rich annotations in the minibuffer
  - [[https://github.com/minad/consult][Consult]]: Useful search and navigation commands
  - [[https://github.com/oantolin/embark][Embark]]: Minibuffer actions and context menu
  - [[https://github.com/oantolin/orderless][Orderless]]: Advanced completion style

  In order to get accustomed with the package ecosystem the following approach
  is recommended:

  1. Start with plain Emacs.
  2. Install and enable Vertico to get incremental minibuffer completion.
  3. Install Orderless and/or configure the built-in completion styles
     for more flexible minibuffer filtering.
  4. Install Marginalia if you like rich minibuffer annotations.
  5. Install Embark and add two keybindings for ~embark-dwim~ and ~embark-act~.
     I am using =M-.= and =C-.=. These commands allow you to act on the object
     at point or in the minibuffer.
  6. Install Consult if you want additional featureful completion commands,
     e.g, the buffer switcher ~consult-buffer~ with preview or the line-based
     search ~consult-line~.
  7. Install Embark-Consult and Wgrep for export from =consult-line= to =occur-mode=
     buffers and from =consult-grep= to editable =grep-mode= buffers.

  You don't have to use all of these components. Use only the ones you like and
  the ones which fit well into your setup. The steps 1. to 4. introduce no new
  commands over plain Emacs. Step 5. introduces the new commands ~embark-act~ and
  ~embark-dwim~. In step 6. you get the Consult commands, some offer new
  functionality not present in Emacs already (e.g., ~consult-line~) and some are
  substitutes (e.g., ~consult-buffer~ for ~switch-to-buffer~).

  There are some special setting you may want to add to your configuration. In
  case you want to use Vertico for completion-at-point/completion-in-region, you
  can use the function ~consult-completion-in-region~ provided by the Consult
  package.

  #+begin_src emacs-lisp
    ;; Use `consult-completion-in-region' if Vertico is enabled.
    ;; Otherwise use the default `completion--in-region' function.
    (setq completion-in-region-function
          (lambda (&rest args)
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args)))
  #+end_src

  Furthermore Consult offers an enhanced =completing-read-multiple= implementation,
  which works well with Vertico.

  #+begin_src emacs-lisp
    (advice-add #'completing-read-multiple
                :override #'consult-completing-read-multiple)
  #+end_src

  You may also want to look into my [[https://github.com/minad/corfu][Corfu]] package, which provides a minimal
  completion system for completion-in-region using overlays. Corfu is developed in
  the same spirit as Vertico.

* Alternatives

  There are many alternative completion UIs, each UI with its own advantages and
  disadvantages.

  Vertico aims to be 100% compliant with all Emacs commands and achieves that
  with a minimal code base, relying purely on ~completing-read~ while avoiding to
  invent its own APIs. Inventing a custom API as Helm or Ivy is explicitly
  avoided in order to increase flexibility and package reuse. Due to its small
  code base and reuse of the Emacs built-in facilities, bugs are less likely to
  occur in comparison to completion UIs or full completion systems, which
  reimplement a lot of functionality.

  Since Vertico only provides the UI, you may want to combine it with some of
  the complementary packages, to give a full-featured completion experience
  similar to Helm or Ivy. Overall the packages in the spirit of Vertico have a
  different style than Helm or Ivy. The idea is to have smaller independent
  components, which one can add and understand step by step. Each component
  focuses on its niche and tries to be as non-intrusive as possible. Vertico
  targets users interested in crafting their Emacs precisely to their liking -
  completion plays an integral part in how the users interacts with Emacs.

  There are at least two other interactive completion UIs, which follow a
  similar philosophy:

  - [[https://github.com/raxod502/selectrum][Selectrum]]: Selectrum has a similar UI as Vertico. Selectrum is more complex
    and not fully compatible with every Emacs completion command ([[https://github.com/raxod502/selectrum/issues/481][Issue #481]]),
    since it uses its own filtering infrastructure, which deviates from the
    standard Emacs completion facilities. Vertico additionally has the ability
    to cycle over candidates, offers more commands for grouping support and
    comes with optional [[#extensions][extensions]].
  - [[https://github.com/oantolin/icomplete-vertical][Icomplete-vertical]]: This package enhances the Emacs builtin Icomplete with a
    vertical display. In contrast to Vertico, Icomplete rotates the candidates
    such that the current candidate always appears at the top. From my
    perspective, candidate rotation feels a bit less intuitive than the UI of
    Vertico or Selectrum. Note that Emacs 28 offers a built-in
    ~icomplete-vertical-mode~.

* Problematic completion commands

  Vertico works well and is robust in most scenarios. However some completion
  commands make certain assumptions about the completion styles and the
  completion UI. Some of these assumptions may not hold in Vertico and as such
  require minor workarounds.

** ~org-set-tags-command~

   ~org-set-tags-command~ implements a completion table which relies on the ~basic~
   completion style and TAB completion. This table does not work well with
   Vertico and Icomplete. The issue can be mitigated by deactivating most of the
   Vertico UI and relying purely on TAB completion. The UI is still enhanced by
   Vertico, since Vertico shows the available tags.

   #+begin_src emacs-lisp
     (defun disable-selection ()
       (when (eq minibuffer-completion-table #'org-tags-completion-function)
         (setq-local vertico-map minibuffer-local-completion-map
                     completion-cycle-threshold nil
                     completion-styles '(basic))))
     (advice-add #'vertico--setup :before #'disable-selection)
   #+end_src

   *Update:* ~org-set-tags-command~ is changed to use ~completing-read-multiple~ in
   the current Org development version (9.5) as has been [[https://lists.gnu.org/archive/html/emacs-orgmode/2020-07/msg00222.html][proposed before]]. This
   fix improves the compatibility with many completion UIs, including Vertico.
   See the recent [[https://lists.gnu.org/archive/html/emacs-orgmode/2021-07/msg00287.html][mailing list discussion]].

** ~org-refile~

   ~org-refile~ uses ~org-olpath-completing-read~ to complete the outline path
   in steps, when ~org-refile-use-outline-path~ is non-nil.

   Unfortunately the implementation of this Org completion table is broken. In
   order to fix the issue at the root, the completion table should make use of
   completion boundaries and should be written in the same way as the built-in
   file completion table.

   In order to workaround the issues with the current implementation it is
   recommended to disable the outline path completion in steps. The completion
   on the full path is also faster since the input string matches directly
   against the full path, which works very well with Orderless.

   #+begin_src emacs-lisp
     (setq org-refile-use-outline-path 'file
           org-outline-path-complete-in-steps nil)
   #+end_src

** ~tmm-menubar~

   The text menu bar works well with Vertico but always shows a =*Completions*=
   buffer, which is unwanted if you are using the Vertico UI. This completion
   buffer can be disabled as follows.

   #+begin_src emacs-lisp
     (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
   #+end_src

** Tramp hostname completion

   In combination with Orderless, hostnames are not made available for
   completion after entering =/ssh:=. In order to avoid this problem, the =basic=
   completion style should be specified for the file completion category.

   #+begin_src emacs-lisp
     (setq completion-styles '(orderless)
           completion-category-overrides '((file (styles basic partial-completion))))
   #+end_src

   For users who are familiar with the =completion-style= machinery: You may also
   define a custom completion style which sets in only for remote files!

   #+begin_src emacs-lisp
     (defun basic-remote-try-completion (string table pred point)
       (and (vertico--remote-p string)
            (completion-basic-try-completion string table pred point)))
     (defun basic-remote-all-completions (string table pred point)
       (and (vertico--remote-p string)
            (completion-basic-all-completions string table pred point)))
     (add-to-list
      'completion-styles-alist
      '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
     (setq completion-styles '(orderless)
           completion-category-overrides '((file (styles basic-remote partial-completion))))
   #+end_src

* Contributions

  Since this package is part of [[http://elpa.gnu.org/packages/vertico.html][GNU ELPA]] contributions require a copyright
  assignment to the FSF.
