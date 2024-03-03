;;/co -*- lexical-binding: t; -*-

;;; General config
(setq-default delete-by-moving-to-trash t)

;;; UI config
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
(setq-default visible-cursor nil ; for non-blinking cursor in console
              help-window-select t  ; always select help window to close it easily
              echo-keystrokes 0.01)  ; show presses keys immediately
(global-display-line-numbers-mode)

;;; editor config
;; (for many variables, only `setq-default' works in default.el)
(setq-default major-mode 'org-mode  ; use org-mode by default instead of fundamental-mode
              indent-tabs-mode nil  ; use spaces for indentation
              standard-indent 2
              tab-width 2
              sh-indentation 2
              nushell-indent-offset 2
              python-indent-offset 2
              css-indent-offset 2
              js-indent-level 2
              go-ts-mode-indent-offset 2
              rust-ts-mode-indent-offset 2
              ess-fancy-comments nil  ; indent comment line as normal code
              select-enable-clipboard nil  ; don't paste to clipboard by default
              interprogram-paste-function nil  ; don't copy clipboard to kill-ring
              interprogram-cut-function nil  ; don't copy kill-ring to clipboard
              show-trailing-whitespace t)

;; Make jump history local to each window
(setq-default xref-history-storage 'xref-window-local-history)

;; put backup files in a dedicated dir
(setq-default backup-directory-alist `((".*" . ,(concat user-emacs-directory "backups/"))))
(setq-default
 auto-save-file-name-transforms
 ;; strip directories (modified from default value)
 `(("\\`\\(/[^/]*:\\)?\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat user-emacs-directory "auto-saves/" "\\3") t)))

;; show paren when cursor is on the closing one rather than behind it except insert mode
(advice-add show-paren-data-function
            :around
            (lambda (orig-fun)
              (cond ((or (equal modaled-state "insert")
                         (looking-at "\\s("))
                     (funcall orig-fun))
                    ((looking-at "\\s)")
                     (save-excursion (forward-char 1) (funcall orig-fun))))))

;;; use-package
;; don't use eval-when-compile to avoid bind-key errors
(require 'use-package)

;; Define normal states early so later substates can override them
(modaled-define-state "insert"
  :sparse t
  :no-suppress t
  :cursor-type 'bar)
(modaled-define-state "select"
  :cursor-type 'box)
(modaled-define-state "normal"
  :cursor-type 'box)
;; major-mode-specific state (keys defined in substates)
(modaled-define-state "major"
  :cursor-type 'box)

;; zoom globally instead of per buffer
(use-package default-text-scale
  :commands default-text-scale-mode
  :bind
  (:map default-text-scale-mode-map
        ("C-=" . default-text-scale-increase)
        ("C--" . default-text-scale-decrease)
        ("C-0" . default-text-scale-reset))
  :init
  (default-text-scale-mode 1)
  :config
  (setq default-text-scale-amount 12))
;; Set default font size
(set-frame-font "Monospace 12" nil t)

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

(use-package minions
  :commands minions-minor-modes-menu)

(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode))

(use-package rainbow-mode
  :commands rainbow-mode
  :demand t)
;; enable it by default
(define-globalized-minor-mode global-rainbow-mode
  rainbow-mode
  (lambda () (rainbow-mode 1))
  :group 'rainbow)
(global-rainbow-mode 1)

(use-package hl-todo
  :commands global-hl-todo-mode
  :init
  (setq hl-todo-keyword-faces
        '(("TODO" . "red")
          ("FIXME" . "red")
          ("BUG" . "red")
          ("WAITING" . "violet")
          ("HACK" . "dark orange")))
  (global-hl-todo-mode))

(use-package dashboard
  :commands dashboard-setup-startup-hook
  :init
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5)))
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; show info about the packages loaded and the init time
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

(use-package smartparens
  :demand t
  :hook
  ((html-mode tsx-ts-mode) . (lambda () (require 'smartparens-html)))
  (rust-ts-mode . (lambda () (require 'smartparens-rust)))
  (python-ts-mode . (lambda () (require 'smartparens-python)))
  (go-ts-mode . (lambda () (require 'smartparens-go)))
  ((c-ts-mode c++-ts-mode) . (lambda () (require 'smartparens-c)))
  ((js-ts-mode typescript-ts-mode) . (lambda () (require 'smartparens-javascript)))
  (latex-mode . (lambda () (require 'smartparens-latex)))
  (org-mode . (lambda () (require 'smartparens-org)))
  :commands (smartparens-global-mode
             sp-get-enclosing-sexp
             sp-beginning-of-sexp
             sp-end-of-sexp)
  :config
  (smartparens-global-mode))

(use-package caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode))
  :hook
  (caddyfile-mode . (lambda ()
                      ; overwrite indent settings in caddyfile-mode
                      (setq-local tab-width 2
                                  indent-tabs-mode nil))))

(use-package combobulate
  :commands (combobulate-navigate-previous
             combobulate-navigate-next
             combobulate-navigate-logical-previous
             combobulate-navigate-logical-next
             combobulate-drag-up
             combobulate-drag-down
             combobulate-navigate-up-list-maybe
             combobulate-navigate-down-list-maybe))

;; xclip (to support clipboard in terminal
(use-package xclip
  :commands (xclip-mode xclip-get-selection xclip-set-selection)
  :init
  (xclip-mode 1))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi :no-confirm))

;; tabs
(use-package centaur-tabs
  :commands (centaur-tabs-mode
             centaur-tabs-forward
             centaur-tabs-backward
             centaur-tabs-forward-group
             centaur-tabs-backward-group
             centaur-tabs-headline-match)
  :custom-face
  (centaur-tabs-active-bar-face ((t (:background "#c4569e"))))
  :init
  (setq centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-icons t
        centaur-tabs-set-close-button nil
        centaur-tabs-show-new-tab-button nil
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t)
  (centaur-tabs-mode 1))

;; Fix from https://github.com/ema2159/centaur-tabs/issues/127#issuecomment-1126913492
(defun fix-centaur-tabs ()
  "Fix tabs in terminal."
  (centaur-tabs-mode -1)
  (centaur-tabs-mode)
  (centaur-tabs-headline-match))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (fix-centaur-tabs)))
              (fix-centaur-tabs)))

;; mode line
(use-package nerd-icons)
(use-package doom-modeline
  :commands (doom-modeline-mode
             doom-modeline-def-modeline
             doom-modeline-set-modeline
             doom-modeline-def-segment)
  :demand t
  :config
  (doom-modeline-def-segment modaled
    "Display modaled states."
    (when (bound-and-true-p modaled-state)
      (propertize
        (cond ((equal modaled-state "normal") "N")
              ((equal modaled-state "insert") "I")
              ((equal modaled-state "select") "S")
              ((equal modaled-state "major") "M"))
        'face (cons 'bold t))))

  (doom-modeline-def-modeline 'my-modeline
    '(bar workspace-name window-number modaled matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-modeline 'default)))
  (doom-modeline-mode 1))

(use-package flycheck
  :commands (global-flycheck-mode
             flycheck-checker-get
             flycheck-get-checker-for-buffer
             flycheck-define-checker
             flycheck-define-command-checker
             flycheck-define-error-level
             flycheck-add-next-checker)
  :custom-face
  (flycheck-debug ((t (:underline t))))
  :init
  (setf (get 'flycheck-debug-overlay 'face) 'flycheck-debug)
  ;; priority lower than info level
  (setf (get 'flycheck-debug-overlay 'priority) 80)
  (flycheck-define-error-level 'debug
    :overlay-category 'flycheck-debug-overlay
    :severity -20)
  (setq flycheck-navigation-minimum-level 'info)
  (flycheck-define-checker cspell
    "Cspell checker."
    :command ("cspell" "lint" source-inplace
              "--no-color"
              "--no-progress"
              "--no-summary")
    :error-patterns
    ((debug line-start (file-name) ":" line ":" column " - " (message) line-end))
    :modes (text-mode
            emacs-lisp-mode
            haskell-mode
            markdown-mode
            org-mode
            c-ts-mode
            c++-ts-mode
            rust-ts-mode
            go-ts-mode
            python-ts-mode
            html-mode
            js-ts-mode
            typescript-ts-mode
            tsx-ts-mode
            bash-ts-mode
            css-ts-mode
            json-ts-mode
            toml-ts-mode
            yaml-ts-mode
            dockerfile-ts-mode
            nix-mode
            latex-mode))
  ;; add to the last one in case of conflicts
  (add-to-list 'flycheck-checkers 'cspell t)
  ;; enable it along with existing checker
  ;; emacs-lisp -> emacs-lisp-checkdoc -> cspell
  (flycheck-add-next-checker 'emacs-lisp-checkdoc 'cspell)
  :config
  (global-flycheck-mode))

;; load the package manually when necessary
;; (use-package flycheck-package)

;; make hl-line more distinguishable (for dired)
(use-package hl-line
  :custom-face
  (hl-line ((t (:background "#4a4a4a")))))

;;; projectile
(use-package projectile
  :commands (projectile-mode projectile-find-file)
  :init
  (projectile-mode 1))

;;; dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package dired
  :commands (dired-up-directory
             dired-find-file
             dired-find-file-other-window
             dired-mark
             dired-toggle-read-only)
  :hook
  (dired-mode . dired-hide-details-mode))
;; dired keybindings for all states
(modaled-define-substate "dired" :no-suppress t)
(modaled-define-keys
  :substates '("dired")
  :bind
  ;; FIXME: allow exiting
  `(("'r" . ("toggle editing mode" . dired-toggle-read-only))
    ("'d" . ("toggle details" . dired-hide-details-mode))))
;; dired specific keybindings for major state
(modaled-define-substate "dired-major" :no-suppress t)
(modaled-define-keys
  :substates '("dired-major")
  :bind
  `(("l" . ("up" . previous-line))
    ("k" . ("down" . next-line))
    ("j" . ("go to parent" . dired-up-directory))
    (";" . ("open" . dired-find-file))))
  ;; ("'q" "cancel edit" nil (call-interactively #'dired-toggle-read-only))
  ;; ("'x" "apply edit" nil (call-interactively #'dired-toggle-read-only)))
;; enable dired substate only for dired mode  & not insert state
(modaled-enable-substate-on-state-change
  "dired"
  :states '("major" "normal")
  :major '(dired-mode wdired-mode))
(modaled-enable-substate-on-state-change
  "dired-major"
  :states '("major")
  :major '(dired-mode))
(defun modaled-dired-update ()
  "Update dired-major settings like curosr and hl-line when state changed."
  (let ((enabled modaled-dired-major-substate-mode))
    (setq cursor-type (if enabled nil 'box))
    (hl-line-mode (if enabled 1 -1))))
(add-hook 'modaled-dired-major-substate-mode-hook
          #'modaled-dired-update)
;; The above hook won't run when changing to wdired-mode
;; Must add the following hook as well
(add-hook 'modaled-dired-substate-mode-hook
          #'modaled-dired-update)

;;; treemacs
(use-package treemacs
  :commands (treemacs treemacs-root-up treemacs-root-down treemacs-load-theme
                      treemacs-RET-action treemacs-add-and-display-current-project))
;; use icon fonts to enable it in terminal
(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
;; treemacs specific keybindings
(modaled-define-substate "treemacs")
(modaled-define-keys
  :substates '("treemacs")
  :bind
  `(("<" . ("root up" . treemacs-root-up))
    (">" . ("root down" . treemacs-root-down))
    ("o" . ("open/close" . treemacs-RET-action))
    ("." . ("reset root" . treemacs-add-and-display-current-project))))
(modaled-enable-substate-on-state-change
  "treemacs"
  :states '("normal" "select")
  :major '(treemacs-mode))

(use-package which-key
  :commands which-key-mode
  :init
  (setq-default which-key-idle-delay 0.01)  ; show desc immediately
  ;; call mode in :init to enable it immediately
  (which-key-mode))

;;; Completion UI in  minibuffer
(use-package vertico
  :commands vertico-mode
  ;; Tidy shadowed file names in find-file
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :init
  ;; call mode in :init to enable it immediately
  (vertico-mode))

;;; Enable rich annotations in minibuffer completion
(use-package marginalia
  :commands marginalia-mode
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be actived in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;;; Fuzzy completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; icons for completion UI in minibuffer
(use-package nerd-icons-completion
  :commands nerd-icons-completion-mode
  :init
  (nerd-icons-completion-mode))

;;; company completion
(use-package company
  :commands (company-manual-begin company-abort)
  :hook (after-init  . global-company-mode)
  :bind (:map company-active-map
              ;; complete selection using tab
              ;; (must use TAB for terminal and <tab> for gui)
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection)
              ("M-TAB" . company-complete-common)
              ("M-<tab>" . company-complete-common)
              ;; unbind following keys to make the default keybindings work
              ("RET" . nil)
              ("<return>" . nil)
              ("C-w" . nil)
              ("<escape>" . (lambda () (interactive) (company-abort) (modaled-set-main-state))))
  :init
  ;; complete on 2 chars instead of 3
  (setq-default company-minimum-prefix-length 2
                ;; prevent completing with wrong cases
                company-dabbrev-downcase nil
                company-dabbrev-ignore-case nil))

;;; eglot LSP client
(use-package eglot
  :commands (eglot-rename
             eglot-format
             eglot-format-buffer
             eglot-code-actions
             eglot-code-action-quickfix)
  :hook
  ((haskell-mode
    c-ts-mode
    c++-ts-mode
    rust-ts-mode
    go-ts-mode
    python-ts-mode
    html-mode
    js-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    bash-ts-mode
    css-ts-mode
    json-ts-mode
    toml-ts-mode
    yaml-ts-mode
    dockerfile-ts-mode
    nix-mode
    latex-mode) . eglot-ensure)
  :init
  ; disable event buffer (hangs frequently in js/ts)
  (setq eglot-events-buffer-size 0))

;; Manage popup window
(use-package popwin
  :commands (popwin-mode
             popwin:close-popup-window)
  :init
  (popwin-mode 1)
  :config
  ;; make help window stick around
  (add-to-list 'popwin:special-display-config '(help-mode :stick t))
  (add-to-list 'popwin:special-display-config '("\\*eldoc.*\\*" :regexp t :noselect t)))

(use-package sideline
  :hook
  ((flycheck-mode
    flymake-mode) . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake sideline-flycheck)))

(use-package sideline-flycheck
  :hook (flycheck-mode . sideline-flycheck-setup))


;; (use-package lsp-mode
;;   :commands (lsp lsp-rename)
;;   ;; make sure the language server is available for each mode
;;   :hook
;;   ((c-ts-mode
;;     c++-ts-mode
;;     rust-ts-mode
;;     go-ts-mode
;;     python-ts-mode
;;     html-mode
;;     js-ts-mode
;;     typescript-ts-mode
;;     tsx-ts-mode
;;     bash-ts-mode
;;     css-ts-mode
;;     json-ts-mode
;;     toml-ts-mode
;;     yaml-ts-mode
;;     dockerfile-ts-mode
;;     nix-mode
;;     latex-mode) . lsp)
;;   (lsp-after-initialize . (lambda ()
;;                             ;; run cspell along with lsp checker
;;                             (flycheck-add-next-checker 'lsp 'cspell)))
;;   :init
;;   (setq-default lsp-auto-guess-root t)
;;   (setq-default lsp-enable-suggest-server-download nil)
;;   ;; disable version log files for ts-server
;;   (setq-default lsp-javascript-preferences-import-module-specifier "relative")
;;   (setq-default lsp-typescript-preferences-import-module-specifier "relative")
;;   (setq-default lsp-clients-typescript-log-verbosity "off")
;;   (setq-default lsp-clients-typescript-preferences
;;                 ;; use js ending for ESM import in TS
;;                 (list :importModuleSpecifierEnding "js"))
;;   ;; to improve lsp-mode performance
;;   (setq-default gc-cons-threshold 100000000)
;;   (setq-default read-process-output-max (* 1024 1024)))

;; (use-package lsp-ui
;;   :commands (lsp-ui-doc-glance)
;;   :init
;;   (setq-default lsp-ui-doc-position 'at-point))

;; gtd
(defvar gtd-directory "~/.config/gtd/")
(unless (file-exists-p gtd-directory)
  (mkdir gtd-directory t))
(defun gtd-file (file)
  "Get path of a FILE in gtd."
  (concat gtd-directory file))
(defun gtd-save ()
  "Save all buffers in gtd."
  (interactive)
  (save-some-buffers t (lambda ()
                         (file-in-directory-p buffer-file-name gtd-directory))))
(defun gtd-action (action)
  "Return a fn to perform a gtd ACTION and save buffers."
  (lambda ()
    (interactive)
    (call-interactively action)
    (gtd-save)))

;; org-mode
(use-package org
  :commands (org-todo
             org-entry-get
             org-set-property
             org-refile
             org-priority
             org-insert-structure-template
             org-edit-special
             org-open-at-point
             org-ctrl-c-ctrl-c
             org-export-dispatch
             org-id-get-create
             org-cycle
             org-previous-visible-heading
             org-next-visible-heading
             org-forward-heading-same-level
             org-backward-heading-same-level)
  :hook
  (org-mode . org-indent-mode)
  :custom
  (org-babel-python-mode 'python-ts-mode)
  :custom-face
  (org-headline-todo ((t (:foreground "#66acda"))))
  (org-headline-done ((t (:foreground "dark gray"))))
  :config
  ;; tree-sitter for org src buffer (map LANG to mode)
  (add-to-list 'org-src-lang-modes '("c" . c-ts))
  (add-to-list 'org-src-lang-modes '("c++" . c++-ts))
  (add-to-list 'org-src-lang-modes '("rust" . rust-ts))
  (add-to-list 'org-src-lang-modes '("go" . go-ts))
  (add-to-list 'org-src-lang-modes '("python" . python-ts))
  (add-to-list 'org-src-lang-modes '("js" . js-ts))
  (add-to-list 'org-src-lang-modes '("ts" . typescript-ts))
  (add-to-list 'org-src-lang-modes '("tsx" . tsx-ts))
  (add-to-list 'org-src-lang-modes '("bash" . bash-ts))
  (add-to-list 'org-src-lang-modes '("zsh" . bash-ts))
  (add-to-list 'org-src-lang-modes '("sh" . bash-ts))
  (add-to-list 'org-src-lang-modes '("css" . css-ts))
  (add-to-list 'org-src-lang-modes '("json" . json-ts))
  (add-to-list 'org-src-lang-modes '("toml" . toml-ts))
  (add-to-list 'org-src-lang-modes '("yaml" . yaml-ts))
  (add-to-list 'org-src-lang-modes '("dockerfile" . dockerfile-ts))
  ;; enable babel execution for languages in code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp t)
     (C . t)  ; for both C and C++
     (js . t)
     (dot . t)
     (haskell . t)
     (python . t)))

  ;; GTD in org-mode
  (setq org-capture-templates
        `(("ti" "gtd inbox" entry
           (file ,(gtd-file "inbox.org"))
           "* TODO %i%?")
          ("ta" "gtd archives" entry
           (file ,(gtd-file "archives.org"))
           "* DONE %i%?")))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("SOMEDAY" . "light sky blue")
          ("WAITING" . "violet")
          ("DONE" . "dark gray")
          ("CANCELLED" . "dark gray")))
  ;; apply faces to todo/done headline
  (setq org-fontify-done-headline t
        org-fontify-todo-headline t)
  (setq org-refile-targets
        `((,(gtd-file "actions.org") :maxlevel . 2)
          (,(gtd-file "archives.org") :maxlevel . 2)))
  ;; save buffer after capture or refile
  (defun save-after-capture-refile ()
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-buffer)))
  (advice-add 'org-capture-refile :after 'save-after-capture-refile))

(defun org-open (loc)
  "Open link at point based on LOC.
LOC can be `current' or `other'."
  (let* ((fn (cond
              ((eq loc 'current) #'find-file)
              ((eq loc 'other) #'find-file-other-window)
              (t (error "Wrong location"))))
         (setup-copy (copy-alist org-link-frame-setup))
         (org-link-frame-setup (push `(file . ,fn) setup-copy)))
    (org-open-at-point)))

;; template completion by inserting <KEY TAB
(use-package org-tempo)
;; support exporting to markdown
(use-package ox-md)
;; for CV export
(use-package org-moderncv)

(defvar note-directory
  "~/.config/notes/data"
  "Personal note directory.")

(use-package org-roam
  :commands (org-capture-kill
             org-capture-finalize
             org-roam-db-autosync-mode
             org-roam-node-find
             org-roam-graph
             org-roam-node-insert
             org-roam-capture
             org-roam-buffer-toggle)
  :custom
  (make-directory note-directory t)
  (org-roam-directory (file-truename note-directory))
  (org-roam-node-display-template
   (concat "${title} "
           "${tags}"))
  (org-roam-capture-templates
   `(("d" "default" plain "%?"
      :unnarrowed t
      :target (file+head
               "${slug}.org"
               ,(concat "#+title: ${title}\n")))))
  :config
  ; for cache consistency
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-agenda
  :commands
  (org-agenda-redo
   org-agenda-todo
   org-agenda-priority
   org-agenda-refile)
  :config
  (setf (cdr (assoc 'todo org-agenda-prefix-format)) " %i %-16:c ")
  (setq org-agenda-files
        `(,(gtd-file "inbox.org")
          ,(gtd-file "actions.org"))))

(use-package org-super-agenda
  :commands org-super-agenda-mode
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        ;; only passed to next group if previous doesn't match
        '((:name "Inbox" :file-path "inbox\\.org\\'")
          (:todo "TODO")
          (:todo "WAITING")
          (:todo "SOMEDAY")))
  ;; disable the keymap on header
  (setq org-super-agenda-header-map (make-sparse-keymap)))

(defun org-insert-category ()
  "Insert category property with current item."
  (interactive)
  (let ((item (org-entry-get nil "ITEM")))
    (org-set-property "CATEGORY" item)))

;; org-mode specific keybindings
(modaled-define-substate "org")
(modaled-define-keys
  :substates '("org")
  :bind
  `(("'o" . ("org open (other window)" . ,(hx :eval (org-open 'other))))
    (([return] ,(kbd "RET")) . ("org open" . ,(hx :eval (org-open 'current))))
    ("'w" . ("org edit" . org-edit-special))
    ("'e" . ("org export" . org-export-dispatch))
    ("'it" . ("org insert template" . ,(hx :eval
                                         (modaled-set-state "insert")
                                         org-insert-structure-template)))
    ("'ii" . ("org insert id" . org-id-get-create))
    ("'ic" . ("org insert category" . org-insert-category))
    ("'l" . ("org toggle link display" . org-toggle-link-display))
    ("'c" . ("org capture" . org-capture))
    ("'<" . ("org promote" . ,(hx :region :eval org-do-promote)))
    ("'>" . ("org demote" . ,(hx :region :eval org-do-demote)))
    ("'J" . ("org promote subtree" . ,(hx :region :eval org-promote-subtree)))
    ("':" . ("org demote subtree" . ,(hx :region :eval org-demote-subtree)))
    ("'L" . ("org promote subtree" . ,(hx :region :eval org-move-subtree-up)))
    ("'K" . ("org demote subtree" . ,(hx :region :eval org-move-subtree-down)))
    ;; org-gtd
    ("'t" . ("org todo" . ,(hx :region :eval org-todo gtd-save)))
    ("'r" . ("org refile" . ,(hx :region :eval org-refile gtd-save)))
    ("'p" . ("org priority" . ,(hx :region :eval org-priority gtd-save)))))
;; enable org substate only for org-mode & not insert state
(modaled-enable-substate-on-state-change
  "org"
  :states '("normal" "select")
  :major '(org-mode))

;; org-agenda-mode specific keybindings
(modaled-define-substate "org-agenda")
(modaled-define-keys
  :substates '("org-agenda")
  :bind
  `(("r" . ("rebuild agenda view" . ,(gtd-action #'org-agenda-redo)))
    ("'t" . ("org todo" . ,(gtd-action #'org-agenda-todo)))
    ("'p" . ("org priority" . ,(gtd-action #'org-agenda-priority)))
    ("'r" . ("org refile" . ,(gtd-action #'org-agenda-refile)))))
(modaled-enable-substate-on-state-change
  "org-agenda"
  :states '("normal" "select")
  :major '(org-agenda-mode))

;; for note searching
(use-package xeft
  :commands (xeft)
  :init
  (setq xeft-directory note-directory)
  (setq xeft-database "~/.config/emacs/xeft/db"))

;; git
(use-package diff-hl
  :commands (diff-hl-margin-mode
             diff-hl-flydiff-mode
             global-diff-hl-mode)
  :init
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  ;; use margin mode to support terminal emacs
  (diff-hl-margin-mode 1))

(use-package magit
  :commands (magit-status
             with-editor-finish
             with-editor-cancel)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package magit-todos
  :commands magit-todos-mode
  :init
  (magit-todos-mode 1))

;; magit-status-mode specific keybindings
(modaled-define-substate "magit-status")
(modaled-define-keys
  :substates '("magit-status")
  :bind
  `(("s" . ("stage" . magit-stage))
    ("S" . ("stage all" . magit-stage-modified))
    ("u" . ("unstage" . magit-unstage))
    ("U" . ("unstage all" . magit-unstage-all))
    ("P" . ("push changes" . magit-push))
    ("F" . ("pull changes" . magit-pull))
    ("d" . ("diff" . magit-diff))
    ("D" . ("discard" . magit-discard))
    ("r" . ("refresh" . magit-refresh))
    ("R" . ("refresh all" . magit-refresh-all))
    ("c" . ("commit" . magit-commit))))
(modaled-enable-substate-on-state-change
  "magit-status"
  :states '("normal" "select")
  :major '(magit-status-mode))


;; vterm (insert as default state)
(use-package vterm
  :commands (vterm-reset-cursor-point vterm--self-insert)
  :hook
  (vterm-mode . (lambda ()
                  ;; turn off trailing whitespaces highlighting
                  (setq show-trailing-whitespace nil))))
;; vterm specific keybindings
(modaled-define-substate "vterm")
; reset to the right position when entering insert mod3
(modaled-define-keys
  :substates '("vterm")
  :bind
  `((("i" "a" "I" "A") . ("insert" . ,(hx :eval hx-no-sel (modaled-set-state "insert") vterm-reset-cursor-point)))))
;; passing control keys to terminal instead of modifying buffer (buffer is read-only)
(modaled-define-substate "vterm-insert"
  :sparse t
  :no-suppress t)
(modaled-define-keys
  :substates '("vterm-insert")
  ; pass keys to terminal
  :bind
  `(((,(kbd "C-w") ,(kbd "C-u") ,(kbd "C-c")) . ("passthrough" . vterm--self-insert))))
;; enable substate only for vterm-mode & not insert state
(modaled-enable-substate-on-state-change
  "vterm"
  :states '("normal" "select")
  :major '(vterm-mode))
(modaled-enable-substate-on-state-change
  "vterm-insert"
  :states '("insert")
  :major '(vterm-mode))

(use-package vterm-toggle
  :commands vterm-toggle
  :config
  ;; show vterm buffer in bottom side
  ;; must be executed after the pkg is load to make sure vterm-buffer-name is defined
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; beancount
(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode))


;;; tree-sitter (put at the end as some packages above may change auto-mode-alist)

;; remap major mode to ts major mode
(defvar ts-mode-remap-alist
  '((c-mode . c-ts-mode)
    (c++-mode . c++-ts-mode)
    (c-or-c++-mode . c-or-c++-ts-mode)
    (javascript-mode . js-ts-mode)
    (js-mode . js-ts-mode)
    (python-mode . python-ts-mode)
    (css-mode . css-ts-mode)
    (js-json-mode . json-ts-mode)
    (conf-toml-mode . toml-ts-mode)
    (sh-mode . bash-ts-mode)))

(defun remap-ts-mode (elem)
  "Remap mode in ELEM to ts mode."
  (let* ((orig (cdr elem))
         (new (cdr (assoc orig ts-mode-remap-alist))))
    (cons (car elem)
          (or new orig))))

;; replace modes in all mode alists
;; `major-mode-remap-alist' only affects `auto-mode-alist' so not using it.
(setq-default interpreter-mode-alist
              (mapcar #'remap-ts-mode interpreter-mode-alist))
(setq-default auto-mode-alist
              (mapcar #'remap-ts-mode auto-mode-alist))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("Containerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\`go.mod\\'" . go-mode-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[cm]?js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.z?sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.[jt]sx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

;; multiple cursors
(defun hx-toggle-multiple-cursors ()
  "Toggle multiple-cursors-mode."
  (interactive)
  (if multiple-cursors-mode
      (mc/disable-multiple-cursors-mode)
    (mc/maybe-multiple-cursors-mode)))

(defun hx-toggle-cursor ()
  "Toggle a cursor at point."
  (interactive)
  (let ((cursor (mc/fake-cursor-at-point)))
    (if cursor
        (mc/remove-fake-cursor cursor)
      (mc/create-fake-cursor-at-point))))

(defun hx-add-cursor ()
  "Add a cursor at point if doesn't exist."
  (interactive)
  (let ((cursor (mc/fake-cursor-at-point)))
    (when (not cursor)
      (mc/create-fake-cursor-at-point))))

(defun hx-remove-cursors ()
  "Remove all extra cursors.
Should be called only before entering multiple-cursors-mode."
  (interactive)
  (if multiple-cursors-mode
      (message "multiple-cursors-mode already enabled! Call hx-toggle-multiple-cursors instead")
    (mc/remove-fake-cursors)))

(use-package multiple-cursors
  :demand t
  :commands (mc/maybe-multiple-cursors-mode
             mc/disable-multiple-cursors-mode
             mc/create-fake-cursor-at-point
             mc/remove-fake-cursor
             mc/fake-cursor-at-point
             mc/remove-fake-cursors)
  :config
  ;; run it once to prevent disabling it immediately after enabling it
  (add-to-list 'mc--default-cmds-to-run-once #'hx-toggle-multiple-cursors)
  ;; need to save/restore hx--mark for each cursor
  (add-to-list 'mc/cursor-specific-vars 'hx--mark))

;; modaled keybindings
(modaled-define-keys
  :states '("normal")
  :bind
  `(("s" . ("enable SELECT state" . ,(hx :eval (modaled-set-state "select"))))
    ;; search (select after search)
    ("/" . ("search forward" . ,(hx :re-hl :eval isearch-forward-regexp backward-char (hx-set-mark isearch-other-end))))
    ("?" . ("search backward" . ,(hx :re-hl :eval isearch-backward-regexp (hx-set-mark (1- isearch-other-end)))))
    ("n" . ("next match" . ,(hx :re-hl :eval isearch-repeat-forward backward-char (hx-set-mark isearch-other-end) isearch-exit)))
    ("N" . ("prev match" . ,(hx :re-hl :eval isearch-repeat-backward (hx-set-mark (1- isearch-other-end)) isearch-exit)))
    ("*" . ("search selection" . ,(hx :eval (setq isearch-string (hx-region-string)))))
    (,(kbd "M-/") . ("search lines" . occur))))
;; Problem: all windows share the same cursor shape
;; (add-hook
;;  'modaled-normal-state-mode-hook
;;  (lambda ()
;;    ;; Set cursor shape for VT520 terminal
;;    ;; see cursor style at https://invisible-island.net/xterm/ctlseqs/ctlseqs.html for more code
;;    (unless (display-graphic-p)
;;      (send-string-to-terminal "\e[2 q"))))


(add-hook
 'modaled-select-state-mode-hook
 (lambda ()
   ;; (unless (display-graphic-p)
   ;;   (send-string-to-terminal "\e[2 q"))
   (unless (equal modaled-state "select")
     ; entering select state
     (unless hx--mark
       (hx-set-mark (point))))))
(modaled-define-keys
  :states '("select")
  :bind
  ;; state changes
  `(("s" . ("exit SELECT state" . modaled-set-main-state))))

;; Common keybindings for normal and select states
;; Note:
;; - ' is reserved for major mode specific keybindings
(modaled-define-keys
  :states '("major" "normal" "select")
  :bind
  ;; movement
  `(("j" . ("left" . ,(hx :re-sel :eval backward-char)))
    (";" . ("right" . ,(hx :re-sel :eval forward-char)))
    ("l" . ("up" . ,(hx :re-sel :eval previous-line)))
    ("k" . ("down" . ,(hx :re-sel :eval next-line)))
    ("w" . ("next word" . ,(hx :re-hl :eval (hx-next-word (equal modaled-state "normal")))))
    ("b" . ("prev word" . ,(hx :re-hl :eval (hx-previous-word (equal modaled-state "normal")))))
    ("f" . ("find next char" . ,(hx :arg "c" :re-hl :eval (hx-find-char (car args) +1 -1 (equal modaled-state "normal")))))
    ("t" . ("find till next char" . ,(hx :arg "c" :re-hl :eval (hx-find-char (car args) +1 -2 (equal modaled-state "normal")))))
    ("F" . ("find prev char" . ,(hx :arg "c" :re-hl :eval (hx-find-char (car args) -1 0 (equal modaled-state "normal")))))
    ("T" . ("find till prev char" . ,(hx :arg "c" :re-hl :eval (hx-find-char (car args) -1 1 (equal modaled-state "normal")))))
    (,(kbd "C-u") . ("scroll up" . ,(hx :re-sel :eval (funcall-interactively #'previous-line 10))))
    (,(kbd "C-d") . ("scroll down" . ,(hx :re-sel :eval (funcall-interactively #'next-line 10))))
    ;; goto mode
    ("gg" . ("go to line" . ,(hx :arg "p" :re-sel :eval (forward-line (- (car args) (line-number-at-pos))))))
    ("ge" . ("end of file" . ,(hx :re-sel :eval (goto-char (point-max)))))
    ("gj" . ("start of line" . ,(hx :re-sel :eval beginning-of-line)))
    ("g;" . ("end of line" . ,(hx :re-sel :eval end-of-line (re-search-backward ".\\|^"))))  ; move to last char if exists
    ("gn" . ("next buffer" . centaur-tabs-forward))
    ("gp" . ("prev buffer" . centaur-tabs-backward))
    ("gN" . ("next buffer" . centaur-tabs-forward-group))
    ("gP" . ("prev buffer" . centaur-tabs-backward-group))
    ("gd" . ("go to def" . ,(hx :eval (let ((xref-prompt-for-identifier nil)) (call-interactively #'xref-find-definitions)))))
    ;; match mode
    ("mm" . ("match char" . ,(hx :re-sel :eval hx-match-char)))
    ("mj" . ("beginning of current pair" . ,(hx :re-sel :eval sp-beginning-of-sexp)))
    ("m;" . ("end of current pair" . ,(hx  :re-sel :eval sp-end-of-sexp)))
    ("msa" . ("select around current pair" . ,(hx :re-hl :eval (hx-match-select :around))))
    ("msi" . ("select inside current pair" . ,(hx :re-hl :eval (hx-match-select :inside))))
    ("ma" . ("surround with pair" . ,(hx :arg "c" :re-hl :eval modaled-set-main-state (hx-match-surround-add (car args)))))
    ("mr" . ("replace surrounding pair" . ,(hx :arg "c" :re-hl :eval modaled-set-main-state (hx-match-surround-replace (car args)))))
    ;; C-i is the same as TAB for kbd and in terminal
    (,(kbd "C-i") . ("jump forward" . xref-go-forward))
    (,(kbd "C-o") . ("jump backward" . xref-go-back))
    ;; changes
    ("i" . ("insert before" . ,(hx :eval hx-no-sel (modaled-set-state "insert"))))
    ("a" . ("insert after" . ,(hx :eval hx-no-sel (modaled-set-state "insert") forward-char)))
    ("I" . ("insert at start of line" . ,(hx :eval hx-no-sel (modaled-set-state "insert") back-to-indentation)))
    ("A" . ("insert at end of line" . ,(hx :eval hx-no-sel (modaled-set-state "insert") end-of-line)))
    ("o" . ("insert below" . ,(hx :eval hx-no-sel (modaled-set-state "insert") end-of-line newline-and-indent)))
    ("O" . ("insert above" . ,(hx :eval hx-no-sel (modaled-set-state "insert") beginning-of-line newline-and-indent (forward-line -1) indent-according-to-mode)))
    ("r" . ("replace" . ,(hx :arg "c" :eval hx-no-sel modaled-set-main-state (hx-region-replace (car args)))))
    ("y" . ("copy" . ,(hx :eval modaled-set-main-state (hx-region-apply #'kill-ring-save))))
    ("d" . ("delete" . ,(hx :eval modaled-set-main-state (hx-region-apply #'delete-region) hx-no-sel)))
    ("e" . ("edit" . ,(hx :eval (modaled-set-state "insert") (hx-region-apply #'delete-region) hx-no-sel)))
    ("P" . ("paste before" . ,(hx :eval (hx-paste (current-kill 0 t) -1) hx-no-sel)))
    ("p" . ("paste after" . ,(hx :eval (hx-paste (current-kill 0 t) +1) hx-no-sel)))
    (,(kbd "M-P") . ("paste before from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") -1) hx-no-sel)))
    (,(kbd "M-p") . ("paste after from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") +1) hx-no-sel)))
    ("J" . ("join lines" . hx-join-lines))
    ("u" . ("undo" . ,(hx :eval hx-no-sel undo)))
    ("U" . ("redo" . ,(hx :eval hx-no-sel undo-redo)))
    (">" . ("indent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly 2))))
    ("<" . ("unindent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly -2))))
    ("." . ("repeat changes" . hx-repeat-change))
    ("=" . ("format" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-region))))
    (,(kbd "C-c") . ("comment/uncomment" . ,(hx :re-hl :eval (hx-extended-region-apply #'comment-or-uncomment-region))))
    ;; selection
    ("x" . ("extend line below" . ,(hx :re-hl :eval hx-extend-line-below)))
    ("X" . ("extend line" . ,(hx :re-hl :eval hx-extend-to-line-bounds)))
    ;; space mode
    (" P" . ("clipboard paste before" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) -1) hx-no-sel)))
    (" p" . ("clipboard paste after" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) +1) hx-no-sel)))
    (" y" . ("copy to clipboard" . ,(hx :eval modaled-set-main-state (xclip-set-selection 'clipboard (hx-region-string)))))
    (" f" . ("find file (projectile)" . projectile-find-file))
    (" F" . ("find file (dired)" . find-file))
    (" b" . ("switch to buffer" . switch-to-buffer))
    (" d" . ("directory tree" . treemacs))
    (" ?" . ("search symbol" . apropos))
    (" k" . ("show eldoc" . hx-show-eldoc))
    ;; view mode
    ("v" . ("toggle visibility" . hx-toggle-visibility))
    ;; multiple cursors
    (" c" . ("toggle multiple-cursors-mode" . hx-toggle-multiple-cursors))
    ("c" . ("add cursor and move next line" . ,(hx :eval hx-add-cursor next-line)))
    ("C" . ("add cursor and move prev line" . ,(hx :eval hx-add-cursor previous-line)))
    (,(kbd "M-c") . ("toggle a cursor at point" . hx-toggle-cursor))
    (,(kbd "M-C") . ("remove all cursors" . hx-remove-cursors))
    ;; gtd
    (" tl" . ("gtd list" . org-todo-list))
    (" ti" . ("gtd inbox" . ,(hx :eval (find-file (gtd-file "inbox.org")))))
    (" ta" . ("gtd actions" . ,(hx :eval (find-file (gtd-file "actions.org")))))
    (" tc" . ("gtd capture" . ,(hx :eval (org-capture nil "ti"))))
    ;; notes (org-roam & xeft)
    (" nf" . ("note find" . org-roam-node-find))
    (" ns" . ("note search" . xeft))
    (" nc" . ("note create" . org-roam-capture))
    (" ni" . ("note insert" . org-roam-node-insert))
    (" nl" . ("note backlinks" . org-roam-buffer-toggle))
    ;; LSP
    (" lr" . ("rename (LSP)" . eglot-rename))
    (" l=" . ("format (LSP)" . eglot-format-buffer))
    (" la" . ("action (LSP)" . eglot-code-actions))
    (" lf" . ("quickfix (LSP)" . eglot-code-action-quickfix))
    ;; git
    (" go" . ("open magit" . magit-status))
    (" gd" . ("show diff" . vc-diff))
    ;; unimpaired (structure moving)
    ("[[" . ("previous" . hx-previous))
    ("]]" . ("next" . hx-next))
    ("[s" . ("previous sibling" . hx-previous-sibling))
    ("]s" . ("next sibling" . hx-next-sibling))
    ("[l" . ("out of list (TS)" . combobulate-navigate-up-list-maybe))
    ("]l" . ("into list (TS)" . combobulate-navigate-down-list-maybe))
    ("[ms" . ("move sibling up (TS)" . combobulate-drag-up))
    ("]ms" . ("move sibling down (TS)" . combobulate-drag-down))
    ("[d" . ("previous diagnostic" . previous-error))
    ("]d" . ("next diagnostic" . next-error))
    ;; misc
    ("!" . ("run shell command" . shell-command))
    ("|" . ("eval expr" . eval-expression))
    ("\\" . ("eval region and print" . ,(hx :eval (hx-region-apply #'eval-region t))))
    (,(kbd "M-\\") . ("eval region" . ,(hx :eval (hx-region-apply #'eval-region))))
    (":" . ("run command" . execute-extended-command))
    ("q" . ("quit window" . quit-window))
    ("Q" . ("kill buffer" . kill-this-buffer))
    ;; major-mode specific command
    ("'x" . ,(hx :region :eval
               (let ((command (lookup-key (current-local-map) (kbd "C-c C-c"))))
                 (when command
                   (call-interactively command)))))))

(modaled-define-keys
  :states '("insert")
  :bind
  `((,(kbd "C-w") . ("delete word backward" . hx-delete-word-backward))
    (,(kbd "M-i") . ("code suggestion (LSP)" . company-manual-begin))
    ;; set tempo-match-finder temporarily to prevent conflicts
    (,(kbd "M-t") . ("tempo complete" . ,(hx :eval (let ((tempo-match-finder hx-tempo-match-finder))
                                                     (tempo-complete-tag)))))
    ;; this makes pasting work in GUI
    (,(kbd "C-V") . ("paste from clipboard" . ,(hx :eval (insert (xclip-get-selection 'clipboard)))))))
;; (add-hook
;;  'modaled-insert-state-mode-hook
;;  (lambda ()
;;    ;; box cursor shape for VTE-based terminal
;;    (unless (display-graphic-p)
;;      (send-string-to-terminal "\e[6 q"))))

;; major-mode specific keys
(modaled-define-keys
  :states '("major")
  :bind
  `(("i" . ("NORMAL state" . ,(hx :eval (modaled-set-state "normal"))))))

(defun hx-save ()
  "Save buffer or finish editing."
  (interactive)
  (call-interactively
   (cond
    ((bound-and-true-p org-capture-mode) #'org-capture-finalize)
    ((or (bound-and-true-p git-commit-mode)
         (eq major-mode 'git-rebase-mode))
     #'with-editor-finish)
    (t #'save-buffer))))

(defun hx-abort ()
  "Abort editing."
  (interactive)
  (call-interactively
   (cond
    ((bound-and-true-p org-capture-mode) #'org-capture-kill)
    ((or (bound-and-true-p git-commit-mode)
         (eq major-mode 'git-rebase-mode))
     #'with-editor-cancel)
    (t #'ignore))))

;; common keybindings for all states
(modaled-define-keys
  :states '("major" "normal" "select" "insert")
  :bind
  `(([escape] . ("main state" . ,(hx :eval modaled-set-main-state hx-format-blank-line hx-no-sel)))
    (,(kbd "M-`") . ("toggle vterm" . vterm-toggle))
    (,(kbd "M-h") . ("split horizontally" . ,(hx :eval split-window-horizontally other-window)))
    (,(kbd "M-v") . ("split vertically" . ,(hx :eval split-window-vertically other-window)))
    (,(kbd "M-q") . ("delete window" . delete-window))
    (,(kbd "M-j") . ("left window" . windmove-left))
    (,(kbd "M-;") . ("right window" . windmove-right))
    (,(kbd "M-l") . ("up window" . windmove-up))
    (,(kbd "M-k") . ("down window" . windmove-down))
    (,(kbd "C-s") . ("save" . hx-save))
    (,(kbd "C-a") . ("abort" . hx-abort))
    (,(kbd "C-q") . ("quit" . save-buffers-kill-terminal))))

(modaled-define-keys
  :global t
  :bind
  `(([escape] . ("keyboard quit" . keyboard-escape-quit))
    ;; only works in GUI as C-= and C-- are managed by terminal emulator otherwise
    (,(kbd "C-=") . ("scale increase" . text-scale-increase))
    (,(kbd "C--") . ("scale decrease" . text-scale-decrease))
    ;; unset C-u for it to be used in vterm
    (,(kbd "C-u") . nil)))

(modaled-define-default-state
  '("major" dired-mode)
  '("insert" vterm-mode xeft-mode)
  '("normal"))

(setq modaled-main-state-alist
      '(((vterm-mode xeft-mode) . "normal")))

;; translate terminal \\e to [escape]
(hx-esc-mode 1)

