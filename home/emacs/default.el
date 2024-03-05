;;/co -*- lexical-binding: t; -*-

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
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ;; refresh magit after saving buffer
  (after-save . magit-after-save-refresh-status))

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
