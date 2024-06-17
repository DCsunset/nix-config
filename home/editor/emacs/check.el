;;; Syntax checking

(use-package flymake-cspell
  :commands (flymake-cspell-setup
             flymake-cspell-set-language-ids)
  :init
  (setq flymake-cspell-diagnostic-type :note)
  :config
  (flymake-cspell-set-language-ids
   ("elisp" emacs-lisp-mode)
   ("org" org-mode)))

(use-package flymake
  :commands flymake-mode)

(use-package sideline
  :hook
  (flymake-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake)))

(define-globalized-minor-mode global-flymake-mode flymake-mode
  (lambda ()
    (unless (derived-mode-p
             'minibuffer-mode
             'vterm-mode
             'special-mode
             'org-agenda-mode)
      (flymake-cspell-setup)
      (flymake-mode 1))))

;; start on init
(add-hook 'after-init-hook #'global-flymake-mode)

