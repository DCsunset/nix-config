;;; Syntax checking

(use-package flymake-cspell
  :commands flymake-cspell-setup
  :init
  (setq flymake-cspell-diagnostic-type :note))

(use-package flymake
  :commands flymake-mode-on)

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
             'special-mode)
      (flymake-cspell-setup)
      (flymake-mode-on))))

;; start on init
(add-hook 'after-init-hook #'global-flymake-mode)

