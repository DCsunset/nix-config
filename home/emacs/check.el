;;; Syntax checking

(use-package flymake-cspell
  :commands flymake-cspell-setup)

(use-package flymake
  :commands flymake-mode-on)

(use-package sideline
  :hook
  (flymake-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake)))

(define-globalized-minor-mode global-flymake-mode flymake-mode
  (lambda ()
    (flymake-cspell-setup)
    (flymake-mode-on)))

;; start on init
(add-hook 'after-init-hook #'global-flymake-mode)

