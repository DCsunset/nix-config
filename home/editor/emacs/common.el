;;; Common packages and functions

;;; General config

;; UI config
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(electric-pair-mode 1)  ; auto complete pair
(setq-default visible-cursor nil ; for non-blinking cursor in console
              help-window-select t  ; always select help window to close it easily
              echo-keystrokes 0.01)  ; show presses keys immediately
(global-display-line-numbers-mode)
(setq ring-bell-function #'ignore)  ; disable bell

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

(use-package gnutls
  :config
  ;; trust lan cert for services like irc
  (add-to-list 'gnutls-trustfiles "/run/secrets/user/local-service/caddy/pki/lan/root.crt"))

(use-package kkp
  :config
  (global-kkp-mode +1)
  ;; translate some keys as kkp will overwrite them to another key in terminal
  (keymap-set local-function-key-map "M-<return>" "M-RET"))

(use-package dash)

(use-package nerd-icons)

(use-package modaled)

(defun modaled-set-insert-state ()
  "Set insert modaled state."
  (interactive)
  (modaled-set-state "insert"))

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


;; Common functions
(defun read-file (file)
  "Read contents of FILE and return as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

