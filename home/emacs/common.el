;;; Common packages and functions

;;; General config

(setq-default delete-by-moving-to-trash t)

;; UI config
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode 1)
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

(use-package dash)

(use-package nerd-icons
  :command (nerd-icons-codicon
            nerd-icons-faicon
            nerd-icons-octicon
            nerd-icons-devicon))

(use-package modaled
  :commands (modaled-set-state
             modaled-define-keys
             modaled-define-state-keys
             modaled-set-default-state
             modaled-set-main-state
             modaled-get-main-state
             modaled-define-substate-keys
             modaled-get-substate-mode
             modaled-define-default-state
             modaled-enable-substate-on-state-change))

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

