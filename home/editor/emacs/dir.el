;; dir related config

(setq-default delete-by-moving-to-trash t)

;; direnv
(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package openwith
  :commands (openwith-make-extension-regexp
            openwith-mode)
  :hook
  (after-init . openwith-mode)
  :config
  (setq openwith-associations
        `((,(openwith-make-extension-regexp
            '("pdf" "dvi"))
          "evince"
          (file))
          (,(openwith-make-extension-regexp
             '("docx?" "xlsx?" "pptx?"
               "odt" "ods" "odp" "odg" "odf"))
           "libreoffice"
           (file))
          (,(openwith-make-extension-regexp
             '("png" "jpe?g" "gif" "tif"))
           "imv"
           (file))
          (,(openwith-make-extension-regexp
             '("mp3" "flac" "aac" "wav"
               "mp4" "mkv" "mpe?g" "flv" "avi" "wmv"))
           "mpv --force-window=immediate"
           (file))))
  (setq large-file-warning-threshold nil))

;; make hl-line more distinguishable (for dired)
(use-package hl-line
  :custom-face
  (hl-line ((t (:background "#4a4a4a")))))

;;; projectile
(use-package projectile
  :commands (projectile-mode
             projectile-find-file
             projectile-ripgrep)
  :init
  (projectile-mode 1))

;;; for projectile-ripgrep
(use-package rg)

;;; dired
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package dired-du
  :custom
  (dired-du-size-format t))

(defvar dired-hide-details-init t
  "Initial state of `dired-hide-details-mode'.")

(defun dired-toggle-hide-details-mode ()
  "Toggle dired-hide-details to all and future dired buffers."
  (interactive)
  (let ((state (not dired-hide-details-init)))
    (setq dired-hide-details-init state)
	  (dolist (buf (buffer-list))
	    (with-current-buffer buf
        (when (memq major-mode '(dired-mode wdired-mode))
          (dired-hide-details-mode (if state 1 -1)))))))

(use-package dired
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode (if dired-hide-details-init 1 -1))))
  (dired-mode . dired-omit-mode)
  :custom
  (dired-listing-switches "-ahl")
  ;; hide files starting with dot
  (dired-omit-files "\\`[.]")
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-kill-when-opening-new-dired-buffer t))

(defun dired-open-marked ()
  "Open all marked files with one command asynchronously."
  (interactive)
  (when-let* ((files (dired-get-marked-files))
              (cmd (or (cadr (assoc (car files)
                                    openwith-associations
                                    (lambda (regex file) (string-match regex file))))
                       (read-from-minibuffer "Open with one command: "))))
    (start-process-shell-command
     "dired-open-marked" nil
     (concat
      "exec nohup " cmd " "
      (mapconcat 'shell-quote-argument files " ")))))

;; dired mode (used with major state)
(modaled-define-substate "dired")
(modaled-define-keys
  :substates '("dired")
  :inherit
  `((modaled-normal-state-keymap . ("l" "k" "L" "K" "C-u" "C-d" "g" "|" "/" "n" "N")))
  :bind
  `((("j" "<left>") . ("up dir" . ,(hx :eval (call-interactively
                                                    (if (eq major-mode 'dired-mode)
                                                        #'dired-up-directory
                                                      #'dired-subtree-up)))))
    ((";" "<right>") . ("down dir" . ,(hx :eval (call-interactively
                                                    (if (eq major-mode 'dired-mode)
                                                        #'dired-find-file
                                                      (unless (dired-subtree--is-expanded-p)
                                                        (dired-sidebar-subtree-toggle))
                                                      #'dired-subtree-down)))))
    ;; TAB is also supported in `hx-toggle-visibility'
    ("RET" . ("open" . dired-find-file))
    ("i" . ("toggle details" . dired-toggle-hide-details-mode))
    ("I" . ("enable dired-du-mode" . dired-du-mode))
    ("h" . ("toggle hidden files" . dired-omit-mode))
    ("m" . ("mark" . ,(hx :region :eval dired-mark)))
    ("M-m" . ("mark by regexp" . dired-mark-files-regexp))
    ("M" . ("toggle all marks" . dired-toggle-marks))
    ("u" . ("unmark" . ,(hx :region :eval dired-unmark)))
    ("U" . ("unmark all" . dired-unmark-all-marks))
    ("d" . ("delete" . ,(hx :let (dired-deletion-confirmer #'y-or-n-p) :eval dired-do-delete)))
    ("D" . ("delete permanently" . ,(hx :let (delete-by-moving-to-trash nil) :eval dired-do-delete)))
    ("H" . ("kill (hide)" . dired-do-kill-lines))
    ("y" . ("copy" . dired-do-copy))
    ("r" . ("rename" . dired-do-rename))
    ("R" . ("replace" . dired-do-find-regexp-and-replace))
    ("c f" . ("create file" . dired-create-empty-file))
    ("c d" . ("create dir" . dired-create-directory))
    ("C m" . ("chmod" . dired-do-chmod))
    ("C o" . ("chown" . dired-do-chown))
    ("C g" . ("chgrp" . dired-do-chgrp))
    ("C t" . ("touch" . dired-do-touch))
    ;; run ! or & to open them separately
    ("o" . ("open (in one command)" . dired-open-marked))
    ;; use C-s or C-a to exit wdired mode
    ("' w" . ("enable wdired mode" . dired-toggle-read-only))
    ("M-RET" . ("open (other window)" . dired-find-file-other-window))))
(modaled-enable-substate-on-state-change
  "dired"
  :states '("major")
  :major '(dired-mode dired-sidebar-mode))

(defun dired-highlight ()
  "Highlight line for Dired."
  (let ((enabled (and (eq major-mode 'dired-mode)
                      modaled-dired-substate-mode)))
    (hl-line-mode (if enabled 1 -1))))

;; The major mode hook won't run when changing from wdired-mode to dired-mode
;; Must use minor mode hook (with modaled-initialize)
(add-hook 'modaled-dired-substate-mode-hook #'dired-highlight)

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar
             dired-sidebar-subtree-toggle)
  :config
  (setq dired-sidebar-theme 'nerd))

(use-package consult
  :commands (consult-ripgrep
             consult-fd
             consult-goto-line
             consult-line
             consult-imenu
             consult-buffer))
