;; dir related config

(setq-default delete-by-moving-to-trash t)

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
             '("png" "jpe?g" "svg" "gif" "tif"))
           "imv"
           (file))
          (,(openwith-make-extension-regexp
             '("mp3" "flac" "aac" "wav"
               "mp4" "mkv" "mpe?g" "flv" "avi" "wmv"))
           "mpv"
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
(use-package dired
  :commands (dired-up-directory
             dired-find-file
             dired-find-file-other-window
             dired-mark
             dired-toggle-read-only)
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-omit-mode)
  :init
  ;; hide files starting with dot
  (setq dired-omit-files "\\`[.]")
  (setq dired-dwim-target t))

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

;; dired mode
(modaled-define-substate "dired")
(modaled-define-keys
  :substates '("dired")
  :bind
  `(("'w" . ("toggle wdired (read-only) mode" . dired-toggle-read-only))
    ("'i" . ("toggle details" . dired-hide-details-mode))
    ("'h" . ("toggle hidden files" . dired-omit-mode))
    ("'m" . ("mark" . ,(hx :region :eval dired-mark)))
    (,(kbd "' M-m") . ("mark by regexp" . dired-mark-files-regexp))
    ("'M" . ("toggle all marks" . dired-toggle-marks))
    ("'u" . ("unmark" . ,(hx :region :eval dired-unmark)))
    ("'U" . ("unmark all" . dired-unmark-all-marks))
    ("'d" . ("delete" . dired-do-delete))
    ("'D" . ("delete permanently" . ,(hx :let (delete-by-moving-to-trash nil) :eval dired-do-delete)))
    ("'k" . ("kill (hide)" . dired-do-kill-lines))
    ("'y" . ("copy" . dired-do-copy))
    ("'r" . ("rename" . dired-do-rename))
    ("'nf" . ("new file" . dired-create-empty-file))
    ("'nd" . ("new dir" . dired-create-directory))
    ;; run ! or & to open them separately
    ("'o" . ("open (in one command)" . dired-open-marked))
    ("'cm" . ("chmod" . dired-do-chmod))
    ("'co" . ("chown" . dired-do-chown))
    ("'cg" . ("chgrp" . dired-do-chgrp))
    ("'ct" . ("touch" . dired-do-touch))
    ;; DEL is backspace
    (("<" ,(kbd "DEL")) . ("go to parent" . dired-up-directory))
    ;; TAB is also supported in `hx-toggle-visibility'
    ((">" ,(kbd "RET")) . ("open" . dired-find-file))
    (,(kbd "M-RET") . ("open (other window)" . dired-find-file-other-window))))
(modaled-enable-substate-on-state-change
  "dired"
  :states '("normal" "select")
  :major '(dired-mode wdired-mode dired-sidebar-mode))

(defun dired-highlight ()
  "Highlight line for Dired."
  (let ((enabled (and (eq major-mode 'dired-mode)
                      modaled-dired-substate-mode)))
    (hl-line-mode (if enabled 1 -1))))

(add-hook 'modaled-dired-substate-mode-hook
          #'dired-highlight)

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
