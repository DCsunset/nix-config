;;; org-mode config

;; template completion by inserting <KEY TAB
(use-package org-tempo
  :after org)
;; support exporting to markdown
(use-package ox-md
  :after org)
;; for CV export
(use-package org-moderncv
  :after org)


;;; gtd

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
  (org-capture-mode . modaled-set-insert-state)
  ;; don't pair angle bracket (used in tempo) in electric pair mode
  (org-mode . (lambda ()
                (setq-local
                 electric-pair-inhibit-predicate
                 `(lambda (c)
                    (if (char-equal c ?<)
                        t
                      ;; use the original pred
                      (,electric-pair-inhibit-predicate c))))))
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

(use-package org-agenda
  :commands (org-agenda-redo
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


;;; notes

(defvar note-directory "~/.config/notes/data")
(unless (file-exists-p note-directory)
  (mkdir note-directory t))

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

;; for note searching
(use-package xeft
  :commands (xeft)
  :init
  (setq xeft-directory note-directory)
  (setq xeft-database "~/.config/emacs/xeft/db"))



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
  `(("r" . ("rebuild agenda view" . org-agenda-redo))
    ("'t" . ("org todo" . ,(hx :eval org-agenda-todo gtd-save)))
    ("'p" . ("org priority" . ,(hx :eval org-agenda-priority gtd-save)))
    ("'r" . ("org refile" . ,(hx :eval org-agenda-refile gtd-save)))))
(modaled-enable-substate-on-state-change
  "org-agenda"
  :states '("normal" "select")
  :major '(org-agenda-mode))
