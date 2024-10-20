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

;; for CJK font alignment in table
(use-package valign
  :hook
  (org-mode . valign-mode))

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
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda ()
                ;; turn off trailing whitespaces highlighting
                (setq show-trailing-whitespace nil)
                ;; don't pair angle bracket (used in tempo) in electric pair mode
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<)
                                   t
                                 ;; use the original pred
                                 (,electric-pair-inhibit-predicate c))))))
  (org-capture-mode . modaled-set-insert-state)
  :custom
  (org-babel-python-mode 'python-ts-mode)
  ;; don't truncate lines (wrap lines instead)
  (org-startup-truncated nil)
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
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "SOMEDAY(s)" "DOING(D)" "PLANNED(p)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("SOMEDAY" . "light sky blue")
          ("PLANNED" . "RoyalBlue1")
          ("WAITING" . "orchid")
          ("DOING" . "sandy brown")
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
  (defun my-org-agenda-prefix ()
    "Prefix string for org agenda items."
    (concat
     (s-truncate
      19
      (s-join "/" (remove nil `(,(org-get-category) ,(nth 1 (org-get-outline-path)))))
      "..")
     ":"))
  (setf (cdr (assoc 'todo org-agenda-prefix-format)) " %i %-20(my-org-agenda-prefix) ")
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
          (:todo "DOING")
          (:todo "SOMEDAY")
          (:todo "PLANNED")))
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
  `(("M-RET" . ("org open (other window)" . ,(hx :eval (org-open 'other))))
    ("RET" . ("org open" . ,(hx :eval (org-open 'current))))
    ("' w" . ("org edit" . org-edit-special))
    ("' e" . ("org export" . org-export-dispatch))
    ("' i" . ("org insert" . (keymap)))
    ("' i T" . ("org insert template" . ,(hx :eval
                                             (modaled-set-state "insert")
                                             org-insert-structure-template)))
    ("' i i" . ("org insert id" . org-id-get-create))
    ("' i c" . ("org insert category" . org-insert-category))
    ("' i t" . ("org insert tag" . org-set-tags-command))
    ("' l" . ("org toggle link display" . org-toggle-link-display))
    ("' c" . ("org capture" . org-capture))
    ("' <" . ("org promote" . ,(hx :region :eval org-do-promote)))
    ("' >" . ("org demote" . ,(hx :region :eval org-do-demote)))
    ("' J" . ("org promote subtree" . ,(hx :region :eval org-promote-subtree)))
    ("' :" . ("org demote subtree" . ,(hx :region :eval org-demote-subtree)))
    ("' L" . ("org promote subtree" . ,(hx :region :eval org-move-subtree-up)))
    ("' K" . ("org demote subtree" . ,(hx :region :eval org-move-subtree-down)))
    ;; org-gtd
    ("' t" . ("org todo" . ,(hx :region :eval org-todo gtd-save)))
    ("' r" . ("org refile" . ,(hx :region :eval org-refile gtd-save)))
    ("' p" . ("org priority" . ,(hx :region :eval org-priority gtd-save)))))
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
    ("' t" . ("agenda todo" . ,(hx :region :eval org-agenda-todo gtd-save)))
    ("' p" . ("agenda priority" . ,(hx :region :eval org-agenda-priority gtd-save)))
    ("' r" . ("agenda refile" . ,(hx :region :eval org-agenda-refile gtd-save)))
    ("' i" . ("agenda insert" . (keymap)))
    ("' i t" . ("agenda insert tag" . org-agenda-set-tags))))
(modaled-enable-substate-on-state-change
  "org-agenda"
  :states '("normal" "select")
  :major '(org-agenda-mode))


;;; notes

(defvar note-directory (expand-file-name "~/.config/notes"))
(unless (file-exists-p note-directory)
  (mkdir note-directory t))

(use-package denote
  :hook
  (dired-mode . denote-dired-mode-in-directories)
  :custom
  (denote-directory note-directory)
  (denote-backlinks-show-context t)
  (denote-dired-directories (list note-directory))
  (denote-dired-directories-include-subdirectories t)
  (denote-prompts '(title))
  (denote-save-buffers t)
  (denote-date-format "%FT%T%z")
  :custom-face
  (denote-faces-link ((t :foreground "turquoise" :underline t))))

;; for searching
(use-package consult-denote
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep))

(modaled-define-keys
  :states '("normal" "select" "major")
  :bind
  `(("SPC n" . ("notes" . (keymap)))
    ("SPC n n" . ("note new" . denote))
    ("SPC n f" . ("note find" . (keymap)))
    ("SPC n f a" . ("find all" . consult-denote-find))
    ("SPC n f l" . ("find links" . denote-find-link))
    ("SPC n f b" . ("find links" . denote-find-backlink))
    ("SPC n s" . ("note search" . consult-denote-grep))
    ("SPC n i" . ("note insert" . (keymap)))
    ("SPC n i l" . ("insert link" . denote-link))
    ("SPC n i d" . ("insert org dblock" . (keymap)))
    ("SPC n i d l" . ("link dblock" . denote-org-extras-dblock-insert-links))
    ("SPC n i d b" . ("backlink dblock" . denote-org-extras-dblock-insert-backlinks))
    ("SPC n i d f" . ("file dblock" . denote-org-extras-dblock-insert-files))
    ("SPC n r" . ("note rename" . denote-rename-file))))

