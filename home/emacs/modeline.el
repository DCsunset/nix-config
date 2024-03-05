;; mode line

(defmacro my-modeline-def-construct (symbol form)
  "Define my modeline segment with SYMBOL and FORM."
  (declare (indent defun))
  `(progn
     (defvar-local ,symbol '(:eval ,form))
     ;; FIXME: debug
     (setq ,symbol '(:eval ,form))
     ;; need this property for :eval in modeline
     (put ',symbol 'risky-local-variable t)))

(defun my-modeline-def-map (cmd1 cmd3)
  "Define mouse map for modeline.
CMD1 for mouse-1 and CMD3 for mouse-3."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] cmd1)
    (define-key map [mode-line down-mouse-3] cmd3)
    map))


;; modaled
(my-modeline-def-construct my-modeline-modaled
  (propertize (capitalize (substring modaled-state 0 1))
              'face 'bold))

;; major mode
(my-modeline-def-construct my-modeline-major-mode
  (capitalize (string-remove-suffix "-mode" (symbol-name major-mode))))

;; buffer
(my-modeline-def-construct my-modeline-buffer
  ;; TODO: truncate path for file if necessary
  (let ((name (buffer-name)))
    (list
     ;; read-only
     (if buffer-read-only
         (concat (nerd-icons-faicon "nf-fa-lock") " ")
       "")
     ;; name
     (propertize name
                 'face (when (buffer-modified-p)
                         '((t :slant italic :weight bold)))
                 'help-echo buffer-file-name))))

;; flymake
(defun my-modeline--flymake-count (type)
  "Count number of flymake errors for specific TYPE."
  (number-to-string
   (--reduce-from (+ acc
                     (if (eq type (flymake-diagnostic-type it)) 1 0))
                  0
                  (flymake-diagnostics))))

(defvar my-modeline--flymake-error-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:error)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:error)))))
(defvar my-modeline--flymake-warning-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:warning)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:warning)))))
(defvar my-modeline--flymake-note-map
  (my-modeline-def-map
   (lambda () (interactive) (flymake-goto-next-error nil '(:note)))
   (lambda () (interactive) (flymake-goto-prev-error nil '(:note)))))

(my-modeline-def-construct my-modeline-flymake
  (list
   (propertize (nerd-icons-codicon "nf-cod-error")
               'face 'error
               'local-map my-modeline--flymake-error-map)
   " "
   (my-modeline--flymake-count :error)
   " "
   (propertize (nerd-icons-codicon "nf-cod-warning")
               'face 'warning
               'local-map my-modeline--flymake-warning-map)
   " "
   (my-modeline--flymake-count :warning)
   " "
   (propertize (nerd-icons-codicon "nf-cod-info")
               'local-map my-modeline--flymake-note-map)
   " "
   (my-modeline--flymake-count :note)))

;; must use setq-default as it will become buffer local when set
(setq-default mode-line-format
              '("%e"
                " "
                my-modeline-modaled
                " "
                my-modeline-buffer

                "        "  ; FIXME: remove this after Emacs 30
                mode-line-format-right-align  ; emacs 30

                my-modeline-flymake
                "  "
                my-modeline-major-mode))

