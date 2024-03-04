;; mode line

(defmacro define-my-modeline-segment (symbol form)
  "Define my modeline segment with SYMBOL and FORM."
  (declare (indent defun))
  `(progn
     (defvar-local ,symbol '(:eval ,form))
     ;; FIXME: debug
     (setq ,symbol '(:eval ,form))
     ;; need this property for :eval in modeline
     (put ',symbol 'risky-local-variable t)))

;; modaled
(define-my-modeline-segment my-modeline-modaled
  (propertize (capitalize (substring modaled-state 0 1))
              'face 'bold))

;; major mode
(define-my-modeline-segment my-modeline-major-mode
  (capitalize (string-remove-suffix "-mode" (symbol-name major-mode))))

;; buffer
(defun my-modeline--get-buffer-info ()
  "Return buffer info."
  ;; TODO: truncate path for file if necessary
  (let ((name (buffer-name)))
    name))
(define-my-modeline-segment my-modeline-buffer-info
  (my-modeline--get-buffer-info))

;; must use setq-default as it will become buffer local when set
(setq-default mode-line-format
              '("%e"
                " "
                my-modeline-modaled
                " "
                my-modeline-buffer-info
                " "
                my-modeline-major-mode))
