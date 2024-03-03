;;; Tempo template definitions

(defun hx-template-handler (arg)
  "Handle custom symbol as ARG in hx tempo templates."
  (cond
   ((eq arg 'date) (format-time-string "%F" (current-time)))
   ((eq arg 'datetime) (format-time-string "%FT%T%z" (current-time)))
   (t nil)))

(defvar hx-tempo-match-finder "\\(%[[:word:]]+\\)\\="
  "Tempo match finder for hx.")

(use-package tempo
  :commands (tempo-define-template
             tempo-use-tag-list
             tempo-complete-tag)
  :config
  (setq tempo-user-elements
        (cons #'hx-template-handler tempo-user-elements))

  ;; note: don't use any taglist as it needs to be added by `tempo-use-tag-list',
  ;; which changes a local variable and hard to change it everywhere
  (tempo-define-template
   "hx-date"
   '(date)
   "%date"
   "Insert date")

  (tempo-define-template
   "hx-datetime"
   '(datetime)
   "%datetime"
   "Insert datetime"))

