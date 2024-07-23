;;; hx.el --- define Keybindings similar to helix using modaled  -*- lexical-binding: t; -*-

(use-package highlight
  :commands (hlt-unhighlight-region hlt-highlight-region))

;; global pair config
(defvar hx-matching-pairs
  '((?\( . ?\))
    (?\{ . ?\})
    (?\[ . ?\])
    (?\< . ?\>)
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))
;; helper functions
(defun hx-update-pairs (remove-list add-list)
  "Update matchings-pairs in local var with REMOVE-LIST and ADD-LIST."
  (setq-local
   hx-matching-pairs
   (-concat add-list
            (--remove (member (car it) remove-list)
                      hx-matching-pairs))))
(defun hx-make-pair (char)
  "Make pair based on CHAR."
  (or (assq char hx-matching-pairs)
      (rassq char hx-matching-pairs)
      (cons char char)))

;; for matching pairs
(use-package expand-region
  :commands (er/mark-outside-pairs
             er/mark-outside-quotes)
  :hook  ; set different pairs based on major mode
  (emacs-lisp-mode . (lambda () (hx-update-pairs '("'" "`") '(("`" . "'")))))
  (rust-ts-mode . (lambda () (hx-update-pairs '() '(("|" . "|")))))
  (latex-mode . (lambda () (hx-update-pairs '() '(("$" . "$")))))
  (org-mode . (lambda () (hx-update-pairs '() '(("~" . "~")
                                                ("=" . "=")
                                                ("/" . "/")
                                                ("_" . "_"))))))


;; hx manages its own mark so that it can remove mark and cut region correctly
;; disable the transient mark mode
(transient-mark-mode -1)

;; Reference: evil
(defun hx--esc-filter (map)
  "Translate \\e to [escape] for MAP if no further event arrives in terminal.

This is necessary to distinguish the meta key and actual escape in terminal."
  (if (and (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
             (= (aref keys (1- (length keys))) ?\e)))
           (sit-for 0.01))  ; wait for certain time to distinguish the actual escape
      [escape]
    map))  ; don't change keymap if it isn't an actual escape

(defun hx--init-esc (frame)
  "Enable escape translation in `input-decode-map' for terminal FRAME."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (eq (terminal-live-p term) t)  ; only patch char only terminal
             (not (terminal-parameter term 'hx-esc-map)))  ; not patched already
        (let ((hx-esc-map (keymap-lookup input-decode-map "ESC")))
          ; remember the old map for restoration later and prevent patching it again
          (set-terminal-parameter term 'hx-esc-map hx-esc-map)
          (keymap-set input-decode-map "ESC"
                      `(menu-item "" ,hx-esc-map :filter ,#'hx--esc-filter)))))))

(defun hx--deinit-esc (frame)
  "Disable escape translation in `input-decode-map' for terminal FRAME."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (eq (terminal-live-p term) t)
        (let ((hx-esc-map (terminal-parameter term 'hx-esc-map)))
          (when hx-esc-map
            (keymap-set input-decode-map "ESC" hx-esc-map)
            (set-terminal-parameter term 'hx-esc-map nil)))))))

(defvar hx-esc-mode nil
  "Non-nil if `hx-esc-mode' is enabled.")

(defun hx-esc-mode (&optional arg)
  "Toggle translation from \\e to [escape] in terminal.

Enable it when ARG is positve and disable it if negative.
Toggle it when ARG is nil or 0."
  (cond
   ((or (null arg) (eq arg 0))
    (hx-esc-mode (if hx-esc-mode -1 1)))  ; toggle
   ((> arg 0)
    (unless hx-esc-mode
      (setq hx-esc-mode t)
      (add-hook 'after-make-frame-functions #'hx--init-esc)
      ; apply init function to existing frames
      (mapc #'hx--init-esc (frame-list))))
   ((< arg 0)
    (when hx-esc-mode
      (remove-hook 'after-make-frame-functions #'hx--deinit-esc)
      ; apply deinit function to existing frames
      (mapc #'hx--deinit-esc (frame-list))
      (setq hx-esc-mode nil)))))

(modaled-define-local-var hx--mark nil
  "Start pos of the selected region in hx.")

(defun within-range (val range)
  "Test if VAL is within RANGE."
  (and (>= val (car range)) (< val (cdr range))))

(defun sizeof-range (range)
  "Get size of RANGE."
  (- (cdr range) (car range)))

(defun hx-region ()
  "Get the currently selected region (start . end)."
  (let* ((pos (point))
         (mark (or hx--mark pos))
         (max-end (point-max)))
    ; inclusive range
    (if (< mark pos)
        (cons mark (min (1+ pos) max-end))
      (cons pos (min (1+ mark) max-end)))))

(defun hx-extended-region ()
  "Extend region to line start and end."
  (let ((region (hx-region))
        start
        end)
    (save-excursion
      (goto-char (car region))
      (setq start (line-beginning-position))
      (goto-char (1- (cdr region)))
      (setq end (line-end-position))
      (cons start end))))

(defun hx-region-string ()
  "Return string of selected region."
  (let ((region (hx-region)))
    (buffer-substring-no-properties (car region) (cdr region))))

(defun hx-region-size ()
  "Return size of selected region."
  (let ((region (hx-region)))
    (- (cdr region) (car region))))

(defun hx-highlight ()
  "Highlight the current region."
  ; use 'region as the highlight face
  (let ((region (hx-region)))
    (hlt-highlight-region (car region) (cdr region))))

(defun hx-unhighlight ()
  "Unhighlight current region."
  ; only unhighlight 'region face
  (let ((region (hx-region)))
    (hlt-unhighlight-region (car region) (cdr region))))

(defun hx-set-mark (pos)
  "Set mark in hx to POS."
  ;; use marker to change pos accordingly when text changes
  (setq hx--mark (if pos (copy-marker pos) nil)))

(defun hx-mark-pos ()
  "Get mark position."
  (and hx--mark (marker-position hx--mark)))

(defun hx-region-apply (fn &rest args)
  "Pass region (start end) to function FN."
  (let ((region (hx-region)))
    (apply fn `(,(car region) ,(cdr region) ,@args))))

(defun hx-extended-region-apply (fn &rest args)
  "Pass region (start end) to function FN."
  (let ((region (hx-extended-region)))
    (apply fn `(,(car region) ,(cdr region) ,@args))))

(defun hx-region-replace (char)
  "Replace region with given CHAR."
  (unless (eq char ?\e)  ; cancel replace when char is ESC
    ;; Return key corresponds to \r not \n
    (when (eq char ?\r) (setq char ?\n))
    (let ((size (hx-region-size))
          (pos (point)))
      (hx-region-apply #'delete-region)
      (insert (make-string size char))
      (goto-char pos))))  ; save-excursion won't work as text is changed

(defun hx-paste (text direction)
  "Paste TEXT around region (before if DIRECTION is negative and after otherwise)."
  (let* ((region (hx-region))
         (pos (if (string-suffix-p "\n" text)
                  ;; paste in previous or next line if text ends with a newline
                  (if (< direction 0)
                      (line-beginning-position)
                    (line-beginning-position 2))
                (if (< direction 0) (car region) (cdr region)))))
    (save-excursion
      (goto-char pos)
      (insert text))))

(defun hx-format-blank-line ()
  "Remove whitespaces if this line is empty."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (and (looking-at "[ \t]+$")
         (delete-region (point)
                        (line-end-position)))))

(defun hx-no-sel ()
  "Clear selection and disable highlight for it."
  (interactive)
  (hx-unhighlight)
  (hx-set-mark nil))


;;; Command records (each register stores a ring of records)
;; Used register (symbol):
;; - m: motion
;; - c: change
(modaled-define-local-var hx--command-records nil
  "Recorded commands.")
(modaled-define-local-var hx--recorded-keys nil
  "Recorded keys in insert state.")
(modaled-define-local-var hx--recording-regs nil
  "Registers it is recording keys into.")
(modaled-define-local-var hx--running-command-record nil
  "Whether it is running command record.")

(defun hx--add-command-record (reg record)
  "Set command RECORD for REG register."
  (let ((ring (or (alist-get reg hx--command-records)
                  (make-ring 10))))
    (ring-insert ring record)
    (setf (alist-get reg hx--command-records) ring)))

(defun hx-run-command-record (reg &optional index)
  "Run latest command record in REG."
  (when-let* ((ring (alist-get reg hx--command-records))
              (rec (ring-ref ring (or index 0)))
              (hx--running-command-record t))
    (pcase-let ((`(,keys ,hx--args) rec))
      (execute-kbd-macro keys))))

(defun hx-select-command-record (reg)
  "Select command record in REG."
  (when-let* ((ring (alist-get reg hx--command-records))
              (rec-list (--map (format "%s   %s"
                                       (key-description (car it))
                                       (cadr it))
                               (ring-elements ring)))
              (rec-str (completing-read "Record to run: " rec-list nil t))
              (rec-idx (-elem-index rec-str rec-list)))
    (hx-run-command-record reg rec-idx)
    ;; promote this record
    (ring-insert ring (ring-remove ring rec-idx))))

(defun hx--record-keys-hook ()
  "Record keys in insert state."
  (when (and (equal modaled-state "insert")
             (not mc--executing-command-for-fake-cursor)
             (not hx--running-command-record))
    ;; must use vector as some key sequences (e.g. [escape]) can't be represented by string
    (setq hx--recorded-keys (vconcat hx--recorded-keys (this-command-keys-vector)))))
(add-hook 'pre-command-hook #'hx--record-keys-hook)

(defun hx--record-keys-watcher (_ new-val _ _)
  "Watcher to finish recording keys when exiting insert state (NEW-VAL not insert)."
  (when (and hx--recording-regs
             (not mc--executing-command-for-fake-cursor)
             (not hx--running-command-record)
             (equal modaled-state "insert")
             (not (equal new-val "insert")))
    (dolist (reg hx--recording-regs)
      (let ((rec (ring-ref (alist-get reg hx--command-records) 0)))
        ;; append recorded keys to the command keys
        (setcar rec
                (vconcat (car rec) hx--recorded-keys))))
    (setq hx--recording-regs nil
          hx--recorded-keys nil)))
(add-variable-watcher 'modaled-state #'hx--record-keys-watcher)

;;; Jump list
(defvar hx--jump-list nil
  "Jump list to quickly jump between positions.")
(defvar hx--jump-list-pos 0
  "Current index in the jump list.")
(defvar hx--jump-list-max-len 20
  "Max length of jump list.")

(defun hx-goto-marker (marker)
  "Go to MARKER (possibly in another buffer."
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker))

(defun hx-jump-forward ()
  "Jump forward (newer) in the jump list."
  (interactive)
  (when (<= hx--jump-list-pos 0)
    (error (message "At the newest of jump list")))
  (setq hx--jump-list-pos (1- hx--jump-list-pos))
  (hx-goto-marker (nth hx--jump-list-pos hx--jump-list)))

(defun hx-jump-backward ()
  "Jump backward (older) in the jump list."
  (interactive)
  (when (>= hx--jump-list-pos (1- (length hx--jump-list)))
    (error (message "At the oldest of jump list")))
  (setq hx--jump-list-pos (1+ hx--jump-list-pos))
  (hx-goto-marker (nth hx--jump-list-pos hx--jump-list)))

(defun hx-jump-save ()
  "Save current pos to jump list."
  (interactive)
  ;; fork from current pos
  (dotimes (_ hx--jump-list-pos)
    (pop hx--jump-list))
  (setq hx--jump-list-pos 0)
  (let ((marker (copy-marker (point))))
    (setq hx--jump-list (-remove-item marker hx--jump-list))
    (push marker hx--jump-list)
    (when (> (length hx--jump-list) hx--jump-list-max-len)
      (nbutlast hx--jump-list)))
  (message "Saved to jump list"))

(defun hx-jump-remove ()
  "Remove current pos to jump list."
  (interactive)
  (if (not hx--jump-list)
      (message "Jump list is empty")
    (setq hx--jump-list (-remove-at hx--jump-list-pos hx--jump-list))
    (when (> hx--jump-list-pos 0)
      (setq hx--jump-list-pos (1- hx--jump-list-pos)))
    (message "Removed from jump list")))

(defun hx-jump-clear ()
  "Clear jump list."
  (interactive)
  (setq hx--jump-list nil
        hx--jump-list-pos 0)
  (message "Cleared jump list"))

(defun hx-jump-goto ()
  "Select and go to a pos in jump list."
  (interactive)
  (let ((marker (consult-global-mark hx--jump-list)))
    (setq hx--jump-list-pos (-elem-index marker hx--jump-list))))

(modaled-define-local-var hx-arg-next nil
  "Set hx-arg for next command.
Use a different name from `hx-arg' to prevent shadowing and allow modifying next.")
(modaled-define-local-var hx-arg-persist nil
  "Do not clear hx-arg after running next command.")

;; clear arg after next command
(defun hx--clear-hx-arg-hook ()
  (when hx-arg-next
    (if hx-arg-persist
        (setq hx-arg-persist nil)
      (setq hx-arg-next nil))))
(add-hook 'post-command-hook #'hx--clear-hx-arg-hook)

(defmacro hx (&rest args)
  "Construct an interactive lambda function based on ARGS.

ARGS is a list separated by options (name starts with :).
The following options are available:
:arg-desc DESC Argument descriptor for the interactive lambda function.
               Argument will be (&rest l-args).
:arg DEF       Read argument for eval forms. Argument will be arg.
               The following definitions supported:
               - (c ARGS): Call `read-char' with args
               - (s ARGS): Call `read-from-minibuffer' with args
               - (b ARGS): Call `y-or-n-p' with args
               - (B ARGS): Call `yes-or-no-p' with args
:jump          Save pos before and after the command to jump list
:rec REGS      Record this command in registers
:save          Save and restore hx state after the command (point, mark, ...)
:let ARGS      Bind variables in `let' macro using pairs.
:region [ARG]  Set native region to `hx-region' for body forms.
               If ARG is t, always enable the region (even when hx--mark unset).
:re-hl         Re-highlight selection.
:re-sel        Update selection based on modaled state.
               Re-highlight if in SELECT state or clear selection otherwise.
:once          Run only once when there are multiple cursors
:eval FORMS    Forms to evaluate in body."
  (let* ((opts
          ;; normalize options by partitioning args by : items
          (let ((parts (-partition-before-pred
                        (lambda (it)
                          ;; it could be a list for funcall
                          (and (symbolp it)
                               (string-prefix-p ":" (symbol-name it))))
                        args)))
            ;; convert to plist
            (seq-reduce (lambda (acc p)
                          (plist-put acc (car p) (cdr p)))
                        parts
                        '())))
         (arg-desc (car (plist-get opts :arg-desc)))
         (rec-regs (plist-get opts :rec))
         (lambda-args (if arg-desc '(&rest l-args) '()))
         (arg-def (car (plist-get opts :arg)))
         (let-bindings (or (plist-get opts :let) '()))
         ;; plist-member returns the list instead of t
         (once (and (plist-member opts :once) t))
         (eval-forms (mapcar (lambda (f)
                               (if (listp f)
                                   f
                                 `(call-interactively #',f)))
                             (plist-get opts :eval)))
         (jump-wrapper (lambda (f)
                         (if (not (plist-member opts :jump)) f
                           `(progn
                              (hx-jump-save)
                              ,f
                              (hx-jump-save)))))
         (save-wrapper (lambda (f)
                         (if (not (plist-member opts :save)) f
                           `(let ((cur (point))
                                  (pos (hx-mark-pos)))
                              ,f
                              (hx-set-mark pos)
                              (goto-char cur)))))
         (region-wrapper (lambda (f)
                           (if (not (plist-member opts :region)) f
                             `(if (or ,(and (plist-get opts :region) t)
                                      hx--mark)
                                  ;; transient-mark-mode must be true
                                  ;; for region-active-p to be true
                                  (let ((transient-mark-mode t)
                                        (region (hx-region)))
                                    (save-mark-and-excursion
                                      (goto-char (car region))
                                      (set-mark (cdr region))
                                      ,f))
                                ,f))))
         (re-hl-wrapper (lambda (f)
                           (if (not (plist-member opts :re-hl)) f
                             `(progn
                                (hx-unhighlight)
                                ,f
                                (hx-highlight)))))
         (re-sel-wrapper (lambda (f)
                           (if (not (plist-member opts :re-sel)) f
                             `(if (equal modaled-state "select")
                                  (progn
                                    (hx-unhighlight)
                                    ,f
                                    (hx-highlight))
                                (hx-no-sel)
                                ,f)))))
    `(lambda ,lambda-args
       (interactive ,arg-desc)
       (unless (and ,once
                    mc--executing-command-for-fake-cursor)
         ;; get current command keys before arg prompt to prevent recording those keys
         (pcase-let* ((keys (this-command-keys-vector))
                      ;; hx--args used to run this command non-interactively
                      (`(,arg ,l-args ,current-prefix-arg ,hx-arg)
                       (or (bound-and-true-p hx--args)
                           (list
                            ,(when arg-def
                               `(apply #',(pcase (car arg-def)
                                            ('c #'read-char)
                                            ('s #'read-from-minibuffer)
                                            ('b #'y-or-n-p)
                                            ('B #'yes-or-no-p)
                                            (_ (error
                                                (message "Invalid hx arg def: %s" arg-def))))
                                       ',(cdr arg-def)))
                            ,(when arg-desc 'l-args)
                            current-prefix-arg
                            hx-arg-next))))
           ;; record command and args (only when not running a record)
           (unless (or mc--executing-command-for-fake-cursor
                       hx--running-command-record)
             (dolist (reg ',rec-regs)
               (hx--add-command-record
                reg
                (list keys (list arg l-args current-prefix-arg hx-arg)))))
           (let ,let-bindings
             ,(funcall
               (-compose jump-wrapper
                         save-wrapper
                         region-wrapper
                         re-hl-wrapper
                         re-sel-wrapper)
               ;; need to catch error to let wrappers finish completely
               `(condition-case err
                    (progn ,@eval-forms)
                  (error
                   (message "%s" (error-message-string err))))))
           ;; record all keys in insert state for this command
           (setq hx--recording-regs ',rec-regs))))))

(defun hx-find-char (char direction offset &optional marking)
  "Find CHAR in DIRECTION and place the cursor with OFFSET from it.
Set mark when MARKING is t."
  (when marking
    (hx-set-mark (point)))
  (let ((search-fn (if (> direction 0) #'search-forward #'search-backward))
        (search-pos (- (point) offset)))
    (when (within-range search-pos (cons (point-min) (point-max)))
      (forward-char (- offset))
      (ignore-errors (funcall search-fn (string char)))
      (forward-char offset))))

(defun hx-next-word (&optional marking)
  "Move forward to next word and set mark when MARKING is t."
  (interactive)
  (ignore-errors
    ;; move forward when at end of this word, start of next word, whitespace, or end of line
    ;; due to inclusive range
    (let ((next-bounds (save-excursion (forward-char) (bounds-of-thing-at-point 'word)))
          (bounds (bounds-of-thing-at-point 'word))
          (next-pos (1+ (point))))
      (when (or
             (looking-at ".$")  ; last char of this line
             (looking-at "[ \t]")  ; whitespace
             (and bounds (= next-pos (cdr bounds)))  ; end of this word
             (and next-bounds (= next-pos (car next-bounds))))  ; start of next word
        (forward-char)))
    ;; skip new line chars
    (skip-chars-forward "\n")
    (when marking
      (hx-set-mark (point)))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if (and bounds (within-range (point) bounds))
          ;; go to end of this word if within range
          (goto-char (cdr bounds))
        ;; go to the start of next word, whitespace, or end of line
        (re-search-forward "\\<\\|[ \t]\\|$"))
      (skip-chars-forward " \t")
      ;; backward char because of inclusive range
      (backward-char))))

(defun hx-previous-word (&optional marking)
  "Move backward to previous word and set mark when MARKING is t."
  (interactive)
  (ignore-errors
    ;; move backward when at start of this word, end of previous word, whitespace, or start of line
    ;; due to inclusive range
    (let ((prev-bounds (save-excursion (backward-char) (bounds-of-thing-at-point 'word)))
          (bounds (bounds-of-thing-at-point 'word))
          (pos (point)))
      (when (or
             (looking-at "^")  ; start of line
             (looking-at "[ \t]")  ; whitespace
             (and bounds (= pos (car bounds)))  ; start of this word
             (and prev-bounds (= pos (cdr prev-bounds))))  ; end of previous word
        (backward-char)))
    ;; skip new line chars
    (skip-chars-backward "\n")
    (when (looking-at "$")
      (backward-char))
    (when marking
      (hx-set-mark (point)))
    (when (looking-at "[ \t]")
      (skip-chars-backward " \t")
      ;; skip one more whitespace at point due to inclusive range
      (when (not (looking-at "^"))
        (backward-char)))
    ;; move to start of previous word or start of line
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (if (and bounds (within-range (point) bounds))
          ;; go to start of this word if within range
          (goto-char (car bounds))
        ;; go to the end of previous word, whitespace or start of line
        (re-search-backward "\\>\\|[ \t]\\|^"))
      (when (looking-at "[ \t]")
        (skip-chars-backward " \t")))))

(defun hx-delete-word-backward ()
  "Delete a word backward without adding it to the `kill-ring'."
  (interactive)
  (delete-region (point) (progn (hx-previous-word) (point))))

(defun hx-delete-line ()
  "Delete content of current line but keep the line itself."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun hx-extend-to-line-bounds ()
  "Extend selection to line bounds."
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position))
        (changed nil))
    (when (or (not hx--mark) (> hx--mark start))
      (hx-set-mark start)
      (setq changed t))
    (when (< (point) end)
      (goto-char end)
      (setq changed t))
    ; return whether the selection has changed
    changed))

(defun hx-extend-line-below ()
  "Select current line or extend to next line if already selected."
  (interactive)
  (unless (hx-extend-to-line-bounds)
    (forward-line 1)
    (hx-extend-to-line-bounds)))

(defun hx-extend-char ()
  "Extend selection by one char."
  (interactive)
  (let ((region (hx-region)))
    (hx-set-mark (max 0 (1- (car region))))
    (goto-char (min (point-max) (cdr region)))))

(defun hx-shrink-char ()
  "Extend selection by one char."
  (interactive)
  (let ((region (hx-region)))
    (when (> (sizeof-range region) 1)
      (hx-set-mark (1+ (car region)))
      (goto-char (- (cdr region) 2)))))

(defun hx-join-lines ()
  "Join lines in selections.

If the selection is within one line, join the next line."
  (interactive)
  (let* ((region (hx-region))
         (beg (car region))
         ;; line-number-at-pos is not inclusive
         (end (1- (cdr region)))
         (beg-line (line-number-at-pos beg))
         (end-line (line-number-at-pos end))
         (lines (if (= beg-line end-line)
                    1
                  (- end-line beg-line))))
    (save-excursion
      (dotimes (_ lines)
        (goto-char beg)
        (end-of-line)
        (delete-char 1)
        (delete-horizontal-space)
        (insert " ")))))


;; match mode

(defun hx-get-surround-pair (&optional type)
  "Return region of surrounding pair that covers point.

Optional TYPE can be \\='quote or \\='bracket."
  (let* ((get-region (lambda (fn)
                       (ignore-errors
                           (funcall fn)
                           (cons (region-beginning)
                                 (region-end)))))
         (quote-regions (when (or (not type)
                                  (eq type 'quote))
                          (mapcar (lambda (i)
                                    (set-mark nil)
                                    (save-mark-and-excursion
                                      (forward-char i)
                                      (funcall get-region #'er/mark-outside-quotes)))
                                  ;; try both this pos or next (for inclusive cursor)
                                  '(0 1))))
         (bracket-regions (when (or (not type)
                                    (eq type 'bracket))
                            (mapcar (lambda (i)
                                      (set-mark nil)
                                      (save-mark-and-excursion
                                        (forward-char i)
                                        (funcall get-region #'er/mark-outside-pairs)))
                                    '(0 1)))))
    ;; return smallest region by weight
    (--min-by (cond
               ((not it) t)
               ((not other) nil)
               (t
                (let ((it-in (within-range (point) it))
                      (other-in (within-range (point) other)))
                  (if (eq it-in other-in)
                      (> (sizeof-range it) (sizeof-range other))
                    other-in))))
              (append quote-regions bracket-regions))))

(defun hx-match-char (&optional type)
  "Go to matching surrounding char at current point.

Optional TYPE and can be specified.
See `hx-get-surround-pair' for TYPE."
  (interactive)
  (when-let* ((bounds (save-excursion
                        (hx-get-surround-pair type)))
              (beg (car bounds))
              (end (cdr bounds)))
    (if (eq (point) beg)
        (goto-char (1- end))
      (goto-char beg))))

(defun hx-match-tag ()
  "Go to matching tag if in jtsx major mode."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-jump-jsx-element-tag-dwim))))

(defun hx-match-select (&optional type ends at-point)
  "Select current matching pair with optional CHAR.

See `hx-get-surround-pair' for TYPE.
ENDS can be one of the following:
\\='outer: Select around the pair including itself
\\='inner: (default) Select only context inside the pair.
AT-POINT means to make sure point is at beg or end."
  (when-let* ((bounds (hx-get-surround-pair type))
              (beg (car bounds))
              (end (cdr bounds)))
    (when (and at-point
               (not (eq beg (point)))
               (not (eq (1- end) (point))))
      (error "Point not at matching pair"))
    (pcase (or ends 'inner)
      ('inner (goto-char (- end 2))
              (hx-set-mark (1+ beg)))
      ('outer (goto-char (1- end))
              (hx-set-mark beg)))))

(defun hx-match-add (char)
  "Add matching CHAR pair around current selection."
  (let ((pair (hx-make-pair char))
        (region (hx-region)))
    ; make sure the mark is set to select inserted chars afterwards
    (when (not hx--mark)
      (hx-set-mark (point)))
    (goto-char (car region))
    (insert (car pair))
    ; need to go one char further as a char has been inserted
    (goto-char (1+ (cdr region)))
    (insert (cdr pair))
    (backward-char 1)))

(defun hx-match-replace (char)
  "Replace matching pair around current selection or at point with CHAR pair."
  (let* ((pair (hx-make-pair char))
         (region (let ((reg (hx-region)))
                   (if (> (cdr reg) (1+ (car reg)))
                       reg
                     ;; run match-select first and make sure point at one end
                     (hx-match-select nil 'outer t)
                     (hx-region))))
         (left (char-after (car region)))
         (right (char-before (cdr region))))
    (if (not (or (eq right (cdr (assoc left hx-matching-pairs)))
                 (eq right left)))
        (message "Current region not surrounded by matching pair")
      (goto-char (car region))
      (delete-char 1)
      (insert (car pair))
      (goto-char (cdr region))
      (delete-char -1)
      (insert (cdr pair))
      (backward-char 1))))

(defun hx-match-delete ()
  "Delete matching pair around current selection."
  (interactive)
  (let* ((region (hx-region))
         (left (char-after (car region)))
         (right (char-before (cdr region))))
    (if (not (or (equal right (cdr (assoc left hx-matching-pairs)))
                 (equal right left)))
        (message "Current region not surrounded by matching pair")
      (goto-char (car region))
      (delete-char 1)
      (hx-set-mark (point))
      ; one char already deleted
      (goto-char (1- (cdr region)))
      (delete-char -1)
      (backward-char 1))))

(defun hx--hide-popup ()
  "Hide popup window and remove it from `post-command-hook'."
  (remove-hook 'pre-command-hook 'hx--hide-popup)
  (call-interactively #'popwin:close-popup-window))

(defun hx--hide-popup-on-next-command ()
  "Hook popup window and remove it from `post-command-hook'."
  (remove-hook 'popwin:after-popup-hook 'hx--hide-popup-on-next-command)
  (add-hook 'pre-command-hook 'hx--hide-popup))

(defun hx-show-eldoc ()
  "Show doc at point with `eldoc' temporarily."
  (interactive)
  (call-interactively #'eldoc)
  (add-hook 'popwin:after-popup-hook 'hx--hide-popup-on-next-command))

(defun hx-toggle-visibility ()
  "Toggle visibility."
  (interactive)
  (pcase major-mode
   ('org-mode (org-cycle))
   ('magit-status-mode (call-interactively #'magit-section-toggle))
   ('dired-sidebar-mode (dired-sidebar-subtree-toggle))
   ((or 'dired-mode 'wdired-mode) (dired-find-file))))


;; structural movement

(defun hx-struct-prev ()
  "Move to nearest prev structure."
  (interactive)
  (pcase major-mode
   ('org-mode (org-previous-visible-heading 1))
   ('magit-status-mode (magit-section-backward))
   (_ (combobulate-navigate-logical-previous))))

(defun hx-struct-next ()
  "Move to nearest next structure."
  (interactive)
  (pcase major-mode
   ('org-mode (org-next-visible-heading 1))
   ('magit-status-mode (magit-section-forward))
   (_ (combobulate-navigate-logical-next))))

(defun hx-struct-backward  ()
  "Move backward in sibling."
  (interactive)
  (pcase major-mode
   ('org-mode (org-backward-heading-same-level 1))
   ('magit-status-mode (magit-section-backward-sibling))
   (_ (combobulate-navigate-previous))))

(defun hx-struct-forward ()
  "Move forward in sibling."
  (interactive)
  (pcase major-mode
   ('org-mode (org-forward-heading-same-level 1))
   ('magit-status-mode (magit-section-forward-sibling))
   (_ (combobulate-navigate-next))))

(defun hx-struct-up ()
  "Move up in hierarchy."
  (interactive)
  (combobulate-navigate-up-list-maybe))

(defun hx-struct-down ()
  "Move up in hierarchy."
  (interactive)
  (combobulate-navigate-down-list-maybe))

(defun hx-struct-drag-forward ()
  "Drag forward in sibling."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-move-jsx-element-forward))))

(defun hx-struct-drag-backward ()
  "Drag forward in sibling."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-move-jsx-element-backward))))

(defun hx-struct-drag-up ()
  "Drag up in hierarchy."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-move-jsx-element-step-in-backward))
    (_ (combobulate-drag-up))))

(defun hx-struct-drag-down ()
  "Drag up in hierarchy."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-move-jsx-element-step-in-forward))
    (_ (combobulate-drag-down))))

(defun hx-struct-wrap ()
  "Wrap struct with a new one."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-wrap-in-jsx-element))))

(defun hx-struct-delete ()
  "Delete struct."
  (interactive)
  (pcase major-mode
    ((guard (memq major-mode '(jtsx-jsx-mode jtsx-tsx-mode)))
     (jtsx-delete-jsx-node))
    (_ (combobulate-kill-node-dwim))))


;;; multiple cursors

(defun hx-toggle-multiple-cursors ()
  "Toggle multiple-cursors-mode."
  (interactive)
  (if multiple-cursors-mode
      (mc/disable-multiple-cursors-mode)
    (mc/maybe-multiple-cursors-mode)))

(defun hx-toggle-cursor-on-click (event)
  "Toggle a cursor on mouse click EVENT."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((position (event-end event)))
    (if (not (windowp (posn-window position)))
        (error "Position not in text area of window"))
    (select-window (posn-window position))
    (let ((pt (posn-point position)))
      (if (numberp pt)
          ;; is there a fake cursor with the actual *point* right where we are?
          (let ((existing (mc/fake-cursor-at-point pt)))
            (if existing
                (mc/remove-fake-cursor existing)
              (save-excursion
                (goto-char pt)
                (mc/create-fake-cursor-at-point))))))))

(defun hx-toggle-cursor ()
  "Toggle a cursor at point."
  (interactive)
  (let ((cursor (mc/fake-cursor-at-point)))
    (if cursor
        (mc/remove-fake-cursor cursor)
      (mc/create-fake-cursor-at-point))))

(defun hx-add-cursor ()
  "Add a cursor at point if doesn't exist."
  (interactive)
  (let ((cursor (mc/fake-cursor-at-point)))
    (when (not cursor)
      (mc/create-fake-cursor-at-point))))

(defun hx-remove-cursors ()
  "Remove all extra cursors.
Should be called only before entering multiple-cursors-mode."
  (interactive)
  (if multiple-cursors-mode
      (message "multiple-cursors-mode already enabled! Call hx-toggle-multiple-cursors instead")
    (mc/remove-fake-cursors)))

(use-package multiple-cursors
  :demand t
  :commands (mc/maybe-multiple-cursors-mode
             mc/disable-multiple-cursors-mode
             mc/create-fake-cursor-at-point
             mc/remove-fake-cursor
             mc/fake-cursor-at-point
             mc/remove-fake-cursors)
  :config
  ;; run it once to prevent disabling it immediately after enabling it
  (add-to-list 'mc--default-cmds-to-run-once #'hx-toggle-multiple-cursors)
  ;; need to save/restore hx--mark for each cursor
  (add-to-list 'mc/cursor-specific-vars 'hx--mark))


;;; keybindings

(modaled-define-keys
  :states '("normal")
  :bind
  `(("v" . ("enable SELECT state" . ,(hx :eval (modaled-set-state "select"))))
    ;; search (select after search)
    ("/" . ("search forward" . ,(hx :re-hl :eval isearch-forward-regexp backward-char (hx-set-mark isearch-other-end))))
    ("?" . ("search backward" . ,(hx :re-hl :eval isearch-backward-regexp (hx-set-mark (1- isearch-other-end)))))
    ("n" . ("next match" . ,(hx :re-hl :eval isearch-repeat-forward backward-char (hx-set-mark isearch-other-end) isearch-exit)))
    ("N" . ("prev match" . ,(hx :re-hl :eval isearch-repeat-backward (hx-set-mark (1- isearch-other-end)) isearch-exit)))
    ("*" . ("search selection" . ,(hx :eval (setq isearch-string (hx-region-string)))))
    ("M-/" . ("search lines" . occur))))

(add-hook
 'modaled-select-state-mode-hook
 (lambda ()
   ;; (unless (display-graphic-p)
   ;;   (send-string-to-terminal "\e[2 q"))
   (unless (equal modaled-state "select")
     ; entering select state
     (unless hx--mark
       (hx-set-mark (point))))))
(modaled-define-keys
  :states '("select")
  :bind
  ;; state changes
  `(("v" . ("exit SELECT state" . modaled-set-main-state))))

;; Common keybindings for normal and select states
;; Note:
;; - ' is reserved for major mode specific keybindings
(modaled-define-keys
  :states '("normal" "select")
  :bind
  ;; movement
  `(("j" . ("left" . ,(hx :re-sel :eval backward-char)))
    (";" . ("right" . ,(hx :re-sel :eval forward-char)))
    ("l" . ("up (visual)" . ,(hx :re-sel :eval previous-line)))
    ("k" . ("down (visual)" . ,(hx :re-sel :eval next-line)))
    ("L" . ("up (text)" . ,(hx :let (line-move-visual nil) :re-sel :eval previous-line)))
    ("K" . ("down (text)" . ,(hx :let (line-move-visual nil) :re-sel :eval next-line)))
    ("C-j" . ("left 10x (visual)" . ,(hx :arg-desc "p" :re-sel :eval
                                       (funcall-interactively #'backward-char (* (car l-args) 10)))))
    ("C-;" . ("right 10x (visual)" . ,(hx :arg-desc "p" :re-sel :eval
                                        (funcall-interactively #'forward-char (* (car l-args) 10)))))
    ("C-l" . ("up 10x (visual)" . ,(hx :arg-desc "p" :re-sel :eval
                                     (funcall-interactively #'previous-line (* (car l-args) 10)))))
    ("C-k" . ("down 10x (visual)" . ,(hx :arg-desc "p" :re-sel :eval
                                       (funcall-interactively #'next-line (* (car l-args) 10)))))
    ("w" . ("next word" . ,(hx :re-hl :eval (hx-next-word (equal modaled-state "normal")))))
    ("b" . ("prev word" . ,(hx :re-hl :eval (hx-previous-word (equal modaled-state "normal")))))
    ("f" . ("find next char" . ,(hx :rec m :arg (c "Find: ") :re-hl :eval (hx-find-char arg +1 -1 (equal modaled-state "normal")))))
    ("t" . ("find till next char" . ,(hx :rec m :arg (c "Till: ") :re-hl :eval (hx-find-char arg +1 -2 (equal modaled-state "normal")))))
    ("F" . ("find prev char" . ,(hx :rec m :arg (c "Find prev: ") :re-hl :eval (hx-find-char arg -1 0 (equal modaled-state "normal")))))
    ("T" . ("find till prev char" . ,(hx :rec m :arg (c "Till prev: ") :re-hl :eval (hx-find-char arg -1 1 (equal modaled-state "normal")))))
    ;; goto mode
    ("g g" . ("go to line" . ,(hx :arg-desc "p" :re-sel :eval (forward-line (- (car l-args) (line-number-at-pos))))))
    ("g i" . ("go to item" . consult-imenu))
    ("g l" . ("go to line interactively" . consult-goto-line))
    ("g s" . ("search and go to line" . consult-line))
    ("g e" . ("end of file" . ,(hx :re-sel :eval (goto-char (point-max)))))
    ("g j" . ("start of line" . ,(hx :re-sel :eval beginning-of-line)))
    ("g ;" . ("end of line" . ,(hx :re-sel :eval end-of-line (re-search-backward ".\\|^"))))  ; move to last char if exists
    ("g d" . ("go to def" . ,(hx :jump :let (xref-prompt-for-identifier nil) :eval xref-find-definitions)))
    ;; match mode
    ("m m" . ("match any char" . ,(hx :rec m :re-sel :eval hx-match-char)))
    ("m b" . ("match bracket" . ,(hx :rec m :re-sel :eval (hx-match-char 'bracket))))
    ("m q" . ("match quote" . ,(hx :rec m :re-sel :eval (hx-match-char 'quote))))
    ("m t" . ("match tag" . ,(hx :rec m :re-sel :eval hx-match-tag)))
    ("m s" . ("match select" . (keymap)))
    ("m s m" . ("select any pair" . ,(hx :rec m :re-hl :eval (hx-match-select))))
    ("m s p" . ("select bracket" . ,(hx :rec m :re-hl :eval (hx-match-select 'bracket))))
    ("m s q" . ("select quote" . ,(hx :rec m :re-hl :eval (hx-match-select 'quote))))
    ("m a" . ("surround with pair" . ,(hx :rec c :arg (c "Surround: ") :re-hl :eval modaled-set-main-state (hx-match-add arg))))
    ("m r c" . ("replace surrounding char" . ,(hx :rec c :arg (c "Surround replace: ") :re-hl :eval modaled-set-main-state (hx-match-replace arg))))
    ("m r t" . ("rename jsx tag" . ,(hx :re-hl :eval modaled-set-main-state jtsx-rename-jsx-element)))
    ;; changes
    ("i" . ("insert before" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert"))))
    ("a" . ("insert after" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert") forward-char)))
    ("I" . ("insert at start of line" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert") back-to-indentation)))
    ("A" . ("insert at end of line" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert") end-of-line)))
    ("o" . ("insert below" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert") end-of-line newline-and-indent)))
    ("O" . ("insert above" . ,(hx :rec c :eval hx-no-sel (modaled-set-state "insert") beginning-of-line newline-and-indent (forward-line -1) indent-according-to-mode)))
    ("r" . ("replace" . ,(hx :rec c :arg (c "Replace: ") :eval modaled-set-main-state (hx-region-replace arg) hx-no-sel)))
    ("y" . ("copy" . ,(hx :eval modaled-set-main-state (hx-region-apply #'kill-ring-save))))
    ("d" . ("delete" . ,(hx :rec c :eval modaled-set-main-state (hx-region-apply #'delete-region) hx-no-sel)))
    ("c" . ("change" . ,(hx :rec c :eval (modaled-set-state "insert") (hx-region-apply #'delete-region) hx-no-sel)))
    ("P" . ("paste before" . ,(hx :rec c :eval (hx-paste (current-kill 0 t) -1) hx-no-sel)))
    ("p" . ("paste after" . ,(hx :rec c :eval (hx-paste (current-kill 0 t) +1) hx-no-sel)))
    ("M-P" . ("paste before from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") -1) hx-no-sel)))
    ("M-p" . ("paste after from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") +1) hx-no-sel)))
    ("J" . ("join lines" . hx-join-lines))
    ("u" . ("undo" . ,(hx :once :eval hx-no-sel undo-fu-only-undo)))
    ("U" . ("redo" . ,(hx :once :eval hx-no-sel undo-fu-only-redo)))
    ("M-U" . ("redo all" . ,(hx :once :eval hx-no-sel undo-fu-only-redo-all)))
    (">" . ("indent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly 2))))
    ("<" . ("unindent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly -2))))
    ("=" . ("format" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-region))))
    ("C-c" . ("comment/uncomment" . ,(hx :re-hl :eval (hx-extended-region-apply #'comment-or-uncomment-region))))
    ;; selection
    ("x" . ("extend line below" . ,(hx :re-hl :eval hx-extend-line-below)))
    ("X" . ("extend line" . ,(hx :re-hl :eval hx-extend-to-line-bounds)))
    ("e" . ("extend one char" . ,(hx :re-hl :eval hx-extend-char)))
    ("E" . ("shrink one char" . ,(hx :re-hl :eval hx-shrink-char)))
    ;; multiple cursors
    ("C" . ("toggle cursor and next line" . ,(hx :let (line-move-visual nil) :eval hx-toggle-cursor next-line)))
    ("M-C" . ("remove all cursors" . hx-remove-cursors))
    ("M-c" . ("toggle multiple-cursors-mode" . hx-toggle-multiple-cursors))
    ("M-<mouse-1>" . ("toggle cursor on click" . hx-toggle-cursor-on-click))
    ;; transformation mode (backtick)
    ("` c" . ("case" . (keymap)))
    ("` c u" . ("upper case" . ,(hx :eval (hx-region-apply #'upcase-region))))
    ("` c l" . ("lower case" . ,(hx :eval (hx-region-apply #'downcase-region))))
    ("` c c" . ("capitalized case" . ,(hx :eval (hx-region-apply #'capitalize-region))))
    ("` s" . ("sort" . (keymap)))
    ("` s l" . ("sort lines" . ,(hx :arg (b "Ascending?") :save :eval (hx-region-apply (-partial #'sort-lines (not arg))))))
    ("` s c" . ("sort columns" . ,(hx :arg (b "Ascending?") :save :eval (hx-region-apply (-partial #'sort-columns (not arg))))))
    ("` r" . ("replace" . (keymap)))
    ("` r r" . ("replace regex" . ,(hx :region :eval replace-regexp)))
    ("` r s" . ("replace string" . ,(hx :region :eval replace-string)))
    ;; structural moving/editing
    ("s j" . ("prev" . ,(hx :rec m :eval hx-struct-prev)))
    ("s ;" . ("next" . ,(hx :rec m :eval hx-struct-next)))
    ("s s j" . ("backward" . ,(hx :rec m :eval hx-struct-backward)))
    ("s s ;" . ("forward" . ,(hx :rec m :eval hx-struct-forward)))
    ("s l" . ("up in hierarchy (TS)" . ,(hx :rec m :eval hx-struct-up)))
    ("s k" . ("down into hierarchy (TS)" . ,(hx :rec m :eval hx-struct-down)))
    ("s d" . ("drag" . (keymap)))
    ("s d j" . ("drag backward in sibling (TS)" . ,(hx :rec c :eval hx-struct-drag-backward)))
    ("s d ;" . ("drag forward in sibling (TS)" . ,(hx :rec c :eval hx-struct-drag-forward)))
    ("s d l" . ("drag up in hierarchy (TS)" . ,(hx :rec c :eval hx-struct-drag-up)))
    ("s d k" . ("drag down in hierarchy (TS)" . ,(hx :rec c :eval hx-struct-drag-down)))
    ("s W" . ("wrap struct (TS)" . ,(hx :rec c :eval hx-struct-wrap)))
    ("s D" . ("delete struct (TS)" . ,(hx :rec c :eval hx-struct-delete)))
    ;; misc
    ("{" . ("jump backward" . hx-jump-backward))
    ("}" . ("jump forward" . hx-jump-forward))
    ("M-s" . ("save to jump list" . hx-jump-save))
    ("M-S" . ("remove from jump list" . hx-jump-remove))
    ("." . ("repeat change" . ,(hx :eval (hx-run-command-record 'c))))
    ("M-." . ("select change" . ,(hx :eval (hx-select-command-record 'c))))
    ("," . ("repeat motion" . ,(hx :eval (hx-run-command-record 'm))))
    ("M-," . ("select motion" . ,(hx :eval (hx-select-command-record 'm))))
    ;; note: TAB is the same as C-i in terminal
    (("TAB" "<tab>") . ("toggle visibility" . hx-toggle-visibility))
    ("|" . ("expr eval" . eval-expression))
    ("\\" . ("eval region" . ,(hx :eval (hx-region-apply #'eval-region t))))
    ("q" . ("quit window" . quit-window))
    ("Q" . ("kill buffer" . kill-this-buffer))
    ;; hx-arg
    ("<f1>" . ("hx-arg f1" . ,(hx :eval
                                (message "hx-arg: 'f1")
                                (setq hx-arg-next 'f1
                                      hx-arg-persist t))))
    ("<f2>" . ("hx-arg f2" . ,(hx :eval
                                (message "hx-arg: 'f2")
                                (setq hx-arg-next 'f2
                                      hx-arg-persist t))))
    ("<f3>" . ("hx-arg f3" . ,(hx :eval
                                (message "hx-arg: 'f3")
                                (setq hx-arg-next 'f3
                                      hx-arg-persist t))))
    ("<f4>" . ("hx-arg f4" . ,(hx :eval
                                (message "hx-arg: 'f4")
                                (setq hx-arg-next 'f4
                                      hx-arg-persist t))))
    ;; major-mode specific command
    ("' x" . ,(hx :region :let (command (keymap-lookup (current-local-map) "C-c C-c"))
               :eval (when command (call-interactively command))))))

;; space mode
(modaled-define-keys
  :states '("normal" "select" "major")
  :bind
  `(("SPC '" . ("Toggle major state" . ,(hx :eval (modaled-set-state
                                                (if (equal modaled-state "major")
                                                    "normal"
                                                  "major")))))
    ("SPC c" . ("clipboard" . (keymap)))
    ("SPC c P" . ("clipboard paste before" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) -1) hx-no-sel)))
    ("SPC c p" . ("clipboard paste after" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) +1) hx-no-sel)))
    ("SPC c y" . ("copy to clipboard" . ,(hx :eval modaled-set-main-state (xclip-set-selection 'clipboard (hx-region-string)))))
    ("SPC p" . ("projectile" . (keymap)))
    ("SPC p f" . ("find file in project" . projectile-find-file))
    ("SPC p s" . ("search in project" . projectile-ripgrep))
    ("SPC d" . ("dired" . (keymap)))
    ("SPC d f" . ("find file in dir" . find-file))
    ("SPC d F" . ("find file in dir (recursive)" . consult-fd))
    ("SPC d s" . ("search in dir" . consult-ripgrep))
    ("SPC d t" . ("dired tree" . dired-sidebar-toggle-sidebar))
    ("SPC d n" . ("new file/dir" . (keymap)))
    ("SPC d n f" . ("new file" . dired-create-empty-file))
    ("SPC d n d" . ("new dir" . dired-create-directory))
    ("SPC b" . ("buffer" . (keymap)))
    ("SPC b g" . ("go to (buffer)" . consult-buffer))
    ("SPC b r" . ("reload buffer" . revert-buffer))
    ("SPC ?" . ("search symbol" . apropos))
    ("SPC k" . ("show eldoc" . hx-show-eldoc))
    ("SPC u" . ("undo tree" . vundo))
    ("SPC j" . ("jump list" . (keymap)))
    ("SPC j g" . ("go to (jump list)" . hx-jump-goto))
    ("SPC j c" . ("clear jump list" . hx-jump-clear))
    ("SPC e p" . ("prev error" . ,(hx :rec m
                                  :let (types (pcase hx-arg
                                                ('f1 '(:error))
                                                ('f2 '(:warning))
                                                ('f3 '(:note))
                                                (_ '(:error :warning))))
                                  :eval (flymake-goto-prev-error nil types t))))
    ("SPC e n" . ("next error" . ,(hx :rec m
                                  :let (types (pcase hx-arg
                                                ('f1 '(:error))
                                                ('f2 '(:warning))
                                                ('f3 '(:note))
                                                (_ '(:error :warning))))
                                  :eval (flymake-goto-next-error nil types t))))
    ;; gtd
    ("SPC t" . ("gtd" . (keymap)))
    ("SPC t l" . ("gtd list" . org-todo-list))
    ("SPC t i" . ("gtd inbox" . ,(hx :eval (find-file (gtd-file "inbox.org")))))
    ("SPC t a" . ("gtd actions" . ,(hx :eval (find-file (gtd-file "actions.org")))))
    ("SPC t c" . ("gtd capture" . ,(hx :eval (org-capture nil "ti"))))
    ;; notes (org-roam & xeft)
    ("SPC n" . ("notes" . (keymap)))
    ("SPC n f" . ("note find" . org-roam-node-find))
    ("SPC n s" . ("note search" . xeft))
    ("SPC n c" . ("note create" . org-roam-capture))
    ("SPC n i" . ("note insert" . org-roam-node-insert))
    ("SPC n l" . ("note backlinks" . org-roam-buffer-toggle))
    ;; LSP
    ("SPC l" . ("lsp" . (keymap)))
    ("SPC l r" . ("rename" . eglot-rename))
    ("SPC l =" . ("format" . eglot-format-buffer))
    ("SPC l a" . ("action" . eglot-code-actions))
    ("SPC l f" . ("quickfix" . eglot-code-action-quickfix))
    ;; shell
    ("SPC s" . ("shell" . (keymap)))
    ("SPC s x" . ("shell command" . shell-command))
    ("SPC s r" . ("shell command on region (as input)" . ,(hx :region t :eval shell-command-on-region)))
    ;; git
    ("SPC g" . ("git" . (keymap)))
    ("SPC g b" . ("blamer-mode" . blamer-mode))
    ("SPC g i" . ("show commit info" . blamer-show-commit-info))
    ("SPC g o" . ("open magit" . magit-status))
    ("SPC g d" . ("show diff" . vc-diff))
    ;; ai
    ("SPC a" . ("ai" . (keymap)))
    ("SPC a c" . ("ai code actions" . (keymap)))
    ("SPC a c c" . ("code complete" . ,(hx :region :eval ellama-code-complete)))
    ("SPC a c a" . ("code add" . ,(hx :region :eval ellama-code-add)))
    ("SPC a c e" . ("code edit" . ,(hx :region :eval ellama-code-edit)))
    ("SPC a c i" . ("code improve" . ,(hx :region :eval ellama-code-improve)))
    ("SPC a c r" . ("code review" . ,(hx :region :eval ellama-code-review)))
    ("SPC a s" . ("ai summarize" . (keymap)))
    ("SPC a s s" . ("summarize" . ,(hx :region :eval ellama-summarize)))
    ("SPC a s w" . ("summarize webpage" . ,(hx :region :eval ellama-summarize-webpage)))
    ("SPC a i" . ("ai improve" . (keymap)))
    ("SPC a i w" . ("improve wording" . ,(hx :region :eval ellama-improve-wording)))
    ("SPC a i g" . ("improve grammar" . ,(hx :region :eval ellama-improve-grammar)))
    ("SPC a i c" . ("improve conciseness" . ,(hx :region :eval ellama-improve-conciseness)))
    ("SPC a a" . ("ai ask" . (keymap)))
    ("SPC a a a" . ("ask about" . ,(hx :region :eval ellama-ask-about)))
    ("SPC a a i" . ("ask interactively" . ,(hx :region :eval ellama-chat)))
    ("SPC a a s" . ("ask selection" . ,(hx :region :eval ellama-ask-selection)))
    ("SPC a t" . ("ai text actions" . (keymap)))
    ("SPC a t t" . ("text translate" . ,(hx :region :eval ellama-translate)))
    ("SPC a t c" . ("text complete" . ,(hx :region :eval ellama-complete)))))

(modaled-define-keys
  :states '("insert")
  :bind
  `(("M-i" . ("code suggestion (LSP)" . company-manual-begin))
    ;; set tempo-match-finder temporarily to prevent conflicts
    ("M-t" . ("tempo complete" . ,(hx :let (tempo-match-finder my-tempo-match-finder) :eval tempo-complete-tag)))))

(modaled-define-keys
  :states '("insert")
  :keymaps '(minibuffer-mode-map)
  :bind
  ;; this makes pasting work in GUI
  `(("C-w" . ("delete word backward" . hx-delete-word-backward))
    ("C-u" . ("delete line" . hx-delete-line))
    ;; In terminal C-S-v is <xterm-paste>
    ;; Note: C-S-v conflicts with M-[ in terminal
    (("C-S-v" "<xterm-paste>") . ("paste from clipboard" . ,(hx :eval (insert (xclip-get-selection 'clipboard)))))))

(defun hx-save ()
  "Save buffer or finish editing."
  (interactive)
  (cond
   ((bound-and-true-p org-capture-mode)
    (org-capture-finalize))
   ((eq major-mode 'wdired-mode)
    (wdired-finish-edit)
    ;; re-highlighting (note mode hook won't be called for some reason)
    (dired-highlight))
   ((or (bound-and-true-p git-commit-mode)
        (eq major-mode 'git-rebase-mode))
    (call-interactively #'with-editor-finish))
   (t (save-buffer))))

(defun hx-abort ()
  "Abort editing."
  (interactive)
  (cond
   ((bound-and-true-p org-capture-mode)
    (org-capture-kill))
   ((eq major-mode 'wdired-mode)
    (wdired-abort-changes)
    ;; reload buffer to fix icons
    (revert-buffer)
    (dired-highlight))
   ((or (bound-and-true-p git-commit-mode)
        (eq major-mode 'git-rebase-mode))
    (call-interactively #'with-editor-cancel))))

;; common keybindings for all states
(modaled-define-keys
  :states '("normal" "select" "insert" "major")
  :bind
  `(("<escape>" . ("main state" . ,(hx :eval modaled-set-main-state hx-format-blank-line hx-no-sel)))
    ("M-SPC" . ("toggle vterm" . vterm-toggle))
    ("M-h" . ("split horizontally" . ,(hx :eval split-window-horizontally other-window)))
    ("M-v" . ("split vertically" . ,(hx :eval split-window-vertically other-window)))
    ("M-q" . ("delete window" . delete-window))
    ("M-j" . ("left window" . windmove-left))
    ("M-;" . ("right window" . windmove-right))
    ("M-l" . ("up window" . windmove-up))
    ("M-k" . ("down window" . windmove-down))
    ("M-J" . ("shrink window horizontally" . shrink-window-horizontally))
    ("M-:" . ("enlarge window horizontally" . enlarge-window-horizontally))
    ("M-L" . ("enlarge window vertically" . enlarge-window))
    ("M-K" . ("shrink window vertically" . shrink-window))
    ("C-M-j" . ("prev tab" . centaur-tabs-backward))
    ("C-M-;" . ("next tab" . centaur-tabs-forward))
    ("C-M-l" . ("prev tab group" . centaur-tabs-backward-group))
    ("C-M-k" . ("next tab group" . centaur-tabs-forward-group))
    ("C-s" . ("save" . hx-save))
    ("C-M-s" . ("save all" . ,(hx :eval (save-some-buffers t))))
    ("C-a" . ("abort" . hx-abort))
    ("C-q" . ("quit" . save-buffers-kill-terminal))))

(modaled-define-keys
  :global t
  :bind
  `(("<escape>" . ("keyboard quit" . keyboard-escape-quit))
    ;; only works in GUI as C-= and C-- are managed by terminal emulator otherwise
    ("C-=" . ("scale increase" . text-scale-increase))
    ("C--" . ("scale decrease" . text-scale-decrease))
    ;; unset M-<down-mouse-1> to prevent conflict with M-<mouse-1>
    (("C-d" "M-<down-mouse-1>") . nil)))

(setq modaled-init-state-fn
      (lambda ()
        (cond
         ((memq major-mode '(vterm-mode xeft-mode))
          "insert")
         ((memq major-mode '(dired-mode dired-sidebar-mode))
          "major")
         (t "normal"))))
(setq modaled-main-state-fn
      (lambda ()
        (cond
         (t "normal"))))

(modaled-setup)

;; translate terminal \\e to [escape]
(hx-esc-mode 1)

