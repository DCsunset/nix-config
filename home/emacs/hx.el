;;; hx.el --- define Keybindings similar to helix using modaled  -*- lexical-binding: t; -*-

(use-package modaled
  :commands (modaled-set-state
             modaled-define-keys
             modaled-define-state-keys
             modaled-set-default-state
             modaled-define-substate-keys
             modaled-get-substate-mode
             modaled-define-default-state
             modaled-enable-substate-on-state-change))
(use-package highlight
  :commands (hlt-unhighlight-region hlt-highlight-region))

; hx manages its own mark so that it can remove mark and cut region correctly
; disable the transient mark mode
(transient-mark-mode -1)

; Reference: evil
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
        (let ((hx-esc-map (lookup-key input-decode-map [?\e])))
          ; remember the old map for restoration later and prevent patching it again
          (set-terminal-parameter term 'hx-esc-map hx-esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,hx-esc-map :filter ,#'hx--esc-filter)))))))

(defun hx--deinit-esc (frame)
  "Disable escape translation in `input-decode-map' for terminal FRAME."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (eq (terminal-live-p term) t)
        (let ((hx-esc-map (terminal-parameter term 'hx-esc-map)))
          (when hx-esc-map
            (define-key input-decode-map [?\e] hx-esc-map)
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
    (hlt-highlight-region (car region) (cdr region) 'region)))

(defun hx-unhighlight ()
  "Unhighlight current region."
  ; only unhighlight 'region face
  (let ((region (hx-region)))
    (hlt-unhighlight-region (car region) (cdr region) 'region)))

(defun hx-set-mark (pos)
  "Set mark in hx to POS."
  ;; use marker to change pos accordingly when text changes
  (setq hx--mark (if pos (copy-marker pos) nil)))

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
  (let ((size (hx-region-size))
        (pos (point)))
    (hx-region-apply #'delete-region)
    (insert (make-string size char))
    (goto-char pos)))  ; save-excursion won't work as text is changed

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
  (save-excursion
    (beginning-of-line)
    (and (looking-at "[ \t]+$")
         (delete-region (point)
                        (line-end-position)))))

; [WIP] record changes
(modaled-define-local-var hx--last-change nil
  "Recorded last change.")
(defun hx-record-change (command)
  "Record change and return the COMMAND."
  (setq hx--last-change command)
  command)
(defun hx-repeat-change ()
  "Repeat last change."
  (when hx--last-change
    (funcall hx--last-change)
    (setq hx--last-change nil)))

(defun hx-cmds (&rest commands)
  "Run a list of COMMANDS interactively."
  (declare (indent defun))
  (lambda ()
    (interactive)
    (dolist (cmd commands)
      (call-interactively cmd))))

(defmacro hx-eval (arg-desc &rest body)
  "Return an interactive lambda (&rest args) with ARG-DESC to evaluate BODY forms."
  (declare (indent defun))
  (let ((args (if arg-desc '(&rest args) '())))
    `(lambda ,args
      (interactive ,arg-desc)
      (progn ,@body))))

(defmacro hx-eval-with-region (arg-desc &rest body)
  "Return an interactive lambda (&rest args) with ARG-DESC to evaluate BODY forms.
Region is activated for other commands to work with the native region."
  (declare (indent defun))
  (let ((args (if arg-desc '(&rest args) '())))
    `(lambda ,args
      (interactive ,arg-desc)
      ;; transient-mark-mode must be true for region-active-p to be true
      (let ((transient-mark-mode t))
        (set-mark hx--mark)
        (progn ,@body)
        (set-mark nil)))))


(defmacro hx-re-mark (&rest body)
  "Re-mark at current point before evaluating BODY forms."
  (declare (indent defun))
  `(progn
     (hx-unhighlight)
     (hx-set-mark (point))
     ,@body
     (hx-highlight)))

(defmacro hx-re-hl (&rest body)
  "Re-highlight selection after evaluating BODY forms."
  (declare (indent defun))
  ; unhighlight previous region first and rehighlight after body
  `(progn
    (hx-unhighlight)
    ,@body
    (hx-highlight)))

(defmacro hx-no-sel-before (&rest body)
  "Clear selection before evaluating BODY forms."
  (declare (indent defun))
  `(progn
     (hx-unhighlight)
     (hx-set-mark nil)
     ,@body))

(defmacro hx-no-sel-after (&rest body)
  "Clear selection after evaluating BODY forms."
  (declare (indent defun))
  `(progn
     ,@body
     (hx-unhighlight)
     (hx-set-mark nil)))

(defmacro hx-update-sel (&rest body)
  "Update selection based on modaled state and evaludate BODY forms.
Re-highlight if in select state or clear selection otherwise."
  (declare (indent defun))
  `(hx-apply-if (equal modaled-state "select") hx-re-hl hx-no-sel-before
    ,@body))


(defmacro hx-apply-if (value then else &rest body)
  "Apply BODY to THEN if VALUE is t or ELSE otherwise."
  (declare (indent defun))
  `(if ,value
       (,then ,@body)
     (,else ,@body)))

(defun hx-find-char (char direction offset &optional marking)
  "Find CHAR in DIRECTION and place the cursor with OFFSET from it.
Set mark when MARKING is t."
  (hx-re-hl
    (when marking
      (hx-set-mark (point)))
    (let ((search-fn (if (> direction 0) #'search-forward #'search-backward))
          (search-pos (- (point) offset)))
      (when (within-range search-pos (cons (point-min) (point-max)))
        (forward-char (- offset))
        (ignore-errors (funcall search-fn (string char)))
        (forward-char offset)))))

(defun hx-next-word (&optional marking)
  "Move forward to next word and set mark when MARKING is t."
  (interactive)
  (ignore-errors
    (hx-re-hl
      ; move forward when at end of this word, start of next word, whitespace, or end of line
      ; due to inclusive range
      (let ((next-bounds (save-excursion (forward-char) (bounds-of-thing-at-point 'word)))
            (bounds (bounds-of-thing-at-point 'word))
            (next-pos (1+ (point))))
        (when (or
                (looking-at ".$")  ; last char of this line
                (looking-at "[[:space:]]")  ; whitespace
                (and bounds (= next-pos (cdr bounds)))  ; end of this word
                (and next-bounds (= next-pos (car next-bounds))))  ; start of next word
          (forward-char)))
      ; skip new line chars
      (skip-chars-forward "\n")
      (when marking
        (hx-set-mark (point)))
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if (and bounds (within-range (point) bounds))
            ; go to end of this word if within range
            (goto-char (cdr bounds))
          ; go to the start of next word or end of line otherwise
          (re-search-forward "\\<\\|$"))
        (skip-chars-forward " \t")
        ; backward char because of inclusive range
        (backward-char)))))

(defun hx-previous-word (&optional marking)
  "Move backward to previous word and set mark when MARKING is t."
  (interactive)
  (ignore-errors
    (hx-re-hl
      ; move backward when at start of this word, end of previous word, whitespace, or start of line
      ; due to inclusive range
      (let ((prev-bounds (save-excursion (backward-char) (bounds-of-thing-at-point 'word)))
            (bounds (bounds-of-thing-at-point 'word))
            (pos (point)))
        (when (or
                (looking-at "^")  ; start of line
                (looking-at "[[:space:]]")  ; whitespace
                (and bounds (= pos (car bounds)))  ; start of this word
                (and prev-bounds (= pos (cdr prev-bounds))))  ; end of previous word
          (backward-char)))
      ; skip new line chars
      (skip-chars-backward "\n")
      (when (looking-at "$")
        (backward-char))
      (when marking
        (hx-set-mark (point)))
      ; move to start of previous word or start of line
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (if (and bounds (within-range (point) bounds))
            ; go to start of this word if within range
            (goto-char (car bounds))
          ; go to the end of previous word or start of line otherwise
          (re-search-backward "\\>\\|^"))))))

(defun hx-delete-word-backward ()
  "Delete a word backward without adding it to the `kill-ring'."
  (interactive)
  (delete-region (point) (progn (hx-previous-word) (point))))

(defun hx-extend-to-line-bounds ()
  "Extend selection to line bounds."
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
  (unless (hx-extend-to-line-bounds)
    (forward-line 1)
    (hx-extend-to-line-bounds)))

(defun hx-join-lines ()
  "Join lines in selecions.

If the selection is within one line, join the next line."
  (let* ((region (hx-region))
         (beg (car region))
         (end (cdr region)))
    (save-excursion
      (when (= (line-number-at-pos beg) (line-number-at-pos end))
        (goto-char end)
        (forward-line 1)
        (setq end (point)))
      (join-line nil beg end))))


(defun hx-get-enclosing-pair (&optional level)
  "Return the balanced expression that wraps point (inclusive).

With LEVEL, acend that many times.
Default is 1 which means the nearest level."
  ;; try two positions as the pointer has a width
  (let ((left (sp-get-enclosing-sexp))
        ;; inclusive
        (right (save-excursion
                 (forward-char 1)
                 (sp-get-enclosing-sexp level))))
    (if (and left right)
        (let* ((lbeg (plist-get left :beg))
               (lend (plist-get left :end))
               (rbeg (plist-get right :beg))
               (rend (plist-get right :end))
               (pos (point))
               ;; test if within range (there are bugs in sp)
               ;; it also prevents going outside boundary (like consecutive closing parens)
               (lwithin (within-range pos (cons lbeg lend)))
               (rwithin (within-range pos (cons rbeg rend))))
          (cond ((and lwithin rwithin)
                 ; return inner pair
                 (if (> lbeg rbeg) left right))
                (lwithin left)
                (rwithin right)))
      (or left right))))


(defun hx-match-char ()
  "Go to matching (or surrounding) char at current point."
  (let ((pair (hx-get-enclosing-pair)))
    (when pair
      (let ((beg (plist-get pair :beg))
            (end (1- (plist-get pair :end)))
            (pos (point)))
        (if (= pos end)
            (goto-char beg)
          (goto-char end))))))

(defun hx-match-select (arg)
  "Selete current matching pair.

ARG can be one of the following:
:around  Select around the pair including itself
:inside  Select only context inside the pair."
  (let ((pair (hx-get-enclosing-pair)))
    (when pair
      ;; :beg is at the pair
      ;; :end is outside the pair
      (let ((beg (plist-get pair :beg))
            (end (1- (plist-get pair :end))))
        (cond ((eq arg :around)
               (goto-char end)
               (hx-set-mark beg))
              ((eq arg :inside)
               (goto-char (1- end))
               ;; only select if there's content
               (if (< (1+ beg) (1- end))
                   (hx-set-mark (1+ beg))
                 (hx-set-mark nil))))))))

(defconst
  hx-matching-char-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?< . ?>)
    (?{ . ?}))
  "Alist of matching chars.")

(defun hx-match-surround-add (char)
  "Surround current selection with matching CHAR pair."
  (let ((pair (or (assq char hx-matching-char-alist)
                  (rassq char hx-matching-char-alist)
                  (cons char char)))
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

(defun hx-match-surround-replace (char)
  "Replace surrounding matching pair around current selection with CHAR pair."
  (let* ((pair (or (assq char hx-matching-char-alist)
                  (rassq char hx-matching-char-alist)
                  (cons char char)))
         (region (hx-region))
         (left (char-after (car region)))
         (right (char-before (cdr region))))
    (if (not (or (eq right (cdr (assq left hx-matching-char-alist)))
                 (eq right left)))
        (message "Current region not surrounded by matching pair")
      (goto-char (car region))
      (delete-char 1)
      (insert (car pair))
      (goto-char (cdr region))
      (delete-char -1)
      (insert (cdr pair))
      (backward-char 1))))

(defun hx-match-surround-delete ()
  "Delete surrounding matching pair around current selection."
  (let* ((region (hx-region))
         (left (char-after (car region)))
         (right (char-before (cdr region))))
    (if (not (or (eq right (cdr (assq left hx-matching-char-alist)))
                 (eq right left)))
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
  (message "hiding")
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
  (cond
   ((eq major-mode 'org-mode) (org-cycle))
   ((eq major-mode 'magit-status-mode) (call-interactively #'magit-section-toggle))))

(defun hx-previous ()
  "Previous in context."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-previous-visible-heading 1))
   ((eq major-mode 'magit-status-mode) (magit-section-backward))
   (t (combobulate-navigate-logical-previous))))

(defun hx-next ()
  "Next in context."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-next-visible-heading 1))
   ((eq major-mode 'magit-status-mode) (magit-section-forward))
   (t (combobulate-navigate-logical-next))))

(defun hx-previous-sibling ()
  "Previous sibling in context."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-backward-heading-same-level 1))
   ((eq major-mode 'magit-status-mode) (magit-section-backward-sibling))
   (t (combobulate-navigate-previous))))

(defun hx-next-sibling ()
  "Next sibling in context."
  (interactive)
  (cond
   ((eq major-mode 'org-mode) (org-forward-heading-same-level 1))
   ((eq major-mode 'magit-status-mode) (magit-section-forward-sibling))
   (t (combobulate-navigate-next))))

