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

; [WIP] record changes
(modaled-define-local-var hx--last-change nil
  "Recorded last change.")
(defun hx-record-change (command)
  "Record change and return the COMMAND."
  (setq hx--last-change command)
  command)
(defun hx-repeat-change ()
  "Repeat last change."
  (interactive)
  (when hx--last-change
    (funcall hx--last-change)
    (setq hx--last-change nil)))

(defun hx-no-sel ()
  "Clear selection and disable highlight for it."
  (interactive)
  (hx-unhighlight)
  (hx-set-mark nil))

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
:save          Save and restore hx state after the command (point, mark, ...)
:let ARGS      Bind variables in `let' macro using pairs.
:region        Set native region to `hx-region' for body forms.
:re-hl         Re-highlight selection.
:re-sel        Update selection based on modaled state.
               Re-highlight if in SELECT state or clear selection otherwise.
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
         (lambda-args (if arg-desc '(&rest l-args) '()))
         (arg-def (car (plist-get opts :arg)))
         (let-bindings (or (plist-get opts :let) '()))
         (eval-forms (mapcar (lambda (f)
                               (if (listp f)
                                   f
                                 `(call-interactively #',f)))
                             (plist-get opts :eval)))
         (save-wrapper (lambda (f)
                         (if (not (plist-member opts :save)) f
                           `(let ((cur (point))
                                  (pos (hx-mark-pos)))
                              ,f
                              (hx-set-mark pos)
                              (goto-char cur)))))
         (region-wrapper (lambda (f)
                           (if (not (plist-member opts :region)) f
                             ;; transient-mark-mode must be true
                             ;; for region-active-p to be true
                             `(let ((transient-mark-mode t))
                                (set-mark hx--mark)
                                ,f
                                (set-mark nil)))))
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
       (let ((arg ,(when arg-def
                     `(apply #',(pcase (car arg-def)
                                  ('c #'read-char)
                                  ('s #'read-from-minibuffer)
                                  ('b #'y-or-n-p)
                                  ('B #'yes-or-no-p)
                                  (_ (error
                                      (message "Invalid hx arg def: %s" arg-def))))
                             ',(cdr arg-def)))))
         (let ,let-bindings
           ,(funcall
             (-compose save-wrapper
                       region-wrapper
                       re-hl-wrapper
                       re-sel-wrapper)
             ;; need to catch error to let wrappers finish completely
             `(condition-case err
                  (progn ,@eval-forms)
                (error
                 (message "%s" (error-message-string err))))))))))

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
   ('magit-status-mode (magit-section-toggle))
   ('dired-sidebar-mode (dired-sidebar-subtree-toggle))))


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
    (,(kbd "M-/") . ("search lines" . occur))))

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
  :states '("major" "normal" "select")
  :bind
  ;; movement
  `(("j" . ("left" . ,(hx :re-sel :eval backward-char)))
    (";" . ("right" . ,(hx :re-sel :eval forward-char)))
    ("l" . ("up" . ,(hx :re-sel :eval previous-line)))
    ("k" . ("down" . ,(hx :re-sel :eval next-line)))
    ("w" . ("next word" . ,(hx :re-hl :eval (hx-next-word (equal modaled-state "normal")))))
    ("b" . ("prev word" . ,(hx :re-hl :eval (hx-previous-word (equal modaled-state "normal")))))
    ("f" . ("find next char" . ,(hx :arg (c "Find: ") :re-hl :eval (hx-find-char arg +1 -1 (equal modaled-state "normal")))))
    ("t" . ("find till next char" . ,(hx :arg (c "Till: ") :re-hl :eval (hx-find-char arg +1 -2 (equal modaled-state "normal")))))
    ("F" . ("find prev char" . ,(hx :arg (c "Find prev: ") :re-hl :eval (hx-find-char arg -1 0 (equal modaled-state "normal")))))
    ("T" . ("find till prev char" . ,(hx :arg (c "Till prev: ") :re-hl :eval (hx-find-char arg -1 1 (equal modaled-state "normal")))))
    (,(kbd "C-u") . ("scroll up" . ,(hx :re-sel :eval (funcall-interactively #'previous-line 10))))
    (,(kbd "C-d") . ("scroll down" . ,(hx :re-sel :eval (funcall-interactively #'next-line 10))))
    ;; goto mode
    ("gg" . ("go to line" . ,(hx :arg-desc "p" :re-sel :eval (forward-line (- (car l-args) (line-number-at-pos))))))
    ("ge" . ("end of file" . ,(hx :re-sel :eval (goto-char (point-max)))))
    ("gj" . ("start of line" . ,(hx :re-sel :eval beginning-of-line)))
    ("g;" . ("end of line" . ,(hx :re-sel :eval end-of-line (re-search-backward ".\\|^"))))  ; move to last char if exists
    ("gn" . ("next buffer" . centaur-tabs-forward))
    ("gp" . ("prev buffer" . centaur-tabs-backward))
    ("gN" . ("next buffer" . centaur-tabs-forward-group))
    ("gP" . ("prev buffer" . centaur-tabs-backward-group))
    ("gd" . ("go to def" . ,(hx :let (xref-prompt-for-identifier nil) :eval xref-find-definitions)))
    ;; match mode
    ("mm" . ("match any char" . ,(hx :re-sel :eval hx-match-char)))
    ("mb" . ("match bracket" . ,(hx :re-sel :eval (hx-match-char 'bracket))))
    ("mq" . ("match quote" . ,(hx :re-sel :eval (hx-match-char 'quote))))
    ("mt" . ("match tag" . ,(hx :re-sel :eval hx-match-tag)))
    ("msm" . ("select any pair" . ,(hx :re-hl :eval (hx-match-select))))
    ("msp" . ("select bracket" . ,(hx :re-hl :eval (hx-match-select 'bracket))))
    ("msq" . ("select quote" . ,(hx :re-hl :eval (hx-match-select 'quote))))
    ("ma" . ("surround with pair" . ,(hx :arg (c "Surround: ") :re-hl :eval modaled-set-main-state (hx-match-add arg))))
    ("mrc" . ("replace surrounding char" . ,(hx :arg (c "Surround replace: ") :re-hl :eval modaled-set-main-state (hx-match-replace arg))))
    ("mrt" . ("rename jsx tag" . ,(hx :re-hl :eval modaled-set-main-state jtsx-rename-jsx-element)))
    (,(kbd "M-[") . ("jump forward" . xref-go-forward))
    (,(kbd "M-]") . ("jump backward" . xref-go-back))
    ;; changes
    ("i" . ("insert before" . ,(hx :eval hx-no-sel (modaled-set-state "insert"))))
    ("a" . ("insert after" . ,(hx :eval hx-no-sel (modaled-set-state "insert") forward-char)))
    ("I" . ("insert at start of line" . ,(hx :eval hx-no-sel (modaled-set-state "insert") back-to-indentation)))
    ("A" . ("insert at end of line" . ,(hx :eval hx-no-sel (modaled-set-state "insert") end-of-line)))
    ("o" . ("insert below" . ,(hx :eval hx-no-sel (modaled-set-state "insert") end-of-line newline-and-indent)))
    ("O" . ("insert above" . ,(hx :eval hx-no-sel (modaled-set-state "insert") beginning-of-line newline-and-indent (forward-line -1) indent-according-to-mode)))
    ("r" . ("replace" . ,(hx :arg (c "Replace: ") :eval modaled-set-main-state (hx-region-replace arg) hx-no-sel)))
    ("y" . ("copy" . ,(hx :eval modaled-set-main-state (hx-region-apply #'kill-ring-save))))
    ("d" . ("delete" . ,(hx :eval modaled-set-main-state (hx-region-apply #'delete-region) hx-no-sel)))
    ("c" . ("change" . ,(hx :eval (modaled-set-state "insert") (hx-region-apply #'delete-region) hx-no-sel)))
    ("P" . ("paste before" . ,(hx :eval (hx-paste (current-kill 0 t) -1) hx-no-sel)))
    ("p" . ("paste after" . ,(hx :eval (hx-paste (current-kill 0 t) +1) hx-no-sel)))
    (,(kbd "M-P") . ("paste before from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") -1) hx-no-sel)))
    (,(kbd "M-p") . ("paste after from kill-ring)" . ,(hx :eval (hx-paste (read-from-kill-ring "To paste: ") +1) hx-no-sel)))
    ("J" . ("join lines" . hx-join-lines))
    ("u" . ("undo" . ,(hx :eval hx-no-sel undo)))
    ("U" . ("redo" . ,(hx :eval hx-no-sel undo-redo)))
    (">" . ("indent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly 2))))
    ("<" . ("unindent" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-rigidly -2))))
    ("." . ("repeat changes" . hx-repeat-change))
    ("=" . ("format" . ,(hx :re-hl :eval (hx-extended-region-apply #'indent-region))))
    (,(kbd "C-c") . ("comment/uncomment" . ,(hx :re-hl :eval (hx-extended-region-apply #'comment-or-uncomment-region))))
    ;; selection
    ("x" . ("extend line below" . ,(hx :re-hl :eval hx-extend-line-below)))
    ("X" . ("extend line" . ,(hx :re-hl :eval hx-extend-to-line-bounds)))
    ("e" . ("extend one char" . ,(hx :re-hl :eval hx-extend-char)))
    ("E" . ("shrink one char" . ,(hx :re-hl :eval hx-shrink-char)))
    ;; space mode
    (" P" . ("clipboard paste before" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) -1) hx-no-sel)))
    (" p" . ("clipboard paste after" . ,(hx :eval (hx-paste (xclip-get-selection 'clipboard) +1) hx-no-sel)))
    (" y" . ("copy to clipboard" . ,(hx :eval modaled-set-main-state (xclip-set-selection 'clipboard (hx-region-string)))))
    (" f" . ("find file (projectile)" . projectile-find-file))
    (" F" . ("find file (dired)" . find-file))
    (" b" . ("switch to buffer" . switch-to-buffer))
    (" d" . ("directory tree" . dired-sidebar-toggle-sidebar))
    (" ?" . ("search symbol" . apropos))
    (" k" . ("show eldoc" . hx-show-eldoc))
    ;; multiple cursors
    (" c" . ("toggle multiple-cursors-mode" . hx-toggle-multiple-cursors))
    (" C" . ("remove all cursors" . hx-remove-cursors))
    ("C" . ("add cursor and move next line" . ,(hx :eval hx-add-cursor next-line)))
    (,(kbd "M-C") . ("Add cursor and move prev line" . ,(hx :eval hx-add-cursor previous-line)))
    (,(kbd "M-c") . ("toggle a cursor at point" . hx-toggle-cursor))
    ;; gtd
    (" tl" . ("gtd list" . org-todo-list))
    (" ti" . ("gtd inbox" . ,(hx :eval (find-file (gtd-file "inbox.org")))))
    (" ta" . ("gtd actions" . ,(hx :eval (find-file (gtd-file "actions.org")))))
    (" tc" . ("gtd capture" . ,(hx :eval (org-capture nil "ti"))))
    ;; notes (org-roam & xeft)
    (" nf" . ("note find" . org-roam-node-find))
    (" ns" . ("note search" . xeft))
    (" nc" . ("note create" . org-roam-capture))
    (" ni" . ("note insert" . org-roam-node-insert))
    (" nl" . ("note backlinks" . org-roam-buffer-toggle))
    ;; LSP
    (" lr" . ("rename (LSP)" . eglot-rename))
    (" l=" . ("format (LSP)" . eglot-format-buffer))
    (" la" . ("action (LSP)" . eglot-code-actions))
    (" lf" . ("quickfix (LSP)" . eglot-code-action-quickfix))
    ;; git
    (" go" . ("open magit" . magit-status))
    (" gd" . ("show diff" . vc-diff))
    ;; transformation mode (backtick)
    ("`cu" . ("upper case" . ,(hx :eval (hx-region-apply #'upcase-region))))
    ("`cl" . ("lower case" . ,(hx :eval (hx-region-apply #'downcase-region))))
    ("`cc" . ("capitalized case" . ,(hx :eval (hx-region-apply #'capitalize-region))))
    ("`sl" . ("sort lines" . ,(hx :arg (b "Ascending?") :save :eval (hx-region-apply (-partial #'sort-lines (not arg))))))
    ("`sc" . ("sort columns" . ,(hx :arg (b "Ascending?") :save :eval (hx-region-apply (-partial #'sort-columns (not arg))))))
    ;; structural moving/editing
    ("sj" . ("prev" . hx-struct-prev))
    ("s;" . ("next" . hx-struct-next))
    ("ssj" . ("backward" . hx-struct-backward))
    ("ss;" . ("forward" . hx-struct-forward))
    ("sl" . ("up in hierarchy (TS)" . hx-struct-up))
    ("sk" . ("down into hierarchy (TS)" . hx-struct-down))
    ("sdj" . ("drag backward in sibling (TS)" . hx-struct-drag-backward))
    ("sd;" . ("drag forward in sibling (TS)" . hx-struct-drag-forward))
    ("sdl" . ("drag up in hierarchy (TS)" . hx-struct-drag-up))
    ("sdk" . ("drag down in hierarchy (TS)" . hx-struct-drag-down))
    ("sW" . ("wrap struct (TS)" . hx-struct-wrap))
    ("sD" . ("delete struct (TS)" . hx-struct-delete))
    ("sej" . ("prev error" . ,(hx :eval (flymake-goto-prev-error nil '(:error :warning) t))))
    ("se;" . ("next error" . ,(hx :eval (flymake-goto-next-error nil '(:error :warning) t))))
    ;; misc
    ;; note: TAB is the same as C-i in terminal
    ((,(kbd "TAB") ,(kbd "<tab>")) . ("toggle visibility" . hx-toggle-visibility))
    ("!" . ("run shell command" . shell-command))
    ("|" . ("eval expr" . eval-expression))
    ("\\" . ("eval region and print" . ,(hx :eval (hx-region-apply #'eval-region t))))
    (,(kbd "M-\\") . ("eval region" . ,(hx :eval (hx-region-apply #'eval-region))))
    (":" . ("run command" . execute-extended-command))
    ("q" . ("quit window" . quit-window))
    ("Q" . ("kill buffer" . kill-this-buffer))
    ;; major-mode specific command
    ("'x" . ,(hx :region :let (command (lookup-key (current-local-map) (kbd "C-c C-c")))
               :eval (when command (call-interactively command))))))

(modaled-define-keys
  :states '("insert")
  :bind
  `((,(kbd "C-w") . ("delete word backward" . hx-delete-word-backward))
    (,(kbd "M-i") . ("code suggestion (LSP)" . company-manual-begin))
    ;; set tempo-match-finder temporarily to prevent conflicts
    (,(kbd "M-t") . ("tempo complete" . ,(hx :let (tempo-match-finder my-tempo-match-finder) :eval tempo-complete-tag)))
    ;; this makes pasting work in GUI
    (,(kbd "C-S-v") . ("paste from clipboard" . ,(hx :eval (insert (xclip-get-selection 'clipboard)))))))
;; (add-hook
;;  'modaled-insert-state-mode-hook
;;  (lambda ()
;;    ;; box cursor shape for VTE-based terminal
;;    (unless (display-graphic-p)
;;      (send-string-to-terminal "\e[6 q"))))

;; major-mode specific keys
(modaled-define-keys
  :states '("major")
  :bind
  `(("i" . ("NORMAL state" . ,(hx :eval (modaled-set-state "normal"))))))

(defun hx-save ()
  "Save buffer or finish editing."
  (interactive)
  (call-interactively
   (cond
    ((bound-and-true-p org-capture-mode) #'org-capture-finalize)
    ((or (bound-and-true-p git-commit-mode)
         (eq major-mode 'git-rebase-mode))
     #'with-editor-finish)
    (t #'save-buffer))))

(defun hx-abort ()
  "Abort editing."
  (interactive)
  (call-interactively
   (cond
    ((bound-and-true-p org-capture-mode) #'org-capture-kill)
    ((or (bound-and-true-p git-commit-mode)
         (eq major-mode 'git-rebase-mode))
     #'with-editor-cancel)
    (t #'ignore))))

;; common keybindings for all states
(modaled-define-keys
  :states '("major" "normal" "select" "insert")
  :bind
  `(([escape] . ("main state" . ,(hx :eval modaled-set-main-state hx-format-blank-line hx-no-sel)))
    (,(kbd "M-SPC") . ("toggle vterm" . vterm-toggle))
    (,(kbd "M-h") . ("split horizontally" . ,(hx :eval split-window-horizontally other-window)))
    (,(kbd "M-v") . ("split vertically" . ,(hx :eval split-window-vertically other-window)))
    (,(kbd "M-q") . ("delete window" . delete-window))
    (,(kbd "M-j") . ("left window" . windmove-left))
    (,(kbd "M-;") . ("right window" . windmove-right))
    (,(kbd "M-l") . ("up window" . windmove-up))
    (,(kbd "M-k") . ("down window" . windmove-down))
    (,(kbd "C-s") . ("save" . hx-save))
    (,(kbd "C-a") . ("abort" . hx-abort))
    (,(kbd "C-q") . ("quit" . save-buffers-kill-terminal))))

(modaled-define-keys
  :global t
  :bind
  `(([escape] . ("keyboard quit" . keyboard-escape-quit))
    ;; only works in GUI as C-= and C-- are managed by terminal emulator otherwise
    (,(kbd "C-=") . ("scale increase" . text-scale-increase))
    (,(kbd "C--") . ("scale decrease" . text-scale-decrease))
    ;; unset C-u for it to be used in vterm
    (,(kbd "C-u") . nil)))

(setq modaled-init-state-fn
      (lambda ()
        (cond
         ((memq major-mode '(dired-mode))
          "major")
         ((memq major-mode '(vterm-mode xeft-mode))
          "insert")
         (t "normal"))))
(setq modaled-main-state-fn
      (lambda ()
        (cond
         ((memq major-mode '(dired-mode))
          "major")
         (t "normal"))))

(modaled-setup)

;; translate terminal \\e to [escape]
(hx-esc-mode 1)

