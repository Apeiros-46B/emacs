; vim:et
; pure slop but it seems to work

; {{{ highlight extension helpers
(defun my-highlight-margin-prefix (side width face)
  (propertize (make-string width ?\s)
              'display `(margin ,side)
              'face face))

(defun my-highlight-prefix (property face)
  (let* ((prefix (copy-sequence
                  (or (get-text-property (line-beginning-position) property)
                      "")))
         (margins (window-margins)))
    (add-face-text-property 0 (length prefix) face 'append prefix)
    (concat (my-highlight-margin-prefix 'left-margin (or (car margins) 0) face)
            prefix
            (my-highlight-margin-prefix 'right-margin (or (cdr margins) 0) face))))
; }}}

; {{{ hl-line, extend to frame edges
(require 'hl-line)
(setq global-hl-line-sticky-flag nil)
(set-face-attribute 'hl-line nil :background (getcol 'bg-cur) :extend t)

(defun my-hl-line-prefix (property)
  (my-highlight-prefix property 'hl-line))

(defun my-global-hl-line-prefixes (&rest _)
  (when (overlayp global-hl-line-overlay)
    (overlay-put global-hl-line-overlay 'line-prefix
                 (my-hl-line-prefix 'line-prefix))
    (overlay-put global-hl-line-overlay 'wrap-prefix
                 (my-hl-line-prefix 'wrap-prefix))))

(advice-add 'global-hl-line-highlight :after #'my-global-hl-line-prefixes)
(global-hl-line-mode 1)
; }}}

; {{{ evil linewise selections extended to frame edges
(defvar-local my-evil-visual-line-overlays nil)

(defun my-evil-visual-line-prefixes (&rest _)
  (mapc #'delete-overlay my-evil-visual-line-overlays)
  (setq my-evil-visual-line-overlays nil)
  (when (and (evil-visual-state-p)
             (eq evil-visual-selection 'line))
    (save-excursion
      (goto-char evil-visual-beginning)
      (while (< (point) evil-visual-end)
        (let ((overlay (make-overlay (line-beginning-position)
                                     (min (1+ (line-end-position)) (point-max)))))
          (overlay-put overlay 'priority 100)
          (overlay-put overlay 'line-prefix
                       (my-highlight-prefix 'line-prefix 'region))
          (overlay-put overlay 'wrap-prefix
                       (my-highlight-prefix 'wrap-prefix 'region))
          (push overlay my-evil-visual-line-overlays))
        (forward-line 1)))))

(advice-add 'evil-visual-highlight :after #'my-evil-visual-line-prefixes)
; }}}

; {{{ org-modern pill overlays on top of line highlights
; TODO: frontmatter when hl-line or visual
(defconst my-org-modern-pill-faces
  '(org-modern-date-active
    org-modern-date-inactive
    org-modern-done
    org-modern-internal-target
    org-modern-label
    org-modern-priority
    org-modern-radio-target
    org-modern-tag
    org-modern-time-active
    org-modern-time-inactive
    org-modern-todo))

(defun my-org-modern-pill-padding-background (position)
  (cond
   ((and (evil-visual-state-p)
         (or (and (overlayp evil-visual-overlay)
                  (<= (overlay-start evil-visual-overlay) position)
                  (< position (overlay-end evil-visual-overlay)))
             (seq-some (lambda (overlay)
                         (and (<= (overlay-start overlay) position)
                              (< position (overlay-end overlay))))
                       evil-visual-block-overlays)))
    (face-attribute 'region :background nil t))
   ((and (overlayp global-hl-line-overlay)
         (eq (overlay-buffer global-hl-line-overlay) (current-buffer))
         (<= (overlay-start global-hl-line-overlay) position)
         (< position (overlay-end global-hl-line-overlay)))
    (face-attribute 'hl-line :background nil t))
   (t (face-attribute 'default :background nil t))))

(defun my-org-modern-pill-buffer-p ()
  "Return non-nil when the current buffer can contain org-modern pills."
  (or (bound-and-true-p org-modern-mode)
      (derived-mode-p 'org-agenda-mode)))

(defun my-org-modern-pill-face (face)
  "Return the org-modern pill face contained in FACE."
  (seq-find (lambda (item) (memq item my-org-modern-pill-faces))
            (ensure-list face)))

(defun my-org-modern-pill-inline-attribute (face attribute)
  "Return ATTRIBUTE from an inline face specification in FACE, if any."
  (seq-some (lambda (item)
              (when (and (listp item) (keywordp (car item)))
                (plist-get item attribute)))
            (ensure-list face)))

(defun my-org-modern-pill-overlay-face (position face)
  (let* ((pill-face (or (my-org-modern-pill-face face) 'org-modern-label))
         (box (copy-tree (face-attribute 'org-modern-label :box nil t)))
         ;; `org-modern-todo-faces' produces inline face specifications,
         ;; followed by `org-modern-label'.  The inline values must win.
         (foreground (or (my-org-modern-pill-inline-attribute face :foreground)
                         (face-attribute pill-face :foreground nil t)))
         (background (or (my-org-modern-pill-inline-attribute face :background)
                         (face-attribute pill-face :background nil t))))
    (setf (plist-get box :color)
          (my-org-modern-pill-padding-background position))
    ;; An Org keyword/frontmatter face can otherwise override the colours
    ;; inherited by FACE.  Put the resolved colours in the overlay itself,
    ;; while leaving height and the other face attributes untouched.
    (cons `(:box ,box
            :foreground ,foreground
            :background ,background)
          (ensure-list face))))

(defun my-org-modern-pill-face-p (face)
  (seq-some (lambda (item)
              (memq item my-org-modern-pill-faces))
            (ensure-list face)))

(defun my-org-modern-pill-overlays (beg end)
  (dolist (overlay (overlays-in beg end))
    (when (overlay-get overlay 'my-org-modern-pill-overlay)
      (delete-overlay overlay)))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let* ((face (get-text-property (point) 'face))
             (next (next-single-property-change (point) 'face nil end)))
        (when (my-org-modern-pill-face-p face)
          (let ((overlay (make-overlay (point) next)))
            (overlay-put overlay 'my-org-modern-pill-overlay t)
            (overlay-put overlay 'modification-hooks
                         '(my-org-modern-pill-overlay-modified))
            (overlay-put overlay 'priority 100)
            (overlay-put overlay 'face
                         (my-org-modern-pill-overlay-face (point) face))))
        (goto-char next)))))

(defun my-org-modern-pill-overlay-modified (overlay after &rest _)
  (when after
    (delete-overlay overlay)))

(defun my-org-modern-fontify-pill-overlays (beg end &rest _)
  (when (my-org-modern-pill-buffer-p)
    (my-org-modern-pill-overlays beg end)))

(defun my-org-modern-refontify-pills (&optional buffer)
  "Immediately fontify BUFFER and rebuild its org-modern pill overlays."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (when (my-org-modern-pill-buffer-p)
        ;; Agenda buffers are read-only, and their text properties are part
        ;; of the agenda display contract.  They only need their overlays
        ;; rebuilt after `org-modern-agenda' has set the base faces.
        (unless (derived-mode-p 'org-agenda-mode)
          (font-lock-flush (point-min) (point-max))
          (font-lock-ensure (point-min) (point-max)))
        (my-org-modern-pill-overlays (point-min) (point-max))))))

(defun my-org-modern-refontify-timestamp (&rest _)
  (my-org-modern-refontify-pills))

(defun my-org-modern-refresh-pill-overlays (&rest _)
  (when (my-org-modern-pill-buffer-p)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'my-org-modern-pill-overlay)
        (overlay-put overlay 'face
                     (my-org-modern-pill-overlay-face
                      (overlay-start overlay)
                      (get-text-property (overlay-start overlay) 'face)))))))

(advice-add 'font-lock-fontify-region :after
            #'my-org-modern-fontify-pill-overlays)
(advice-add 'org-time-stamp :after #'my-org-modern-refontify-timestamp)
(add-hook 'org-capture-mode-hook #'my-org-modern-refontify-pills)
(add-hook 'org-agenda-finalize-hook #'my-org-modern-refontify-pills t)
(advice-add 'global-hl-line-highlight :after
            #'my-org-modern-refresh-pill-overlays)
(advice-add 'evil-visual-highlight :after
            #'my-org-modern-refresh-pill-overlays)
; }}}
