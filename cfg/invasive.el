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

(defun my-org-modern-pill-overlay-face (position face)
  (let ((box (copy-tree (face-attribute 'org-modern-label :box nil t))))
    (setf (plist-get box :color)
          (my-org-modern-pill-padding-background position))
    (cons `(:box ,box) (ensure-list face))))

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
  (when (bound-and-true-p org-modern-mode)
    (my-org-modern-pill-overlays beg end)))

(defun my-org-modern-refresh-pill-overlays (&rest _)
  (when (derived-mode-p 'org-mode)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'my-org-modern-pill-overlay)
        (overlay-put overlay 'face
                     (my-org-modern-pill-overlay-face
                      (overlay-start overlay)
                      (get-text-property (overlay-start overlay) 'face)))))))

(advice-add 'font-lock-fontify-region :after
            #'my-org-modern-fontify-pill-overlays)
(advice-add 'global-hl-line-highlight :after
            #'my-org-modern-refresh-pill-overlays)
(advice-add 'evil-visual-highlight :after
            #'my-org-modern-refresh-pill-overlays)
; }}}
