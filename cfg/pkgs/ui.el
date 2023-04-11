; THEME
(use-package nano-theme
  :commands nano-dark nano-light
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))

; {{{ define custom colors
(setq frame-background-mode 'dark)

(setq nano-dark-foreground (getcol 'fg1)
      nano-dark-background (getcol 'bg1)
      nano-dark-highlight  (getcol 'bg3)
      nano-dark-critical   (getcol 'red)
      nano-dark-salient    (getcol 'green)
      nano-dark-strong     (getcol 'fg1)
      nano-dark-popout     (getcol 'blue)
      nano-dark-subtle     (getcol 'bg2)
      nano-dark-faded      (getcol 'fg2))
; }}}

; {{{ font settings
(setq nano-fonts-use t)
(custom-set-faces
  '(nano-mono     ((t (:family "JetBrainsMono Nerd Font Mono" :height 130 :weight normal))))
  '(nano-mono-alt ((t (:inherit nano-mono))))
  '(nano-sans     ((t (:inherit nano-mono))))
  '(nano-serif    ((t (:inherit nano-mono))))
  '(nano-italic   ((t (:inherit nano-mono)))))
; }}}

(nano-dark)

; {{{ layout
; frame settings
(setq frame-resize-pixelwise t)
(setq default-frame-alist
  (append (list
    '(min-height . 1)
    '(height     . 40)
    '(min-width  . 1)
    '(width      . 80)
    '(vertical-scroll-bars . nil)
    '(internal-border-width . 20)
    '(left-fringe    . 0)
    '(right-fringe   . 0)
    '(tool-bar-lines . 0)
    '(menu-bar-lines . 0))))

; no ugly checkbox button
(setq widget-image-enable nil)
; }}}

; {{{ options
; no stuff on startup
(setq inhibit-startup-screen            t)
(setq inhibit-startup-message           t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message           nil)
(setq initial-buffer-choice             nil)

; frame title
(setq frame-title-format "emacs")

; no dialogs
(setq use-file-dialog nil)
(setq use-dialog-box  nil)
(setq pop-up-windows  nil)

; initial major mode
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)

; font lock
(setq font-lock-maximum-decoration nil)
(setq font-lock-maximum-size nil)

; utf-8
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

; misc
(setq indicate-empty-lines nil)
(setq cursor-in-non-selected-windows nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq completion-styles '(basic substring))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(fset 'yes-or-no-p 'y-or-n-p)

(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)
(setq window-min-height 1)
; }}}

; {{{ re-enable some font features
; bolds & italics
(set-face-attribute 'default nil :foreground (getcol 'fg1) :weight 'regular)
(set-face-attribute 'bold    nil :foreground (getcol 'fg1) :weight 'bold)
(set-face-attribute 'italic  nil :foreground (getcol 'fg1) :weight 'semilight :slant 'italic)
; }}}
; END THEME

; MODELINE
(use-package nano-modeline
  :custom
    ; {{{ custom options
    (nano-modeline-position 'top)
    (nano-modeline-space-top 0.0)
    (nano-modeline-space-bottom 0.0)
    (nano-modeline-prefix 'status)
    (nano-modeline-prefix-padding t)
    (nano-modeline-display-tab-number t)
    ; }}}

  :config
    ; {{{ override default modeline function
    (defun nano-modeline-default-mode (&optional icon)
      (let ((icon (or icon (plist-get (cdr (assoc 'text-mode nano-modeline-mode-formats)) :icon)))
          ; {{{ buffer name (taking into account narrowed bufs)
          (buffer-name (cond
                        ((and (derived-mode-p 'org-mode)
                              (buffer-narrowed-p)
                              (buffer-base-buffer))
                         (format"%s [%s]" (buffer-base-buffer)
                                (org-link-display-format
                                (substring-no-properties (or (org-get-heading 'no-tags)
                                                         "-")))))
                        ((and (buffer-narrowed-p)
                              (buffer-base-buffer))
                         (format"%s [narrow]" (buffer-base-buffer)))
                        (t
                         (format-mode-line "%b"))))
          ; }}}

          ; {{{ others
          (mode-name (nano-modeline-mode-name))
          (branch    (nano-modeline-vc-branch))
          (position  (format-mode-line "%l:%c")))
          ; }}}

        ; {{{ render modeline
        (nano-modeline-render
          icon
          buffer-name
          (concat "(" mode-name (if branch (concat ", " branch) "") ")")
          position)))
        ; }}}
    ; }}}

    ; {{{ custom faces
    (custom-set-faces
      ; {{{ main
      `(nano-modeline-active ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg1)))))
      `(nano-modeline-active-name ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg1) :weight bold))))
      `(nano-modeline-active-primary ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg1) :weight semilight))))
      `(nano-modeline-active-secondary ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg2) :weight normal))))

      `(nano-modeline-inactive ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg2)))))
      `(nano-modeline-inactive-name ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg2)))))
      `(nano-modeline-inactive-primary ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg2) :weight semilight))))
      `(nano-modeline-inactive-secondary ((t (:background ,(getcol 'bg2) :foreground ,(getcol 'fg2) :weight light))))
      ; }}}

      ; {{{ status component
      `(nano-modeline-active-status-RO ((t (:background ,(getcol 'fg2) :foreground ,(getcol 'bg1) :weight bold))))
      `(nano-modeline-active-status-RW ((t (:background ,(getcol 'fg2) :foreground ,(getcol 'bg1) :weight bold))))
      `(nano-modeline-active-status-** ((t (:background ,(getcol 'red) :foreground ,(getcol 'bg1) :weight bold))))

      `(nano-modeline-inactive-status-RO ((t (:background ,(getcol 'bg4)    :foreground ,(getcol 'fg2) :weight bold))))
      `(nano-modeline-inactive-status-RW ((t (:background ,(getcol 'bg4)    :foreground ,(getcol 'fg2) :weight bold))))
      `(nano-modeline-inactive-status-** ((t (:background ,(getcol 'bg-red) :foreground ,(getcol 'red) :weight bold)))))
      ; }}}
    ; }}}

    ; activate
    (nano-modeline-mode))
; END MODELINE
