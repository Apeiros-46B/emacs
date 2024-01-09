; vim:et

; {{{ wrapping
(setq-default fill-column 88)
(global-visual-line-mode t)
(add-hook 'text-mode-hook 'auto-fill-mode) ; hard wrap
(ldr-defkm "Twh" 'auto-fill-mode)

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom (fill-column-enable-sensible-window-split t)
  :init (ldr-defkm "Tws" 'visual-fill-column-mode))
; }}}

; {{{ backup settings
(setq
  ; backup files in ~/emacs/config/path/backups
  backup-directory-alist `(("." . ,(get-cfg-path "cache/backups")))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)
; }}}

; no blink
(blink-cursor-mode 0)

; scroll one line at a time
(setq scroll-conservatively most-positive-fixnum)

; no startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq server-client-instructions nil)

; Monday is the start of the week.
(setq calendar-week-start-day 1)

; save command history
(savehist-mode 1)

; always use symmetric encryption
(setq epa-file-select-keys 'symmetric)

; use minibuffer for pinentry
(setq epa-pinentry-mode 'loopback)
