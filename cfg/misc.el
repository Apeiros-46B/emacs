; vim:et

; file backups
(setq
  ; backup files in (emacs config path)/cache/backups
  backup-directory-alist `(("." . ,(get-cfg-path "cache/backups")))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 5
  kept-old-versions 2
  version-control t)

; no blink
(blink-cursor-mode 0)

; wrapping
(setq-default fill-column 88)
(global-visual-line-mode t)

; dabbrev
(setq dabbrev-check-all-buffers nil)
(setq dabbrev-check-other-buffers nil)
(defkm '(normal insert) "M-;" 'dabbrev-expand)

; scrolling
(setq scroll-conservatively most-positive-fixnum) ; one line at a time
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-defer-time 0)

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
