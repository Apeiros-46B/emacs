; vim:et

; {{{ wrapping
(setq-default fill-column 88)
(global-visual-line-mode t)
(add-hook 'text-mode-hook #'auto-fill-mode) ; hard wrap
(ldr-defkm "Twh" 'auto-fill-mode)

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom (fill-column-enable-sensible-window-split t)
  :init (ldr-defkm "Tws" 'visual-fill-column-mode))
; }}}

; {{{ backup settings
(setq
  ; backup files in ~/emacs/config/path/backups
  backup-directory-alist `(("." . ,(concat (file-truename user-emacs-directory) "cache/backups")))
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

; don't add a newline on save
(setq require-final-newline nil)
