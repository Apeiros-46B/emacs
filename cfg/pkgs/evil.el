; for g; and g, motions and the . register
(use-package goto-chg)

; main package
(use-package evil
  :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)

  :config
    ; undo system
    (evil-set-undo-system 'undo-redo)

    ; fix keybinds in agenda
    (evil-set-initial-state 'org-agenda-mode 'normal)

    ; 2 spc indentation
    (setq evil-shift-width 2)

    ; :set noshowmode
    (setq evil-insert-state-message nil
          evil-visual-state-message nil
          evil-replace-state-message nil)

    ; {{{ state indicator via cursor shape & color
    (setq evil-emacs-state-cursor    `(box        ,(getcol 'red   ))
          evil-normal-state-cursor   `( box       ,(getcol 'fg1   ))
          evil-insert-state-cursor   `((bar . 2)  ,(getcol 'blue  ))
          evil-visual-state-cursor   `( box       ,(getcol 'purple))
          evil-motion-state-cursor   `( box       ,(getcol 'fg2   ))
          evil-replace-state-cursor  `((hbar . 2) ,(getcol 'red   ))
          evil-operator-state-cursor `((hbar . 2) ,(getcol 'green )))
    ; }}}

    ; activate
    (evil-mode 1)

    ; {{{ custom bindings
    (defkm 'insert "C-S-v" "C-r \""))
    ; }}}

; {{{ vim's C-a and C-x
(use-package evil-numbers
  :commands evil-numbers/inc-at-pt evil-numbers/dec-at-pt
  :init
    (defkm '(normal visual) "C-a"   'evil-numbers/inc-at-pt)
    (defkm '(normal visual) "C-S-a" 'evil-numbers/dec-at-pt))
; }}}

; bindings for misc things
(use-package evil-collection
  :config (evil-collection-init))

; bindings for org-mode
(use-package evil-org
  :after org
  :commands evil-org-mode

  :init
    ; :hook STILL doesn't work aaaa
    (add-hook 'org-mode-hook #'evil-org-mode)
    (add-hook 'org-agenda-mode-hook #'evil-org-mode)

  :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

; surround
(use-package evil-surround
  :config (global-evil-surround-mode 1))
