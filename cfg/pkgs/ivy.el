; vim:et

(use-package counsel
  :init
    ; {{{ custom keybindings
    (ldr-defkm "fb" 'counsel-switch-buffer)
    (ldr-defkm "fc" 'counsel-unicode-char)
    (ldr-defkm "ff" 'counsel-find-file)
    (ldr-defkm "fF" 'counsel-fzf)
    (ldr-defkm "fh" 'counsel-faces)
    (ldr-defkm "fr" 'counsel-recentf)
    (ldr-defkm "fw" 'counsel-rg))
    (ldr-defkm "f'" 'counsel-evil-marks)
    (ldr-defkm "f;" 'counsel-bookmark)

    ; org-mode
    (ldr-defkm "oa"    'counsel-org-entity)
    (ldr-defkm "oe"    'counsel-org-entity)
    (ldr-defkm "oo"    'counsel-org-goto)
    (ldr-defkm "oO"    'counsel-org-goto-all)
    (ldr-defkm "o SPC" 'counsel-org-tag)

    (ldr-defkm 'org-agenda-mode-map "o SPC" 'counsel-org-tag-agenda)
    ; }}}

(use-package smex)

(use-package ivy
  :init
    (setq enable-recursive-minibuffers t)

  :custom
    ; {{{ custom options
    (ivy-height 6)
    (ivy-count-format "")
    (ivy-initial-inputs-alist: '((counsel-minor . "^+")
                                 (counsel-package . "^+")
                                 (counsel-org-capture . "^")
                                 (counsel-M-x . "^")
                                 (counsel-refile . "")
                                 (org-agenda-refile . "")
                                 (org-capture-refile . "")
                                 (Man-completion-table . "^")
                                 (woman . "^")))
    (ivy-use-virtual-buffers nil)
    ; }}}

  :config
    ; {{{ custom faces
    (custom-set-faces
      `(ivy-minibuffer-match-face-1 ((t (:foreground ,(getcol 'green)))))
      `(ivy-minibuffer-match-face-2 ((t (:foreground ,(getcol 'green)))))
      `(ivy-minibuffer-match-face-3 ((t (:foreground ,(getcol 'green)))))
      `(ivy-minibuffer-match-face-4 ((t (:foreground ,(getcol 'green)))))

      `(ivy-current-match              ((t (:background ,(getcol 'bg-green) :foreground ,(getcol 'fg1)))))
      `(ivy-minibuffer-match-highlight ((t (:background ,(getcol 'bg-green) :foreground ,(getcol 'fg1)))))
      `(ivy-confirm-face               ((t (:background ,(getcol 'bg1) :foreground ,(getcol 'green)))))
      `(ivy-match-required-face        ((t (:background ,(getcol 'bg1) :foreground ,(getcol 'red))))))
    ; }}}

    ; activate
    (ivy-mode 1))
