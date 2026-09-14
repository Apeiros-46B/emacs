; vim:et

; for g; and g, motions and the . register
(use-package goto-chg)

; main package
(use-package evil
  :init
    (setq evil-want-C-u-scroll t
          evil-want-integration t
          evil-want-keybinding nil
          evil-search-module 'evil-search
          evil-ex-search-highlight-all t)

    (setq select-enable-clipboard nil
          select-enable-primary nil)

  :config
    ; {{{ behavior
    (defun my-copy-to-system-clipboard (text)
      "Copy TEXT to the system clipboard without changing the kill ring."
      (gui-set-selection 'CLIPBOARD text))

    (defun my-paste-from-system-clipboard ()
      "Insert text from the system clipboard without using the kill ring."
      (interactive)
      (let ((text (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
                      (ignore-errors (gui-get-selection 'CLIPBOARD 'STRING)))))
        (unless (stringp text) (user-error "System clipboard does not contain text"))
        (insert text)))

    (defun my-paste-from-system-clipboard-after-point ()
      "Insert text from the system clipboard after the Evil cursor."
      (interactive)
      (unless (eolp) (forward-char 1))
      (my-paste-from-system-clipboard))

    (evil-define-operator my-evil-yank-to-system-clipboard (beg end type)
      "Copy the Evil range from BEG to END to the system clipboard."
      :move-point nil
      :repeat nil
      (interactive "<R>")
      (let ((text
             (pcase type
               ((or 'line 'screen-line)
                (let ((text (filter-buffer-substring beg end)))
                  (if (or (zerop (length text))
                          (= (aref text (1- (length text))) ?\n))
                      text
                    (concat text "\n"))))
               ('block
                (let ((lines (list nil)))
                  (evil-apply-on-rectangle #'extract-rectangle-line
                                           beg end lines)
                  (mapconcat #'identity (nreverse (cdr lines)) "\n")))
               (_ (filter-buffer-substring beg end)))))
        (my-copy-to-system-clipboard text)))

    (evil-define-motion my-evil-visual-next-line (count)
      :type exclusive
      (if (eq evil-visual-selection 'char)
          (evil-next-visual-line count)
        (evil-next-line count)))
    (evil-define-motion my-evil-visual-previous-line (count)
      :type exclusive
      (if (eq evil-visual-selection 'char)
          (evil-previous-visual-line count)
        (evil-previous-line count)))

    ; screen line movement outside operator-pending
    (defkm :states 'insert "<down>" #'evil-next-visual-line)
    (defkm :states 'insert "<up>" #'evil-previous-visual-line)
    (defkm :states 'normal "j" #'evil-next-visual-line)
    (defkm :states 'normal "k" #'evil-previous-visual-line)
    (defkm :states 'visual "j" #'my-evil-visual-next-line)
    (defkm :states 'visual "k" #'my-evil-visual-previous-line)

    (defkm :states 'insert "C-S-v" #'my-paste-from-system-clipboard)
    (defkm :states 'normal "C-S-v" #'my-paste-from-system-clipboard-after-point)
    (ldr-defkm "y" #'my-evil-yank-to-system-clipboard)
    (define-key minibuffer-local-map (kbd "C-S-v") #'my-paste-from-system-clipboard)

    ; replace lost C-u
    (defkm "M-u" #'universal-argument)

    ; acts like :nmap cc :nohlsearch<CR> while preserving c{motion}
    (defun my-evil-c ()
      (interactive)
      (let ((event (read-event)))
        (if (eq event ?c)
            (evil-ex-nohighlight)
          (setq unread-command-events
                (append (list event) unread-command-events))
          ; `evil-change' derives operator from `this-command', preserve when delegating so
          ; compound changes retain their original range
          (condition-case nil
              (let ((this-command 'evil-change)
                    (real-this-command 'evil-change))
                (call-interactively #'evil-change))
            (error nil)))))

    (defkm :states 'normal "c" #'my-evil-c)

    ; undo system
    (evil-set-undo-system 'undo-redo)

    ; fix keybinds in agenda
    (evil-set-initial-state 'org-agenda-mode 'normal)

    ; 2 spc indentation
    (setq evil-shift-width 2)
    ; }}}

    ; {{{ style
    ; :set noshowmode
    (setq evil-insert-state-message nil
          evil-visual-state-message nil
          evil-replace-state-message nil)

    ; state indicator via cursor shape & color
    (setq evil-emacs-state-cursor    `(box        ,(getcol 'red   ))
          evil-normal-state-cursor   `( box       ,(getcol 'fg1   ))
          evil-insert-state-cursor   `((bar . 2)  ,(getcol 'blue  ))
          evil-visual-state-cursor   `( box       ,(getcol 'purple))
          evil-motion-state-cursor   `( box       ,(getcol 'fg2   ))
          evil-replace-state-cursor  `((hbar . 2) ,(getcol 'red   ))
          evil-operator-state-cursor `((hbar . 2) ,(getcol 'green )))

    (custom-set-faces
      `(evil-ex-info ((t (:foreground ,(getcol 'blue) :weight bold))))
      `(evil-ex-search ((t (:inherit isearch))))
      `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))
      `(evil-ex-substitute-matches ((t (:background ,(getcol 'bg-red) :foreground ,(getcol 'red)))))
      `(evil-ex-substitute-replacement ((t (:background ,(getcol 'bg-green) :foreground ,(getcol 'green))))))
    ; }}}

    (evil-mode 1)

  :custom
    (evil-move-cursor-back t))

; vim's C-a and C-x
(use-package evil-numbers
  :commands evil-numbers/inc-at-pt evil-numbers/dec-at-pt

  :init
    (defkm :states '(normal visual) "C-a"   'evil-numbers/inc-at-pt)
    (defkm :states '(normal visual) "C-S-a" 'evil-numbers/dec-at-pt))

; bindings for misc things
(use-package evil-collection
  :config (evil-collection-init))

; bindings for org-mode
(use-package evil-org
  :defer t
  :commands evil-org-mode

  :hook
    (org-mode . evil-org-mode)
    (org-agenda-mode . evil-org-mode)

  :config
    (setq evil-org-special-o/O nil)
    (evil-define-key '(normal insert) 'evil-org-mode
      (kbd "<C-return>") (lambda ()
        (interactive)
        (org-insert-item (org-at-item-checkbox-p)))
      (kbd "<M-return>") #'org-ctrl-c-ret)

    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

; surround
(use-package evil-surround
  :config (global-evil-surround-mode 1))

; avy (not really evil, but close enough)
(use-package avy
  :config
    (defkm :states 'normal "s" #'avy-goto-char-2)
    (defkm :states 'visual "s" #'evil-change)
    (custom-set-faces
      `(avy-lead-face   ((t (:background ,(getcol 'aqua) :foreground ,(getcol 'bg1)))))
      `(avy-lead-face-0 ((t (:background ,(getcol 'bg-aqua) :foreground ,(getcol 'aqua)))))
      `(avy-lead-face-1 ((t (:background ,(getcol 'bg-green) :foreground ,(getcol 'green)))))
      `(avy-lead-face-2 ((t (:background ,(getcol 'bg4) :foreground ,(getcol 'fg2)))))))
