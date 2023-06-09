; {{{ org
(use-package org
  :mode ("\\.org\\'" . org-mode)

  :init
    ; {{{ custom keymaps
    ; should always be available
    (ldr-defkm "a" 'org-agenda)
    (ldr-defkm "c" 'org-capture)

    ; magic
    (ldr-defkm 'org-mode-map "SPC" 'org-ctrl-c-ctrl-c)

    (ldr-defkm 'org-mode-map "op" 'org-priority)
    (ldr-defkm 'org-mode-map "ot" 'org-time-stamp)
    (ldr-defkm 'org-mode-map "ol" 'org-latex-preview)

    ; link creation
    (ldr-defkm "l" 'org-store-link)
    (ldr-defkm 'org-mode-map "L" 'org-insert-link)

    (defkm '(normal visual) 'org-mode-map "C-SPC" 'org-toggle-checkbox)
    (defkm '(normal visual) 'org-mode-map "gt" 'org-todo)
    (defkm 'normal 'org-mode-map "RET" 'org-open-at-point)

    ; folding/cycling
    (defkm 'normal 'org-mode-map "za" 'org-cycle)
    (defkm 'normal 'org-mode-map "zA" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zM" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zR" 'org-show-all)
    (defkm 'normal 'org-mode-map "zx" 'org-set-startup-visibility)

    ; promotion, demotion, and swapping
    (defkm 'insert 'org-mode-map "C-t" 'org-demote-subtree)
    (defkm 'insert 'org-mode-map "C-d" 'org-promote-subtree)

    (defkm 'insert 'org-mode-map "C-j" 'org-move-subtree-down)
    (defkm 'insert 'org-mode-map "C-k" 'org-move-subtree-up)

    ; misc
    (defkm 'insert 'org-mode-map "C-*" 'org-toggle-heading)
    (ldr-defkm 'normal 'org-mode-map "*" 'org-toggle-heading)
    ; }}}

  :custom
    ; {{{ custom options
    ; {{{ functionality
    (org-dir (file-truename "~/org/"))
    (org-agenda-files `(,(file-truename "~/org/agenda")))

    ; don't clutter my fs with latex image cache
    (org-preview-latex-image-directory (concat (file-truename user-emacs-directory) "cache/ltximg/" (buffer-file-name)))

    (org-log-into-drawer t)
    (org-log-done 'time)

    (org-adapt-indentation t)
    (org-return-follows-link nil)
    (org-cycle-separator-lines 2)

    (org-todo-keywords '((sequence "TODO(t!)" "NOW(n!)" "WAIT(w!)" "MAYBE(m!)" "PASSIVE(p!)" "|" "DONE(d!)" "CANCELLED(c!)" "DELEGATED(D!)")))

    (org-priority-highest 1)
    (org-priority-lowest  8)
    (org-priority-default 3)

    (org-auto-align-tags nil)
    (org-tags-column 0)
    (org-catch-invisible-edits 'show-and-error)
    (org-special-ctrl-a/e t)
    (org-insert-heading-respect-content t)

    (org-image-actual-width nil)
    ; }}}

    ; {{{ visuals
    (org-hide-emphasis-markers t)
    (org-link-descriptive t)
    (org-pretty-entities t)
    (org-ellipsis " ↪ ")

    (org-agenda-tags-column 0)
    (org-agenda-block-separator ?─)
    (org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000 2200) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (org-agenda-current-time-string "now ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    ; }}}
    ; }}}

  :config
    ; option depends on default value
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

    ; {{{ custom faces
    (set-face-attribute 'org-block            nil :background (getcol 'bg1))
    (set-face-attribute 'org-block-begin-line nil :underline nil)
    (set-face-attribute 'org-block-end-line   nil :overline nil)

    (set-face-attribute 'org-level-1 nil :foreground (getcol 'green)  :weight 'bold)
    (set-face-attribute 'org-level-2 nil :foreground (getcol 'aqua)   :weight 'bold)
    (set-face-attribute 'org-level-3 nil :foreground (getcol 'blue)   :weight 'bold)
    (set-face-attribute 'org-level-4 nil :foreground (getcol 'purple) :weight 'bold)
    (set-face-attribute 'org-level-5 nil :foreground (getcol 'red)    :weight 'bold)

    (set-face-attribute 'org-link nil :foreground (getcol 'purple) :underline t)
    ; }}}

    ; {{{ org-agenda
    ; helper function
    (defun apeiros/org-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.
      PRIORITY may be a number from 1-8."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))

    (setq org-agenda-custom-commands
      '(("c" "Combined agenda view"
       ((tags "PRIORITY=\"1\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-overriding-header "High-priority unfinished tasks:")))
        (agenda "")
        (alltodo ""
         ((org-agenda-skip-function
         '(or (apeiros/org-skip-subtree-if-priority 1)
            (org-agenda-skip-if nil '(scheduled deadline)))))))))))
    ; }}}
; }}}

; {{{ org-roam
(use-package org-roam
  :after org

  :custom
    (org-roam-directory (file-truename "~/org/"))

  :init
    ; {{{ custom keymaps
    (ldr-defkm "rl" 'org-roam-buffer-toggle)
    (ldr-defkm "rf" 'org-roam-node-find)
    (ldr-defkm "rg" 'org-roam-graph)
    (ldr-defkm "ri" 'org-roam-node-insert)
    (ldr-defkm "rc" 'org-roam-capture)
    (ldr-defkm "rj" 'org-roam-dailies-capture-today)
    ; }}}

  :config
    (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
    (org-roam-db-autosync-mode)
    (require 'org-roam-protocol))
; }}}

; {{{ org-modern
(use-package org-modern
  :after org
  :commands org-modern-mode org-modern-agenda global-org-modern-mode

  :init
    ; activation hooks (use-package :hook refuses to work)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

    ; custom keymaps
    (ldr-defkm 'org-mode-map "oc" 'org-modern-mode) ; c for concealer, force of habit from nvim

  :custom
    ; {{{ custom options
    (org-modern-star '("◉" " ◈" "  ◉" "   ◈" "    ◉"))
    (org-modern-timestamp '(" %Y.%m.%d:%u " . " %H:%M "))
    (org-modern-table-vertical 1)

    (org-modern-list
          '((?- . "•")
            (?+ . "•")
            (?* . "•")))

    (org-modern-checkbox
          '((?X  . "[󰄬]")
            (?-  . "[-]")
            (?\s . "[ ]")))

    (org-modern-todo-faces
          `(("NOW"       :foreground ,(getcol 'bg1) :background ,(getcol 'red))
            ("WAIT"      :foreground ,(getcol 'bg1) :background ,(getcol 'orange))
            ("MAYBE"     :foreground ,(getcol 'bg1) :background ,(getcol 'purple))
            ("TODO"      :foreground ,(getcol 'bg1) :background ,(getcol 'fg2))
            ("PASSIVE"   :foreground ,(getcol 'bg1) :background ,(getcol 'green))
            ("DONE"      :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            ("CANCELLED" :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            ("DELEGATED" :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))))

    (org-modern-block-name '("$" . "$"))
    (org-modern-block-fringe 16)

    (org-modern-internal-target '(" 󰌹 " t " "))
    (org-modern-radio           '(" 󰌹 " t " "))

    (org-modern-progress nil)
    ; }}}

  :config
    ; {{{ custom faces
    (set-face-attribute 'org-modern-symbol nil :foreground (getcol 'fg2))
    (set-face-attribute 'org-modern-label nil :foreground (getcol 'fg2) :height 0.9)

    (set-face-attribute 'org-modern-tag nil :foreground (getcol 'fg2))

    (set-face-attribute 'org-modern-internal-target nil :foreground (getcol 'fg2) :background (getcol 'bg2))
    (set-face-attribute 'org-modern-radio-target    nil :foreground (getcol 'fg2) :background (getcol 'bg2))

    (set-face-attribute 'org-modern-done nil :foreground (getcol 'fg2) :background (getcol 'bg2))
    (set-face-attribute 'org-modern-todo nil :foreground (getcol 'bg2) :background (getcol 'green) :inverse-video nil)

    (set-face-attribute 'org-modern-priority nil :foreground (getcol 'bg1) :background (getcol 'fg2) :inverse-video nil)

    (set-face-attribute 'org-modern-statistics nil :foreground (getcol 'green) :background (getcol 'bg3))

    (set-face-attribute 'org-modern-date-active   nil :foreground (getcol 'fg2) :background (getcol 'bg3))
    (set-face-attribute 'org-modern-time-active   nil :foreground (getcol 'fg2) :background (getcol 'bg4))
    (set-face-attribute 'org-modern-date-inactive nil :foreground (getcol 'fg2) :background (getcol 'bg3))
    (set-face-attribute 'org-modern-time-inactive nil :foreground (getcol 'fg2) :background (getcol 'bg4))

    (set-face-attribute 'org-modern-horizontal-rule nil :strike-through (getcol 'bg4) :foreground (getcol 'bg1)))
    ; }}}
;}}}

; {{{ org-fragtog + org-appear
; :hook STILL refuses to work >:(
(use-package org-fragtog
  :commands org-fragtog-mode
  :init (add-hook 'org-mode-hook #'org-fragtog-mode))

(use-package org-appear
  :commands org-appear-mode

  :init
    ; load hook
    (add-hook 'org-mode-hook #'org-appear-mode)

  :custom
    ; {{{ custom options
    (org-appear-autoemphasis   t)
    (org-appear-autolinks      t)
    (org-appear-autosubmarkers t)
    (org-appear-autoentities   t)
    (org-appear-autokeywords   nil)
    (org-appear-inside-latex   nil)
    (org-appear-delay          0)
    (org-appear-trigger        'always))
    ; }}}
; }}}
