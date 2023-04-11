(use-package org
  :mode ("\\.org\\'" . org-mode)

  :init
    ; {{{ custom keymaps
    (ldr-defkm "oa" 'org-agenda)
    (ldr-defkm "ol" 'org-latex-preview)
    (ldr-defkm "op" 'org-priority)
    (ldr-defkm "ot" 'org-time-stamp)

    (defkm '(normal visual) "C-SPC" 'org-toggle-checkbox)
    (defkm '(normal visual) "gt" 'org-todo)
    (defkm 'normal "RET" 'org-open-at-point)
    ; }}}

  :custom
    ; {{{ custom options
    (org-dir (file-truename "~/org/"))

    (org-adapt-indentation 'headline-data)
    (org-return-follows-link t)
    (org-cycle-separator-lines -1)

    (org-todo-keywords '((sequence "NOW(n!)" "TODO(t!)" "MAYBE(m!)" "WAIT(w!)" "|" "DONE(d!)" "CANCEL(c!)" "DELEGATE(o!)")))

    (org-priority-highest 1)
    (org-priority-lowest  8)
    (org-priority-default 3)

    (org-agenda-files `(,(file-truename "~/org/agenda")))
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
      '(("c" "Simple agenda view"
       ((tags "PRIORITY=\"1\""
         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-overriding-header "High-priority unfinished tasks:")))
        (agenda "")
        (alltodo ""
         ((org-agenda-skip-function
         '(or (apeiros/org-skip-subtree-if-priority 1)
            (org-agenda-skip-if nil '(scheduled deadline)))))))))))
    ; }}}

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

(use-package org-modern
  :after org
  :commands org-modern-mode org-modern-agenda global-org-modern-mode

  :init
    ; activation hooks (use-package :hook refuses to work)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  :custom
    ; {{{ fix org options
    (org-auto-align-tags nil)
    (org-tags-column 0)
    (org-catch-invisible-edits 'show-and-error)
    (org-special-ctrl-a/e t)
    (org-insert-heading-respect-content t)

    (org-hide-emphasis-markers t)
    (org-pretty-entities t)
    (org-ellipsis " ↪ ")

    (org-agenda-tags-column 0)
    (org-agenda-block-separator ?─)
    (org-agenda-time-grid '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
    (org-agenda-current-time-string "now ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
    ; }}}

    ; {{{ custom options
    (org-modern-star '("◉" " ◈" "  ◉" "   ◈" "    ◉"))
    (org-modern-timestamp '(" %Y.%m.%d " . " %H:%M "))
    (org-modern-table-vertical 1)

    (org-modern-list
          '((?- . "•")
            (?+ . "•")
            (?* . "•")))

    (org-modern-checkbox
          '((?X  . "[]")
            (?-  . "[-]")
            (?\s . "[ ]")))

    (org-modern-todo-faces
          `(("NOW"      :foreground ,(getcol 'bg1) :background ,(getcol 'red))
            ("TODO"     :foreground ,(getcol 'bg1) :background ,(getcol 'fg2))
            ("MAYBE"    :foreground ,(getcol 'bg1) :background ,(getcol 'purple))
            ("WAIT"     :foreground ,(getcol 'bg1) :background ,(getcol 'orange))
            ("DONE"     :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            ("CANCEL"   :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            ("DELEGATE" :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))))

    (org-modern-block-name '("$" . "$"))
    (org-modern-block-fringe 16)

    (org-modern-internal-target '("  " t " "))
    (org-modern-radio           '("  " t " "))

    (org-modern-progress nil)
    ; }}}

  :config
    ; {{{ custom faces
    (set-face-attribute 'org-modern-symbol nil :foreground (getcol 'fg2))
    (set-face-attribute 'org-modern-label nil :foreground (getcol 'fg2))

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
