; vim:et

; {{{ org
(use-package org
  :mode ("\\.org\\'" . org-mode)

  ; skip subtree when folding/cycling
  :hook
    (org-cycle . (lambda (state)
      (when (eq state 'children)
        (setq org-cycle-subtree-status 'subtree))))
    (evil-org-mode . (lambda ()
      ; override the evil-org-mode keymaps
      (defkm 'normal 'org-agenda-mode-map "C-]" 'org-agenda-goto)
      (defkm 'normal 'org-agenda-mode-map "RET" 'org-agenda-goto)))

  :commands
    my-org-capture-slipbox
    my-org-capture-journal
    my-org-capture-agenda
    my-org-goto-capture-file
    my-org-goto-journal-file
    my-org-goto-agenda-dir

  :init
    ; {{{ custom keymaps (org)
    (ldr-defkm "a" 'org-agenda)
    (ldr-defkm "cc" 'my-org-capture-slipbox)
    (ldr-defkm "gc" 'my-org-goto-capture-file)
    (ldr-defkm "cj" 'my-org-capture-journal)
    (ldr-defkm "gj" 'my-org-goto-journal-file)
    (ldr-defkm "ca" 'my-org-capture-agenda)
    (ldr-defkm "ga" 'my-org-goto-agenda-dir)

    ; magic
    (ldr-defkm 'org-mode-map "SPC" 'org-ctrl-c-ctrl-c)

    (ldr-defkm 'org-mode-map "op" 'org-priority)
    (ldr-defkm 'org-mode-map "ot" 'org-time-stamp)
    (ldr-defkm 'org-mode-map "ol" 'org-latex-preview)

    ; link creation
    (ldr-defkm "ol" 'org-store-link)
    (ldr-defkm 'org-mode-map "oi" 'org-insert-link)

    (defkm 'normal 'org-mode-map "C-]" 'org-open-at-point)
    (defkm 'normal 'org-mode-map "RET" 'org-open-at-point)
    (defkm 'normal 'org-agenda-mode-map "C-]" 'org-agenda-goto)
    (defkm 'normal 'org-agenda-mode-map "RET" 'org-agenda-goto)
    (defkm '(normal visual) 'org-mode-map "C-SPC" 'org-toggle-checkbox)
    (defkm '(normal visual) 'org-mode-map "gt" 'org-todo)

    ; folding/cycling
    ; I don't use the evil-*-fold commands because they
    ; don't leave empty lines between folded headers
    (defkm 'normal 'org-mode-map "za" 'org-cycle)
    (defkm 'normal 'org-mode-map "zA" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zM" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zR" 'org-fold-show-all)
    (defkm 'normal 'org-mode-map "zx" 'org-cycle-set-startup-visibility)

    ; promotion, demotion, and swapping
    (defkm 'insert 'org-mode-map "C-t" 'org-demote-subtree)
    (defkm 'insert 'org-mode-map "C-d" 'org-promote-subtree)
    (defkm 'insert 'org-mode-map "C-j" 'org-move-subtree-down)
    (defkm 'insert 'org-mode-map "C-k" 'org-move-subtree-up)

    ; misc
    (ldr-defkm 'normal 'org-mode-map "*" 'org-toggle-heading)
    ; }}}

    ; {{{ custom keymaps (org-roam)
    (ldr-defkm "rl" 'org-roam-buffer-toggle)
    (ldr-defkm "rf" 'org-roam-node-find)
    (ldr-defkm "ri" 'org-roam-node-insert)
    (ldr-defkm "rg" 'org-roam-graph)
    (ldr-defkm "rn" 'org-roam-capture)
    (ldr-defkm "rd" 'org-roam-dailies-capture-today)
    (ldr-defkm "rD" 'org-roam-dailies-goto-today)
    (ldr-defkm "rs" 'org-roam-db-sync)
    ; }}}

    ; {{{ custom keymaps (org-roam-ui)
    (ldr-defkm "ru" 'org-roam-ui-open)
    ; }}}

  :custom
    ; {{{ custom options
    ; {{{ functionality
    (org-directory (directory-file-name (file-truename "~/org/")))
    (org-agenda-files `(,(concat org-directory "/agenda")))

    ; don't clutter my fs with latex image cache
    (org-preview-latex-image-directory (concat (file-truename user-emacs-directory) "cache/ltximg/" (buffer-file-name)))

    (org-log-into-drawer t)
    (org-log-done 'time)

    (org-adapt-indentation t)
    (org-return-follows-link nil)
    (org-cycle-separator-lines 2)

    (org-todo-keywords
      '((sequence "TODO(t!)"
                  "NOW(n!)"
                  "WAIT(w!)"
                  "MAYBE(m!)"
                  "ONGOING(o!)"
                  "|"
                  "DONE(d!)"
                  "CANCELLED(c!)"
                  "DELEGATED(D!)")))

    (org-priority-highest 1)
    (org-priority-lowest  8)
    (org-priority-default 3)

    (org-auto-align-tags nil)
    (org-tags-column 0)
    (org-catch-invisible-edits 'show-and-error)
    (org-special-ctrl-a/e t)
    (org-insert-heading-respect-content t)

    (org-agenda-format-date "%Y.%m.%d:%u")
    (org-agenda-prefix-format
      '((agenda . " %i %-10:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-10:c")
        (tags . " %i %-10:c")
        (search . " %i %-10:c")))

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

    (set-face-attribute 'org-link    nil :foreground (getcol 'purple) :underline t)
    (set-face-attribute 'org-list-dt nil :foreground (getcol 'green))
    ; }}}

    ; {{{ org-agenda
    ; helper function
    (defun my-org-skip-subtree-if-priority (priority)
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
         '(or (my-org-skip-subtree-if-priority 1)
            (org-agenda-skip-if nil '(scheduled deadline))))))))))

    (defun my-org-goto-agenda-dir ()
      (interactive)
      (dired org-agenda-files))
    ; }}}

    ; {{{ quick capture and goto capture file
    (setq my-org-slipbox-file (concat org-directory "/capture.org"))
    (setq my-org-journal-file (concat org-directory "/secure/journal.org.gpg"))

    (setq org-capture-templates
      `(("s" "slipbox" entry (file ,my-org-slipbox-file) "* [%<%Y-%m-%d %H:%M>] %?"
         :empty-lines-before 2)
        ("j" "journal" entry (file ,my-org-journal-file) "* [%<%Y-%m-%d>]\n  - %?"
         :empty-lines-before 2)
        ("t" "task/school" entry
         (file+headline ,(concat (car org-agenda-files) "/school.org") "tasks")
         "* TODO [#3] %?\n   DEADLINE: "
         :empty-lines 2)))

    (defun my-org-capture-slipbox ()
      (interactive)
      (org-capture nil "s")
      (evil-insert-state))

    (defun my-org-capture-journal ()
      (interactive)
      (org-capture nil "j")
      (evil-insert-state))

    (defun my-org-capture-agenda ()
      (interactive)
      (org-capture nil "t")
      (evil-insert-state))

    (defun my-org-goto-capture-file ()
      (interactive)
      (find-file my-org-slipbox-file))

    (defun my-org-goto-journal-file ()
      (interactive)
      (find-file my-org-journal-file)))
    ; }}}
; }}}

; {{{ org-roam
(use-package org-roam
  :after org

  :hook
    (org-roam-capture-new-node . evil-insert-state)

  :custom
    ; {{{ custom options
    (org-roam-directory org-directory)

    ; prevent encrypted files from being included in org-roam
    (org-roam-file-exclude-regexp '("data/" ".*[.]org[.]gpg$"))

    ; node display in capture/find selector
    (org-roam-node-display-template
      (concat
        (propertize "${type:12}" 'face 'org-tag)
        " → ${title:*} "
        (propertize "${tags:30}" 'face 'org-tag)))

    ; fix org-roam-ui only showing one tag
    (org-roam-database-connector 'sqlite)
    ; }}}

  :config
    (org-roam-db-autosync-enable)
    (require 'org-roam-protocol)

    ; {{{ capture templates
    (cl-flet ((capture-template (key path1 path2)
      (let ((path (concat (symbol-name path1) "/" (symbol-name path2))))
        `(,key ,path plain "%?"
          :target
            (file+head ,(concat path "/%<%Y.%m.%d>_${slug}.org")
              "#+date: <%<%Y-%m-%d %a>>\n#+title: ${title}\n#+filetags: ")
          :immediate-finish t
          :unnarrowed t))))
      (setq org-roam-capture-templates
        `(,(capture-template "m" 'main  'school)
          ,(capture-template "r" 'ref   'school)
          ,(capture-template "f" 'final 'school)
          ,(capture-template "M" 'main  'other)
          ,(capture-template "R" 'ref   'other)
          ,(capture-template "F" 'final 'other))))
    ; }}}

    ; {{{ node "types"
    (cl-defmethod org-roam-node-type ((node org-roam-node))
      "Return the TYPE of NODE."
      (condition-case nil
        (directory-file-name
          (file-name-directory
            (file-relative-name (org-roam-node-file node) org-roam-directory)))
        (error ""))))
    ; }}}
; }}}

; {{{ org-roam-ui
(use-package org-roam-ui
  :after org-roam

  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))

  :hook
    (org-mode . (lambda ()
      (unless (default-value 'org-roam-ui-mode) org-roam-ui-mode)))

  :custom
    ; {{{ custom options
    (org-roam-ui-sync-theme nil)
    (org-roam-ui-follow t)
    (org-roam-ui-update-on-save t)
    (org-roam-ui-open-on-start nil)
    (org-roam-ui-custom-theme
      `((bg      . ,(getcol 'bg1))
        (bg-alt  . ,(getcol 'bg2))
        (base1   . ,(getcol 'bg3))
        (base2   . ,(getcol 'bg4))
        (fg      . ,(getcol 'fg1))
        (fg-alt  . ,(getcol 'fg2))
        (red     . ,(getcol 'red))
        (orange  . ,(getcol 'orange))
        (yellow  . ,(getcol 'yellow))
        (green   . ,(getcol 'green))
        (cyan    . ,(getcol 'aqua))
        (blue    . ,(getcol 'blue))
        (violet  . ,(getcol 'purple))
        (magenta . ,(getcol 'purple)))))
    ; }}}
; }}}

; {{{ org-modern
(use-package org-modern
  :after org

  :commands
    org-modern-mode
    org-modern-agenda
    global-org-modern-mode

  :hook
    (org-mode            . org-modern-mode)
    (org-agenda-finalize . org-modern-agenda)

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
            ("ONGOING"   :foreground ,(getcol 'bg1) :background ,(getcol 'green))
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
(use-package org-fragtog
  :commands org-fragtog-mode
  :hook (org-mode . org-fragtog-mode))

(use-package org-appear
  :commands org-appear-mode
  :hook (org-mode . org-appear-mode)

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
