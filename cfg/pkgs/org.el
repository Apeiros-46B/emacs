; vim:et

; {{{ org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands
    my-org-capture-tmp
    my-org-capture-agenda
    my-org-goto-capture-file
    my-org-goto-agenda-dir

  :hook
    ; skip subtree when folding/cycling
    (org-cycle . (lambda (state)
      (when (eq state 'children)
        (setq org-cycle-subtree-status 'subtree))))

    ; override the evil-org-mode keymaps
    (evil-org-mode . (lambda ()
      (defkm 'normal 'org-agenda-mode-map "C-]" 'org-agenda-goto)
      (defkm 'normal 'org-agenda-mode-map "RET" 'org-agenda-goto)))

  :init
    ; {{{ custom keymaps
    (ldr-defkm "a" 'org-agenda)
    (ldr-defkm "cc" 'my-org-capture-tmp)
    (ldr-defkm "gc" 'my-org-goto-tmp-file)
    (ldr-defkm "ca" 'my-org-capture-agenda)
    (ldr-defkm "ga" 'my-org-goto-agenda-dir)

    ; magic
    (ldr-defkm 'org-mode-map "SPC" 'org-ctrl-c-ctrl-c)

    ; link creation
    (ldr-defkm "ol" 'org-store-link)
    (ldr-defkm 'org-mode-map "oi" 'org-insert-link)

    (ldr-defkm 'org-mode-map "op" 'org-priority)
    (ldr-defkm 'org-mode-map "ot" 'org-time-stamp)
    (ldr-defkm 'org-mode-map "oL" 'org-latex-preview)
    (ldr-defkm 'org-mode-map "oI" 'org-toggle-inline-images)

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

  :custom
    ; {{{ custom options
    ; {{{ functionality
    (org-directory (directory-file-name (file-truename "~/org/")))
    (org-agenda-files `(,(concat org-directory "/agenda")))

    ; don't clutter my fs with latex image cache
    (org-preview-latex-image-directory (get-cfg-path "cache/ltximg/"))

    (org-log-into-drawer t)
    (org-log-done 'time)

    (org-return-follows-link nil)
    (org-cycle-separator-lines 2)

    (org-auto-align-tags nil)
    (org-tags-column 0)
    (org-catch-invisible-edits 'show-and-error)
    (org-special-ctrl-a/e t)
    (org-insert-heading-respect-content t)

    (org-todo-keywords
      '((sequence "TASK(t!)" "STUDY(s!)" "WORK(w!)" "|" "DONE(d!)")))
    (org-todo-repeat-to-state t) ; use prev state when repeating task

    (org-priority-highest 1)
    (org-priority-lowest  8)
    (org-priority-default 3)

    (org-agenda-skip-scheduled-if-deadline-is-shown t)
    (org-agenda-repeating-timestamp-show-all nil)
    (org-agenda-format-date "%Y.%m.%d:%u")
    (org-agenda-prefix-format
      '((agenda . " %i %-10:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-10:c")
        (tags . " %i %-10:c")
        (search . " %i %-10:c")))

    (org-image-actual-width nil)
    (org-image-max-width 660) ; 3/4 * fillcolumn(88) * charwidth(10)
    (org-startup-with-inline-images t)
    ; }}}

    ; {{{ visuals
    (org-startup-indented t)
    (org-indent-indentation-per-level 1)

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
    ; org-protocol
    (require 'org-protocol)

    ; {{{ custom options (depends on default value)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))

    ; open links in current pane
    (setf (alist-get 'file org-link-frame-setup) 'find-file)
    ; }}}

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
    ; }}}

    ; {{{ quick capture and goto capture files
    (add-hook 'org-capture-mode-hook 'evil-insert-state)

    (setq my-org-tmp-file (concat org-directory "/tmp.org"))

    (setq org-capture-templates
      `(("t" "temporary" entry (file ,my-org-tmp-file) "* [%<%Y-%m-%d %a %H:%M>] %?"
         :empty-lines-before 2)
        ("a" "task/school" entry
         (file+headline ,(concat (car org-agenda-files) "/school.org") "tasks")
         "* TASK [#3] %?\n   DEADLINE: "
         :empty-lines 2)
        ("w" "website" entry (file ,my-org-tmp-file)
          "* [%<%Y-%m-%d %a %H:%M>] %?\n  - [[%:link][%:description]]\n  - \"%i\""
          :empty-lines 1)))

    (defun my-org-capture-tmp () (interactive) (org-capture nil "t"))
    (defun my-org-capture-agenda () (interactive) (org-capture nil "a"))

    (defun my-org-goto-capture-file () (interactive) (find-file my-org-tmp-file))
    (defun my-org-goto-agenda-dir ()
      (interactive)
      (dired org-agenda-files)))
    ; }}}
; }}}

; {{{ org-roam
(use-package org-roam
  :after org

  :hook
    (org-roam-capture-new-node . evil-insert-state)

  :init
    ; {{{ custom keymaps
    (ldr-defkm "rl" 'org-roam-buffer-toggle)
    (ldr-defkm "rf" 'org-roam-node-find)
    (ldr-defkm "ri" 'org-roam-node-insert)
    (ldr-defkm "rg" 'org-roam-graph)
    (ldr-defkm "rn" 'org-roam-capture)
    (ldr-defkm "rd" 'org-roam-dailies-capture-today)
    (ldr-defkm "rD" 'org-roam-dailies-goto-today)
    (ldr-defkm "rs" 'org-roam-db-sync)
    ; }}}

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
        `(,(capture-template "s" 'main  'school)
          ,(capture-template "m" 'main  'other)
          ,(capture-template "r" 'ref   'other))))
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

  :init
    ; custom keymaps
    (ldr-defkm "ru" 'org-roam-ui-open)

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
    (org-modern-star "replace")
    (org-modern-replace-stars "◉◈◉◈◉")
    (org-modern-hide-stars " ")

    ; FIX: setting a custom format breaks editing and showing extra info (repeats)
    ; (org-modern-timestamp '(" %Y.%m.%d:%u " . " %H:%M "))
    (org-modern-table-vertical 1)

    (org-modern-list
          '((?- . "•")
            (?+ . "▪")
            (?* . "–")))

    (org-modern-checkbox
          '((?X  . "[󰄬]")
            (?-  . "[-]")
            (?\s . "[ ]")))

    (org-modern-block-name '("$" . "$"))
    (org-modern-block-fringe 16)

    (org-modern-internal-target '(" 󰌹 " t " "))
    (org-modern-radio-target    '(" 󰖩 " t " "))

    (org-modern-progress nil)
    ; }}}

    ; {{{ custom todo and priority faces
    (org-modern-todo-faces
          `(("TASK"      :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            ("STUDY"     :foreground ,(getcol 'bg1) :background ,(getcol 'green))
            ("WORK"      :foreground ,(getcol 'bg1) :background ,(getcol 'purple))
            ("DONE"      :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))))

    (org-modern-priority-faces
          `((?1 :foreground ,(getcol 'bg1) :background ,(getcol 'red))
            (?2 :foreground ,(getcol 'bg1) :background ,(getcol 'yellow))
            (?3 :foreground ,(getcol 'bg1) :background ,(getcol 'green))
            (?4 :foreground ,(getcol 'bg1) :background ,(getcol 'fg2))
            (?5 :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            (?6 :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            (?7 :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))
            (?8 :foreground ,(getcol 'fg2) :background ,(getcol 'bg3))))
    ; }}}

  :config
    ; {{{ other custom faces
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
  :hook (org-mode . org-fragtog-mode)

  :custom
    ; don't show fragments inside tables
    (org-fragtog-ignore-predicates '(org-at-table-p)))

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

; {{{ org-tree-slide
(use-package org-tree-slide
  :after (org hide-mode-line)

  :commands
    org-tree-slide-mode
    org-tree-slide-move-next-tree
    org-tree-slide-move-previous-tree
    org-tree-slide-content

  :hook
    (org-tree-slide-mode . hide-mode-line-mode)
    (org-tree-slide-mode . (lambda ()
      (if (boundp 'my-presentation-active)
        (progn
          (makunbound 'my-presentation-active)
          (text-scale-set 0))
        (setq my-presentation-active t)
        (text-scale-set 4))))

  :custom
    ; {{{ custom options
    (org-tree-slide-header nil)
    (org-tree-slide-slide-in-effect nil)
    (org-tree-slide-never-touch-face t)
    (org-tree-slide-activate-message "Presentation started")
    (org-tree-slide-deactivate-message "Quit")
    (org-tree-slide-subtrees-skipped nil)
    (org-tree-slide-breadcrumbs "->")
    ; }}}
  )

  :init
    ; {{{ custom keymaps
    (ldr-defkm 'org-mode-map "os" 'org-tree-slide-mode)
    (ldr-defkm 'org-tree-slide-mode-map "q" 'org-tree-slide-mode)
    (ldr-defkm 'org-tree-slide-mode-map "C-SPC" 'org-tree-slide-move-previous-tree)
    (ldr-defkm 'org-tree-slide-mode-map "SPC" 'org-tree-slide-move-next-tree)
    ; }}}
; }}}

; {{{ org-download
(use-package org-download
  :after org

  :custom
    ; custom options
    (org-download-image-dir (concat org-directory "/img"))
    (org-download-heading-lvl nil) ; don't save under heading dirs
    (org-download-abbreviate-filename-function 'expand-file-name) ; absolute paths

  :init
    ; custom keymaps
    (ldr-defkm 'normal 'org-mode-map "ip" 'org-download-clipboard)
    (ldr-defkm 'normal 'org-mode-map "iP" 'org-download-yank)
    (ldr-defkm 'normal 'org-mode-map "ii" 'org-download-image)
    (ldr-defkm 'normal 'org-mode-map "ir" 'org-download-rename-at-point)
    (ldr-defkm 'normal 'org-mode-map "id" 'my-org-download-delete)

  :config
    ; no annotations
    (defun org-download-annotate-default (link) "Annotate LINK." "")

    ; deletion for previewed images
    (defun my-org-download-delete ()
      (interactive)

      ; unrender preview
      (org-remove-inline-images (point) (+ 1 (point)))

      (move-point-visually 1) ; move point onto the link body
      (org-download-delete))) ; delete image
; }}}
