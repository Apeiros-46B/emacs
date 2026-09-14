; vim:et
; TODO: org-ql for searching capture and journal

; {{{ org
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :commands
    my-org-capture
    my-org-goto-current-capture-file
    my-org-goto-agenda-dir
    my-org-open-current-journal
    my-org-close-clean-journal-buffers
    my-org-insert-date
    my-org-insert-datetime
    my-org-insert-datetime-now

  :hook
    ; use slab font in org files
    (org-mode . (lambda ()
      (face-remap-add-relative 'default :family (face-attribute 'nano-serif :family))))

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
    (ldr-defkm "c" 'my-org-capture)
    (ldr-defkm "gc" 'my-org-goto-current-capture-file)
    (ldr-defkm "ga" 'my-org-goto-agenda-dir)
    (ldr-defkm "gj" 'my-org-open-current-journal)
    (ldr-defkm "gk" 'my-org-close-clean-journal-buffers)

    (ldr-defkm 'org-mode-map "SPC" 'org-ctrl-c-ctrl-c)

    ; link creation
    (ldr-defkm "oy" 'org-store-link)
    (ldr-defkm 'org-mode-map "op" 'org-insert-link)

    (ldr-defkm 'org-mode-map "ol" 'org-latex-preview)
    (ldr-defkm 'org-mode-map "oi" 'my-org-toggle-inline-previews)

    (ldr-defkm 'org-mode-map "od" 'my-org-insert-date)
    (ldr-defkm 'org-mode-map "ot" 'my-org-insert-datetime)
    (ldr-defkm 'org-mode-map "on" 'my-org-insert-datetime-now)

    (defkm 'normal 'org-mode-map "C-]" 'org-open-at-point)
    (defkm 'normal 'org-mode-map "RET" 'org-open-at-point)
    (defkm 'normal 'org-agenda-mode-map "C-]" 'org-agenda-goto)
    (defkm 'normal 'org-agenda-mode-map "RET" 'org-agenda-goto)
    (defkm '(normal visual) 'org-mode-map "C-SPC" 'org-toggle-checkbox)
    (defkm '(normal visual) 'org-mode-map "gt" 'org-todo)
    (defkm '(normal visual) 'org-mode-map "gp" 'org-priority)

    ; folding/cycling
    ; I don't use the evil-*-fold commands because they
    ; don't leave empty lines between folded headers
    ; TODO: ^above comment is stale, these don't leave empty lines either
    (defkm 'normal 'org-mode-map "za" 'org-cycle)
    (defkm 'normal 'org-mode-map "zA" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zM" 'org-global-cycle)
    (defkm 'normal 'org-mode-map "zR" 'org-fold-show-all)
    (defkm 'normal 'org-mode-map "zx" 'org-cycle-set-startup-visibility)

    ; promotion and demotion
    (defkm 'insert 'org-mode-map "C-t" 'org-demote-subtree)
    (defkm 'insert 'org-mode-map "C-d" 'org-promote-subtree)

    ; misc
    (ldr-defkm 'normal 'org-mode-map "h" 'org-toggle-heading)
    ; }}}

    (defun my-org-toggle-inline-previews ()
      "Toggle image previews in the current section or whole document."
      (interactive)
      (pcase-let ((`(,beg . ,end)
                   (if (org-before-first-heading-p)
                       (cons (point-min) (point-max))
                     (save-excursion
                       (org-back-to-heading t)
                       (cons (point) (org-entry-end-position))))))
        (if (org-link-preview--get-overlays beg end)
            (org-link-preview-clear beg end)
          (org-link-preview-region t t beg end))))

  :custom
    ; {{{ custom options
    ; {{{ functionality
    (org-directory (directory-file-name (file-truename "~/org/")))
    (org-agenda-files `(,(concat org-directory "/agenda")))
    (org-attach-id-dir (concat org-directory "/.blob/org-attach"))
    (org-attach-id-to-path-function-list '(identity))
    (org-attach-preferred-new-method 'id)
    (org-attach-use-inheritance nil)

    ; don't clutter my fs with latex image cache
    (org-preview-latex-image-directory (get-cfg-path "cache/ltximg/"))
    (org-preview-latex-default-process 'dvisvgm)

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
      '((sequence "TODO(t!)" "EXEC(e!)" "WAIT(w!)" "PERM(p!)" "|" "DONE(d!)")))
    (org-todo-repeat-to-state t) ; use prev state when repeating task

    (org-priority-highest 1)
    (org-priority-lowest  4)
    (org-priority-default 2)

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
    (org-startup-with-inline-images nil)
    (org-link-preview-include-descriptive t)
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
    (require 'org-protocol)

    ; {{{ custom options (depends on default value)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

    ; open links in current pane
    (setf (alist-get 'file org-link-frame-setup) 'find-file)
    ; }}}

    ; {{{ custom faces
    ; use mono font for code segments
    (set-face-attribute 'org-block    nil :family (face-attribute 'nano-mono :family))
    (set-face-attribute 'org-code     nil :family (face-attribute 'nano-mono :family))
    (set-face-attribute 'org-verbatim nil :family (face-attribute 'nano-mono :family))

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
    (defun my-org-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.
      PRIORITY may be a number from 1-4."
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

    ; {{{ org-attach
    (require 'org-attach)

    (defun my-org-annex-add-file (file)
      "Add FILE to git-annex when it is a regular file under `org-directory`."
      (let ((file (expand-file-name file)))
        (when (and (file-regular-p file)
                   (file-in-directory-p file org-directory))
          (let ((default-directory org-directory)
                (relative-file (file-relative-name file org-directory)))
            (if (eq 0 (process-file "git" nil nil nil "annex" "add" "--" relative-file))
                (message "Added %s to git-annex" relative-file)
              (message "Failed adding %s to git-annex" relative-file))))))

    (defun my-org-attach-annex-files (&optional dir)
      "Add files in DIR to git-annex. DIR is supplied by `org-attach-after-change-hook`."
      (let ((dir (or dir (org-attach-dir))))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t directory-files-no-dot-files-regexp))
            (when (file-regular-p file)
              (my-org-annex-add-file file))))))

    (add-hook 'org-attach-after-change-hook #'my-org-attach-annex-files)
    ;; `org-attach-buffer` runs the hook before it writes the attachment.
    ;; Run once more after it returns so buffer attachments are included.
    (defun my-org-attach-annex-buffer-after (&rest _) (my-org-attach-annex-files))
    (advice-add 'org-attach-buffer :after #'my-org-attach-annex-buffer-after)
    ; }}}

    ; {{{ capture log
    (add-hook 'org-capture-mode-hook #'evil-insert-state)

    (setq my-org-capture-dir (concat org-directory "/capture/"))

    (defun my-org-capture-current-file ()
      "Return the append-only capture log for the current month."
      (make-directory my-org-capture-dir t)
      (expand-file-name (format-time-string "%Y-%m.org") my-org-capture-dir))

    (cl-flet ((task-capture-template (key tag &optional narrow_tag)
      `(,key ,(concat (or narrow_tag tag) " (task)") entry
        (file+headline ,(concat (car org-agenda-files) "/" tag ".org") "tasks")
        ,(concat "* TODO %? :" tag ":" (when narrow_tag (concat narrow_tag ":"))
                 "\nDEADLINE: ")
        :empty-lines 2)))
      (setq org-capture-templates
        `(("c" "capture log" entry (file my-org-capture-current-file)
           "* [%<%Y-%m-%d %a %H:%M>] %?"
           :empty-lines-before 2)
          ,(task-capture-template "s" "school")
          ,(task-capture-template "w" "work")
          ,(task-capture-template "d" "projects" "dev")
          ,(task-capture-template "a" "projects" "art"))))

    (defun my-org-capture () (interactive) (org-capture nil))
    (defun my-org-goto-current-capture-file ()
      (interactive)
      (find-file (my-org-capture-current-file)))
    (defun my-org-goto-agenda-dir () (interactive) (dired org-agenda-files))
    ; }}}

    ; {{{ journal
    (setq my-org-journal-current-dir (concat org-directory "/journal/cur/"))
    (setq my-org-journal-template-file (concat org-directory "/journal/template.org"))

    (defun my-org-journal-current-file ()
      (expand-file-name (format-time-string "%Y-%m-%d.org") my-org-journal-current-dir))

    (defun my-org-open-current-journal ()
      "Open today's journal, creating it from the journal template if needed."
      (interactive)
      (let* ((file (my-org-journal-current-file))
             (new-file (not (file-exists-p file))))
        (make-directory my-org-journal-current-dir t)
        (find-file file)
        (when new-file
          (require 'org-capture)
          (insert
           (org-capture-fill-template
            (with-temp-buffer
              (insert-file-contents my-org-journal-template-file)
              (buffer-string))))
          (save-buffer))))

    (defun my-org-close-clean-journal-buffers ()
      "Close unmodified Org buffers anywhere under the journal directory."
      (interactive)
      (let ((journal-dir (expand-file-name "journal/" org-directory))
            modified-buffers
            closed-count)
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when (and buffer-file-name
                       (string-match-p "\\.org\\'" buffer-file-name)
                       (file-in-directory-p (expand-file-name buffer-file-name)
                                            journal-dir))
              (if (buffer-modified-p)
                  (push (buffer-name) modified-buffers)
                (kill-buffer buffer)
                (setq closed-count (1+ (or closed-count 0)))))))
        (if modified-buffers
            (message "Warning: kept modified journal buffer%s: %s"
                     (if (= (length modified-buffers) 1) "" "s")
                     (mapconcat #'identity (nreverse modified-buffers) ", "))
          (message "Closed %d unmodified journal buffer%s"
                   (or closed-count 0)
                   (if (= (or closed-count 0) 1) "" "s")))))
    ; }}}

    ; {{{ better timestamps
    (defun my-org-move-after-evil-point ()
      (when (and (bound-and-true-p evil-local-mode)
                 (evil-normal-state-p)
                 (not (eolp)))
        (forward-char 1)))

    (defun my-org-agenda-file-p ()
      (and buffer-file-name
           (file-in-directory-p (expand-file-name buffer-file-name)
                                (expand-file-name "agenda/" org-directory))))

    (defun my-org-insert-timestamp (time with-time)
      (my-org-move-after-evil-point)
      (org-insert-time-stamp time with-time (not (my-org-agenda-file-p))))

    (defun my-org-insert-date ()
      "Prompt for a date, defaulting to today, and insert it."
      (interactive)
      (my-org-insert-timestamp
       (org-read-date nil t nil "Date: " (current-time)) nil))

    (defun my-org-insert-datetime (&optional time)
      "Prompt for a date and time, defaulting to now, and insert it.
      When TIME is non-nil, insert it without prompting."
      (interactive)
      (my-org-insert-timestamp
       (or time (org-read-date t t nil "Date and time: " (current-time))) t))

    (defun my-org-insert-datetime-now ()
      "Insert the current date and time without prompting."
      (interactive)
      (my-org-insert-datetime (current-time))))
    ; }}}
; }}}

; {{{ org-roam
(use-package org-roam
  :defer t

  :commands
    org-roam-buffer-toggle
    org-roam-node-find
    org-roam-node-insert
    org-roam-capture
    org-roam-db-sync

  :hook
    (org-roam-capture-new-node . evil-insert-state)

  :init
    ; {{{ custom keymaps
    ; TODO: theme the org-roam-buffer
    ; TODO: capturing shows existing items and doesn't let you make a new one if your new
    ; note title is a substring of an existing one due to the search menu
    (ldr-defkm "rl" 'org-roam-buffer-toggle)
    (ldr-defkm "rf" 'org-roam-node-find)
    (ldr-defkm "ri" 'org-roam-node-insert)
    (ldr-defkm "rn" 'org-roam-capture)
    (ldr-defkm "rs" 'org-roam-db-sync)
    ; }}}

  :custom
    ; {{{ custom options
    (org-roam-directory (concat org-directory "/roam"))

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
    (cl-flet ((capture-template (key tag &optional narrow_tag)
      `(,key ,(or narrow_tag tag) plain "%?"
        :target
          (file+head ,(concat tag "/%<%Y-%m-%d>_${slug}.org")
            ,(concat "#+DATE: [%<%Y-%m-%d %a>]\n#+TITLE: ${title}\n#+FILETAGS: :" tag ":"
                     (when narrow_tag (concat narrow_tag ":"))))
        :immediate-finish t
        :jump-to-captured t
        :unnarrowed t)))
      (setq org-roam-capture-templates
        `(,(capture-template "s" "school")
          ,(capture-template "w" "work")
          ,(capture-template "d" "projects" "dev")
          ,(capture-template "a" "projects" "art"))))
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
  :defer t

  :commands org-roam-ui-open

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
          '((?- . "·")
            (?+ . "∘")
            (?* . "▸")))

    (org-modern-checkbox
          '((?X  . "[󰄬]")
            (?-  . "[-]")
            (?\s . "[ ]")))

    (org-modern-block-name '(">" . ">"))
    (org-modern-block-fringe 16)

    (org-modern-internal-target '(" 󰌹 " t " "))
    (org-modern-radio-target    '(" 󰖩 " t " "))

    (org-modern-progress 9)
    ; }}}

    ; {{{ custom todo and priority faces
    (org-modern-todo-faces
      `(("TODO"      :foreground ,(getcol 'bg1)   :background ,(getcol 'green))
        ("EXEC"      :foreground ,(getcol 'bg1)   :background ,(getcol 'red))
        ("WAIT"      :foreground ,(getcol 'bg1)   :background ,(getcol 'yellow))
        ("PERM"      :foreground ,(getcol 'bg1)   :background ,(getcol 'purple))
        ("DONE"      :foreground ,(getcol 'green) :background ,(getcol 'bg-green))))

    (org-modern-priority-faces
      `((?1 :foreground ,(getcol 'bg1) :background ,(getcol 'red))
        (?2 :foreground ,(getcol 'bg1) :background ,(getcol 'yellow))
        (?3 :foreground ,(getcol 'bg1) :background ,(getcol 'green))
        (?4 :foreground ,(getcol 'bg1) :background ,(getcol 'fg2))))
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

; {{{ org-download
(use-package org-download
  :defer t

  :commands
    org-download-clipboard
    org-download-yank
    org-download-image
    org-download-rename-at-point
    my-org-download-delete

  :custom
    ; custom options
    (org-download-method 'directory)
    (org-download-heading-lvl nil) ; don't save under heading dirs
    (org-download-abbreviate-filename-function #'my-org-download-abbrev-to-org)

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

    ; {{{ ugh
    (defun my-org-download-abbrev-to-org (filename)
      "Format FILENAME as a path rooted at `org-directory'."
      (concat
       (file-name-as-directory
        (abbreviate-file-name (expand-file-name org-directory)))
       (file-relative-name (expand-file-name filename) org-directory)))

    (defun my-org-download-link-format (filename)
      "Format FILENAME as an Org-rooted file link labelled by its basename."
      (format "[[file:%s][%s]]\n"
              (org-link-escape
               (funcall org-download-abbreviate-filename-function filename))
              (org-link-escape (file-name-nondirectory filename))))

    (setq org-download-link-format-function #'my-org-download-link-format)

    (defun my-org-download-id-directory (&rest _)
      "Return the ID-specific directory for the current Org heading's downloads."
      (expand-file-name
       (org-id-get-create)
       (expand-file-name ".blob/org-download/" org-directory)))
    (advice-add 'org-download--dir-1 :override #'my-org-download-id-directory)

    ; this is horrible but idk how else to do it
    (defvar my-org-download-annex--pending (make-hash-table :test #'equal))
    (defun my-org-download-annex--when-ready (file attempts)
      (cond
       ((not (file-exists-p file))
        (when (> attempts 0)
          (run-at-time 0.2 nil #'my-org-download-annex--when-ready
                       file (1- attempts))))
       ((zerop (file-attribute-size (file-attributes file)))
        (when (> attempts 0)
          (run-at-time 0.2 nil #'my-org-download-annex--when-ready
                       file (1- attempts))))
       (t
        (remhash file my-org-download-annex--pending)
        (my-org-annex-add-file file))))

    (defun my-org-download-annex-file (file)
      "Add FILE to git-annex once org-download has finished writing it."
      (let ((file (expand-file-name file)))
        (when (and (file-in-directory-p file org-directory)
                   (not (gethash file my-org-download-annex--pending)))
          (puthash file t my-org-download-annex--pending)
          (my-org-download-annex--when-ready file 50))))

    (advice-add 'org-download--image :after
                (lambda (_link filename)
                  (my-org-download-annex-file filename)))
    (advice-add 'org-download-insert-link :after
                (lambda (_link filename)
                  (my-org-download-annex-file filename)))

    (defun my-org-download-delete ()
      "Delete the org-download file link at point and its local file.
      Unlike `org-download-delete`, this expands the `~/org` paths written by
      `my-org-download-link-format` and works when point is on a link description."
      (interactive)
      (let ((link (org-element-context)))
        (unless (and (eq (org-element-type link) 'link)
                     (string= (org-element-property :type link) "file"))
          (user-error "Not on a file link"))
        (let* ((beg (org-element-property :begin link))
               (end (org-element-property :end link))
               (file (expand-file-name
                      (org-link-unescape (org-element-property :path link))))
               (download-dir (expand-file-name ".blob/org-download/" org-directory)))
          (unless (file-in-directory-p file download-dir)
            (user-error "Not an org-download file: %s" file))
          (org-link-preview-clear beg end)
          (when (file-exists-p file)
            (delete-file file))
          (delete-region beg end)
          (when (and (eolp) (not (eobp)))
            (delete-char 1))))))
    ; }}}
; }}}
