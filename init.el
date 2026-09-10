; vim:et

; we need these options before any other elisp code
(setq max-specpdl-size 3200)
(setq max-lisp-eval-depth 3200)
(setq debug-on-error t)

; hide stuff during loading
(tool-bar-mode   -1)
(menu-bar-mode   -1)
(scroll-bar-mode -1)
(setq-default mode-line-format nil)

; config reloading
(global-set-key (kbd "<f5>")
  (lambda ()
    (interactive)
    (load-file user-init-file)))

(defun get-cfg-path (&rest paths)
  (apply #'concat (file-truename user-emacs-directory) paths))
(defun loadcfg (file)
  (load (concat (expand-file-name file (get-cfg-path "cfg/")) ".el")))

; cache folder
(mkdir (get-cfg-path "cache/transient/") :parents)
(setq transient-levels-file  (get-cfg-path "cache/transient/levels.el")
      transient-values-file  (get-cfg-path "cache/transient/values.el")
      transient-history-file (get-cfg-path "cache/transient/history.el"))

; {{{ straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; integrate with use-package
(straight-use-package 'use-package)
(use-package straight :custom (straight-use-package-by-default t))

; always ensure
(setq use-package-always-ensure t)
; }}}

; {{{ GCMH
(use-package gcmh
  :straight (gcmh :host gitlab :repo "koral/gcmh")
  :demand t
  :config
    (gcmh-mode 1))
; }}}

; {{{ color palettes
(setq my-palette 'everforest)

(pcase my-palette
  ; {{{ elysium
  ('elysium
    (setq my-lightmode t
          customcolors
          '(bg1      "#ffffff"
            bg2      "#f4f4f4"
            bg3      "#ebebeb"
            bg4      "#e4e4e4"
            fg1      "#333333"
            fg2      "#777777"
            red      "#904961"
            orange   "#90502a"
            yellow   "#b38143"
            green    "#546b4f"
            aqua     "#406b75"
            blue     "#535d9c"
            purple   "#79508a"
            visual   "#e4dce8"
            bg-red   "#e9dbdf"
            bg-green "#dce2da")))
  ; }}}
  ; {{{ paradise
  ('paradise
    (setq my-lightmode nil
          customcolors
          '(bg1      "#151515"
            bg2      "#202020"
            bg3      "#2a2a2a"
            bg4      "#343434"
            fg1      "#e3e3e3"
            fg2      "#999999"
            red      "#b66467"
            orange   "#c9987b" ; 50/50 mix of red and yellow (srgb->linear->mix->srgb)
            yellow   "#d9bc8c"
            green    "#8c977d"
            aqua     "#8aa6a2"
            blue     "#8da3b9"
            purple   "#a988b0"
            visual   "#493b4c" ; 15/85 mix of purple and bg1
            bg-red   "#4e2c2d" ; same for red
            bg-green "#3c4136"))) ; same for green
  ; }}}
  ; {{{ everforest
  ('everforest
    (setq my-lightmode nil
          customcolors
          '(bg1      "#2b3339"
            bg2      "#323c41"
            bg3      "#3a454a"
            bg4      "#445055"
            fg1      "#d3c6aa"
            fg2      "#859289"
            red      "#e67e80"
            orange   "#e69875"
            yellow   "#dbbc7f"
            green    "#a7c080"
            aqua     "#83c092"
            blue     "#7fbbb3"
            purple   "#d699b6"
            visual   "#503946"
            bg-red   "#4e3e43"
            bg-green "#404d44"))))
  ; }}}

(defun getcol (name) (plist-get customcolors name))
; }}}

; {{{ packages & config
; some minor tweak packages (e.g. visual-fill-column) are under cfg/misc.el instead

(use-package general)

(general-create-definer defkm
  :keymaps 'override)

(general-create-definer ldr-defkm
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")

; all ui packages (nano-theme, nano-modeline, etc.)
(loadcfg "pkgs/ui")

; evil
(loadcfg "pkgs/evil")

; ivy & related
(loadcfg "pkgs/ivy")

; org
(loadcfg "pkgs/org")

; yasnippet
(loadcfg "pkgs/yas")

; misc packages
(loadcfg "pkgs/misc")
; }}}

; base emacs config
(loadcfg "misc")
(loadcfg "faces")
(loadcfg "keymaps")
