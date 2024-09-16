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

; cache folder
(mkdir (concat (file-truename user-emacs-directory) "cache/") :parents)

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

; {{{ helper functions
(setq customcolors
  '(bg1    "#2b3339"
    bg2    "#323c41"
    bg3    "#3a454a"
    bg4    "#445055"
    fg1    "#d3c6aa"
    fg2    "#859289"

    red    "#e67e80"
    orange "#e69875"
    yellow "#dbbc7f"
    green  "#a7c080"
    aqua   "#83c092"
    blue   "#7fbbb3"
    purple "#d699b6"

    visual   "#503946"
    bg-red   "#4e3e43"
    bg-green "#404d44"))

; get a color
(defun getcol (name) (plist-get customcolors name))

; get a file path relative to the config folder
(defun get-cfg-path (&rest paths) (apply #'concat (file-truename user-emacs-directory) paths))

; load a config
(defun loadcfg (file) (load (concat (expand-file-name file (get-cfg-path "cfg/")) ".el")))

; define or set faces, akin to custom-set-faces
(defun custom-defset-faces (&rest args)
  (progn
    (dolist (pair args)
      (let ((face (car pair)))
        (if (not (facep face)) (defface face '() "Defined via custom-defset-faces"))))
    (apply #'custom-set-faces args)))
; }}}

; {{{ packages & config
; some minor tweak packages (e.g. visual-fill-column) are under cfg/misc.el instead

; {{{ keybinding facilities
(use-package general)

(general-create-definer defkm
  :keymaps 'override)

(general-create-definer ldr-defkm
  :states '(normal insert emacs)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")
; }}}

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
