; vim:et

(use-package hide-mode-line)

; {{{ wrapping
(setq-default fill-column 88)
(global-visual-line-mode t)

(use-package adaptive-wrap
  :hook (text-mode . adaptive-wrap-prefix-mode))

; TODO: this currently shrinks the modeline and makes it
; non-continuous when there is a split. temp workaround is just to
; shrink the emacs frame size another way
(use-package visual-fill-column
  :commands visual-fill-column-mode
  :custom (visual-fill-column-enable-sensible-window-split t))
; }}}

(use-package ligature
  :config
    (ligature-set-ligatures 't
      '("<-" "->" "=>"
        "<--" "-->" "<==" "==>"
        "<-|" "|->" "<=|" "|=>"
        "<->" "<=>" "<==>"
        "<=" "==" "!=" ">="
        "<<<" "<>" ">>>"
        "::" ".." "..."
        "##" "###" "####"))

    ; enable for all modes
    (global-ligature-mode t))
