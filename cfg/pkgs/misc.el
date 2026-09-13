; vim:et

(use-package hide-mode-line)

; TODO: disabled for now since this currently shrinks the modeline and makes it
; non-continuous when there is a split that is larger than the column count
; (use-package visual-fill-column
;   :commands visual-fill-column-mode
;   :custom (visual-fill-column-enable-sensible-window-split t))

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
