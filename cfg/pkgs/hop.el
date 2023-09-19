; pcre dependency
(use-package pcre
  :straight (pcre :host github :repo "syohex/emacs-pcre"
                  :pre-build ("make" "all")
                  :files ("pcre.el" "pcre-core.so")))

; hop
(use-package hop
  :straight (hop :host github :repo "Animeshz/hop.el")
  :commands hop-word hop-char hop-line hop-line-skip-whitespace

  :init
    ; {{{ keymaps
    (ldr-defkm "Hh" 'hop-word)
    (ldr-defkm "Hc" 'hop-char)
    (ldr-defkm "Hn" 'hop-line)
    (ldr-defkm "HN" 'hop-line-skip-whitespace)
    ; }}}

  :config
    ; {{{ faces
    (set-face-attribute 'hop-face-dim-unmatched nil :foreground (getcol 'fg2)    :weight 'semilight :slant 'italic)
    (set-face-attribute 'hop-face-double-char-1 nil :foreground (getcol 'green)  :weight 'bold      :slant 'normal)
    (set-face-attribute 'hop-face-double-char-2 nil :foreground (getcol 'green)  :weight 'regular   :slant 'normal)
    (set-face-attribute 'hop-face-single-char   nil :foreground (getcol 'orange) :weight 'bold      :slant 'normal))
    ; }}}
