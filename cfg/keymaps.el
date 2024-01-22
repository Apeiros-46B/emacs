; vim:et

; convenience
(defkm 'normal "WW"    'save-buffer)
(defkm 'normal "C-q"   'evil-visual-block)
(defkm 'normal "C-S-j" 'eval-print-last-sexp)

; buffers
(ldr-defkm "x" 'kill-current-buffer)
(ldr-defkm "j" 'previous-buffer)
(ldr-defkm "k" 'next-buffer)

; help
(ldr-defkm "Hb" 'describe-bindings)
(ldr-defkm "Hf" 'describe-function)
(ldr-defkm "Hj" 'describe-key-briefly)
(ldr-defkm "Hk" 'describe-key)
(ldr-defkm "HK" 'describe-keymap)
(ldr-defkm "Hm" 'describe-mode)
(ldr-defkm "HM" 'describe-minor-mode)
(ldr-defkm "Hs" 'describe-symbol)
(ldr-defkm "Hv" 'describe-variable)
