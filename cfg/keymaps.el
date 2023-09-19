; muscle memory
(defkm 'normal "WW"    'save-buffer)
(defkm 'normal "C-q"   'evil-visual-block)
(defkm 'normal "C-S-j" 'eval-print-last-sexp)

; buffers
(ldr-defkm "x" 'kill-current-buffer)
(ldr-defkm "j" 'previous-buffer)
(ldr-defkm "k" 'next-buffer)

; other
(ldr-defkm "ff" 'find-file)
