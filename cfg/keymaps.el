; muscle memory
(defkm 'normal "WW" 'save-buffer)
(defkm 'normal "C-q" 'evil-visual-block)
(defkm '(normal visual) "C-j" 'evil-join)
(defkm 'normal "C-S-j" 'eval-print-last-sexp)

; buffers
(ldr-defkm "bd" 'kill-current-buffer)
(ldr-defkm "bh" 'previous-buffer)
(ldr-defkm "bl" 'next-buffer)

(defkm 'normal "X" 'kill-current-buffer)
(defkm 'normal "J" 'previous-buffer)
(defkm 'normal "K" 'next-buffer)

; other
(ldr-defkm "ff" 'dired)
