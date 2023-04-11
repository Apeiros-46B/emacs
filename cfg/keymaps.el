; muscle memory
(defkm 'normal "WW" 'save-buffer)
(defkm 'normal "C-q" 'evil-visual-block)
(defkm '(normal visual) "C-j" 'evil-join)
(defkm 'normal "C-J" 'eval-print-last-sexp)

; buffers
(defkm 'normal "X" 'kill-current-buffer)
(defkm 'normal "J" 'previous-buffer)
(defkm 'normal "K" 'next-buffer)

; other
(ldr-defkm "ff" 'dired)
