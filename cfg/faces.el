; vim:et

(custom-set-faces
  `(minibuffer-prompt ((t (:background ,(getcol 'bg1) :foreground ,(getcol 'green) :weight bold))))

  ; keybind
  `(help-key-binding ((t (:inherit nil :foreground ,(getcol 'blue)))))

  ; selected region
  `(region ((t (:background ,(getcol 'visual))))))

; dim minibuffer text
(add-hook 'minibuffer-setup-hook
  (lambda ()
    (set (make-local-variable 'face-remapping-alist)
      `((default :foreground ,(getcol 'fg2))))))

; dim echo area text
; this doesn't work when using a daemon; it complains about the buffer not existing

; (with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default `(:foreground ,(getcol 'fg2))))
; (with-current-buffer " *Echo Area 1*" (face-remap-add-relative 'default `(:foreground ,(getcol 'fg2))))
