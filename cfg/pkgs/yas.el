; vim:et

(use-package yasnippet
  :hook
    ; activate in org-mode
    (org-mode . yas-minor-mode)

    ; expand snippets marked with condition: 'auto
    (post-command . (lambda ()
      (when yas-minor-mode
        (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
          (yas-expand)))))

  :init
    ; custom keymaps
    (defkm 'insert "C-;" 'yas-expand)
    (ldr-defkm "sr" 'yas-reload-all)
    (ldr-defkm "sn" 'yas-new-snippet)
    (ldr-defkm "se" 'yas-visit-snippet-file)

  :custom
    ; custom options
    (yas-snippet-dirs `(,(get-cfg-path "cfg/snippets/")))

  :config
    ; load snippets
    (yas-reload-all))
