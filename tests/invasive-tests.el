;;; invasive-tests.el --- isolated label regressions -*- lexical-binding: t; -*-

;; `emacs -Q --batch -l tests/invasive-tests.el -f ert-run-tests-batch-and-exit`
;; for GUI coverage, omit --batch and prefix with:
;; `xvfb-run -a env -u WAYLAND_DISPLAY GDK_BACKEND=x11`

(require 'ert)

(defconst invasive-test-root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))
(dolist (dir (directory-files (expand-file-name "straight/build" invasive-test-root)
                             t "^[^. ]"))
  (when (file-directory-p dir) (add-to-list 'load-path dir)))
(require 'org)
(require 'org-modern)
(require 'evil)

;; load only the relevant config, without starting the init or daemon.
(defun getcol (_) "#eeeeee")
(load (expand-file-name "cfg/invasive.el" invasive-test-root) nil t)
(set-face-attribute 'org-modern-label nil :height 0.9)
(set-face-attribute 'region nil :background "#ff00ff" :foreground "#000000")

(defmacro invasive-test-buffer (&rest body)
  `(let ((org-modern-todo-faces
          '(("TODO" :foreground "#ffffff" :background "#008800"))))
     (save-window-excursion
       (with-temp-buffer
         (switch-to-buffer (current-buffer))
         (insert "* TODO heading\nText <2026-09-14 Mon>\n")
         (org-mode)
         (org-modern-mode 1)
         (evil-local-mode 1)
         (jit-lock-mode 1)
         (jit-lock-register #'font-lock-fontify-region)
         (font-lock-ensure)
         ,@body))))

(defun invasive-test-pill (pos)
  (seq-find (lambda (ov) (overlay-get ov 'my-org-modern-pill-overlay))
            (overlays-at pos)))

(ert-deftest invasive-partial-fontification ()
  (invasive-test-buffer
   (dotimes (_ 3)
     (goto-char 4)
     (insert "X")
     (delete-char -1)
     (jit-lock-fontify-now (point-min) (point-max))
     (dolist (pos '(3 4 5 6 21 25 30 36))
       (should (invasive-test-pill pos))))))

(ert-deftest invasive-fontify-preserves-returned-bounds ()
  (invasive-test-buffer
   (should (equal (font-lock-fontify-region 4 5) '(jit-lock-bounds 1 . 16)))
   (should (= (overlay-start (invasive-test-pill 4)) 3))
   (should (= (overlay-end (invasive-test-pill 4)) 7))))

(ert-deftest invasive-gui-label-highlights ()
  (skip-unless (display-graphic-p))
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 200)
  (invasive-test-buffer
   (setq cursor-type nil)
   (goto-char (point-max))
   (global-hl-line-highlight)
   (redisplay t)
   (let ((size (window-text-pixel-size nil 3 7)))
     ;; an overlay must not change the label's text metrics.
     (delete-overlay (invasive-test-pill 4))
     (redisplay t)
     (should (equal size (window-text-pixel-size nil 3 7)))
     (my-org-modern-pill-overlays (point-min) (point-max))
     (goto-char 4)
     (global-hl-line-highlight)
     (dolist (selection '(nil char line block))
       (when selection
         (evil-visual-state)
         (evil-visual-make-selection 3 7 selection)
         (evil-visual-highlight)
         (should (seq-some (lambda (ov) (eq (overlay-get ov 'face) 'region))
                           (overlays-at 4))))
       (redisplay t)
       (should (equal size (window-text-pixel-size nil 3 7)))
       ;; this green occurs only in the TODO label, so its presence in the
       ;; rendered frame proves that the competing highlight did not hide it.
       (should (string-match-p (regexp-quote "rgb(0%, 53.333333%, 0%)")
                               (x-export-frames nil 'svg)))
       (when selection (evil-normal-state)))
     (goto-char (point-max))
     (global-hl-line-highlight)
     (redisplay t)
     (should (equal size (window-text-pixel-size nil 3 7)))
     (should (string-match-p (regexp-quote "rgb(0%, 53.333333%, 0%)")
                             (x-export-frames nil 'svg))))))
