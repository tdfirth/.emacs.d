;;; lang.el --- Language support.
;;; Commentary:
;;; Each language gets it's own little section...
(require 'keybindings)

(defmacro tdf/lang-hook (hook &rest args)
  "Execute ARGS on HOOK."
  `(add-hook ,hook (lambda () (progn ,@args))))

;;; Code:
;; elisp
(defun indent-buffer ()
  "Indent an entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


(tdf/lang-hook 'emacs-lisp-mode-hook
               (company-mode))

;; go
(use-package go-mode
  :mode ("\\.go\\'" . go-mode))

(tdf/define-ctrl-c-keys
 :keymaps 'go-mode-map
 "C-m r" '(:ignore t :which-key "run")
 "C-m b" '(:ignore t :which-key "build")
 "C-t t" '(cargo-process-current-test :which-key "current")
 "C-t f" '(cargo-process-current-file-tests :which-key "file")
 "C-t p" '(cargo-process-test :which-key "project")
 )

(tdf/lang-hook 'go-mode-hook
               (lsp))
;; py
(tdf/define-ctrl-c-keys
 :keymaps 'python-mode-map
 "C-v" '(:ignore t :which-key "virtualenvs")
 "C-v a" '(pyvenv-activate :which-key "activate")
 "C-v w" '(pyvenv-workon :which-key "workon")
 )

(defun tdf/pyvenv-autoload ()
  "Automatically load the virtual environment specified in a .venv file."
  (require 'projectile)
  (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
    (if (file-exists-p pfile)
        (pyvenv-workon (with-temp-buffer
                         (insert-file-contents pfile)
                         (nth 0 (split-string (buffer-string))))))))

(tdf/lang-hook 'python-mode-hook
               (pyvenv-mode)
               (tdf/pyvenv-autoload)
               (lsp))

;; rs
(use-package rust-mode)

(defun tdf/cargo-process-run ()
  "Build and run Rust code."
  (interactive)
  (cargo-process-run)
  (let ((orig-win (selected-window))
        (run-win
         (display-buffer (get-buffer "*Cargo Run*") nil 'visible)))
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)))

(tdf/define-ctrl-c-keys
 :keymaps 'rust-mode-map
 "C-m r" '(tdf/cargo-process-run :which-key "run")
 "C-m b" '(cargo-process-build :which-key "build")
 "C-t t" '(cargo-process-current-test :which-key "current")
 "C-t f" '(cargo-process-current-file-tests :which-key "file")
 "C-t p" '(cargo-process-test :which-key "project")
 )

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(tdf/lang-hook 'rust-mode-hook
               (setq indent-tabs-mode nil)
               (lsp))

;; ocaml
(tdf/lang-hook 'tuareg-mode-hook
               (merlin-mode)
               (lsp))

;; reason
(tdf/lang-hook 'reason-mode-hook
               (merlin-mode)
               (lsp))

;; docker
(tdf/lang-hook 'dockerfile-mode-hook
               (lsp))

;; html
(tdf/lang-hook 'html-mode-hook
               (lsp))
;; css
(tdf/lang-hook 'css-mode-hook
               (lsp))

;; html
(tdf/lang-hook 'yaml-mode-hook
               (lsp))

(provide 'lang)
;;; lang.el ends here
