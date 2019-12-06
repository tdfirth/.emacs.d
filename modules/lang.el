;;; lang.el
(require 'keybindings)

;; elisp
(tdf/define-keys
 :keymaps 'emacs-lisp-mode-map
 "me" '(:ignore t :which-key "eval")
 "meb" '(eval-buffer :which-key "eval buffer")
 "mer" '(eval-region :which-key "eval region")
 )

(defun indent-buffer ()
  "Indent an entire buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(setq tdf/format-fn 'indent-buffer)


;; go


;; py
(tdf/define-keys
 :keymaps 'python-mode-map
 "mv" '(:ignore t :which-key "virtualenvs")
 "mva" '(pyvenv-activate :which-key "activate")
 "mvw" '(pyvenv-workon :which-key "workon")
 )

(defun tdf/pyvenv-autoload ()
  "Automatically load the virtual environment specified in a .venv file."
  (require 'projectile)
  (let* ((pdir (projectile-project-root)) (pfile (concat pdir ".venv")))
    (if (file-exists-p pfile)
        (pyvenv-workon (with-temp-buffer
                         (insert-file-contents pfile)
                         (nth 0 (split-string (buffer-string))))))))

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (pyvenv-mode)
              (tdf/pyvenv-autoload)
              (lsp)
              (setq tdf/format-fn 'lsp-format-buffer))))

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

(tdf/define-keys
 :keymaps 'rust-mode-map
 "mr" '(tdf/cargo-process-run :which-key "run")
 "mb" '(cargo-process-build :which-key "build")
 "mt" '(:ignore t :which-key "test")
 "mtt" '(cargo-process-current-test :which-key "current")
 "mtf" '(cargo-process-current-file-tests :which-key "file")
 "mtp" '(cargo-process-test :which-key "project")
 )

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
          (lambda ()
            (progn
              (setq indent-tabs-mode nil)
              (lsp)
              (setq tdf/format-fn 'rust-format-buffer))))

;; ocaml


(provide 'lang)
;;; lang.el ends here
