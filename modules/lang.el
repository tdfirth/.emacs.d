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

(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (setq tdf/format-fn 'lsp-format-buffer))))

;; rs
(use-package rust-mode)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook
          (lambda ()
            (progn
              (setq indent-tabs-mode nil)
              (setq tdf/format-fn 'rust-format-buffer)
              )))

;; ocaml


(provide 'lang)
;;; lang.el ends here
