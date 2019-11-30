;;; lang.el
(require 'keybindings)

;; elisp
(tdf/define-keys
 :keymaps 'emacs-lisp-mode-map
 "e" '(:ignore t :which-key "eval")
 "eb" '(eval-buffer :which-key "eval buffer")
 )

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(setq tdf/format-fn 'indent-buffer)


;; go


;; py


;; rs
(use-package rust-mode
  :config
  (require 'rust-mode))

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; ocaml


;;; lang.el ends here
