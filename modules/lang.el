;;; lang.el
(require 'keybindings)

;; elisp
(tdf/define-keys
 :keymaps 'emacs-lisp-mode-map
 "me" '(:ignore t :which-key "eval")
 "meb" '(eval-buffer :which-key "eval buffer")
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
          (lambda ()
            (progn
              (setq indent-tabs-mode nil)
              (setq tdf/format-fn 'rust-format-buffer)
              )))

;; ocaml


;;; lang.el ends here
