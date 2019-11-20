(setq user-full-name "Tom Firth"
      user-mail-address "thomas.d.firth@gmail.com")
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(require 'cl)

;; performance
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold 100000000)

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; other
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/autosave/" t)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(add-to-list 'load-path (concat (file-name-directory load-file-name) "modules"))
(require 'appearance)
(require 'packages)
(require 'keybindings)

(setq gc-cons-threshold 50000000)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck company which-key wgrep solarized-theme smartparens markdown-mode magit general evil-leader counsel-projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
