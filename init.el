;;; init.el
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

;; editor
(defconst tdf-emacs-dir user-emacs-directory)
(defconst tdf-module-dir (concat tdf-emacs-dir "modules"))
(defconst tdf-local-dir (concat tdf-emacs-dir ".local/"))
(setq package-user-dir (concat tdf-local-dir "vendor/"))
(defconst tdf-cache-dir(concat tdf-local-dir "cache/"))
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      auto-save-list-file-name (concat tdf-cache-dir "autosave/")
      backup-directory-alist `(("." . ,(concat tdf-cache-dir "backup/"))))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(add-to-list 'load-path (concat (file-name-directory load-file-name) "modules"))
(require 'appearance)
(require 'packages)
(require 'keybindings)

;; Reset gc for normal use.
(setq gc-cons-threshold 16777216)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rust-mode evil-magit flycheck company which-key wgrep solarized-theme smartparens markdown-mode magit general evil-leader counsel-projectile)))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
