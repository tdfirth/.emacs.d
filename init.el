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
      backup-directory-alist `(("." . ,(concat tdf-cache-dir "backup/")))
      custom-file (concat tdf-local-dir "custom.el"))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(add-to-list 'load-path (concat (file-name-directory load-file-name) "modules"))
(require 'appearance)
(require 'packages)
(require 'keybindings)
(require 'lang)

;; Reset gc for normal use.
(setq gc-cons-threshold 16777216)

;;; init.el ends here
