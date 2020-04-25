;;; init.el
(require 'cl)

(setq user-full-name "Tom Firth"
      user-mail-address "thomas.d.firth@gmail.com")

;; startup performance
(setq gc-cons-threshold most-positive-fixnum)
(setq large-file-warning-threshold 100000000)

;; editor
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defvar user-home (substitute-in-file-name "$HOME/"))
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

(add-to-list 'load-path tdf-module-dir)
(require 'appearance)
(require 'packages)
(require 'keybindings)
(require 'lang)
(require 'org)

;; Reset gc for normal use.
(setq gc-cons-threshold 16777216)

;;; init.el ends here
