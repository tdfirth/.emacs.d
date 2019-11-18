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
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(add-to-list 'load-path (concat (file-name-directory load-file-name) "modules"))
(require 'appearance)
(require 'packages)
(require 'keybindings)

(setq gc-cons-threshold 50000000)
