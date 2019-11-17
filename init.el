(setq user-full-name "Tom Firth"
      user-mail-address "thomas.d.firth@gmail.com")
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setq exec-path (append exec-path '("/usr/local/bin")))
(require 'cl)

;; PERFORMANCE
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; ENCODING
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; PACKAGES
(load "package")
(package-initialize)
(add-to-list 'package-archives 
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar tdfirth/packages '(evil
                           ivy
                           magit
                           markdown-mode
                           org
                           projectile
                           solarized-theme
                           smartparens
                           which-key
                           )
  "Default packages")

(defun tdfirth/packages-installed-p ()
  (loop for pkg in tdfirth/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)
        ))

(unless (tdfirth/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg tdfirth/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(evil-mode 1)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(smartparens-global-mode 1)
(show-paren-mode t)

;; APPEARANCE
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(set-frame-font "Inconsolata 16" nil t)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(load-theme 'solarized-light t)

(global-hl-line-mode +1)
;; (global-display-line-numbers-mode)
(column-number-mode t)
(size-indication-mode t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq-default tab-width 2
              indent-tabs-mode nil)


;; UTIL
(setq make-backup-files nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (solarized-theme markdown-mode magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; KEY BINDINGS
;; GENERAL
;(global-set-key (kbd "C-j") 'next-line)
;(global-set-key (kbd "C-k") 'previous-line)
;(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
;(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)


;; IVY/COUNSEL
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)
