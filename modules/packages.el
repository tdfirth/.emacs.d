;;; packages.el
(require 'package)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)

(package-initialize)

(defvar tdf/packages '(company
                       counsel-projectile
                       evil
                       flycheck
                       general
                       ivy
                       magit
                       markdown-mode
                       org
                       perspective
                       projectile
                       smartparens
                       solarized-theme
                       use-package
                       wgrep
                       which-key
                       )
  "Required packages")

(defun tdf/packages-installed-p ()
  (loop for pkg in tdf/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (tdf/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg tdf/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; TODO manage own package list so that we can setup packages before emacs starts.
(require 'use-package)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package evil
  :config
  (evil-mode 1))

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (concat tdf-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200
        recentf-exclude
        (list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
              "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
              ;; ignore private temp files
              (concat "^" (recentf-apply-filename-handlers tdf-local-dir))))
  ;; TODO look at the switch window hook in doom.
  )

(use-package smartparens
  :config
  (require 'smartparens-config)

  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already, so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  ;; But if someone does want overlays enabled, evil users will be stricken with
  ;; an off-by-one issue where smartparens assumes you're outside the pair when
  ;; you're really at the last character in insert mode. We must correct this
  ;; vile injustice.
  (setq sp-show-pair-from-inside t)
  ;; ...and stay highlighted until we've truly escaped the pair!
  (setq sp-cancel-autoskip-on-backward-movement nil)
  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for somoe modes), we halve it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 50)
  ;; This speeds up smartparens. No pair has any business being longer than 4
  ;; characters; if they must, the modes that need it set it buffer-locally.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  (add-hook 'minibuffer-setup-hook
             (defun tdf-init-smartparens-in-minibuffer-maybe-h ()
               "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or `evil-ex'."
               (when (memq this-command '(eval-expression evil-ex))
                 (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar tdf-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
             (defun tdf-enable-smartparens-mode-maybe-h ()
               (when tdf-buffer-smartparens-mode
                 (turn-on-smartparens-mode)
                 (kill-local-variable 'tdf-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
             (defun tdf-disable-smartparens-mode-maybe-h ()
               (when smartparens-mode
                 (setq-local tdf-buffer-smartparens-mode t)
                 (turn-off-smartparens-mode))))

  (smartparens-global-mode 1)
  (show-paren-mode t))

(use-package projectile
  :init
  (setq projectile-cache-file (concat tdf-cache-dir "projectile.cache")
        projectile-known-projects-file (concat tdf-cache-dir "projectile.projects")
        projectile-require-project-root t
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800 ; expire after a week
        projectile-sort-order 'recentf
        projectile-use-git-grep t) ; use git-grep for text searches
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy))

;; wgrep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'packages)
