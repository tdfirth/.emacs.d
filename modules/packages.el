;;; packages.el --- Package list and config.
;;; Commentary:
;;; Structured pretty crudely, just add a package to the big list and
;;; add any config you need with use-package.
;;; Code:
(require 'package)
(require 'cl)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)

;; TODO look at imenu further and customise
;; TODO setup dap mode with lsp
;; TODO setup docker and docker-mode with lsp
;; TODO set up reason ml with lsp reason-language-server doesn't seem to play nicely at all with lsp-mode :(
;; TODO set up c and c++ with lsp

(package-initialize)

(defvar tdf/packages '(
                       company
                       company-lsp
                       counsel-projectile
                       dap-mode
                       diminish
                       docker
                       dockerfile-mode
                       evil
                       exec-path-from-shell
                       flycheck
                       flycheck-rust
                       general
                       go-mode
                       ivy
                       lsp-mode
                       lsp-ui
                       markdown-mode
                       org
                       projectile
                       pyvenv
                       rust-mode
                       smartparens
                       solarized-theme
                       use-package
                       yaml-mode
                       yasnippet
                       wgrep
                       which-key
                       )
  "Required packages.")

(defun tdf/packages-installed-p ()
  "Check to see if a package is installed."
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
;; TODO review all use-packages and put the appropriate commands option in to restrict the crap.
(require 'use-package)

;; Set up exec path etc with exec-path-from-shell
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "WORKON_HOME")

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package evil
  :config
  (evil-mode 1))

(use-package company
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side            . bottom)
                 (reusable-frames . visible)
                 (window-height   . 0.33))))

(use-package ivy
  :config
  ;; (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (ivy-mode 1))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-save-file (concat tdf-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200
        recentf-exclude
        (list "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
              "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
              "\\.git/"
              ;; ignore private temp files
              (concat "^" (recentf-apply-filename-handlers (file-truename tdf-local-dir)))))
  ;; TODO look at the switch window hook in doom.
  )

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (list (concat tdf-local-dir "snippets")))
  (yas-global-mode 1))

(use-package lsp-mode
  :commands lsp
  :custom (lsp-log-max 100000)
  :config
  (setq lsp-session-file (concat tdf-cache-dir "lsp/session"))
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :hook (lsp-mode-hook . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-sideline-enable nil
        lsp-ui-peek-peek-height 5))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates nil))

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

(setq split-height-threshold 0)
(setq compilation-window-height 10)
;; (defun tdf/resize-compilation-buffer ()
;;   "Resizes the compilation buffer regardless of window arrangement."
;;   (when (not (get-buffer-window "*compilation*"))
;;     (save-selected-window
;;       (save-excursion
;;         (let* ((w (split-window-vertically))
;;                (h (window-height w)))
;;           (select-window w)
;;           (switch-to-buffer "*compilation*")
;;           (shrink-window (- h compilation-window-height)))))))
;; (add-hook 'compilation-mode-hook 'tdf/resize-compilation-buffer)


(provide 'packages)
;;; packages.el ends here
