(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

(package-initialize)

(defvar tdfirth/packages '(counsel-projectile
                           evil
                           general
                           ivy
                           magit
                           markdown-mode
                           org
                           projectile
                           solarized-theme
                           smartparens
                           which-key
                           )
  "Required packages")

(defun tdfirth/packages-installed-p ()
  (loop for pkg in tdfirth/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (tdfirth/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg tdfirth/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; solarized-light
(load-theme 'solarized-light t)

;; evil
(evil-mode 1)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; smartparens
(smartparens-global-mode 1)
(show-paren-mode t)

(provide 'packages)
