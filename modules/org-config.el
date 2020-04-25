;;; org.el --- OrgMode.
;;; Commentary:
;;; Config for org mode
;;; Code:

;; Install cutting-edge version of org-mode, and from a mirror, because
;; code.orgmode.org runs on a potato.
;; https://github.com/emacs-straight/org-mode
;; (use-package org-mode
;;   :ensure t
;;   :recipe (:host github
;;                  :repo "emacs-straight/org-mode"
;;                  :files ("*.el" "lisp/*.el" "contrib/lisp/*.el"))
;; :pin "30d0b6e1f6")
;; ...And prevent other packages from pulling org; org-plus-contrib satisfies
;; the dependency already: https://github.com/raxod502/straight.el/issues/352
;; (use-package org
;; :recipe (:local-repo nil))
(setq org-indirect-buffer-display 'current-window
      org-eldoc-breadcrumb-separator " → "
      org-enforce-todo-dependencies t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-whole-heading-line t
      org-footnote-auto-label 'plain
      org-hide-leading-stars t
      org-hide-leading-stars-before-indent-mode t
      org-image-actual-width nil
      org-list-description-max-indent 4
      org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . success))
      org-startup-indented t
      org-tags-column 0
      org-use-sub-superscripts '{}
      org-pretty-entities t)

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3))
      ;; Without this, completers like ivy/helm are only given the first level of
      ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
      ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
      ;; target! e.g. FILE/Tasks/heading/subheading
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; HACK Face specs fed directly to `org-todo-keyword-faces' don't respect
;;      underlying faces like the `org-todo' face does, so we define our own
;;      intermediary faces that extend from org-todo.
(with-no-warnings
  (custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) ""))
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "PROJ(p)"  ; An ongoing project that cannot be completed in one step
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something is holding up this task; or it is paused
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")) ; Task was completed
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)))
