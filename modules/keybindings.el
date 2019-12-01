;;; keybindings.el
(defvar tdf/escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users.")

(defun tdf/escape ()
  "Run `tdf-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'tdf/escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'tdf/escape)

;; Format on save
(defvar tdf/format-fn nil
  "The format function to invoke for a given buffer/major mode.")

(defun tdf/format-buffer ()
  "Run `tdf/format-fn'."
  (funcall tdf/format-fn))

(add-hook 'before-save-hook 'tdf/format-buffer)

;; Keybindings
(defmacro tdf/define-keys (&rest args)
  "Defines keybindings on the SPC leader.
The first item in ARGS should be the keymaps argument if necessary.
The rest of ARGS should just be the keybinding format as expected by general."
  `(general-define-key
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    ,@args))

(tdf/define-keys
 "SPC" '(counsel-M-x :which-key "M-x")
 "/" '(counsel-projectile-rg :which-key "rg")
 "TAB" '(company-complete :which-key "complete")
 ;; buffers
 "b" '(:ignore t :which-key "buffer")
 "bb" '(counsel-switch-buffer :which-key "switch buffer")
 "bk" '(kill-current-buffer :which-key "kill buffer")
 "bn" '(switch-to-next-buffer :which-key "next buffer")
 "bp" '(switch-to-prev-buffer :which-key "previous buffer")
 ;; dired
 "d" '(:ignore t :which-key "dired")
 ;; errors
 "e" '(:ignore t :which-key "errors")
 "el" '(flycheck-list-errors :which-key "list errors")
 "en" '(flycheck-next-error :which-key "next error")
 "ep" '(flycheck-previous-error :which-key "previous error")
 ;; files
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")
 "fr" '(counsel-recentf :which-key "find file")
 ;; git
 "g" '(:ignore t :which-key "magit")
 "gg" '(magit-status :which-key "magit status")
 ;; major mode
 "m" '(:ignore t :which-key "toggle")
 ;; org mode
 "o" '(:ignore t :which-key "toggle")
 ;; projectile
 "p" '(:ignore t :which-key "projectile")
 "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
 "pf" '(projectile-find-file :which-key "find file")
 "pp" '(counsel-projectile-switch-project :which-key "switch projet")
 "pK" '(projectile-kill-buffers :which-key "kill buffers")
 "ps" '(projectile-save-project-buffers :which-key "save buffers")
 ;; toggle
 "t" '(:ignore t :which-key "toggle")
 "tt" '(load-theme :which-key "theme")
 "tw" '(which-key-mode :which-key "which key")
 ;; windows
 "w" '(:ignore t :which-key "window")
 "wh" '(evil-window-left :which-key "left")
 "wj" '(evil-window-down :which-key "down")
 "wk" '(evil-window-up :which-key "up")
 "wl" '(evil-window-right :which-key "right")
 "ws" '(evil-window-left :which-key "split")
 "wsv" '(evil-window-vsplit :which-key "vertical")
 "wsh" '(evil-window-split :which-key "horizontal")
 )

(defun tdf/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

(general-define-key
 :states '(normal visual)
 :prefix "C-c"
 ";" '(tdf/comment-or-uncomment-region-or-line :which-key "comment line/region")
 )

(general-define-key
 :keymaps '(ivy-occur-grep-mode-map)
 :prefix "C-c"
 "C-e" '(ivy-wgrep-change-to-wgrep-mode :which-key "edit results"))

(provide 'keybindings)

;;; keybindings.el ends here
