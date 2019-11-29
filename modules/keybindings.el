;;; keybindings.el
(defvar tdf/escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users")

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

;; SPC
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
  "/" '(counsel-projectile-rg :wich-key "rg")
  "SPC" '(counsel-M-x :which-key "M-x")
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-switch-buffer :which-key "switch buffer")
  "bk" '(kill-current-buffer :which-key "kill buffer")
  "bn" '(switch-to-next-buffer :which-key "next buffer")
  "bp" '(switch-to-prev-buffer :which-key "previous buffer")
  "c" '(:ignore t :which-key "comment")
  "cl" '(comment-or-uncomment-region-or-line :which-key "comment line")
  "d" '(:ignore t :which-key "dired")
  "f" '(:ignore t :which-key "files")
  "ff" '(counsel-find-file :which-key "find file")
  "fr" '(counsel-recentf :which-key "find file")
  "g" '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "magit status")
  "m" '(:ignore t :which-key "modes")
  "mw" '(which-key-mode :which-key "which key")
  "p" '(:ignore t :which-key "projectile")
  "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
  "pf" '(projectile-find-file :which-key "find file")
  "pp" '(counsel-projectile-switch-project :which-key "switch projet")
  "pK" '(projectile-kill-buffers :which-key "kill buffers")
  "ps" '(projectile-save-project-buffers :which-key "save buffers")
  "w" '(:ignore t :which-key "window")
  "wv" '(evil-window-vsplit :which-key "window")
  "wh" '(evil-window-split :which-key "window")
  "t" '(:ignore t :which-key "test")
  )
;; TODO this pattern will give consistent and quick keybindings between language modes, though it's a little cumbersome.
;; can write a macro to provide easy bindings for the common tasks.
;; Run tests, type check, find symbol, format buffer (that should be hooked on save), go to definition, find usages
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :keymaps 'emacs-lisp-mode-map
  "ts" '(counsel-M-x :which-key "test lisp mode")
)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :keymaps 'sh-mode-map
 "ts" '(counsel-switch-buffer :which-key "test shell mode")
)

(defun tdf/comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

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
