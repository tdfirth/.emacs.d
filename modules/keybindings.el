;;; keybindings.el
; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
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
  "'" '(iterm-focus :which-key "iterm")
  "?" '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
  "/" '(counsel-projectile-rg :wich-key "rg")
  "TAB" '(ivy-switch-buffer :which-key "prev buffer")
  "." '(avy-goto-word-or-subword-1  :which-key "go to word")
  "SPC" '(counsel-M-x :which-key "M-x")
  "a" '(hydra-launcher/body :which-key "applications")
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
  "m" '(:ignore t :which-key "major")
  "p" '(:ignore t :which-key "projectile")
  "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
  "pf" '(projectile-find-file :which-key "find file")
  "pp" '(counsel-projectile-switch-project :which-key "switch projet")
  "pK" '(projectile-kill-buffers :which-key "kill buffers")
  "ps" '(projectile-save-project-buffers :which-key "save buffers")
  "w" '(:ignore t :which-key "window")
  "wv" '(evil-window-vsplit :which-key "window")
  "wh" '(evil-window-split :which-key "window")
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
