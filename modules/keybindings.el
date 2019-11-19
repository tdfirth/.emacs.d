; https://sam217pa.github.io/2016/09/23/keybindings-strategies-in-emacs/
; https://sam217pa.github.io/2016/09/11/nuclear-power-editing-via-ivy-and-ag/
(defvar tdfirth/escape-hook nil
  "A hook run when C-g is pressed (or ESC in normal mode, for evil users")

(defun tdfirth/escape ()
  "Run `tdfirth-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'tdfirth/escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'tdfirth/escape)

;; SPC
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
  "'" '(iterm-focus :which-key "iterm")
  "?" '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
  "/" '(counsel-ag :wich-key "ag")
  "TAB" '(ivy-switch-buffer :which-key "prev buffer")
  "." '(avy-goto-word-or-subword-1  :which-key "go to word")
  "SPC" '(counsel-M-x :which-key "M-x")
  "a" '(hydra-launcher/body :which-key "applications")
  "b" '(:ignore t :which-key "buffer")
  "bb" '(counsel-switch-buffer :which-key "switch buffer")
  "c" '(:ignore t :which-key "comment")
  "cl" '(comment-or-uncomment-region-or-line :which-key "comment line")
  "d" '(:ignore t :which-key "dired")
  "f" '(:ignore t :which-key "files")
  "ff" '(counsel-find-file :which-key "find file")
  "g" '(:ignore t :which-key "magit")
  "p" '(:ignore t :which-key "projectile")
  "w" '(:ignore t :which-key "window")
  )

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-line)))

;; C-c
(general-define-key
 :states '(normal visual)
 :prefix "C-c"
  "/" '(comment-or-uncomment-region-or-line :which-key "comment line/region")
  )

(provide 'keybindings)
