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

;; IVY/COUNSEL
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-SPC") 'counsel-M-x)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-c b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c C-f") 'counsel-find-file)

;; SPC
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
  "'" '(iterm-focus :which-key "iterm")
  "?" '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
  "/" '(counsel-ag :wich-key "ag")
  "TAB" '(ivy-switch-buffer :which-key "prev buffer")
  "." '(avy-goto-word-or-subword-1  :which-key "go to word")
  "SPC" '(counsel-M-x :which-key "M-x")
  "a" '(hydra-launcher/body :which-key "Applications")
  "b" '(hydra-buffer/body t :which-key "Buffer")
  "c" '(:ignore t :which-key "Comment")
  "cl" '(comment-or-uncomment-region-or-line :which-key "comment line")
  "w" '(hydra-window/body :which-key "Window")
  "f" '(:ignore t :which-key "Files")
  "fd" '(counsel-git :which-key "find in git dir")
  )

(provide 'keybindings)
