;;; keybindings.el --- Keybindings.
;;; Commentary:
;;; Core navigation/language keybindings and support code.
;;; Code:
(defvar tdf/escape-hook nil
  "A hook run when \\[keyboard-quit] is pressed (or ESC in normal mode, for evil users.")

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
(defvar tdf/formatters '((emacs-lisp-mode . indent-buffer)))

(defun tdf/add-formatter (mode formatter)
  "Add FORMATTER for MODE."
  (add-to-list 'tdf/formatters (list mode formatter)))

(defun tdf/format-buffer ()
  "Format buffers before saving."
  (funcall (cdr (assoc major-mode tdf/formatters)))
  nil)

(add-hook 'before-save-hook 'tdf/format-buffer)

;; Dired
;; Make dired open in the same window when using RET or ^
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil) ; disables warning
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Comment/uncomment
(defun tdf/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)
    (forward-line)))

;; Keybindings
(defmacro tdf/define-spc-keys (&rest args)
  "Defines keybindings on the SPC leader for navigation.
The first item in ARGS should be the keymaps argument if necessary.
The rest of ARGS should just be the keybinding format as expected by general."
  `(general-define-key
    :states '(normal visual insert emacs treemacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    ,@args))

(tdf/define-spc-keys
 "SPC" '(counsel-M-x :which-key "M-x")
 "/" '(counsel-projectile-rg :which-key "rg")
 "`" '(shell-command :which-key "shell")
 ;; apropos
 "a" '(counsel-apropos :which-key "apropos")
 ;; buffers
 "b" '(:ignore t :which-key "buffer")
 "bb" '(ivy-switch-buffer :which-key "switch buffer")
 "bk" '(kill-current-buffer :which-key "kill buffer")
 "bn" '(switch-to-next-buffer :which-key "next buffer")
 "bp" '(switch-to-prev-buffer :which-key "previous buffer")
 ;; dired
 "d" '(:ignore t :which-key "dired")
 "dd" '(dired :which-key "dired")
 ;; elisp
 "e" '(:ignore t :which-key "elisp")
 "ee" '(eval-expression :which-key "eval expression")
 "eb" '(eval-buffer :which-key "eval buffer")
 "er" '(eval-region :which-key "eval region")
 ;; files
 "f" '(:ignore t :which-key "files")
 "ff" '(counsel-find-file :which-key "find file")
 "fr" '(counsel-recentf :which-key "find file")
 ;; major mode
 "m" '(:ignore t :which-key "modes")
 "mt" '(load-theme :which-key "theme")
 "mw" '(which-key-mode :which-key "which key")
 ;; org mode
 "o" '(:ignore t :which-key "toggle")
 ;; projectile
 "p" '(:ignore t :which-key "projectile")
 "pb" '(projectile-switch-to-buffer :which-key "switch buffer")
 "pf" '(projectile-find-file :which-key "find file")
 "pp" '(counsel-projectile-switch-project :which-key "switch projet")
 "pK" '(projectile-kill-buffers :which-key "kill buffers")
 "ps" '(projectile-save-project-buffers :which-key "save buffers")
 ;; windows
 "w" '(:ignore t :which-key "window")
 "w=" '(balance-windows :which-key "balance")
 "wh" '(evil-window-left :which-key "left")
 "wj" '(evil-window-down :which-key "down")
 "wk" '(evil-window-up :which-key "up")
 "wl" '(evil-window-right :which-key "right")
 "ws" '(:ignore t :which-key "split")
 "wsv" '(evil-window-vsplit :which-key "vertical")
 "wsh" '(evil-window-split :which-key "horizontal")
 )

(defmacro tdf/define-ctrl-c-keys (&rest args)
  "Defines keybindings on the C-c leader. I use all of these language
related bindings.  The first item in ARGS should be the keymaps argument if
necessary.  The rest of ARGS should just be the keybinding format as expected
by general."
  `(general-define-key
    :states '(normal visual insert emacs)
    :prefix "C-c"
    ,@args))

(tdf/define-ctrl-c-keys
 "RET" '(company-complete :which-key "complete")
 "C-;" '(tdf/comment-or-uncomment-region-or-line :which-key "comment line/region")
 ;; errors
 "C-e" '(:ignore t :which-key "errors")
 "C-e l" '(flycheck-list-errors :which-key "list errors")
 "C-e n" '(flycheck-next-error :which-key "next error")
 "C-e p" '(flycheck-previous-error :which-key "previous error")
 ;; find
 "C-f" '(:ignore t :which-key "goto")
 "C-f d" '(lsp-find-definition :which-key "definition")
 "C-f u" '(lsp-find-references :which-key "uses")
 "C-f i" '(lsp-ui-imenu :which-key "imenu")
 ;; make
 "C-m" '(:ignore t :which-key "make")
 ;; rename
 "C-r" '(lsp-rename :which-key "lsp rename")
 ;; test
 "C-t" '(:ignore  :which-key "test")
 )

(general-define-key
 :keymaps '(ivy-occur-grep-mode-map)
 :prefix "C-c"
 "C-e" '(ivy-wgrep-change-to-wgrep-mode :which-key "edit results"))

(provide 'keybindings)
;;; keybindings.el ends here
