;;; appearance.el

(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;;(blink-cursor-mode -1)

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(global-hl-line-mode +1)
;; (global-display-line-numbers-mode)
(column-number-mode t)
(size-indication-mode t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq-default tab-width 2
              indent-tabs-mode nil)

(provide 'appearance)
