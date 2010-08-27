;; Turn off the splash screen
(setq inhibit-splash-screen t)

;; Turn cursor blinking off
(blink-cursor-mode 0)

;; Turn off any chrome, etc.
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
;; For now having a menu bar can be handy
;(if (fboundp 'menu-bar-mode)
;    (menu-bar-mode -1))

;; Set the fringe to 15 px wide
(custom-set-variables
 '(fringe-mode 15 nil (fringe))
 '(org-export-html-style-include-default nil)
 '(org-export-html-use-infojs nil))

;; Faces for various modes
(custom-set-faces
 ;; Standard font
 '(default ((t (:background "#262626" :foreground "#d0d0d0" :height 112 :width normal :foundry "outline" :family "Consolas"))))

 ;; Cursor
 '(cursor ((t (:background "#e4e4e4"))))

 ;; Fringe
 '(fringe ((((class color) (background dark)) (:background "#3a3a3a" :foreground "#626262"))))

 ;; Shell and Eshell faces
 '(comint-highlight-prompt ((((min-colors 88) (background dark)) (:foreground "#d0d0d0"))))
 '(eshell-ls-directory ((((class color) (background dark)) (:foreground "#aaaaaa" :weight bold))))
 '(eshell-ls-executable ((((class color) (background dark)) (:foreground "#ffcc33" :weight bold))))
 '(eshell-prompt ((t (:foreground "#ccff00" :weight bold))))

 ;; Dired faces
 '(diredp-date-time ((t (:foreground "#dddddd"))))
 '(diredp-deletion ((t (:background "Red" :foreground "#cccccc"))))
 '(diredp-dir-heading ((t (:background "#eeeeee" :foreground "#333333"))))
 '(diredp-dir-priv ((t (:background "#666666" :foreground "#f8b20c"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-file-name ((t (:foreground "#f8b20c"))))
 '(diredp-file-suffix ((t (:foreground "#ffcc33"))))
 '(diredp-flag-mark ((t (:background "#999999" :foreground "yellow"))))
 '(diredp-flag-mark-line ((t (:background "#888888"))))
 '(diredp-ignored-file-name ((t (:foreground "#777777"))))
 '(diredp-no-priv ((t (:foreground "#999999"))))
 '(diredp-read-priv ((t (:background "#777777"))))
 '(diredp-write-priv ((t (:background "#666666"))))

 ;; org-mode face
 '(org-agenda-dimmed-todo-face ((((background dark)) (:foreground "#cccccc"))))
 '(org-document-info ((((class color) (background dark)) (:foreground "#767676"))))
 '(org-document-info-keyword ((t (:foreground "#4e4e4e"))))
 '(org-document-title ((((class color) (background dark)) (:foreground "#767676" :weight normal))))
 '(org-done ((t (:bold t :foreground "ForestGreen"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "#4e4e4e"))))
 '(org-todo ((t (:bold t :foreground "Red"))))

 ;; Font Lock faces
 '(font-lock-builtin-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ffaf00"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#4e4e4e"))))
 '(font-lock-comment-face ((nil (:foreground "#767676"))))
 '(font-lock-constant-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ff8700"))))
 '(font-lock-doc-face ((t (:foreground "#6c6c6c"))))
 '(font-lock-function-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ffd700" :weight normal))))
 '(font-lock-keyword-face ((((class color) (min-colors 88) (background dark)) (:foreground "#d7ff00" :weight normal))))
 '(font-lock-negation-char-face ((t (:foreground "#dddddd"))))
 '(font-lock-string-face ((((class color) (min-colors 88) (background dark)) (:foreground "#d7af00" :slant italic))))
 '(font-lock-type-face ((((class color) (min-colors 88) (background dark)) (:foreground "#afd700"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 88) (background dark)) (:foreground "#ffff00"))))

 ;; Minibuffer prompts
 '(minibuffer-prompt ((((class color) (min-colors 88) (background dark)) (:background "#ff0000" :foreground "#d0d0d0"))))
 '(minibuffer-noticeable-prompt ((((class color) (min-colors 88) (background dark)) (:background "#ff5f00" :foreground "#262626"))))

 ;; Mode lines
 '(mode-line ((((class color) (min-colors 88)) (:background "#3a3a3a" :foreground "#767676" :box (:line-width 1 :style pressed-button)))))
 '(mode-line-buffer-id ((((class color) (min-colors 88)) (:foreground "#d0d0d0"))))
 '(mode-line-emphasis ((((class color) (min-colors 88)) (:background "#ff0000" :foreground "#ffffff"))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:background "#ffff00" :foreground "#ffffff"))))
 '(mode-line-inactive ((((class color) (min-colors 88)) (:background "#d0d0d0" :foreground "#262626" :box (:line-width 1 :style pressed-button)))))

 ;; Selection highlighting
 '(region ((((class color) (min-colors 88) (background dark)) (:background "#4e4e4e" :foreground "#ff5f00"))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#4e4e4e" :foreground "#d75f00"))))
 '(show-paren-match ((((class color) (background dark)) (:background "#666666" :foreground "#ffcc33")))))
