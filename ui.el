(setq inhibit-splash-screen t)
(blink-cursor-mode 0)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

(custom-set-variables
 '(fringe-mode 15 nil (fringe))
 '(org-export-html-style-include-default nil)
 '(org-export-html-use-infojs nil))

(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 50))

(custom-set-faces
 ;; Standard font
 '(default ((t (:background "#262626" :foreground "#d0d0d0" :height 130 :width normal :foundry "outline" :family "Monaco"))))

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
 '(org-code ((t (:foreground "#767676"))))
 '(org-document-info ((((class color) (background dark)) (:foreground "#767676"))))
 '(org-document-info-keyword ((t (:foreground "#4e4e4e"))))
 '(org-document-title ((((class color) (background dark)) (:foreground "#767676" :weight normal))))
 '(org-done ((t (:bold t :foreground "ForestGreen"))))
 '(org-ellipsis ((((class color) (background dark)) (:foreground "#767676"))))
 '(org-footnote ((((class color) (background dark)) (:foreground "#ff7800" :underline t))))
 '(org-hide ((((background dark)) (:foreground "#4e4e4e"))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#ff7e00"))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#ff9400"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#ffb200"))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "#ffd500"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "#fef100"))))
 '(org-level-6 ((t (:inherit outline-6 :foreground "#dbfd00"))))
 '(org-level-7 ((t (:inherit outline-7 :foreground "#b3ff00"))))
 '(org-level-8 ((t (:foreground "#98f000" :slant normal))))
 '(org-link ((((class color) (background dark)) (:foreground "#ff3700" :underline t))))
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

 ;; JS2 Faces
 '(js2-function-param-face ((t (:foreground "#ffd500"))))

 ;; Minibuffer prompts
 '(minibuffer-prompt ((((class color) (min-colors 88) (background dark)) (:background "#262626" :foreground "#ffd500"))))
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
 '(show-paren-match ((((class color) (background dark)) (:background "#666666" :foreground "#ffcc33"))))

 ;; IDO colors
 '(ido-subdir ((t (:foreground "#CCCCCC")))) ;; Face used by ido for highlighting subdirs in the alternatives.
 '(ido-first-match ((t (:foreground "#ff7e00")))) ;; Face used by ido for highlighting first match.
 '(ido-only-match ((t (:foreground "#ff0000")))) ;; Face used by ido for highlighting only match.
 '(ido-indicator ((t (:foreground "#767676")))) ;; Face used by ido for highlighting its indicators (don't actually use this)
 '(ido-incomplete-regexp ((t (:foreground "#FFFFFF")))) ;; Ido face for indicating incomplete regexps. (don't use this either)

)

(provide 'ui)
