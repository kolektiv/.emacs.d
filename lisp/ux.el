;; General

(setq inhibit-splash-screen t)
(blink-cursor-mode 0)

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Fringe

(custom-set-variables
 '(fringe-mode 15 nil (fringe)))

;; Frames

(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 50))

;; Faces

(custom-set-faces
 
 ;; Default
 
 '(default
    ((t
      (:background "#262626"
       :foreground "#d0d0d0"
       :height 130
       :width normal
       :foundry "outline"
       :family "Monaco"))))

 ;; Cursor
 
 '(cursor
   ((t
     (:background "#e4e4e4"))))

 ;; Fringe
 
 '(fringe
   ((((class color)
      (background dark))
     (:background "#3a3a3a"
      :foreground "#626262"))))

 ;; Mode

 '(mode-line
   ((((class color)
      (min-colors 88))
     (:background "#3a3a3a"
      :foreground "#767676"
      :box (:line-width 1
            :style pressed-button)))))

 '(mode-line-buffer-id
   ((((class color)
      (min-colors 88))
     (:foreground "#d0d0d0"))))

 '(mode-line-emphasis
   ((((class color)
      (min-colors 88))
     (:background "#ff0000"
      :foreground "#ffffff"))))

 '(mode-line-highlight
   ((((class color)
      (min-colors 88))
     (:background "#ffff00"
      :foreground "#ffffff"))))

 '(mode-line-inactive
   ((((class color)
      (min-colors 88))
     (:background "#d0d0d0"
      :foreground "#262626"
      :box (:line-width 1
            :style pressed-button)))))

 ;; Mini Buffer

 '(minibuffer-prompt
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "#262626"
      :foreground "#ffd500"))))
 
 '(minibuffer-noticeable-prompt
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "#ff5f00"
      :foreground "#262626"))))

 ;; Selection

 '(region
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "#4e4e4e"
      :foreground "#ff5f00"))))
 
 '(highlight
   ((((class color)
      (min-colors 88)
      (background dark))
     (:background "#4e4e4e"
      :foreground "#d75f00"))))

 '(show-paren-match
   ((((class color)
      (background dark))
     (:background "#666666"
      :foreground "#ffcc33"))))
 
 ;; Font Lock
 
 '(font-lock-builtin-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#ffaf00"))))
 
 '(font-lock-comment-delimiter-face
   ((t
     (:foreground "#4e4e4e"))))
 
 '(font-lock-comment-face
   ((nil
     (:foreground "#767676"))))
 
 '(font-lock-constant-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#ff8700"))))
 
 '(font-lock-doc-face
   ((t
     (:foreground "#6c6c6c"))))
 
 '(font-lock-function-name-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#ffd700"
      :weight normal))))
 
 '(font-lock-keyword-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#d7ff00"
      :weight normal))))

 '(font-lock-negation-char-face
   ((t
     (:foreground "#dddddd"))))

 '(font-lock-string-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#d7af00"
      :slant italic))))

 '(font-lock-type-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#afd700"))))

 '(font-lock-variable-name-face
   ((((class color)
      (min-colors 88)
      (background dark))
     (:foreground "#ffff00")))))

;; Provide

(provide 'ux)
