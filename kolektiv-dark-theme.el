;;; kolektiv-dark-theme.el --- Personal Theme

;; Author: Andrew Cherry <andrew@xyncro.com>
;; URL: https://github.com/kolektiv/.emacs.d

;;; Commentary:

;; A minimal, largely monochrome in normal operation, dark theme, designed
;; to let the code take front of stage with minimal distraction.

;;; Code:

;; ==============================================================================

;; Groups and Faces

;; Custom faces and groups are defined where applicable, due to the need for
;; more granularity of control over some package faces.

;; ------------------------------------------------------------------------------

;; Projectile

(defgroup projectile-faces nil
  "Font-lock faces for projectile."
  :group 'projectile
  :group 'faces)

(defface projectile-mode-line
  '((t :inherit mode-line))
  "Face used for projectile-mode-line information."
  :group 'projectile-faces)

;; ==============================================================================

;; Theme

(deftheme kolektiv-dark
  "Personal Theme (Dark)")

(let (

	  ;; Specifications

	  (class '((class color) (min-colors 89)))

	  ;; Background
	  
	  (gray-1-1 "#303030")
      (gray-1+0 "#333333")
	  (gray-1+1 "#3a3a3a")
      (gray-1+2 "#3f3f3f")
	  (gray-2+0 "#444444")
	  (gray-3+0 "#555555")

	  ;; Foreground - Low Contrast

	  (gray-4+0 "#444444")
	  (gray-5+0 "#555555")
	  (gray-6+0 "#666666")
	  (gray-7+0 "#777777")
	  (gray-8+0 "#888888")
	  (gray-9+0 "#999999")

	  ;; Foreground - High Contrast
	  
      (gray-4 "#aaaaaa")
      (gray-5 "#bbbbbb")
      (gray-6 "#cccccc")
      (gray-7 "#dddddd")
      (gray-8 "#eeeeee")

	  ;; Semantic

      (error-1   "#ed381c")
	  (info-1    "#1dd13b")
	  (success-1 "#1dd13b")
	  (warning-1 "#ed6f1c"))

  (custom-theme-set-faces 'kolektiv-dark

   ;; Base

   `(bold                                    ((,class (:weight normal))))
   `(cursor                                  ((,class (:background "#dddddd"))))
   `(default                                 ((,class (:family "Fira Code" :weight light :height 130 :foreground ,gray-9+0 :background ,gray-1+0))))
   `(error                                   ((,class (:foreground ,error-1 :weight normal))))
   `(fringe                                  ((,class (:background ,gray-1-1 :foreground ,gray-6+0))))
   `(match                                   ((,class (:background "#dddddd"))))
   `(region                                  ((,class (:background "#222222"))))
   `(secondary-selection                     ((,class (:background "#111111"))))
   `(show-paren-match                        ((,class (:background "#888888"))))
   `(success                                 ((,class (:foreground ,success-1 :weight normal))))
   `(warning                                 ((,class (:foreground ,warning-1 :weight normal))))

   ;; Dired

   `(dired-flagged                           ((,class (:strike-through t))))
   `(dired-marked                            ((,class (:underline t))))
   `(dired-symlink                           ((,class (:slant italic))))

   ;; Font Lock

   `(font-lock-builtin-face                  ((,class (:foreground "#bbbbbb"))))
   `(font-lock-comment-face                  ((,class (:foreground ,gray-6+0))))
   `(font-lock-comment-delimiter-face        ((,class (:foreground ,gray-4+0))))
   `(font-lock-constant-face                 ((,class (:foreground "#dddddd"))))
   `(font-lock-function-name-face            ((,class (:foreground "#eeeeee" :weight normal))))
   `(font-lock-keyword-face                  ((,class (:foreground "#cccccc"))))
   `(font-lock-string-face                   ((,class (:foreground "#aaaaaa" :background ,gray-1+1))))
   `(font-lock-type-face                     ((,class (:foreground "#eeeeee" :weight normal))))
   `(font-lock-variable-name-face            ((,class (:foreground "#dddddd"))))

   ;; Flycheck

   `(flycheck-fringe-error                   ((,class (:foreground ,error-1 :background ,error-1))))
   `(flycheck-fringe-info                    ((,class (:foreground ,info-1 :background ,info-1))))
   `(flycheck-fringe-warning                 ((,class (:foreground ,warning-1 :background ,warning-1))))
   `(flycheck-error                          ((,class (:underline ,error-1))))
   `(flycheck-info                           ((,class (:underline ,info-1))))
   `(flycheck-warning                        ((,class (:underline ,warning-1))))

   ;; Fringe

   `(fringe                                  ((,class (:background ,gray-1-1 :foreground ,gray-5+0))))

   ;; Minibuffer

   `(minibuffer-prompt                       ((,class (:foreground "#aaaaaa" :box (:line-width 5 :color ,gray-1+0)))))
   `(minibuffer-noticeable-prompt            ((,class (:inherit minibuffer-prompt :foreground "#eeeeee"))))
   
   ;; Mode Line
   
   `(mode-line                               ((,class (:foreground ,gray-7+0 :background ,gray-2+0 :box (:line-width 5 :color ,gray-2+0)))))
   `(mode-line-buffer-id                     ((,class (:foreground "#dddddd" :weight light))))
   `(mode-line-emphasis                      ((,class (:foreground "#ffffff" :weight normal))))
   `(mode-line-highlight                     ((,class (:background ,gray-3+0 :box (:line-width 5 :color ,gray-3+0)))))
   `(mode-line-inactive                      ((,class (:inherit mode-line :background ,gray-1+2 :box (:line-width 5 :color ,gray-1+2)))))

   ;; Projectile

   `(projectile-mode-line                    ((,class (:foreground "#999999"))))
   
   ;; Rainbow Delimiters

   `(rainbow-delimiters-depth-1-face         ((,class (:foreground ,gray-5+0))))
   `(rainbow-delimiters-depth-2-face         ((,class (:foreground ,gray-6+0))))
   `(rainbow-delimiters-depth-3-face         ((,class (:foreground ,gray-7+0))))
   `(rainbow-delimiters-depth-4-face         ((,class (:foreground ,gray-8+0))))
   `(rainbow-delimiters-depth-5-face         ((,class (:foreground ,gray-9+0))))
   `(rainbow-delimiters-depth-6-face         ((,class (:foreground ,gray-9+0))))
   `(rainbow-delimiters-depth-7-face         ((,class (:foreground ,gray-9+0))))
   `(rainbow-delimiters-depth-8-face         ((,class (:foreground ,gray-9+0))))
   `(rainbow-delimiters-mismatched-face      ((,class (:foreground ,warning-1))))
   `(rainbow-delimiters-unmatched-face       ((,class (:foreground ,error-1))))

   ;; Version Control

   `(vc-state-base                           ((,class (:foreground ,gray-6+0))))
   `(vc-conflict-state                       ((,class (:inherit vc-state-base :foreground ,error-1))))
   `(vc-edited-state                         ((,class (:inherit vc-state-base :foreground ,gray-9+0))))
   `(vc-locally-added-state                  ((,class (:inherit vc-state-base :foreground ,gray-9+0 :slant italic))))
   `(vc-removed-state                        ((,class (:inherit vc-state-base :foreground ,gray-9+0 :strike-through t))))))

;; =============================================================================

;; Provide

(provide-theme 'kolektiv-dark)

;;; kolektiv-dark-theme.el ends here
