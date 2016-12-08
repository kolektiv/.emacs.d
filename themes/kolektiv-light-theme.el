;;; kolektiv-light-theme.el --- Personal Theme

;; Author: Andrew Cherry <andrew@xyncro.com>
;; URL: https://github.com/kolektiv/.emacs.d

;;; Commentary:

;; A minimal, largely monochrome in normal operation, light theme, designed
;; to let the code take front of stage with minimal distraction.

;;; Code:

;; =============================================================================

;; Groups and Faces

;; Custom faces and groups are defined where applicable, due to the need for
;; more granularity of control over some package faces.

;; -----------------------------------------------------------------------------

;; Projectile

(defgroup projectile-faces nil
  "Faces for projectile."
  :group 'projectile
  :group 'faces)

(defface projectile-mode-line
  '((t :inherit mode-line))
  "Face used for projectile-mode-line information."
  :group 'projectile-faces)

;; =============================================================================

;; Theme

(deftheme kolektiv-light
  "Personal Theme (Light)")

(let (

      ;; Specification

      (class '((class color) (min-colors 89)))

      ;; Background/Base

      (bg-a-3 "#fafafa")
      (bg-a-2 "#fafafa")
      (bg-a-1 "#fafafa")
      (bg-a+0 "#ffffff") ;; Baseline
      (bg-a+1 "#f1f1f1")
      (bg-a+2 "#e0e0e0")
      (bg-a+3 "#dddddd")
      (bg-a+4 "#cccccc")

      ;; Background/Contrast

      (bg-b-4 "#888888")
      (bg-b-3 "#777777")
      (bg-b-2 "#666666")
      (bg-b-1 "#555555")
      (bg-b+0 "#444444") ;; Baseline
      (bg-b+1 "#333333")
      (bg-b+2 "#222222")
      (bg-b+3 "#111111")
      (bg-b+4 "#000000")

      ;; Foreground/Base

      (fg-a-5 "#bbbbbb")
      (fg-a-4 "#aaaaaa")
      (fg-a-3 "#999999")
      (fg-a-2 "#888888")
      (fg-a-1 "#777777")
      (fg-a+0 "#666666") ;; Baseline
      (fg-a+1 "#555555")
      (fg-a+2 "#444444")
      (fg-a+3 "#333333")
      (fg-a+4 "#222222")
      (fg-a+5 "#111111")

      ;; Foreground/Contrast

      (fg-b+0 "#cccccc") ;; Baseline

      ;; Semantic

      (error-1   "#ed381c")
      (info-1    "#1dd13b")
      (success-1 "#1dd13b")
      (warning-1 "#ed6f1c"))

  (custom-theme-set-faces 'kolektiv-light

   ;; Basic

   `(bold                                    ((,class (:weight normal))))
   `(cursor                                  ((,class (:background ,bg-b+2))))
   `(default                                 ((,class (:background ,bg-a+0 :foreground ,fg-a+0 :family "Fira Code" :weight light :height 130))))
   `(error                                   ((,class (:background ,bg-a-1 :foreground ,error-1 :weight normal))))
   `(fixed-pitch                             ((,class (:inherit default))))
   `(fringe                                  ((,class (:background ,bg-a-1 :foreground ,fg-a-4))))
   `(highlight                               ((,class (:background ,bg-b-4))))
   `(isearch                                 ((,class (:background ,bg-b-2))))
   `(lazy-highlight                          ((,class (:background ,bg-b-3))))
   `(match                                   ((,class (:background ,bg-b-1))))
   `(minibuffer-prompt                       ((,class (:inherit default :foreground ,fg-a+1 :box (:line-width 5 :color ,bg-a+0)))))
   `(minibuffer-noticeable-prompt            ((,class (:inherit minibuffer-prompt :foreground ,fg-a+5))))
   `(mode-line                               ((,class (:inherit default :foreground ,fg-a-2 :background ,bg-a+3 :box (:line-width 5 :color ,bg-a+3)))))
   `(mode-line-buffer-id                     ((,class (:foreground ,fg-a+4))))
   `(mode-line-emphasis                      ((,class (:foreground ,fg-a+5 :weight normal))))
   `(mode-line-highlight                     ((,class (:background ,bg-a+4 :box (:line-width 5 :color ,bg-a+4)))))
   `(mode-line-inactive                      ((,class (:inherir mode-line :background ,bg-a+2 :box (:line-width 5 :color ,bg-a+2)))))
   `(region                                  ((,class (:background ,bg-a-2))))
   `(secondary-selection                     ((,class (:background ,bg-a-3))))
   `(success                                 ((,class (:foreground ,success-1))))
   `(warning                                 ((,class (:foreground ,warning-1))))

   ;; Dired

   `(dired-flagged                           ((,class (:inherit default :strike-through t))))
   `(dired-marked                            ((,class (:inherit default :underline t))))
   `(dired-symlink                           ((,class (:inherit default :slant italic))))

   ;; Font Lock

   `(font-lock-builtin-face                  ((,class (:foreground ,fg-a+2))))
   `(font-lock-comment-face                  ((,class (:foreground ,fg-a-3))))
   `(font-lock-comment-delimiter-face        ((,class (:foreground ,fg-a-5))))
   `(font-lock-constant-face                 ((,class (:foreground ,fg-a+4))))
   `(font-lock-function-name-face            ((,class (:foreground ,fg-a+5 :weight normal))))
   `(font-lock-keyword-face                  ((,class (:foreground ,fg-a+3))))
   `(font-lock-string-face                   ((,class (:foreground ,fg-a+1 :background ,bg-a+1))))
   `(font-lock-type-face                     ((,class (:foreground ,fg-a+5 :weight normal))))
   `(font-lock-variable-name-face            ((,class (:foreground ,fg-a+4))))

   ;; Flycheck

   `(flycheck-fringe-error                   ((,class (:inherit fringe :foreground ,error-1 :background ,error-1))))
   `(flycheck-fringe-info                    ((,class (:inherit fringe :foreground ,info-1 :background ,info-1))))
   `(flycheck-fringe-warning                 ((,class (:inherit fringe :foreground ,warning-1 :background ,warning-1))))
   `(flycheck-error                          ((,class (:underline ,error-1))))
   `(flycheck-info                           ((,class (:underline ,info-1))))
   `(flycheck-warning                        ((,class (:underline ,warning-1))))

   ;; Ivy

   `(ivy-action                              ((,class (:inherit default :foreground "#ffff00"))))
   `(ivy-confirm-face                        ((,class (:inherit default :foreground "#ff00ff"))))
   `(ivy-current-match                       ((,class (:inherit default :foreground ,fg-b+0 :background ,bg-b+3 :weight normal))))
   `(ivy-match-required-face                 ((,class (:inherit default :foreground ,warning-1))))
   `(ivy-minibuffer-match-face-1             ((,class (:inherit default :foreground ,fg-b+0 :background ,bg-b-1))))
   `(ivy-minibuffer-match-face-2             ((,class (:inherit default :foreground ,fg-b+0 :background ,bg-b-2))))
   `(ivy-minibuffer-match-face-3             ((,class (:inherit default :foreground ,fg-b+0 :background ,bg-b-3))))
   `(ivy-minibuffer-match-face-4             ((,class (:inherit default :foreground ,fg-b+0 :background ,bg-b-4))))
   `(ivy-modified-buffer                     ((,class (:inherit default :foreground ,fg-a+4 :weight normal))))
   `(ivy-remote                              ((,class (:inherit default :foreground ,fg-a+1 :underline ,fg-a-3))))
   `(ivy-subdir                              ((,class (:inherit default :foreground ,fg-a-3 :underline t))))
   `(ivy-virtual                             ((,class (:inherit default :foreground ,fg-a-3))))

   ;; Paren Showing

   `(show-paren-match                        ((,class (:background ,bg-b-2))))
   `(show-paren-mismatch                     ((,class (:background ,warning-1))))

   ;; Projectile

   `(projectile-mode-line                    ((,class (:foreground ,fg-a+0))))

   ;; Rainbow Delimiters

   `(rainbow-delimiters-depth-1-face         ((,class (:foreground ,fg-a-4))))
   `(rainbow-delimiters-depth-2-face         ((,class (:foreground ,fg-a-3))))
   `(rainbow-delimiters-depth-3-face         ((,class (:foreground ,fg-a-2))))
   `(rainbow-delimiters-depth-4-face         ((,class (:foreground ,fg-a-1))))
   `(rainbow-delimiters-depth-5-face         ((,class (:foreground ,fg-a+0))))
   `(rainbow-delimiters-depth-6-face         ((,class (:foreground ,fg-a+0))))
   `(rainbow-delimiters-depth-7-face         ((,class (:foreground ,fg-a+0))))
   `(rainbow-delimiters-depth-8-face         ((,class (:foreground ,fg-a+0))))
   `(rainbow-delimiters-mismatched-face      ((,class (:foreground ,warning-1))))
   `(rainbow-delimiters-unmatched-face       ((,class (:foreground ,error-1))))

   ;; Version Control

   `(vc-state-base                           ((,class (:foreground ,fg-a-3))))
   `(vc-conflict-state                       ((,class (:foreground ,error-1))))
   `(vc-edited-state                         ((,class (:foreground ,fg-a+0))))
   `(vc-locally-added-state                  ((,class (:foreground ,fg-a+0 :underline t))))
   `(vc-removed-state                        ((,class (:foreground ,fg-a+0 :strike-through t))))))

;; =============================================================================

;; Provide

(provide-theme 'kolektiv-light)

;;; kolektiv-light-theme.el ends here
