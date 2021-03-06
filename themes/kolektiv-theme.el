;;; kolektiv-theme.el --- Personal Theme

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
  "Faces for projectile."
  :group 'projectile
  :group 'faces)

(defface projectile-mode-line
  '((t :inherit mode-line))
  "Face used for projectile-mode-line information."
  :group 'projectile-faces)

;; ==============================================================================

;; Theme

(deftheme kolektiv
  "Personal Theme")

(let (

      ;; Specification

      (class '((class color) (min-colors 89)))

      ;; Background/Base

      (bg-a-3 "#121212") ;; 233
      (bg-a-2 "#1c1c1c") ;; 234
      (bg-a-1 "#262626") ;; 235
      (bg-a+0 "#303030") ;; Baseline
      (bg-a+1 "#3a3a3a") ;; 237
      (bg-a+2 "#444444") ;; 238
      (bg-a+3 "#4e4e4e") ;; 239
      (bg-a+4 "#585858") ;; 240

      ;; Background/Contrast

      (bg-b-4 "#6c6c6c") ;; 242
      (bg-b-3 "#767676") ;; 243
      (bg-b-2 "#8a8a8a") ;; 245
      (bg-b-1 "#9e9e9e") ;; 247
      (bg-b+0 "#a8a8a8") ;; Baseline
      (bg-b+1 "#c6c6c6") ;; 251
      (bg-b+2 "#dadada") ;; 253
      (bg-b+3 "#eeeeee") ;; 255
      (bg-b+4 "#ffffff") ;; 231 *

      ;; Foreground/Base

      (fg-a-7 "#222222") ;; 235
      (fg-a-6 "#303030") ;; 236
      (fg-a-5 "#444444") ;; 238
      (fg-a-4 "#585858") ;; 240
      (fg-a-3 "#626262") ;; 242
      (fg-a-2 "#767676") ;; 243
      (fg-a-1 "#8a8a8a") ;; 245
      (fg-a+0 "#949494") ;; Baseline
      (fg-a+1 "#a8a8a8") ;; 248
      (fg-a+2 "#bcbcbc") ;; 250
      (fg-a+3 "#c6c6c6") ;; 251
      (fg-a+4 "#dadada") ;; 253
      (fg-a+5 "#eeeeee") ;; 255

      ;; Foreground/Contrast

      (fg-b+0 "#3a3a3a") ;; Baseline

      ;; Semantic

      (error-1   "#d70000")  ;; 160
      (info-1    "#ffdd00")  ;; 154
      (success-1 "#afff00")  ;; 154
      (warning-1 "#ff5f00")) ;; 202

  (custom-theme-set-faces

   'kolektiv

   ;; Basic

   `(bold                                    ((,class (:weight normal))))
   `(cursor                                  ((,class (:background ,bg-b+2))))
   `(default                                 ((,class (:background ,bg-a+0 :foreground ,fg-a+0 :family "Fira Code" :weight light :height 130))))
   `(error                                   ((,class (:background ,bg-a-1 :foreground ,error-1 :weight normal))))
   `(fixed-pitch                             ((,class (:inherit default))))
   `(fringe                                  ((,class (:background ,bg-a-1 :foreground ,fg-a-4))))
   `(header-line                             ((,class (:inherit mode-line))))
   `(highlight                               ((,class (:background ,bg-b-4))))
   `(isearch                                 ((,class (:background ,bg-b-2))))
   `(lazy-highlight                          ((,class (:background ,bg-b-3))))
   `(match                                   ((,class (:background ,bg-b-1))))
   `(minibuffer-prompt                       ((,class (:inherit default :foreground ,fg-a+1 :box (:line-width 5 :color ,bg-a+0)))))
   `(minibuffer-noticeable-prompt            ((,class (:inherit minibuffer-prompt :foreground ,fg-a+5))))
   `(mode-line                               ((,class (:inherit default :foreground ,fg-a-1 :background ,bg-a+3 :box (:line-width 5 :color ,bg-a+3)))))
   `(mode-line-buffer-id                     ((,class (:foreground ,fg-a+4))))
   `(mode-line-emphasis                      ((,class (:foreground ,fg-a+5 :weight normal))))
   `(mode-line-highlight                     ((,class (:background ,bg-a+4 :box (:line-width 5 :color ,bg-a+4)))))
   `(mode-line-inactive                      ((,class (:inherit mode-line :background ,bg-a+2 :box (:line-width 5 :color ,bg-a+2)))))
   `(region                                  ((,class (:background ,bg-a-2))))
   `(secondary-selection                     ((,class (:background ,bg-a-3))))
   `(success                                 ((,class (:foreground ,success-1))))
   `(warning                                 ((,class (:foreground ,warning-1))))

   ;; Company

   `(company-scrollbar-bg                    ((,class (:background ,bg-b-1))))
   `(company-scrollbar-fg                    ((,class (:background ,bg-b-4))))
   `(company-tooltip                         ((,class (:background ,bg-b+0 :foreground ,fg-a-6))))
   `(company-tooltip-selection               ((,class (:background ,bg-b+3 :foreground ,fg-a-7))))

   ;; Dired

   `(dired-flagged                           ((,class (:inherit default :strike-through t))))
   `(dired-marked                            ((,class (:inherit default :underline t))))
   `(dired-symlink                           ((,class (:inherit default :slant italic))))

   ;; Dired+

   `(diredp-date-time                        ((,class (:inherit default :foreground ,fg-a-4))))
   `(diredp-dir-heading                      ((,class (:inherit default :foreground ,fg-a-2))))
   `(diredp-dir-name                         ((,class (:inherit default :foreground ,fg-a-3 :underline t))))
   `(diredp-file-name                        ((,class (:inherit default :foreground ,fg-a+3))))
   `(diredp-file-suffix                      ((,class (:inherit default :foreground ,fg-a-4))))
   `(diredp-number                           ((,class (:inherit default :foreground ,fg-a-1))))

   ;; ERC

   `(erc-action-face                         ((,class (:weight normal))))
   `(erc-bold-face                           ((,class (:weight normal))))
   `(erc-button                              ((,class (:weight normal))))
   `(erc-current-nick-face                   ((,class (:inherit font-lock-constant-face :underline t))))
   `(erc-dangerous-host-face                 ((,class (:inherit warning))))
   `(erc-direct-msg-face                     ((,class (:inherit erc-default-face))))
   `(erc-error-face                          ((,class (:inherit error))))
   `(erc-fool-face                           ((,class (:inherit font-lock-comment-delimiter-face))))
   `(erc-input-face                          ((,class (:inherit font-lock-type-face))))
   `(erc-keyword-face                        ((,class (:inherit success))))
   `(erc-my-nick-face                        ((,class (:inherit font-lock-type-face :underline t))))
   `(erc-my-nick-prefix-face                 ((,class (:inherit erc-nick-default-face))))
   `(erc-nick-default-face                   ((,class (:inherit font-lock-keyword-face))))
   `(erc-nick-msg-face                       ((,class (:inherit font-lock-type-face))))
   `(erc-nick-prefix-face                    ((,class (:inherit erc-nick-default-face))))
   `(erc-notice-face                         ((,class (:inherit font-lock-comment-face))))
   `(erc-prompt-face                         ((,class (:inherit font-lock-type-face :weight light))))
   `(erc-timestamp-face                      ((,class (:inherit font-lock-comment-delimiter-face))))

   ;; Ethan-WSpace

   `(ethan-wspace-face                       ((,class (:background ,warning-1))))

   ;; Font Lock

   `(font-lock-builtin-face                  ((,class (:foreground ,fg-a+2))))
   `(font-lock-comment-face                  ((,class (:foreground ,fg-a-3))))
   `(font-lock-comment-delimiter-face        ((,class (:foreground ,fg-a-5))))
   `(font-lock-constant-face                 ((,class (:foreground ,fg-a+4))))
   `(font-lock-function-name-face            ((,class (:foreground ,fg-a+5 :weight normal))))
   `(font-lock-keyword-face                  ((,class (:foreground ,fg-a+3))))
   `(font-lock-preprocessor-face             ((,class (:foreground ,fg-a-2))))
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

   ;; Flycheck Color Mode Line

   `(flycheck-color-mode-line-error-face     ((,class (:foreground ,error-1))))
   `(flycheck-color-mode-line-info-face      ((,class (:foreground ,info-1))))
   `(flycheck-color-mode-line-warning-face   ((,class (:foreground ,warning-1))))
   `(flycheck-color-mode-line-success-face   ((,class (:foreground ,success-1))))

   ;; Flyspell

   `(flyspell-duplicate                      ((,class (:underline ,error-1))))
   `(flyspell-incorrect                      ((,class (:underline ,warning-1))))

   ;; Git

   `(git-commit-summary                      ((,class (:foreground ,fg-a+3 :weight light))))

   ;; Git Gutter

   `(git-gutter:added                        ((,class (:inherit default :foreground ,fg-a-3))))
   `(git-gutter:deleted                      ((,class (:inherit default :foreground ,fg-a-3))))
   `(git-gutter:modified                     ((,class (:inherit default :foreground ,fg-a-3))))
   `(git-gutter:separator                    ((,class (:inherit default :foreground ,fg-a-4))))

   ;; Indent Guide

   `(indent-guide-face                       ((,class (:foreground ,fg-a-5))))

   ;; Ivy

   `(ivy-action                              ((,class (:inherit default :foreground ,fg-a+3 :weight normal))))
   `(ivy-confirm-face                        ((,class (:inherit default :foreground ,error-1))))
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

   ;; JS2

   `(js2-error                               ((,class (:foreground ,error-1 :underline t))))
   `(js2-external-variable                   ((,class (:foreground ,warning-1))))
   `(js2-function-param                      ((,class (:foreground ,fg-a-2 :underline ,fg-a-5))))

   ;; Magit

   `(magit-diff-file-heading                 ((,class (:foreground ,fg-a+0))))
   `(magit-diff-file-heading-highlight       ((,class (:foreground ,fg-b+0 :background ,bg-b+3))))
   `(magit-popup-disabled-argument           ((,class (:foreground ,fg-a-1 :strike-through t))))
   `(magit-popup-key                         ((,class (:foreground ,fg-a+2))))
   `(magit-section-heading                   ((,class (:foreground ,fg-a-2 :weight light :underline t))))
   `(magit-section-heading-selection         ((,class (:inherit magit-section-heading :foreground ,fg-a+1))))
   `(magit-section-highlight                 ((,class (:background ,bg-a+1))))
   `(magit-section-secondary-heading         ((,class (:inherit magit-section-header :underline nil))))

   ;; Markdown

   `(markdown-bold-face                      ((,class (:inherit font-lock-variable-name-face))))
   `(markdown-header-face                    ((,class (:inherit font-lock-function-name-face))))
   `(markdown-metadata-key-face              ((,class (:inherit font-lock-variable-name-face))))

   ;; Outline

   `(outline-1                               ((,class (:foreground ,fg-a+5 :weight light))))
   `(outline-2                               ((,class (:foreground ,fg-a+4 :weight light))))
   `(outline-3                               ((,class (:foreground ,fg-a+3 :weight light))))
   `(outline-4                               ((,class (:foreground ,fg-a+2 :weight light))))
   `(outline-5                               ((,class (:foreground ,fg-a+1 :weight light))))
   `(outline-6                               ((,class (:foreground ,fg-a+0 :weight light))))
   `(outline-7                               ((,class (:foreground ,fg-a-1 :weight light))))

   ;; Paren Showing

   `(show-paren-match                        ((,class (:background ,bg-b-2))))
   `(show-paren-mismatch                     ((,class (:background ,warning-1))))

   ;; Projectile

   `(projectile-mode-line                    ((,class (:foreground ,fg-a+4))))

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

   ;; Tide

   `(tide-hl-identifier-face                 ((,class (:background ,bg-a+2))))

   ;; Version Control

   `(vc-state-base                           ((,class (:foreground ,fg-a-3))))
   `(vc-conflict-state                       ((,class (:foreground ,error-1))))
   `(vc-edited-state                         ((,class (:foreground ,fg-a+0))))
   `(vc-locally-added-state                  ((,class (:foreground ,fg-a+0 :underline t))))
   `(vc-removed-state                        ((,class (:foreground ,fg-a+0 :strike-through t))))))

;; ==============================================================================

;; Provide

(provide-theme 'kolektiv)

;;; kolektiv-theme.el ends here
