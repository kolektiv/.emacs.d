;; Packages

(ac/packages/require 'markdown-mode)

;; Load

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; Provide

(provide 'markdown-setup)
