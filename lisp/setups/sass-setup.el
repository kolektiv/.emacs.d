;; Packages

(ac/packages/require 'scss-mode)

;; Load

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Provide

(provide 'sass-setup)
