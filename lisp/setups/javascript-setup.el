;; Packages

(ac/packages/require 'js2-mode)

;; Load

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq-default js2-include-node-externs t)
(setq-default js2-basic-offset 2)

;; Provide

(provide 'javascript-setup)
