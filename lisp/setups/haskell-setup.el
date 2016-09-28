;; Packages

(ac/packages/require 'haskell-mode)

;; Load

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; Provide

(provide 'haskell-setup)
