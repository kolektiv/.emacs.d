; Always use fontlock when possible
(global-font-lock-mode t)

; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

; Turn the damn bell off
(setq visible-bell 1)

; Shorten questions
(defalias 'yes-or-no-p 'y-or-n-p)

; Sensible behaviour about newlines at file end
(setq require-final-newline t)
(setq next-line-add-newlines nil)

; Sensible tab widths by default
(setq-default tab-width 4)

; Spaces, not tabs!
(setq-default indent-tabs-mode nil)

; Use Dired+
(require 'dired+)
(toggle-diredp-find-file-reuse-dir t)

; Use Backup-Dir (don't spread *~ files everywhere!)
(setq 
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-old-versions 2
 kept-new-versions 10
 version-control t)

(setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))

(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

; Let me actually type # on a mac!
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

; Use Uniquify
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'forward
 uniquify-separator ":")

; Use IDO mode
(require 'ido)
(ido-mode)

; Use Markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

; Use Sass mode
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

; Tell the internal shell to use ansi mode so it can deal with colours.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Use js2 mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(setq-default js2-include-node-externs t)
(setq-default js2-basic-offset 2)

; Haskell mode
(add-to-list 'exec-path "~/.cabal/bin")
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

; Idris mode
(require 'idris-mode)

(provide 'common)
