; Always use fontlock when possible
(global-font-lock-mode t)

; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

; Shorten questions
(defalias 'yes-or-no-p 'y-or-n-p)

; Sensible behaviour about newlines at file end
(setq require-final-newline t)
(setq next-line-add-newlines nil)

; Use Dired+
(require 'dired+)
(toggle-dired-find-file-reuse-dir 1)

; Use Backup-Dir (don't spread *~ files everywhere!)
(require 'backup-dir)
(make-variable-buffer-local 'backup-inhibited)
(setq bkup-backup-directory-info
      '((t "~/.backup" ok-create full-path prepend-name)))
(setq delete-old-versions t
      kept-old-versions 1
      kept-new-versions 3
      version-control t)

; Tell the internal shell to use ansi mode so it can deal with colours.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Set up yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")

; Set up scala-mode
(require 'scala-mode-auto)
(yas/load-directory "~/.emacs.d/packages/scala-mode/contrib/yasnippet/snippets")
(add-hook 'scala-mode-hook
	  '(lambda ()
	     (yas/minor-mode-on)))

; Set up io-mode
(require 'io-mode)
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))

; Set up ioke-mode
(require 'ioke-mode)
(require 'inf-ioke)
(add-to-list 'auto-mode-alist '("\\.ik$" . ioke-mode))

; Set up lua-mode
(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-hook 'lua-mode-hook 'turn-on-font-lock)

; Set up csharp-mode
(require 'csharp-mode)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(defun my-csharp-mode-fn ()
  ; Personal mode hooks here...
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-fn t)

; Set up haskell-mode
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

; Set up org-mode from packages
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
