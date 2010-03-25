; Always use fontlock when possible
(global-font-lock-mode t)

; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

; Shorten questions
(defalias 'yes-or-no-p 'y-or-n-p)

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