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

; Sensible tab widths by default
(setq-default tab-width 4)

; Use Dired+
; (require 'dired+)
; (toggle-diredp-find-file-reuse-dir t)

; Use Backup-Dir (don't spread *~ files everywhere!)
(setq 
 backup-by-copying t
 backup-directory-alist
 `(("." . "~/.saves"))
 delete-old-versions t
 kept-old-versions 2
 kept-new-versions 10
 version-control t)

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

; Use Puppet mode
(require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

; Tell the internal shell to use ansi mode so it can deal with colours.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'common)
