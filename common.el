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
(setq-default js2-include-node-externs t)
(setq-default js2-basic-offset 2)

; User web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(defun my-web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

(provide 'common)
