;; Editing

(global-font-lock-mode t)

(setq line-number-mode t)
(setq column-number-mode t)

(setq require-final-newline t)
(setq next-line-add-newlines nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Interaction

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key
 (kbd "M-3")
 '(lambda () (interactive) (insert "#")))

;; Backups

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

;; Provide

(provide 'core)
