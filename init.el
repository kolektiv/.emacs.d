;;; init.el --- Personal Emacs Configuration

;; Author: Andrew Cherry <andrew@xyncro.com>
;; Keywords: emacs, init
;; URL: https://github.com/kolektiv/.emacs.d

;;; Commentary:

;; Personal Emacs configuration, with the various customisations and
;; tweaks I like.

;;; History:

;; 2016-11-30: Changing to use use-package, and simplifying to single file basis.

;;; Code:

;; Customisation

;; Set the customisation files used by the automatic customisation tooling to
;; use a separate file to the core init.el to avoid cluttering this file with
;; auto-generated cruft.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Theme

;; Load my custom theme (dark variant), without prompting as it's theoretically
;; trusted local code.

(load-theme 'kolektiv-dark t)

;; Interface

;; Set preferences for UI options, including the removal of surplus chrome,
;; toolbars, and other graphical widgets which duplicate functionality better
;; used via keyboard interaction.

(setq inhibit-splash-screen t)

(blink-cursor-mode 0)
(fringe-mode 16)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 60))

;; Editing

;; Set the preferences for global editing to sensible defaults, including line
;; and column numbers, newline handling, tabs, and any other general text
;; editing default that arises.

(global-font-lock-mode t)

(setq
 column-number-mode t
 line-number-mode t
 require-final-newline t
 next-line-add-newlines nil
 indent-tabs-mode nil)

(setq-default
 line-spacing 0.2
 tab-width 4
 truncate-lines t)

;; Interaction

;; Override some of the default Emacs interactions for more concise alternatives
;; such as allowing Y or N instead of a full Yes or No response to buffer
;; interactions.

(defalias 'yes-or-no-p 'y-or-n-p)

;; Backups

;; Override the default Emacs automatic backup behaviour to avoid polluting
;; working directories with backup and temporary copies of files. Ensure a
;; sensible level of backup history.

(setq
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 backup-by-copying t
 backup-directory-alist `((".*" . ,temporary-file-directory))
 delete-old-versions t
 kept-old-versions 2
 kept-new-versions 10
 version-control t)

;; Common Lisp

;; Allow the use of Common Lisp style programming features within Emacs Lisp
;; where useful.

(require 'cl-lib)

;; Package Management Infrastructure

(require 'package)

(eval-when-compile
  (setq
   package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
					  ("org"          . "http://orgmode.org/elpa/")
					  ("melpa"        . "http://melpa.org/packages/")
					  ("melpa-stable" . "http://stable.melpa.org/packages/"))
   package-archive-priorities '(("melpa-stable" . 1))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

;; Packages

;; Specific packages, required and managed via use-package, adding features and
;; modes as required.

;; Flycheck

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :init
  (use-package exec-path-from-shell
	:ensure t
	:config
	(exec-path-from-shell-initialize))
  :config
  (global-flycheck-mode))

;; Magit

(use-package magit
  :ensure t
  :pin melpa-stable
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :config
  (global-magit-file-mode))

;; Rainbow Delimiters

(use-package rainbow-delimiters
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Module

(provide 'init)

;;; init.el ends here
