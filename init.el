;;; init.el --- Personal Emacs Configuration

;; Author: Andrew Cherry <andrew@xyncro.com>
;; Keywords: emacs, init
;; URL: https://github.com/kolektiv/.emacs.d

;;; Commentary:

;; Personal Emacs configuration, with the various customisations and
;; tweaks I like - this configuration should be relatively portable amongst
;; posix type systems, but makes no effort whatsoever to support Windows.

;;; History:

;; 2016-11-30: Changing to use use-package, and simplifying to single file.
;; 2017-02-10: Noting that this section will not be maintained - see Git.

;;; Code:

;; =============================================================================

;; Core

;; Configuration of modes/modules/functions built in to Emacs, and which are
;; thus configured "raw" rather than configured lazily using use-package as
;; configured in the latter part of the init file.

;; -----------------------------------------------------------------------------

;; Core/Backups

;; Override the default Emacs automatic backup behaviour to avoid polluting
;; working directories with backup and temporary copies of files. Ensure a
;; sensible level of backup history.

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 10
      version-control t)

;; -----------------------------------------------------------------------------

;; Core/Common Lisp

;; Allow the use of Common Lisp style programming features within Emacs Lisp
;; where useful.

(require 'cl-lib)

;; -----------------------------------------------------------------------------

;; Core/Customisation

;; Settings for custom mode, along with configuration to use a discrete
;; custom.el file instead of storing customized values within init.el.

(setq-default custom-buffer-done-kill t
              custom-buffer-indent 4)

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

;; -----------------------------------------------------------------------------

;; Core/Editing

;; Set the preferences for global editing to sensible defaults, including line
;; and column numbers, newline handling, tabs, and any other general text
;; editing default that arises.

(setq-default indent-tabs-mode nil
              line-spacing 0.2
              mode-require-final-newline nil
              tab-width 4
              truncate-lines t
              show-paren-delay 0)

(setq column-number-mode t
      line-number-mode t
      next-line-add-newlines nil
      indent-tabs-mode nil)

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(delete-selection-mode 1)
(global-font-lock-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)

;; -----------------------------------------------------------------------------

;; Core/ERC

;; Set the preferences for ERC to suitable defaults.

(setq-default erc-autoaway-mode t
              erc-away-nickname "kolektiv.afk"
              erc-nick "kolektiv")

;; -----------------------------------------------------------------------------

;; Core/Interaction

;; Override some of the default Emacs interactions for more concise alternatives
;; such as allowing Y or N instead of a full Yes or No response to buffer
;; interactions.

(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------

;; Core/Interface

;; Set preferences for UI options, including the removal of surplus chrome,
;; toolbars, and other graphical widgets which duplicate functionality better
;; used via keyboard interaction.

(setq inhibit-splash-screen t
      ring-bell-function 'ignore)

(add-to-list 'default-frame-alist (cons 'width 120))
(add-to-list 'default-frame-alist (cons 'height 60))

(blink-cursor-mode 0)
(fringe-mode 16)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; -----------------------------------------------------------------------------

;; Core/Package Management

;; Configure the Emacs built-in package management tooling to use the correct
;; set of repositories with appropriate priorities, and use the use-package
;; macro for further package based configuration (including the dependencies
;; needed for binding and mode diminishment).

(require 'package)

(eval-when-compile
  (setq package-archives
        '(("gnu"          . "http://elpa.gnu.org/packages/")
          ("melpa"        . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/")
          ("org"          . "http://orgmode.org/elpa/"))
        package-archive-priorities
        '(("melpa"        . 1))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

;; -----------------------------------------------------------------------------

;; Core/Theme

;; Add local themes to the custom theme load path, then load the kolektiv custom
;; theme (dark variant) without prompting (as it's theoretically trusted local
;; code).

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'kolektiv-dark t)

;; =============================================================================

;; Packages

;; Specific packages, required and managed via use-package, adding features and
;; modes as required. Extensions to specific packages (for example, counsel and
;; counsel-projectile) are included as part of the main "parent" package where
;; appropriate.

;; Configuration of packages *used by* other packages is located in the using
;; package (for example, ivy completion for projectile is configured in
;; projectile).

;; -----------------------------------------------------------------------------

;; Packages/Counsel

;; Better M-x and related functions, using the underlying Ivy support for better
;; completion modes. Light and effective. Projectile integration is also added
;; to support Ivy/Counsel based find, etc. in projectile-mode.

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c g"   . counsel-git)
   ("C-c j"   . counsel-git-grep)
   ("C-c k"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  :config (counsel-mode)
  :diminish counsel-mode
  :ensure t)

(use-package counsel-projectile
  :config (counsel-projectile-on)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Dired

;; Dired and Dired+ configuration, including setting Dired to use the GNU ls
;; implementation if running on OS X and the implementation has been installed
;; as part of coreutils. Listing switches are also set to sort directories
;; first.

(use-package dired
  :config
  (progn
    (if (eq system-type 'darwin)
        (let ((gls "/usr/local/bin/gls"))
          (if (file-exists-p gls)
              (setq insert-directory-program gls))))
    (setq dired-listing-switches "-lXGh --group-directories-first")))

;; For Dired+, the detailed listing is set as the default, and reuse of a single
;; Dired buffer is enabled to avoid cluttering Emacs with hundreds of Dired
;; buffers.

(use-package dired+
  :ensure t
  :init
  (progn
    (setq-default diredp-hide-details-initially-flag nil)
    (diredp-toggle-find-file-reuse-dir 1)))

;; -----------------------------------------------------------------------------

;; Packages/Ensime

;; High level Scala mode, integrating with the external Ensime engine providing
;; more advanced code analysis, completion, checking, etc. than the purely
;; syntactical analysis availble from the more basic scala-mode (on top of which
;; Ensime is built).

(use-package ensime
  :disabled t
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Ethan-WSpace

(use-package ethan-wspace
  :config (global-ethan-wspace-mode 1)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Exec-Path-From-Shell

;; Ensure that the path is initialized from environment settings defined and
;; configured by the appropriate shell.

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Flycheck

;; Integrates external checking processes with buffer editing, providing
;; in-place warning, error, etc. notification via highlighting and tool tips.

(use-package flycheck
  :config (global-flycheck-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Flyspell

;; Use Flyspell automatically for appropriate modes, and use likeness-based
;; correction matching in the case of multiple potential corrections.

(use-package flyspell
  :config
  (progn
    (setq flyspell-sort-corrections nil
          ispell-dictionary "british-ise-w_accents"
          ispell-program-name "aspell")
    ;; Uncomment following for flyspell of prog-mode comments and strings.
    ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (add-hook 'text-mode-hook 'flyspell-mode))
  :diminish flyspell-mode
  :ensure t)

;; Use Ivy integration for correction.

(use-package flyspell-correct-ivy
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-word-generic))
  :demand t
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Fill-Column-Indicator

(use-package fill-column-indicator
  :config
  (progn
    (setq fci-rule-color "#444444"
          fci-rule-column 80
          fci-rule-use-dashes nil)
    (add-hook 'prog-mode-hook 'turn-on-fci-mode))
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Flx

;; Fuzzy matching, used by Ivy and Counsel for the fuzzy search preferred for
;; general usage.

(use-package flx
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/HCL

;; Hashicorp Configuration Language mode, highlighting basic HCL.

(use-package hcl-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Indent-Guide

(use-package indent-guide
  :config (indent-guide-global-mode)
  :diminish (indent-guide-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Ivy

;; Ivy completion matching framework, underlying the Counsel and Swiper
;; implementations. Configured to taste, particularly around the fuzzy matching
;; approach, using flx as the underlying provider.

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :config
  (progn
    (setq ivy-count-format "(%d/%d) "
          ivy-format-function 'ivy-format-function-line
          ivy-initial-inputs-alist nil
          ivy-re-builders-alist '((t . ivy--regex-fuzzy))
          ivy-use-virtual-buffers t
          ivy-wrap t)
    (add-to-list 'ivy-ignore-buffers "\\*magit")
    (add-to-list 'ivy-ignore-buffers "\\*Flycheck")
    (ivy-mode 1))
  :diminish (ivy-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Magit

;; Powerful and comprehensive Git porcelain avoiding the need to move outside of
;; Emacs for version control management. Integrated with Ivy and enabled
;; globally.

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :config
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (global-magit-file-mode))
  :demand t
  :diminish (auto-revert-mode)
  :ensure t)

;; Use magithub to provide additional integration with Github through the Magit
;; porcelain.

(use-package magithub
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Markdown

(use-package markdown-mode
  :commands (gfm-mode markdown-mode)
  :config (setq markdown-command "multimarkdown")
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

;; -----------------------------------------------------------------------------

;; Packages/Org

(use-package org
  :ensure org-plus-contrib
  :pin org)

;; -----------------------------------------------------------------------------

;; Packages/Projectile

(use-package projectile
  :config
  (progn
    (setq projectile-completion-system 'ivy
          projectile-mode-line
          '(:eval
            (when (ignore-errors (projectile-project-root))
              (propertize
               (format " Project[%s]" (projectile-project-name))
               'face 'projectile-mode-line))))
    (projectile-mode))
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Rainbow Delimiters

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/SBT

(use-package sbt-mode
  :commands (sbt-command sbt-start)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Scala

(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode))

;; -----------------------------------------------------------------------------

;; Packages/Swiper

(use-package swiper
  :bind ("C-s" . swiper)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Terraform

(use-package terraform-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Text

;; Configuration of the base text-mode. Usage of other specific packages such
;; as Flyspell are configured within the configuration of those packages.

;; -----------------------------------------------------------------------------

;; Packages/YAML

(use-package yaml-mode
  :ensure t)

;; =============================================================================

;; Module

(provide 'init)

;;; init.el ends here
