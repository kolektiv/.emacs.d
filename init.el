;; init.el --- Personal Emacs Configuration

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
;; (global-prettify-symbols-mode 1)
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
        '(("melpa-stable" . 1))))

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

;; Packages/Add-Node-Modules-Path

;; Add the node_modules/.bin/ directory to the local exec-path when it is found
;; in a parent directory, allowing tools to use local node installs of tools
;; such as eslint, etc.

(use-package add-node-modules-path
  :config
  (progn
    (add-hook 'js-mode 'add-node-modules-path)
    (add-hook 'js2-mode-hook 'add-node-modules-path))
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Aggressive-Indent

;; Make re-indentation of code more aggressive, continually re-aligning things
;; as appropriate while editing.

(use-package aggressive-indent
  :config
  (progn
    (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'purescript-mode)
    (global-aggressive-indent-mode 1))
  :diminish (aggressive-indent-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Alchemist (Elixir)

;; Elixir/Mix integration/IDE.

(use-package alchemist
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Auto-Package-Update

;; Keep packages up to date, removing old packages. Override the periodicity to
;; check regularly, in this case once a day.

(use-package auto-package-update
  :config
  (progn
    (setq auto-package-update-delete-old-versions t
          auto-package-update-interval 1)
    (auto-package-update-maybe))
  :demand t
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Company

;; Company mode enabled globally, for use with modes which integrate in
;; particular. Also added is company-flx, which enables fuzzy matching for
;; company-mode completions (if using a CAPF backend). This seems to work quite
;; well, but may be re-evaluated if it becomes to slow/unstable.

(use-package company
  :bind (("C-SPC" . company-complete))
  :config
  (progn
    (setq company-auto-complete t
          company-idle-delay 1
          company-tooltip-minimum-width 30)
    (global-company-mode))
  :diminish (company-mode)
  :ensure t)

(use-package company-flx
  :config (company-flx-mode 1)
  :ensure t)

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
  :diminish (counsel-mode)
  :ensure t)

(use-package counsel-projectile
  :config (counsel-projectile-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Dired

;; Dired and Dired+ configuration, including setting Dired to use the GNU ls
;; implementation if running on OS X and the implementation has been installed
;; as part of coreutils. Listing switches are also set to sort directories
;; first.

;; For Dired+, the detailed listing is set as the default, and reuse of a single
;; Dired buffer is enabled to avoid cluttering Emacs with hundreds of Dired
;; buffers.

(use-package dired
  :config
  (progn
    (if (eq system-type 'darwin)
        (let ((gls "/usr/local/bin/gls"))
          (if (file-exists-p gls)
              (setq insert-directory-program gls))))
    (setq dired-listing-switches "-lXGh --group-directories-first")))

;; (use-package dired+
;;   :demand t
;;   :ensure t
;;   :init
;;   (progn
;;     (setq-default diredp-hide-details-initially-flag nil)
;;     (diredp-toggle-find-file-reuse-dir 1)))

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

;; Ivy integration is included for Flyspell correction functionality, bound to
;; C-; - note that save functionality is then available using Ivy actions.

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

(use-package flyspell-correct-ivy
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-previous-word-generic))
  :demand t
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Fill-Column-Indicator

;; Fill column indicator (configured for a classic 80 column width). A function
;; to temporarily suspend fci-mode is configured, and run when company-mode is
;; actively displaying a frontend. This prevents a positioning bug where the
;; frontend popup will be displayed after the fill column indicator character.

(use-package fill-column-indicator
  :config
  (progn
    (eval-when-compile
      (defun suspend-fci-mode (command)
        (when (string= "show" command)
          (turn-off-fci-mode))
        (when (string= "hide" command)
          (turn-on-fci-mode))))
    (advice-add 'company-call-frontends :before #'suspend-fci-mode)
    (setq fci-rule-color "#3a3a3a"
          fci-rule-column 80
          fci-rule-use-dashes nil)
    (add-hook 'prog-mode-hook 'turn-on-fci-mode))
  :disabled t
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Flx

;; Fuzzy matching, used by Ivy and Counsel for the fuzzy search preferred for
;; general usage.

(use-package flx
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Focus

;; Focus mode, diminishing unfocused text while reading/editing.

(use-package focus
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Git-Gutter

;; Show changes from the current Git version of a file in the left hand gutter,
;; indicating changes through slightly customised symbols. Automatically update
;; after 2 seconds, without waiting for save (the default).

(use-package git-gutter
  :config
  (progn
    (setq git-gutter:added-sign "++"
          git-gutter:always-show-separator t
          git-gutter:deleted-sign "--"
          git-gutter:modified-sign "=="
          git-gutter:update-interval 2)
    (global-git-gutter-mode 1))
  :diminish (git-gutter-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/GraphQL

;; A graphql major mode for editing graphql schema.

(use-package graphql-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Haskell

;; Haskell mode

(use-package haskell-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/HCL

;; Hashicorp Configuration Language mode, highlighting basic HCL.

(use-package hcl-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/HIndent

;; Haskell indentation integration with haskell-mode.

(use-package hindent
  :config (add-hook 'haskell-mode-hook #'hindent-mode)
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

;; Packages/JS2

;; A "modern" JavaScript mode, set for all .js files.

(use-package js2-mode
  :config (setq js-switch-indent-offset 2
                js-indent-level 2
                js2-basic-offset 2
                js2-include-node-externs t)
  :ensure t
  :mode (("\\.js\\'" . js2-mode)))

;; -----------------------------------------------------------------------------

;; Packages/JSON

;; JSON support.

(use-package json-mode
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;; -----------------------------------------------------------------------------

;; Packages/JSX

;; JSX support (using rjsx-mode) for React JS, including support for embedded
;; markup, etc.

(use-package rjsx-mode
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Magit

;; Powerful and comprehensive Git porcelain avoiding the need to move outside of
;; Emacs for version control management. Integrated with Ivy and enabled
;; globally.

;; Magithub is included to provide additional integration with Github through
;; the Magit porcelain. This is currently disabled given slightly odd process
;; behaviour.

(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch-popup))
  :config
  (progn
    (setq magit-completing-read-function 'ivy-completing-read)
    (global-magit-file-mode))
  :diminish (auto-revert-mode)
  :ensure t)

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t)
  :disabled t
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
  :config (setq org-startup-indented t)
  :ensure org-plus-contrib
  :pin org)

;; -----------------------------------------------------------------------------

;; Packages/PlantUML

(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode-map))
  :ensure t)

(use-package flycheck-plantuml
  :config (flycheck-plantuml-setup)
  :ensure t)

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
    (projectile-global-mode))
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Purescript

(use-package purescript-mode
  :config (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Rainbow-Delimiters

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/Rainbow

;; Display colour codes visually within buffers (hex, X, etc.)

(use-package rainbow-mode
  :config (add-hook 'prog-mode-hook 'rainbow-mode)
  :diminish (rainbow-mode)
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

;; Packages/TIDE

(use-package tide
  :config
  (progn
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook 'tide-setup))
  :ensure t)

;; -----------------------------------------------------------------------------

;; Packages/TypeScript

(use-package typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)))

;; -----------------------------------------------------------------------------

;; Packages/Uniquify

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/")
  :demand t)

;; -----------------------------------------------------------------------------

;; Packages/YAML

(use-package yaml-mode
  :config
  (progn
    (add-hook 'yaml-mode-hook 'turn-off-flyspell)
    (add-hook 'yaml-mode-hook 'turn-on-fci-mode))
  :ensure t)

;; =============================================================================

;; Module

(provide 'init)

;;; init.el ends here
