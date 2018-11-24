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

;; ==============================================================================

;; Core

;; Configuration of modes/modules/functions built in to Emacs, and which are
;; thus configured "raw" rather than configured lazily using use-package as
;; configured in the latter part of the init file.

;; ------------------------------------------------------------------------------

;; Core / Backups

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

;; ------------------------------------------------------------------------------

;; Core / Common Lisp

;; Allow the use of Common Lisp style programming features within Emacs Lisp
;; where useful.

(require 'cl-lib)

;; ------------------------------------------------------------------------------

;; Core / Customisation

;; Settings for custom mode, along with configuration to use a discrete
;; custom.el file instead of storing customized values within init.el.

(setq-default custom-buffer-done-kill t
              custom-buffer-indent 4)

(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)

;; ------------------------------------------------------------------------------

;; Core / Editing

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
(global-prettify-symbols-mode 1)
(show-paren-mode 1)
(transient-mark-mode 1)

;; ------------------------------------------------------------------------------

;; Core / ERC

;; Set the preferences for ERC to suitable defaults.

(setq-default erc-autoaway-mode t
              erc-away-nickname "kolektiv.afk"
              erc-nick "kolektiv")

;; ------------------------------------------------------------------------------

;; Core / Interaction

;; Override some of the default Emacs interactions for more concise alternatives
;; such as allowing Y or N instead of a full Yes or No response to buffer
;; interactions. Additionally, ensure that sensible key bindings are used when
;; on a Mac.

(defalias 'yes-or-no-p 'y-or-n-p)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt)
  (setq mac-right-option-modifier 'none)
  (global-set-key [kp-delete] 'delete-char))

;; ------------------------------------------------------------------------------

;; Core / Interface

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

;; ------------------------------------------------------------------------------

;; Core / Package Management

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
        '(("melpa" . 2)
          ("melpa-stable" . 1))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  (package-install 'bind-key))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)

;; ------------------------------------------------------------------------------

;; Core / Theme

;; Add local themes to the custom theme load path, then load the kolektiv custom
;; theme without prompting (as it's theoretically trusted local code).

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'kolektiv t)

;; ==============================================================================

;; Extensions

;; Packages (managed via the use-package macro) for extensionsfunctionality such
;; as completion, search, mini-buffer facilities, etc.

;; ------------------------------------------------------------------------------

;; Extensions / Aggressive Indent

;; Aggressive indenting, on the fly re-indenting while editing.

(use-package aggressive-indent
  :config (progn
            (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'purescript-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'typescript-mode)
            (global-aggressive-indent-mode 1))
  :diminish (aggressive-indent-mode)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Auto Package Update

;; Keep packages up to date, removing old packages. Override the periodicity to
;; check regularly, in this case once a day.

(use-package auto-package-update
  :config (auto-package-update-maybe)
  :custom ((auto-package-update-delete-old-versions t)
           (auto-package-update-interval 1))
  :demand t
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Company

;; Company mode enabled globally, for use with modes which integrate in
;; particular. Also added is company-flx, which enables fuzzy matching for
;; company-mode completions (if using a CAPF backend). This seems to work quite
;; well, but may be re-evaluated if it becomes to slow/unstable.

(use-package company
  :bind (("C-SPC" . company-complete))
  :custom ((company-auto-complete t)
           (company-idle-delay 1)
           (company-tooltip-minimum-width 30))
  :config (with-no-warnings
            (global-company-mode))
  :diminish (company-mode)
  :ensure t)

(use-package company-flx
  :config (company-flx-mode 1)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Counsel

;; Better M-x and related functions, using the underlying Ivy support for better
;; completion modes. Light and effective. Projectile integration is also added
;; to support Ivy/Counsel based find, etc. in projectile-mode.

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
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

;; ------------------------------------------------------------------------------

;; Extensions / Cyphejor

;; Major mode name customisation system, shortening/cypher-ing commonly used
;; names to de-clutter the mode line.

(use-package cyphejor
  :config (cyphejor-mode 1)
  :custom ((cyphejor-rules
            '(:downcase
              ("bookmark"    "→")
              ("buffer"      "β")
              ("diff"        "Δ")
              ("dired"       "δ")
              ("emacs"       "ε")
              ("inferior"    "i" :prefix)
              ("interaction" "i" :prefix)
              ("interactive" "i" :prefix)
              ("lisp"        "λ" :postfix)
              ("menu"        "▤" :postfix)
              ("mode"        "")
              ("package"     "↓")
              ("python"      "π")
              ("purescript"  "ps")
              ("shell"       "sh" :postfix)
              ("text"        "ξ")
              ("wdired"      "↯δ"))))
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Dired (and Dired+)

;; Dired and Dired+ configuration, including setting Dired to use the GNU ls
;; implementation if running on OS X and the implementation has been installed
;; as part of coreutils. Listing switches are also set to sort directories
;; first.

;; For Dired+, the detailed listing is set as the default, and reuse of a single
;; Dired buffer is enabled to avoid cluttering Emacs with hundreds of Dired
;; buffers. Dired+ is loaded from the local packages directory, as it is not
;; currently available on MELPA or similar.

(use-package dired
  :config (progn
            (if (eq system-type 'darwin)
                (let ((gls "/usr/local/bin/gls"))
                  (if (file-exists-p gls)
                      (setq insert-directory-program gls))))
            (load "~/.emacs.d/packages/diredp.el")
            (with-no-warnings
              (diredp-toggle-find-file-reuse-dir 1)))
  :custom ((dired-listing-switches "-laXGh --group-directories-first"))
  :init (setq-default diredp-hide-details-initially-flag nil))

;; ------------------------------------------------------------------------------

;; Extensions / Emacs

(use-package emacs
  :diminish (eldoc-mode . " ed"))

;; ------------------------------------------------------------------------------

;; Extensions / Ethan WSpace

;; Globally configured whitespace management and removal.

(use-package ethan-wspace
  :config (global-ethan-wspace-mode 1)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Exec Path From Shell

;; Ensure that the path is initialized from environment settings defined and
;; configured by the appropriate shell.

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Fill Column Indicator

;; Fill column indicator (configured for a classic 80 column width). A function
;; to temporarily suspend fci-mode is configured, and run when company-mode is
;; actively displaying a frontend. This prevents a positioning bug where the
;; frontend popup will be displayed after the fill column indicator character.

(use-package fill-column-indicator
  :config (progn
            (eval-when-compile
              (defun suspend-fci-mode (command)
                (when (string= "show" command)
                  (turn-off-fci-mode))
                (when (string= "hide" command)
                  (turn-on-fci-mode))))
            (advice-add 'company-call-frontends :before #'suspend-fci-mode))
  :custom ((fci-rule-color "#1c1c1c")
           (fci-rule-column 80)
           (fci-rule-use-dashes nil))
  :disabled t
  :ensure t
  :hook (prog-mode . fci-mode))

;; ------------------------------------------------------------------------------

;; Extensions / Flycheck

;; Integrates external checking processes with buffer editing, providing
;; in-place warning, error, etc. notification via highlighting and tool tips.

(use-package flycheck
  :config (global-flycheck-mode)
  :custom ((flycheck-mode-line-prefix "fc"))
  :ensure t)

(use-package flycheck-color-mode-line
  :custom ((flycheck-color-mode-line-face-to-color 'mode-line-buffer-id))
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-inline
  :ensure t
  :hook (flycheck-mode . flycheck-inline-mode))

;; ------------------------------------------------------------------------------

;; Extensions / Flyspell

;; Use Flyspell automatically for appropriate modes, and use likeness-based
;; correction matching in the case of multiple potential corrections.

;; Ivy integration is included for Flyspell correction functionality, bound to
;; C-; - note that save functionality is then available using Ivy actions.

(use-package flyspell
  :custom ((flyspell-sort-corrections nil)
           (ispell-dictionary "british-ise-w_accents")
           (ispell-program-name "aspell"))
  :diminish flyspell-mode
  :ensure t
  :hook (text-mode . flyspell-mode))

(use-package flyspell-correct-ivy
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-previous-word-generic))
  :demand t
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Flx

;; Fuzzy matching, used by Ivy and Counsel for the fuzzy search preferred for
;; general usage.

(use-package flx
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Focus

;; Focus mode, diminishing unfocused text while reading/editing.

(use-package focus
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Git-Gutter

;; Show changes from the current Git version of a file in the left hand gutter,
;; indicating changes through slightly customised symbols. Automatically update
;; after 2 seconds, without waiting for save (the default).

(use-package git-gutter
  :config (global-git-gutter-mode 1)
  :custom ((git-gutter:added-sign "++")
           (git-gutter:always-show-separator t)
           (git-gutter:deleted-sign "--")
           (git-gutter:modified-sign "==")
           (git-gutter:update-interval 2))
  :diminish (git-gutter-mode)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Indent Guide

;; Shows a vertical indent guide to help determine appropriate levels of
;; indentation in indentation-relevant languages.

(use-package indent-guide
  :config (indent-guide-global-mode)
  :diminish (indent-guide-mode)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Ivy

;; Ivy completion matching framework, underlying the Counsel and Swiper
;; implementations. Configured to taste, particularly around the fuzzy matching
;; approach, using flx as the underlying provider.

(use-package ivy
  :bind ("C-c C-r" . ivy-resume)
  :config (progn
            (add-to-list 'ivy-ignore-buffers "\\*magit")
            (add-to-list 'ivy-ignore-buffers "\\*Flycheck")
            (with-no-warnings
              (ivy-mode 1)))
  :custom ((ivy-count-format "(%d/%d) ")
           (ivy-format-function 'ivy-format-function-line)
           (ivy-initial-inputs-alist nil)
           (ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
           (ivy-use-virtual-buffers t)
           (ivy-wrap t))
  :diminish ivy-mode
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Magit

;; Powerful and comprehensive Git porcelain avoiding the need to move outside of
;; Emacs for version control management. Integrated with Ivy and enabled
;; globally.

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config (global-magit-file-mode)
  :custom (magit-completing-read-function 'ivy-completing-read)
  :diminish (auto-revert-mode)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Projectile

;; Global projectile management/switching system, allowing fast access to
;; a logical "project" (by default a Git repository).

(use-package projectile
  :bind ("C-c p" . projectile-command-map)
  :config (with-no-warnings
            (projectile-mode +1))
  :custom ((projectile-completion-system 'ivy)
           (projectile-mode-line-function
            '(lambda ()
               (when (ignore-errors (projectile-project-root))
                 (propertize
                  (format " prj:%s" (projectile-project-name))
                  'face '(:foreground "orange"))))))
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Rainbow Delimiters

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ------------------------------------------------------------------------------

;; Extensions / Rainbow

;; Display colour codes visually within buffers (hex, X, etc.)

(use-package rainbow-mode
  :diminish (rainbow-mode)
  :ensure t
  :hook (prog-mode . rainbow-mode))

;; ------------------------------------------------------------------------------

;; Extensions / Swiper

(use-package swiper
  :bind ("C-s" . swiper)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Extensions / Uniquify

(use-package uniquify
  :custom ((uniquify-buffer-name-style 'forward)
           (uniquify-separator "/"))
  :demand t)

;; ==============================================================================

;; Languages

;; Packages (managed via the use-package macro) for functionality for working
;; with specific languages or language families.

;; ------------------------------------------------------------------------------

;; Languages / Docker

(use-package dockerfile-mode
  :ensure t)

;; ------------------------------------------------------------------------------

;; Languages / GraphQL

(use-package graphql-mode
  :ensure t)

;; ------------------------------------------------------------------------------

;; Languages / Haskell

(use-package haskell-mode
  :ensure t)

(use-package hindent
  :ensure t
  :hook (haskell-mode . hindent-mode))

;; ------------------------------------------------------------------------------

;; Languages / HCL (with Terraform, etc)

;; HCL

(use-package hcl-mode
  :ensure t)

;; Terraform

(use-package terraform-mode
  :ensure t)

;; ------------------------------------------------------------------------------

;; Languages / JavaScript (plus JSON), TypeScript, and Node

;; JavaScript

(use-package js2-mode
  :custom ((js-switch-indent-offset 2)
           (js-indent-level 2)
           (js2-basic-offset 2)
           (js2-include-node-externs t))
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.mjs\\'" . js2-mode)))

(use-package rjsx-mode
  :ensure t)

;; JSON

(use-package json-mode
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

;; TypeScript

(use-package tide
  :custom ((tide-completion-detailed nil)
           (tide-hl-identifier-idle-time 0))
  :ensure t
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(use-package typescript-mode
  :custom ((typescript-indent-level 2))
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)))

;; Node

(use-package add-node-modules-path
  :ensure t
  :hook ((js-mode . add-node-modules-path)
         (js2-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)))

;; ------------------------------------------------------------------------------

;; Languages / Markdown

(use-package markdown-mode
  :commands (gfm-mode markdown-mode)
  :custom ((markdown-command "multimarkdown"))
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; ------------------------------------------------------------------------------

;; Languages / Org

(use-package org
  :custom ((org-startup-indented t))
  :ensure org-plus-contrib
  :pin org)

;; ------------------------------------------------------------------------------

;; Languages / Purescript

(use-package purescript-mode
  :diminish "ps"
  :ensure t
  :mode "\\.purs\\'")

(use-package psc-ide
  :custom ((psc-ide-use-npm-bin t))
  :diminish "ps-ide"
  :ensure t
  :hook (purescript-mode . (lambda ()
                             (psc-ide-mode)
                             (turn-on-purescript-indentation))))

;; ------------------------------------------------------------------------------

;; Languages / PlantUML

(use-package plantuml-mode
  :ensure t
  :mode (("\\.plantuml\\'" . plantuml-mode)))

(use-package flycheck-plantuml
  :config (flycheck-plantuml-setup)
  :ensure t)

;; ------------------------------------------------------------------------------

;; Languages / Rust

;; Rust

(use-package rust-mode
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

;; Cargo

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;; ------------------------------------------------------------------------------

;; Languages / YAML

(use-package yaml-mode
  :ensure t
  :hook ((yaml-mode . turn-off-flyspell)
         (yaml-mode . fci-mode)))

;; =============================================================================

;; Module

(provide 'init)

;;; init.el ends here
