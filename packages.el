; Define additional third party package repositories
(setq archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

; Define required third party packages
(setq packages
      '(clojure-mode
		fsharp-mode
		haskell-mode
       	js2-mode
		lua-mode
		markdown-mode
		scala-mode2
		dired+))

; Load package.el package management system
(require 'package)

; Add third party package repositories to package.el repositories
(dolist (archive archives)
  (add-to-list 'package-archives archive))

; Initialize package.el package management system
(package-initialize)

; Refresh available packages (can take a while, but Emacs stays open!)
(package-refresh-contents)

; Install required third party packages if not installed already
(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

; Provide this file as a requirable module
(provide 'packages)
