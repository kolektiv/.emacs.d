; Use Common Lisp
(require 'cl)

; Set up emacs-root in a cross platform way
(defvar emacs-root (file-name-directory load-file-name))

; Add useful elisp directories to the path
(labels ((add-path (p)
		   (add-to-list 'load-path
				(concat emacs-root p))))
  (add-path "")
  (add-path "packages")
  (add-path "packages/csharp-mode")
  (add-path "packages/haskell-mode")
  (add-path "packages/lua-mode")
  (add-path "packages/scala-mode")
  (add-path "packages/io-mode")
  (add-path "packages/ioke-mode")
  (add-path "packages/yasnippet")
  (add-path "packages/js2-mode")
  (add-path "packages/coffee-mode")
  (add-path "packages/org-mode/lisp")
  (add-path "packages/markdown-mode")
  (add-path "packages/mustache-mode")
  (add-path "packages/go-mode")
  (add-path "packages/less-css-mode")
  (add-path "packages/json-mode")
  (add-path "packages/fsharp-mode"))

; Load common settings for all platforms.
(load "common.el")

; Load Windows specific setup if we're running under Windows.
(if (or (eq system-type 'windows-nt)
	(eq system-type 'cygwin))
    (load "windows.el"))

; Load OS X specific setup if running under OS X.
(if (eq system-type 'darwin)
    (load "osx.el"))

(load "visuals.el")

; Load startup file (things to do once everything is loaded!)
(load "on-start.el")
