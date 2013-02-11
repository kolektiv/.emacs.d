(setq archives
      '(("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq packages
      '(clojure-mode
		haskell-mode
       	js2-mode
		lua-mode
		markdown-mode
		scala-mode2
		dired+))

(require 'package)

(dolist (archive archives)
  (add-to-list 'package-archives archive))

(package-initialize)
;; (package-refresh-contents)

(dolist (package packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'packages)
