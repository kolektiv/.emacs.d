;; Package

(require 'package)

;; Archives

(add-to-list
 'package-archives
 '("marmalade" . "https://marmalade-repo.org/packages/"))

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/"))

;; Initialize

(package-initialize)
(package-refresh-contents)

;; Require

(defun ac/packages/require (p)
  (unless (package-installed-p p)
    (package-install p)))

;; Load

(defun ac/setups/require (s)
  (require (intern (file-name-sans-extension s))))

(defun ac/setups/find (directory &optional full match nosort)
  (delete "." (delete ".." (directory-files directory full match nosort))))

(defun ac/setups/load (d)
  (mapc 'ac/setups/require (ac/setups/find d)))

(ac/setups/load (eval ac/paths/setups))

;; Provide

(provide 'setups)
