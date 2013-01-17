(defvar emacs-root 
  (file-name-directory load-file-name))

(add-to-list 'load-path emacs-root)

(require 'cl)
(require 'packages)
