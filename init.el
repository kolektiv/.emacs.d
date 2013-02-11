(add-to-list 'load-path 
	     user-emacs-directory)
(add-to-list 'load-path 
	     (concat user-emacs-directory 
		     (convert-standard-filename "os/")))

(require 'cl)
(require 'packages)
(require system-type)
(require 'ui)
