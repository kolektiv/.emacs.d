; Add current Emacs dir to load path
(add-to-list 'load-path 
             user-emacs-directory)

; Add Emacs OS dir (OS specific settings) to load path
(add-to-list 'load-path 
             (concat user-emacs-directory 
                     (convert-standard-filename "os/")))

; Load Common Lisp compatibility functions
(require 'cl)

; Load packages from archives using package.el
(require 'packages)

; Load general customisations
(require 'common)

; Load OS specific customisations
(require system-type)

; Load UI customisations
(require 'ui)
