;; Commmon Lisp

(require 'cl)

;; Paths

(defun ac/paths/make (p)
  (concat user-emacs-directory
	  (convert-standard-filename p)))

(defun ac/paths/add (p)
  (add-to-list 'load-path (eval p)))

(defun ac/paths/load (ps)
  (mapc 'ac/paths/add ps))

(setq ac/paths/lisp (ac/paths/make "lisp"))
(setq ac/paths/os (ac/paths/make "lisp/os"))
(setq ac/paths/setups (ac/paths/make "lisp/setups"))

(ac/paths/load '(ac/paths/lisp
                 ac/paths/os
                 ac/paths/setups))

;; Core

(require 'core)

;; UX

(require 'ux)

;; OS

(require 'os)

;; Setups

(require 'setups)
