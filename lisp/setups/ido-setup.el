;; Packages

(ac/packages/require 'ido)

;; Load

(ido-mode)

;; UX

(custom-set-faces

 ;; IDO
 
 '(ido-subdir
   ((t
     (:foreground "#CCCCCC"))))
 
 '(ido-first-match
   ((t
     (:foreground "#ff7e00"))))
 
 '(ido-only-match
   ((t
     (:foreground "#ff0000"))))
 
 '(ido-indicator
   ((t
     (:foreground "#767676"))))
 
 '(ido-incomplete-regexp
   ((t
     (:foreground "#FFFFFF")))))

;; Provide

(provide 'ido-setup)
