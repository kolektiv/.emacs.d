; Set up Cygwin.
(require 'setup-cygwin)

; Make sure that Cygwin is at the start of the path, so we always use cygwin
; utils if available (like ctags)
(setenv "PATH" 
	(concat "C:\\Cygwin\\bin;" 
		(getenv "PATH")))

; Set Tramp to use sshx. Don't listen to all those who say plink.
; That shit is whack.
(setq tramp-default-method "sshx")