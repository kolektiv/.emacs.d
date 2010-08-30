; Always load up an emacs shell up on start
; (eshell)

;; Set up our standard org-mode publishing projects
(require 'org-publish)
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-project-alist
      '(
	("userguide-content"
	 :base-directory "~/code/m3/Documentation/Source"
	 :base-extension "org"
	 :publishing-directory "~/code/m3/Documentation/Public"
	 :recursive t
	 :publishing-function org-publish-org-to-html
	 :headline-levels 4
	 :auto-sitemap t
	 :sitemap-title "M3 Documentation"
	 :sitemap-filename "index.org"
	 :sitemap-sort-folders last
	 :auto-preamble t
	 :author "Andrew Cherry"
	 :email "andrew.cherry@snowvalley.com"
	 :style "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/m3.css\" />")

	("userguide-static"
	 :base-directory "~/code/m3/Documentation/Source"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf"
	 :publishing-directory "~/code/m3/Documentation/Public"
	 :recursive t
	 :publishing-function org-publish-attachment)

	("userguide"
	 :components ("userguide-content" "userguide-static"))
	))
