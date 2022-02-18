;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

;; load the publishing system
(require 'ox-publish)

;; Custom functions

;; Taken from https://taingram.org/blog/org-mode-blog.html
(defun my/org-sitemap-date-entry-format (entry style project)
  "Format ENTRY in org-publish PROJECT Sitemap format ENTRY ENTRY STYLE format that includes date."
  (let ((filename (org-publish-find-title entry project)))
    (if (= (length filename) 0)
        (format "*%s*" entry)
      (format "{{{timestamp(%s)}}} [[file:%s][%s]]"
              (format-time-string "%Y-%m-%d"
                                  (org-publish-find-date entry project))
              entry
              filename))))

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))


;; Define the publishing project
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil

      org-html-head "<link rel=\"stylesheet\" href=\"/CSS/simple.css\" />
<link rel=\"stylesheet\" href= \"/CSS/navbar.css\" />
<link rel=\"stylesheet\" href= \"/CSS/misc.css\" />"
      org-html-postamble "<hr/>
<footer>
<p>Author: James Leslie</p>
<p>Made with <a href=\"https://www.gnu.org/software/emacs/\">Emacs</a> and <a href=\"https://orgmode.org/\">org-mode</a></p>
<p>Last modified on %C</p>
</footer>"
      )



(setq org-publish-project-alist
      (list
       (list "my-site"
	     :recursive nil             ;; We want to use another function for blog generation
	     :base-directory "./content"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html

	     :with-author nil           ;; Don't include author name
	     :with-creator nil          ;; Include Emacs and Org versions in footer
	     :with-toc nil              ;; Don't include a table of contents
	     :section-numbers nil       ;; Don't include section numbers
	     :with-date nil         ;; Don't include time stamp in file
	     :with-timestamps nil

	     :html-preamble "<div class=\"topnav\">
<a href=\"/index.html\">Home</a>
<a href=\"/blog/index.html\">Blog</a>
<a href=\"/contact.html\">Contact</a>
</div>")
       (list "blog"
	     :base-directory "./content/blog"
	     :base-extension "org"
	     :publishing-directory "./public/blog"
	     :publishing-function 'org-html-publish-to-html
	     :auto-sitemap t            ;; Builds a blog post page
	     :sitemap-title "Blog Posts"
	     :sitemap-filename "index.org"
	     :sitemap-sort-files 'anti-chronologically
	     :sitemap-format-entry 'my/org-sitemap-date-entry-format
	     :with-date t

	     :with-author nil           ;; Don't include author name
	     :with-creator nil          ;; Include Emacs and Org versions in footer
	     :with-toc nil              ;; Don't include a table of contents
	     :section-numbers nil       ;; Don't include section numbers
	     :with-date nil         ;; Don't include time stamp in file
	     :with-timestamps nil
	     :with-title nil
	     :html-head-extra "<link rel=\"stylesheet\" href=\"/CSS/theorem.css\" />"
	     :html-preamble "<div class=\"topnav\">
<a href=\"/index.html\">Home</a>
<a href=\"/blog/index.html\">Blog</a>
<a href=\"/contact.html\">Contact</a>
</div>
<h1 class=\"title\">%t</h1>%d")
       (list "static"
	     :base-directory "./content"
	     :base-extension "css\\|txt\\|jpg\\|gif\\|png\\|jpeg\\|pdf"
	     :recursive t
	     :publishing-directory "./public"
	     :publishing-function 'org-publish-attachment)))



;;; RSS
(package-install 'org-static-blog)


(setq org-static-blog-publish-title "James Leslie's Blog")
(setq org-static-blog-publish-url "https://jeslie0.github.io")

(setq org-static-blog-posts-directory "./content/blog")
(setq org-static-blog-publish-directory "./RSS/")

(setq org-static-blog-drafts-directory "./drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header "<link rel=\"stylesheet\" href=\"/CSS/theorem.css\" />")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble "")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble "")

;; This HTML code is inserted into the index page between the preamble and
;;   the blog posts
(setq org-static-blog-index-front-matter "")

;; Generate the site output
(org-publish-all) ;; Add t here when testing html and css changes. Remove when just updating content
(org-static-blog-publish)
(message "Build Complete")
