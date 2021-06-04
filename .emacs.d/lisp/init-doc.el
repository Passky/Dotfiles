(my-add-package 'edit-indirect) ; edit in code block
(my-add-package 'bing-dict) ; search words in bing

;; {{ markdown
(my-add-package 'markdown-mode)
(my-add-package 'markdown-toc)
(advice-add #'markdown--command-map-prompt :override #'ignore)
(advice-add #'markdown--style-map-prompt   :override #'ignore)
(add-auto-mode 'gfm-mode "README\\(?:\\.md\\)?\\'") ; in markdown-mode.el
(with-eval-after-load 'markdown-mode
  (defun markdown-insert-ruby-tag (text ruby)
	"Quick insertion of ruby tag with `TEXT' and `RUBY'."
	(interactive "sText: \nsRuby: \n")
	(insert (format "<ruby>%s<rp>(</rp><rt>%s</rt><rp>)</rp></ruby>" text ruby)))

  (markdown-toc-mode 1)
  (with-eval-after-load 'evil-collection
	(evil-collection-define-key 'normal 'markdown-mode-map
	  (kbd "<tab>") 'markdown-cycle
	  (kbd "S-<tab>") 'markdown-shifttab))

  (setq markdown-header-scaling t
		markdown-enable-wiki-links t
		markdown-italic-underscore t
		markdown-asymmetric-header t
		markdown-gfm-uppercase-checkbox t
		markdown-live-preview-delete-export 'delete-on-export ; delete preview.html
		markdown-fontify-code-blocks-natively t)
  )

;; {{ org
(my-add-package 'org-re-reveal) ; org->pdf
(my-add-package 'toc-org) ;
(my-add-package 'ob-rust)
(my-add-package 'ob-go)
(my-add-package 'valign) ; visual align table for all lang!

;; Drag to insert Image
(my-add-package 'org-download)
(add-hook 'org-mode-hook 'org-download-enable)
(add-hook 'dired-mode-hook 'org-download-enable)
(after! org-download
  (defun +org-download-method (link)
	(org-download--fullname (org-link-unescape link)))
  (setq org-download-method '+org-download-method)

  (setq org-download-annotate-function (lambda (_link) "")
		org-download-method 'attach
		org-download-screenshot-method "screencapture -i %s"))

;; (my-add-package 'org) ; don't use newer org-mode
(add-hook 'org-mode-hook #'(lambda ()
							 (org-indent-mode 1)
							 (toc-org-enable)
							 (when *gui*
							   (valign-mode t)) ; it would have problem issue which I have not met with,so let it be.
							 ))

;; (add-auto-mode 'outline-mode "\\.org\\(_archive\\)?$")
(with-eval-after-load 'org

  (setq org-log-done 'time ; track when todo get done
		org-return-follows-link t ; use ret instead of C-ret
		org-startup-indented t ; not Outline with only one * and replace others with <space>
		org-fontify-done-headline t ; let DONE: comes with a line of deletion.
		)
  
  ;; https://emacs-china.org/t/topic/3971
  ;; NOTE:There's a bug.
  ;; auto save DONE to *.org_archive
  (defun archive-done-tasks ()
	(interactive)
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward
			  (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
		(goto-char (line-beginning-position))
		(org-archive-subtree))))

  ;; (defun enable-auto-archive ()
  ;; 		 (add-hook 'after-save-hook 'archive-done-tasks))
  ;; (add-hook 'org-mode-hook 'enable-auto-archive)
  
  (defvar load-language-list '((emacs-lisp . t)
							   (perl . t)
							   (python . t) ; handle ipython
							   (ruby . t)
							   (js . t)
							   (shell . t) ; calls default shell
							   (css . t)
							   (sass . t)
							   (C . t) ; handle c++/c/D lang
							   (java . t)
							   (rust . t) ; needs extra package to support
							   (go . t)
							   (plantuml . t)))

  (org-babel-do-load-languages 'org-babel-load-languages
							   load-language-list)

  (after! org-crypt
	(setq org-crypt-key nil)
	)
  ;; hack for org-babel
  ;; little change from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-lsp.el
  (cl-defmacro my-lsp-org-babel-enable (lang)
	"Support LANG in org source code block."
	(cl-check-type lang stringp)
	(let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
		   (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
	  `(progn
		 (defun ,intern-pre (info)
		   (let ((my-block-temp-file-name (->> info caddr (alist-get :file))))
			 (unless my-block-temp-file-name
			   ;; (user-error "LSP:: specify `:file' property to enable")  ; This won't work without :file header argumenst
			   (message "Please Input :file property, now using random generate.")
			   (setq my-block-temp-file-name (my-generate-random-string 5))
			   )
			 (setq-local buffer-file-name my-block-temp-file-name)
			 (my-ensure 'lsp-mode)
			 (lsp-deferred)
			 ))
		 (if (fboundp ',edit-pre)
			 (advice-add ',edit-pre :after ',intern-pre)
		   (progn
			 (defun ,edit-pre (info)
			   (,intern-pre info))
			 (put ',edit-pre 'function-documentation
				  (format "Prepare local buffer environment for org source block (%s)."
						  (upcase ,lang))))))))

  (defvar my-org-babel-lang-list
	'("go" "python" "ipython" "ruby" "js" "css" "sass" "C" "rust" "java" "shell" "sh" "C++" ))

  (dolist (lang my-org-babel-lang-list)
	(eval `(my-lsp-org-babel-enable ,lang))))

;; {{ rst
;; Most used by python community.
(after! rst
  (add-hook 'rst-adjust 'rst-toc-update))

;; {{ read pdf by emacs
;; TODO: EAF is great,considering add it.
;; {{ builtin doc-view,reading pdf
(with-eval-after-load 'doc-view
  ;; This requires external ghostscript/mudraw/pdfdraw(mupdf) program to work.
  (setq doc-view-resolution 250))

;; {{ epub
(my-add-package 'nov)
(add-auto-mode 'nov-mode "\\.epub\\'")

;; {{ rss
(define-advice newsticker--cache-read (:around (func &rest args))
  "Read cache data without prompt."
  (cl-letf* (((symbol-function 'y-or-n-p) (lambda (_) t)))
	(apply func args)))

(after! newst-backend
  (setq ;; Keep obsolete items for a month
   newsticker-keep-obsolete-items t
   newsticker-obsolete-item-max-age (* 30 86400)
   ;; Sane behavior
   newsticker-automatically-mark-items-as-old nil
   newsticker-automatically-mark-visited-items-as-old t
   ;; Emacs async sucks,so when it dose not work,set it to extern
   newsticker-retrieval-method 'intern
   ;; Improve the default display
   newsticker-treeview-listwindow-height 20
   newsticker-treeview-date-format "%F %a, %H:%M  "
   newsticker-url-list-defaults nil
   newsticker-url-list '(
						 ;; ("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
						 ("LWN (Linux Weekly News)" "https://lwn.net/headlines/rss")
						 ("Renmingwang en English" "http://en.people.cn/rss/90000.xml")
						 ("Renmingwang" "http://www.people.com.cn/rss/world.xml")
						 ("虎嗅" "https://www.huxiu.com/rss/0.xml")
						 ("BBC news" "http://feeds.bbci.co.uk/news/rss.xml")
						 ("Hacker News" "https://hnrss.org/frontpage")
						 ("Zhihu Daily" "https://www.zhihu.com/rss")
						 )))

;; {{ websearch
(after! webjump
  (setq webjump-sites '(;; Internet search engines.
                   ("Bing" .
                    [simple-query "www.bing.com"
                                  "www.bing.com/search?q=" ""])
                   ("Google" .
                    [simple-query "www.google.com"
                                  "www.google.com/search?q=" ""])
                   ("Wikipedia" .
                    [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
                   ("Urban Dictionary" .
                    [simple-query "urbandictionary.com" "www.urbandictionary.com/define.php?term=" ""])
                   ("Ludwig Guru" .
                    [simple-query "ludwig.guru" "ludwig.guru/s/" ""])
                   ("Etymology" .
                    [simple-query "etymonline.com" "etymonline.com/word/" ""])
                   ("Stack Overflow" .
                    [simple-query "stackoverflow.com" "stackoverflow.com/search?q=" ""])
                   ("TLDR" .
                    [simple-query "linux.cn" "tldr.linux.cn/cmd/" "/"])
                   ("Man Search" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" ""])
                   ("Man Go" .
                    [simple-query "archlinux.org" "man.archlinux.org/search?q=" "&go=Go"])

                   ;; Language specific engines.
                   ("x86 Instructions Reference" .
                    [simple-query "www.felixcloutier.com"
                                  "www.felixcloutier.com/x86/" ""])
                   ("Python Docs" .
                    [simple-query "docs.python.org"
                                  "docs.python.org/3/search.html?q=" ""])
                   ("Racket Docs" .
                    [simple-query "docs.racket-lang.org"
                                  "https://docs.racket-lang.org/search/index.html?q=" ""])
                   ("Cpp Reference" .
                    [simple-query "en.cppreference.com"
                                  "en.cppreference.com/mwiki/index.php?search=" ""]))))

;; {{ Latex work space
(my-add-package 'latex-preview-pane) ; on the fly latex build and pdf preview
;; \resizebox{\textwidth}{!}{...} make width same with screen and resize with height
(add-hook 'latex-mode-hook #'(lambda ()
							  (flymake-start)
							  (latex-preview-pane-mode)))
;; REVIEW: Use org-latex-impatient in org for quick preview

;; TODO in emacs28 there'a builtin `dictionary' package
(autoload 'dictionary-search "dictionary" "" t)
(after! dictionary
  (add-hook 'dictionary-mode-hook #'(lambda ()
									  (setq-local imenu-generic-expression '((nil "^From <<\\(.+\\)>>" 1)))))
  (setq dictionary-server "dict.org" ; anyway default localhost will fail
		dictionary-default-popup-strategy "lev" ; lex search
		dictionary-description-open-delimiter "<<"
		dictionary-description-close-delimiter ">>"))




(provide 'init-doc)
;;; init-doc.el ends here.
