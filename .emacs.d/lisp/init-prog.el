(my-add-package 'command-log-mode)
(my-add-package 'gitconfig-mode)
(my-add-package 'lua-mode)
(my-add-package 'yaml-mode)
(my-add-package 'writeroom-mode)
(my-add-package 'haml-mode)
(my-add-package 'scss-mode)
(my-add-package 'jade-mode)
;; (my-add-package 'msvc) ; provide completion for msvc project,but I use cmake more now.

;;{{ Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
	(add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
	(add-to-list 'interpreter-mode-alist (cons pattern mode))))
;;}}

(defconst my-nice-comment-enable-hook (list
									   'java-mode-hook  ; use lsp-java
									   'c-mode-hook
									   'c++-mode-hook
									   'js-mode-hook
									   'typescript-mode-hook
									   )
  "Enable nice comment(`M-j') in these modes.")

(dolist (hook my-nice-comment-enable-hook)
  (add-hook hook (lambda () 
				   (local-set-key [remap comment-indent-new-line] 'c-indent-new-comment-line) ; default `M-j'
				   (setq-local comment-multi-line t))))

;; new variables in emacs28, deciding comment style
(setq c-doc-comment-style
	  '((java-mode . javadoc)
		(pike-mode . autodoc)
		(c-mode . javadoc)
		(c-mode . javadoc)
		))

;; {{ c#-mode
(my-add-package 'csharp-mode) ; it could use tree-sitter for better highlight
;; (add-auto-mode 'csharp-tree-sitter-mode
;; 			   "\\.cs\\'")
(with-eval-after-load 'csharp-mode
  )

;;{{ cc-mode
(add-auto-mode 'c++-mode
			   "\\.cxx\\'")
(add-auto-mode 'cc-mode
			   "\\.cxx\\'")

(with-eval-after-load 'cc-mode
  (setq c-offsets-alist '((inline-open           . 0)
						  (brace-list-open       . 0)
						  (inextern-lang         . 0)
						  (statement-case-open   . 4)
						  (access-label          . -)
						  (case-label            . 0)
						  (member-init-intro     . +)
						  (topmost-intro         . 0)
						  (inlambda              . 0) ;; better indentation for lambda
						  (innamespace           . -) ;; no indentation after namespace
						  (arglist-cont-nonempty . +)))
  (setq c-basic-offset 4)
  )


(my-add-package 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook 'modern-c++-font-lock-mode)
(with-eval-after-load 'modern-cpp-font-lock
  (setq modern-c++-literal-binary-prefix-face 'font-lock-constant-face
		modern-c++-literal-binary-infix-face  'font-lock-constant-face
		modern-c++-literal-binary-suffix-face 'font-lock-constant-face
		modern-c++-literal-octal-prefix-face  'font-lock-constant-face
		modern-c++-literal-octal-infix-face   'font-lock-constant-face
		modern-c++-literal-octal-suffix-face  'font-lock-constant-face
		modern-c++-literal-hex-prefix-face    'font-lock-constant-face
		modern-c++-literal-hex-infix-face     'font-lock-constant-face
		modern-c++-literal-hex-suffix-face    'font-lock-constant-face
		modern-c++-literal-dec-infix-face     'font-lock-constant-face
		modern-c++-literal-dec-suffix-face    'font-lock-constant-face))

(my-add-package 'cmake-mode)
(my-add-package 'cpputils-cmake)
(add-auto-mode 'cmake-mode "\\CMakeLists.txt\\'" "\\.cmake\\'")
;; }}

;;{{ python-mode
(my-add-package 'elpy) ; python eonvironment
(my-add-package 'yapfify)
(with-eval-after-load 'python
  (setq electric-indent-chars (delq ?: electric-indent-chars))
  (setq python-indent-offset 4)
  (when (executable-find "ipython3")
	(setq python-shell-interpreter-args "--simple-prompt -i")
	(setq python-shell-interpreter "ipython3"))
  (setq python-indent-guess-indent-offset-verbose nil)
  (with-eval-after-load 'elpy
	(remove-hook 'elpy-modules 'elpy-module-flymake)
	(when (executable-find "ipython3")
	  (add-hook 'elpy-mode-hook 'elpy-use-ipython "ipython3"))
	))
;;}}

;;{{ java-mode
(add-auto-mode 'java-mode
			   ;; java
			   "\\.java\\'"
			   "\\.aj\\'"
			   ;; makefile
			   "\\.ninja$" )
;;}}

;;{{ rust-mode
(my-add-package 'cargo)
(my-add-package 'rust-mode)
(add-auto-mode 'rust-mode "\\.rs\\'")
(with-eval-after-load 'rust-mode
  ;; TODO: when this feature become more mature, enable it
  ;; It's TOO annoying now.
  ;; (setq rust-format-on-save (executable-find "rustfmt"))
  )
(add-hook 'rust-mode-hook 'cargo-minor-mode)
;;}}

;;{{ go-mode
(my-add-package 'go-mode)
(add-auto-mode 'go-mode "\\.go\\'")
(with-eval-after-load 'go-mode
  (with-eval-after-load 'exec-path-from-shell
	(exec-path-from-shell-copy-envs '("GOPATH" "GOPROXY")))
  (setq godoc-reuse-buffer t))
;;}}

;;{{ web and js/ts
(my-add-package 'web-mode)
(add-auto-mode 'web-mode "\\.html\\'" "\\.vue\\'") ; vue file support
(with-eval-after-load 'web-mode
  (setq web-mode-enable-auto-closing t ; enable auto close tag in text-mode
		web-mode-auto-close-style 2 ; close on < and </
		web-mode-enable-auto-pairing t
		web-mode-enable-css-colorization t
		web-mode-imenu-regexp-list
		'(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
		  ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
		  ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
		  ;; angular imenu
		  (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

(add-hook 'mhtml-mode-hook
		  #'(lambda ()
					 (setq sgml-quick-keys 'close)
					 ))

;; mock file
(add-auto-mode 'js-mode "\\.mock.js\\'")
(add-interpreter-mode 'js-mode "node")

(add-auto-mode 'js-mode
			   "\\.js\\(\\.erb\\)?\\'"
			   "\\.babelrc\\'")
;; jsx
(add-auto-mode 'js-mode
			   "\\.jsx\\'"
			   "components\\/.*\\.js\\'")

(my-add-package 'typescript-mode)
(add-auto-mode 'typescript-mode "\\.ts$"
			   "\\.tsx\\'" )
;;}}

;;{{ haskell-mode with dante
(my-add-package 'haskell-mode)
(my-add-package 'dante)
(add-auto-mode 'haskell-mode "\\.ghci\\'")
(add-hook 'haskell-mode-hook #'(lambda ()
								 (haskell-indentation-mode)
								 (haskell-doc-mode)
								 ;; (dante-mode)
								 (interactive-haskell-mode)
								 (turn-on-haskell-indentation)
								 (flymake-start)
								 ))
(with-eval-after-load 'haskell-mode
  (setq haskell-completing-read-function 'completing-read
		haskell-process-check-cabal-config-on-load nil
		;; haskell-process-suggest-add-package nil
		;; haskell-process-suggest-hoogle-imports nil
		;; haskell-process-suggest-language-pragmas nil
		;; haskell-process-suggest-overloaded-strings nil
		;; haskell-process-suggest-restart nil
		))
;;}}

;;{{ sql-mode
(add-auto-mode 'sql-mode "\\.sql\\'")
(add-hook 'sql-interactive-mode 'toggle-truncate-lines)
;;}}

;;{{ sh-mode
(add-auto-mode 'sh-mode
			   "\\.bash\\(_profile\\|_history\\|rc\\.local\\|rc\\)?$"
			   "/PKGBUILD\\'"
			   "\\.z?sh$"
			   "\\.env$")
(with-eval-after-load 'sh-script
  (setq sh-basic-offset 2
		sh-indentation 2))

;; {{ others
(my-add-package 'vimrc-mode)
(add-auto-mode 'vimrc-mode "\\.?vim\\(rc\\)?$")

(my-add-package 'csv-mode)
(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(add-auto-mode 'text-mode
			   "TAGS\\'"
			   "\\.ctags\\'")

(my-add-package 'groovy-mode)
(add-auto-mode 'groovy-mode
			   "\\.groovy\\'"
			   "\\.gradle\\'" )

;; Binary edit.
(add-auto-mode 'hexl-mode
			   "\\.o\\'"
			   "\\.out\\'"
			   "\\.exe\\'"
			   "\\.class\\'" )

;; clojure
(add-auto-mode 'clojure-mode
			   "\\.clj\\'")

;; gitignore
(add-auto-mode 'conf-unix-mode
			   "\\.gitignore\\'")

(add-auto-mode 'conf-space-mode
			   "/cgdbrc\\'"
			   "/.clang-format\\'")

(provide 'init-prog)
;;; init-prog.el ends here
