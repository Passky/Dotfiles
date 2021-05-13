;;; Code:
;;{{ lsp-mode
(my-add-package 'lsp-mode)
(my-add-package 'lsp-java)
(my-add-package 'lsp-pyright)
(defconst my-lsp-enable-hook (list
							  ;; 'java-mode-hook  ; use lsp-java
							  ;; 'verilog-mode-hook ; pip install --user hdl-checker
							  ;; TODO: When lsp-haskell and lsp-racket works,add 'em
							  'sh-mode-hook
							  'xml-mode-hook
							  'yaml-mode-hook
							  'php-mode-hook
							  'clojure-mode-hook
							  'c-mode-hook
							  'cc-mode-hook
							  'c++-mode-hook
							  'js-mode-hook
							  'web-mode-hook
							  ;; 'html-mode-hook ; there's builtin snippet completion
							  'typescript-mode-hook
							  'css-mode-hook
							  'go-mode-hook
							  'latex-mode-hook
							  'rust-mode-hook)
  "Enable `lsp' in these modes.")

(dolist (hook my-lsp-enable-hook)
  (add-hook hook 'lsp)) ; lsp-deferred = lsp
(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)

(add-hook 'java-mode-hook #'(lambda ()
							  ;; set up lsp-java
							  (require 'lsp-java)
							  (setq lsp-java--download-root "https://gitee.com/passkyw/config/raw/master/install/"
									;; lsp-java-workspace-dir (expand-file-name "workspace" my-ignore-directory)
									;; dap-java-test-runner (expand-file-name "eclipse.jdt.ls/test-runner/junit-platform-console-standalone.jar" my-ignore-directory)
									lsp-java-jdt-download-url "http://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")
							  (setq lsp-java-completion-overwrite nil
									lsp-java-completion-max-results 100
									lsp-java-progress-reports-enabled nil
									lsp-java-selection-range-enabled nil
									;; lsp-java-server-launch-mode 'LightWeight ; default hybrid
									)
							  (lsp-deferred)))

(add-hook 'python-mode-hook #'(lambda ()
								(require 'lsp-pyright)
								(setq lsp-pyright-auto-import-completions t
									  lsp-pyright-use-library-code-for-types t
									  lsp-pyright-auto-search-paths t)
								(when (executable-find "python3")
								  (setq lsp-pyright-python-executable-cmd "python3"))
								(lsp-deferred)
								(general-define-key
								 :keymaps 'python-mode-map
								 [remap lsp-format-buffer] 'yapfify-buffer
								 [remap lsp-format-region] 'yapfify-region)
								))

(with-eval-after-load 'lsp-mode
  (general-define-key
   :keymaps 'lsp-mode-map
   [remap xref-find-references] 'lsp-ui-peek-find-references
   [remap evil-lookup] 'lsp-ui-doc-show
   )

  ;; {{ HACK: let capf work with files within lsp
  ;; copy from doom-emacs
  (defvar my-lsp-backends
	'((company-files company-capf :with company-dabbrev)
	  (company-keywords))
	"Backends for `lsp-mode' completion.")

  ;; NOTE: I think I have fixed this by change the value of `lsp-completion-provider'
  ;; (add-hook 'lsp-completion-mode-hook
  ;; 			(defun lsp-init-company-backends-h ()
  ;; 			  (when lsp-completion-mode
  ;; 				(set (make-local-variable 'company-backends)
  ;; 					 my-lsp-backends))))

  ;; {{ python
  (when *win64*
	;; NOTE: pyls requires this
	(setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))

  ;; {{ rust

  ;; {{ C/Cpp
  (defconst ccls-args nil)
  (if *win64*
	  (defconst clangd-args '("-j=2"
							  "--background-index"
							  "--clang-tidy"
							  "--cross-file-rename"
							  "--completion-style=bundled"
							  "--target=x86_64-w64-mingw"
							  "--pch-storage=memory"
							  "--header-insertion-decorators=0"))
	(defconst clangd-args '("-j=2"
							"--background-index"
							"--clang-tidy"
							"--cross-file-rename"
							"--completion-style=bundled"
							"--pch-storage=memory"
							"--header-insertion-decorators=0")))
  

  ;; Prefer `clangd' over `ccls'
  (cond ((executable-find "clangd") (setq lsp-clients-clangd-executable "clangd"
										  lsp-clients-clangd-args clangd-args))
		((executable-find "ccls") (setq lsp-clients-clangd-executable "ccls"
										lsp-clients-clangd-args ccls-args)))

  (setq lsp-keymap-prefix "C-c l"

		lsp-keep-workspace-alive nil ; auto kill lang server
		lsp-restart 'auto-restart ; default interactive is TOO annoying!
		lsp-response-timeout 30  ; wait more from server's response,ts-server is too slow so we just wait it longer.
		lsp-auto-configure t
		lsp-auto-guess-root t  ; auto guess root

		;; performance
		lsp-enable-symbol-highlighting t
		lsp-enable-folding nil
		lsp-enable-text-document-color nil
		lsp-enable-file-watchers nil
		lsp-enable-semantic-highlighting nil
		lsp-lens-enable nil ; actionable contextual information interspersed
		lsp-enable-imenu t
		lsp-idle-delay 0.5 ; lazy refresh


		lsp-completion-provider :none ; avoid lsp forcely change `company-backends'
		lsp-diagnostic-package :flymake ; prefer flymake
		lsp-enable-snippet t ; causes issue in parens
		lsp-eldoc-enable-hover t ; eldoc hover
		lsp-signature-auto-activate t     ; show function signature
		lsp-headerline-breadcrumb-enable nil ; headerline,conflicts with hack in org
		lsp-headerline-breadcrumb-enable-diagnostics nil
		lsp-modeline-code-actions-enable t

		lsp-print-performance nil
		lsp-log-io nil                    ; enable log only for debug
		lsp-enable-indentation nil
		lsp-enable-on-type-formatting nil))
;; }}

;;{{ lsp-ui
(my-add-package 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-enable t
		lsp-ui-doc-max-height 20
		lsp-ui-doc-max-width 60
		lsp-ui-doc-delay 3 ; I use `K'
		lsp-ui-doc-include-signature t
		lsp-ui-doc-use-webkit nil
		lsp-ui-doc-border (face-foreground 'default)
		lsp-ui-doc-position 'at-point

		lsp-ui-sideline-enable nil
		lsp-ui-sideline-show-hover nil
		lsp-ui-sideline-show-diagnostics nil
		lsp-ui-sideline-show-code-actions t
		lsp-ui-sideline-ignore-duplicate t

		lsp-ui-imenu-enable t
		lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
							  ,(face-foreground 'font-lock-string-face)
							  ,(face-foreground 'font-lock-constant-face)
							  ,(face-foreground 'font-lock-variable-name-face)))
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  )

;; dap-mode for debug
;; pip install ptvsd pytest
(my-add-package 'dap-mode)
(when (executable-find "node")
  (with-eval-after-load 'dap-mode
	(when (executable-find "python3")
	  (setq dap-python-executable "python3"))
	(dap-auto-configure-mode)

	(add-hook 'python-mode-hook #'(lambda () (require 'dap-python)))
	(add-hook 'ruby-mode-hook #'(lambda () (require 'dap-ruby)))
	;; (go-mode (lambda () (require 'dap-go)))
	(add-hook 'java-mode-hook #'(lambda () (require 'dap-java)))
	(add-hook 'cc-mode-hook #'(lambda () (require 'dap-lldb)))
	(add-hook 'js-mode-hook #'(lambda () (require 'dap-firefox)))
	;; template for python project
	(dap-register-debug-template "My App"
								 (list :type "python"
									   :args "-i"
									   :cwd nil
									   :env '(("DEBUG" . "1"))
									   :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
									   :request "launch"
									   :name "My App"))
	)
  )
;;}}
(provide 'init-lsp-mode)
;;; init-lsp-mode.el ends here.
