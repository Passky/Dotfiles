;;; -*- coding: utf-8; lexical-binding: t; -*-

;; Without this comment emacs25 adds (package-initialize) here
;; (package-initialize)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message ";; Free as in Freedom.\n")

;; Set high tolerance for bad elisp
(setq max-lisp-eval-depth 10000) ;; default 500
(setq max-specpdl-size 50000) ;; default 1300
;; use lexical binding
(setq lexical-binding t)
(setq-default lexical-binding t)

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defconst *darwin* (eq system-type 'darwin)
  "Using OSX?")
(defconst *win64* (eq system-type 'windows-nt)
  "Using MS-DOS?")
(defconst *cygwin* (eq system-type 'cygwin)
  "Using cygwin?")
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux))
  "Using linux?")
(defconst *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix))
  "Using unix?")
(defconst *wsl* (and (executable-find "powershell.exe") *linux*)
  "Using windows's linux subsystem?")

(defconst *gui* (display-graphic-p)
  "Using gui?")

(defconst *emacs27* (<= emacs-major-version 27)
  "Using non master version?")

(defconst my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst my-site-lisp-dir (concat my-emacs-d "manual-package")
  "Directory of site-lisp.")

(defconst my-lisp-dir (concat my-emacs-d "lisp")
  "Directory of Lisp.")

(defun my-vc-merge-p ()
  "Use Emacs for git merge only?"
  (boundp 'startup-now))


(defun require-init (pkg &optional extern-path-to-dir)
  "Load `PKG' file from .emacs.d/lisp/pkg.el or optional EXTREN-PATH-TO-DIR/pkg.el."
  (condition-case err ; handle err,so little typo won't stop reading the set up
	  (if extern-path-to-dir ; if has optional args, read from extern-path
		  (load (file-truename (format "%s/%s" extern-path-to-dir pkg)) t t)
		(load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t))
	((debug error) (warn "Error loading %s: %s" pkg
						 (error-message-string err))))
  )

(defun local-require (pkg)
  "Load PKG from `my-site-lisp-dir'."
  (require-init pkg (format "%s/%s" my-site-lisp-dir pkg)))

;; add site-list dir to load-path
(defun my-add-subdirs-to-load-path (dir)
  "Recursive add DIR to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
	(add-to-list 'load-path dir)
	(normal-top-level-add-subdirs-to-load-path)))

;;{{
;; this is also set in early-init.el
(let ((file-name-handler-alist nil)) ;; speed up load-actions
  (my-add-subdirs-to-load-path my-site-lisp-dir)
  (setq my-setup-files (list
						'init-bootstrap ;; setup pakcage-repo and many package-about functions and basic packages
						'init-misc ; my misc
						'init-func ; my functions
						'init-mykey ;; replace global-set-key or define-key in long set
						'init-base ;; most builtin package and mode 
						(when *win64*
						  'init-windows)
						'init-evil ; evil
						;; 'init-my-model-edit ;
						'init-ffip
						'init-gui ; font set up
						'init-ui ; theme mode-line ...
						'init-search ; ido/icomplete/ivy and hydra/ivy
						'init-life ; less code,more life
						;; 'init-shell
						'init-clipboard ; load it after init-func!
						'init-gnus
						'init-completion ;company with ispell and yasnippet setup
						'init-workflow      ; git,debugger
						'init-prog ; programming language auto-mode and little lsp abooud
						'init-lisp     ; set up for lisp-family
						'init-lsp-mode ; set up lsp
						;; {{ Not for coding
						'init-vc   ; version control
						'init-lang ; language spicific confgration,including chinese/japanese...
						'init-doc ; markdown/org/pdf/bing-dict
						'init-eww
						;;}}
						))
  (mapc #'require-init my-setup-files)
  ;;}}

  (require-init '.custom.el "~/")
  ;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
  ;; See `custom-file' for details.
  (load (setq custom-file (expand-file-name (concat my-emacs-d "custom-set-variables.el"))) t t))

(defvar best-gc-cons-threshold
  16777216
  "Best default gc threshold value.  Should NOT be too big!")

;; reset gc threshold to better defaults after a timer
(my-delay-after-init #'(lambda ()
						(setq gc-cons-threshold best-gc-cons-threshold
							  read-process-output-max (* 3 1024 1024) ; Increase the amount of data which Emacs reads from the process to enhance lsp performance
							  gc-cons-percentage 0.1
							  )
						) 3)

(put 'erase-buffer 'disabled nil)



;; benchmark
(add-hook 'window-setup-hook #'(lambda () (setq my-init-time (float-time (time-subtract (current-time) before-init-time)))))

(defun my-init-time ()
  "Tell my startup time."
  (interactive)
  (message "%f" my-init-time))

;; window manager
;; (setq-default tlwm-default-bind t)
;; (local-require 'wm)
;; (global-tlwm-mode)

(provide 'init)
;;; init.el ends here.
