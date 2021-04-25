;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:

;; scroll smooth
(setq mouse-wheel-scroll-amount '(0.07))
(setq mouse-wheel-progressive-speed nil)

;; Smooth scroll & friends
(setq scroll-step 2
	  scroll-margin 3
	  hscroll-step 2
	  hscroll-margin 2
	  scroll-conservatively 101
	  scroll-up-aggressively 0.01
	  scroll-down-aggressively 0.01
	  scroll-preserve-screen-position 'always)
;;}}

;;{{ improve performance
;; Suppress GUI features and more
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-x-resources t
      inhibit-default-init t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Pixelwise resize
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; Improve display
(setq display-raw-bytes-as-hex t
      redisplay-skip-fontification-on-input t)

;; don't disturb while open a big file
(setq large-file-warning-threshold nil)

;; no bell-ring
(setq ring-bell-function 'ignore) 

;; No backup files
(setq make-backup-files nil
	  auto-save-silent t
      auto-save-default nil)

;; No lock files
(setq create-lockfiles nil)

;; Always load the newest file
(setq load-prefer-newer t)

;; No gc for font caches
(setq inhibit-compacting-font-caches t)

;; kill bufer without ask
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function 
										kill-buffer-query-functions))

;; improve performance for too too big file
(setq so-long-threshold 850) ; default threshold is too small
(add-hook 'after-init-hook 'global-so-long-mode)

;; improve performance for long-line
;; though correct bidi is good,but it has issues with performance.
(setq-default bidi-paragraph-direction 'left-to-right ; never care about bidirectional text
			  bidi-inhibit-bpa t) ; for long-line

;; IO related tuning
(setq process-adaptive-read-buffering nil)
;; }}

;;{{ Ignore read-only-error
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (when (eq buffer-read-only nil)
	(command-error-default-function data context caller)
	)
  (read-only-mode 0)
  ;; (message "You are editing Read_Only file!")
  )

(setq command-error-function #'my-command-error-function)

;; {{ Base configration
;; auto reload changed file
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq auto-revert-check-vc-info t
	  auto-revert-interval 3
	  auto-revert-avoid-polling t ; quite a trange doc string
	  auto-revert-verbose nil ; message
	  global-auto-revert-non-file-buffers t)

;; Disable auto vertical scroll for tall lines
(setq auto-window-vscroll nil)

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p) ; TODO: remove this in emacs28
(setq use-short-answers t) ; NOTE: emacs28 new variable
;; Inhibit switching out from `y-or-n-p' and `read-char-choice'
(setq y-or-n-p-use-read-key t
      read-char-choice-use-read-key t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode t) ; do not replace tab with space

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq-default tab-always-indent 'complete) ; self-insert

;; {{ and other
(after! comint
  ;; Make the prompt of "*Python*" buffer readonly
  (setq comint-prompt-read-only t
		comint-scroll-to-bottom-on-input 'all)
  )

;; https://github.com/casouri/lunarymacs/
;; So nice!
(defun luna-hungry-delete (&rest _)
  "Delete backwards and also take away white spaces around point."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end))
    (catch 'end
      (let ((p (point)) beg end line-count)
        (save-excursion
          (skip-chars-backward " \t\n")
          (setq beg (point))
          (goto-char p)
          (skip-chars-forward " \t\n")
          (setq end (point)))
        (setq line-count
              (cl-count ?\n (buffer-substring-no-properties beg end)))
        (if (or (eq beg end)
                (eq (ppss-depth (syntax-ppss)) 0)
                (save-excursion (skip-chars-backward " \t")
                                (not (eq (char-before) ?\n))))
            (backward-delete-char-untabify 1)
          (delete-region beg end)
          (cond ((eq (char-after) ?})
                 (insert "\n")
                 (indent-for-tab-command))
                ((eq (char-after) ?\))
                 nil)
                ((> line-count 1)
                 (insert "\n")
                 (indent-for-tab-command))
                (t (insert " "))))))))

(general-define-key
 [remap backward-delete-char-untabify] 'backward-delete-char
 [remap backward-delete-char-untabify] #'luna-hungry-delete
 [remap delete-indentation] #'luna-hungry-delete
 [remap delete-backward-char] #'luna-hungry-delete
 [remap c-electric-backspace] #'luna-hungry-delete)

(setq history-delete-duplicates t)
;; }}

;; {{ auto close pairs
(after! elec-pair
  (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit))

(my-delay-eval #'(lambda () (electric-pair-mode t)) 0.5)


;; {{ Tools
;; hack for gc
(defvar my-gc-cons-threshold
  100000006
  "100 mb")
(my-add-package 'gcmh)
(my-delay-eval #'(lambda ()
						 (setq read-process-output-max (* 3 1024 1024) ; Increase the amount of data which Emacs reads from the process to enhance lsp performance
							   gc-cons-percentage 0.1)
						 (gcmh-mode)
						 (setq gcmh-idle-delay 10
							   gcmh-high-cons-threshold my-gc-cons-threshold)) 3)

;; this delete buffers in buffer list which were unused for 3 days
(my-delay-eval #'(lambda ()
						(midnight-mode t)))

;; save cursor place
(save-place-mode 1)

;; save history over restart
(savehist-mode)

;; recent files
(my-delay-eval #'(lambda () (recentf-mode t)) 0.7)
(defun my-recentf ()
  "Open recentf by `completing-read'."
  (interactive)
  (unless recentf-mode
	(recentf-mode t))
  (my-completing-read "Recentf-files: " recentf-list :action 'find-file))

(defun my-recentf-the-other-window ()
  "Open recentf by `completing-read'."
  (interactive)
  (unless recentf-mode
	(recentf-mode t))
  (my-completing-read "Recentf-files: " recentf-list :action 'find-file-other-window))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-filename-handlers 'abbreviate-file-name) ; use non absolute path
  (setq recentf-keep '(file-remote-p file-readable-p)
		recentf-max-saved-items 2048
		recentf-exclude '("/tmp/"
						  "/ssh:"
						  "/sudo:"
						  "recentf$"
						  "company-statistics-cache\\.el$"
						  ;; ctags
						  "/TAGS$"
						  ;; global
						  "/GTAGS$"
						  "/GRAGS$"
						  "/GPATH$"
						  ;; binary
						  "\\.mkv$"
						  "\\.mp[34]$"
						  "\\.avi$"
						  "\\.wav$"
						  "\\.pdf$"
						  "\\.docx?$"
						  "\\.xlsx?$"
						  ;; sub-titles
						  "\\.sub$"
						  "\\.srt$"
						  "\\.ass$"
						  ".cache"
						  "cache"
						  "^/tmp/"
						  "/ssh:"
						  "/su\\(do\\)?:"
						  "^/usr/include/"
						  "COMMIT_EDITMSG\\'"
						  ;; "/home/[a-z]\+/\\.[a-df-z]" ; configuration file should not be excluded
						  )))
;; }}

;;{{ grep
(with-eval-after-load 'grep
  ;; eacl and other general grep (rgrep, grep ...) setup
  (dolist (v '("auto"
			   "target"
			   "node_modules"
			   "bower_components"
			   "*dist"
			   ".sass_cache"
			   ".cache"
			   ".npm"
			   "elpa"))
	(add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*.min.js"
			   "*.map"
			   "*.bundle.js"
			   "*.min.css"
			   "tags"
			   "TAGS"
			   "GTAGS"
			   "GRTAGS"
			   "GPATH"
			   "cscope.files"
			   "*.json"
			   "*.log"))
	(add-to-list 'grep-find-ignored-files v))
  )
;;}}

;; system monitor
(setq-default proced-auto-update-flag t ; auto-update
			  proced-auto-update-interval 2)
(with-eval-after-load 'proced
  )

;; browse url
(with-eval-after-load 'browse-url
  (setq browse-url-generic-program (or (executable-find "firefox")
									   (executable-find "chromium")
									   (executable-find "google-chrome-stable")
									   (executable-find "google-chrome")
									   (when (eq system-type 'darwin) "open")
									   (when (eq system-type 'gnu/linux) "xdg-open"))
		;; use `W' in dired to open file by extra program.
		browse-url-handlers '(("\\`file:" . browse-url-default-browser)))

  ;; wsl-specific setup
  (when *wsl*
	;; copy from https://hkvim.com/post/windows-setup
	;; really helpful!
	(defun wsl-browse-url-xdg-open (url &optional ignored)
	  (interactive (browse-url-interactive-arg "URL: "))
	  (shell-command-to-string (concat "explorer.exe " url)))
	(advice-add #'browse-url-xdg-open :override #'wsl-browse-url-xdg-open)
	))

;; side bar
(with-eval-after-load 'speedbar
  (setq speedbar-use-images nil
		speedbar-show-unknown-files t
		speedbar-indentation-width 2))

(provide 'init-base)
;;; base ends here
