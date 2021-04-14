;; {{ highlight
;; highlight TODO/FIXME/NOTE...
(my-add-package 'hl-todo)
(add-hook 'prog-mode-hook 'global-hl-todo-mode)
(with-eval-after-load 'hl-todo
  (setq hl-todo-highlight-punctuation ":"
		hl-todo-keyword-faces
		`(("TODO"       . ,(face-foreground 'warning))
		  ("FIXME"      . ,(face-foreground 'error))
		  ("WARNING"    . ,(face-foreground 'error))
		  ("HACK"       . ,(face-foreground 'font-lock-constant-face))
		  ("REVIEW"     . ,(face-foreground 'font-lock-keyword-face))
		  ("NOTE"       . ,(face-foreground 'success))
		  ("DEPRECATED" . ,(face-foreground 'font-lock-doc-face))
		  ("NEXT" . ,(face-foreground 'warning))
		  ("DONE" . ,(face-foreground 'success))
		  )))

;; Highlight the current line.
;; NOTE: `tty-defined-color-alist' won't be correct until `tty-setup-hook',but that won't get triggerred in gui emacs
(add-hook 'window-setup-hook #'(lambda ()
								 (when (or (>= (length tty-defined-color-alist) 256) *gui*)
								   (global-hl-line-mode)
								   )))

(with-eval-after-load 'hl-line
  ;; (set-face-background hl-line-face "grey10")
  ;; (custom-set-faces '(hl-line ((t (:underline nil)))))
  )
;; }}

;; {{ prettify
(defcustom my-prettify-symbols-alist
  '(("lambda" . ?λ)
	("<-" . ?←)
	("->" . ?→)
	("->>" . ?↠)
	("=>" . ?⇒)
	("map" . ?↦)
	("/=" . ?≠)
	("!=" . ?≠)
	("==" . ?≡)
	("<=" . ?≤)
	(">=" . ?≥)
	("=<<" . (?= (Br . Bl) ?≪))
	(">>=" . (?≫ (Br . Bl) ?=))
	("<=<" . ?↢)
	(">=>" . ?↣)
	("&&" . ?∧)
	("||" . ?∨)
	("not" . ?¬))
  "Alist of symbol prettifications.
Nil to use font supports ligatures."
  :group 'my
  :type '(alist :key-type string :value-type (choice character sexp)))

(add-hook 'prog-mode-hook #'(lambda ()
							  (setq-default prettify-symbols-alist my-prettify-symbols-alist)
							  (setq prettify-symbols-unprettify-at-point 'right-edge)
							  (prettify-symbols-mode t)
							  ))

;; (+measure-time
;;  (format-mode-line mode-line-format))

;;{{ linum-mode
;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(defvar my-linum-inhibit-modes
  '(eshell-mode
	shell-mode
	profiler-report-mode
	ffip-diff-mode
	dictionary-mode
	erc-mode
	dired-mode
	help-mode
	text-mode
	fundamental-mode
	jabber-roster-mode
	jabber-chat-mode
	inferior-js-mode
	inferior-python-mode
	ivy-occur-grep-mode ; better performance
	ivy-occur-mode ; better performance
	twittering-mode
	compilation-mode
	weibo-timeline-mode
	woman-mode
	Info-mode
	calc-mode
	calc-trail-mode
	comint-mode
	gnus-group-mode
	gud-mode
	org-mode
	vc-git-log-edit-mode
	log-edit-mode
	term-mode
	w3m-mode
	speedbar-mode
	gnus-summary-mode
	gnus-article-mode
	calendar-mode)
  "Major modes without line number.")

(defun display-line-numbers-mode-hook-setup ()
  "Disable line in big files and specific modes."
  (if (or (my-buffer-too-big-p) (memq major-mode my-linum-inhibit-modes)) 
	  (display-line-numbers-mode 0)
	(display-line-numbers-mode 1)
	))

(defun my-enable-linum ()
  "As its name."
  (interactive)
  (display-line-numbers-mode 1)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode-hook-setup))

(defun my-disable-linum ()
  "As its name."
  (interactive)
  (display-line-numbers-mode 0)
  (remove-hook 'prog-mode-hook 'display-line-numbers-mode-hook-setup))

;; (add-hook 'after-init-hook 'my-enable-linum)

;; {{ Page-reak-line && visual-line
(my-add-package 'page-break-lines)
(add-hook 'after-init-hook 'global-page-break-lines-mode)
(add-hook 'after-init-hook 'global-visual-line-mode)

;;{{ whitespace
;; (add-hook 'after-init-hook 'global-whitespace-mode)
(with-eval-after-load 'whitespace
  (remove-hook 'find-file-hook 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)
  (setq whitespace-auto-cleanup t
		whitespace-line-column 110
		whitespace-rescan-timer-time nil
		whitespace-silent t
		whitespace-style '(face space-before-tab lines-tail tabs spaces)
		))

;; {{ Tab-bar-mode
;; If the value is `1', then hide the tab bar when it has only one tab,
(after! tab-bar
  (setq-default tab-bar-show 1
				tab-bar-new-button-show nil
				tab-bar-close-button-show nil
				tab-bar-new-tab-choice "*scratch*"))

(defmacro my-make-select-tab (num)
  (let* ((func-name (intern (format "my-select-tab-%s" num))))
	`(defun ,func-name ()
	   (interactive)
	   (tab-bar-select-tab ,num))))

(dolist (i (range 1 10))
  (eval `(my-make-select-tab ,i)))

;; {{ Highlight matching paren
(my-delay-eval #'(lambda () (show-paren-mode t)) 1.5)
(with-eval-after-load 'paren
  (set-face-foreground 'show-paren-match "red")
  (set-face-bold-p 'show-paren-match t)
  (set-face-background 'show-paren-match nil)
  (set-face-underline 'show-paren-match t)
  (setq show-paren-delay 0.1
		show-paren-when-point-inside-paren t
		show-paren-when-point-in-periphery t))

;; highlight brackets according to their depth
(add-hook 'prog-mode 'rainbow-delimiters-mode)
(with-eval-after-load 'rainbow-delimiters
  (setq rainbow-delimiters-max-face-count 3))

;;{{ Theme && modeline
;; (my-add-package 'modus-themes)
(my-add-package 'gruvbox-theme)
(when t;; (not (bound-and-true-p *dump*))
  (setq modus-themes-bold-constructs t
		modus-themes-variable-pitch-ui t
		modus-themes-variable-pitch-headings t
		modus-themes-slanted-constructs nil
		;; modus-themes-mode-line '3d
		)
  (load-theme 'gruvbox-dark-hard t))

(defun my-disable-enabled-theme ()
  "Disable all enabled theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(my-add-package 'doom-modeline)
(after! doom-modeline
  (setq doom-modeline-height 12
		doom-modeline-icon nil
		doom-modeline-unicode-fallback t
		doom-modeline-mu4e nil
		doom-modeline-irc nil
		doom-modeline-gnus nil
		doom-modeline-github nil
		doom-modeline-persp-name nil
		doom-modeline-enable-word-count nil
		))

(if (not *win64*)
	(add-hook 'after-init-hook #'doom-modeline-mode)
  ;; Do not calling extra process,
  ;; which will cost much in windows
  (setq-default mode-line-format
				(list
				 ;; the buffer name; the file name as a tool tip
				 '(:eval evil-mode-line-tag)
				 '(:eval (propertize "%b " 'face nil 'help-echo (buffer-file-name)))
				 ;; line and column
				 "(" ;; '%02' to set to 2 chars at least; prevents flickering
				 "%02l" "," "%01c"
				 ") "
				 "["
				 ;; the current major mode for the buffer.
				 '(:eval mode-name)
				 " "
				 ;; buffer file encoding
				 '(:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
						   (if (memq (plist-get sys :category)
									 '(coding-category-undecided coding-category-utf-8))
							   "UTF-8"
							 (upcase (symbol-name (plist-get sys :name))))))
				 " "
				 ;; was this buffer modified since the last save?
				 '(:eval (if (buffer-modified-p)
							 (propertize "Moded"
										 'face nil
										 'help-echo "Buffer has been modified")
						   (propertize "Saved"
									   'face nil
									   'help-echo "Buffer has been saved")
						   ))

				 ;; is this buffer read-only?
				 '(:eval (when buffer-read-only
						   (concat ","  (propertize "RO" 'face nil 'help-echo "Buffer is read-only"))))
				 "] "
				 ;;global-mode-string, org-timer-set-timer in org-mode need this
				 '(:eval global-mode-string)
				 " "
				 ;; show process
				 '(:eval mode-line-process)
				 ;; Don't show `minor-mode'
				 ;; minor-mode-alist  ;; list of minor modes
				 )))


;; Highlight some operations
(my-add-package 'symbol-overlay)
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

(my-add-package 'volatile-highlights)
(add-hook 'after-init-hook 'volatile-highlights-mode)
(with-eval-after-load 'volatile-highlights
  (with-no-warnings
	(when (fboundp 'pulse-momentary-highlight-region)
	  (defun my-vhl-pulse (beg end &optional _buf face)
		"Pulse the changes."
		(pulse-momentary-highlight-region beg end face))
	  (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))


;; {{ which-key-mode
(my-add-package 'which-key)
(my-delay-eval #'(lambda () (which-key-mode t)) 0.3)
(with-eval-after-load 'which-key
  ;; (setq which-key-allow-imprecise-window-fit t) ; performance
  (setq which-key-separator ":"
		which-key-idle-delay 0.5
		which-key-add-column-padding 1))
;; }}

(provide 'init-ui)
;;; init-ui.el ends here
