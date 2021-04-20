;;; package --- Summary -*- lexical-binding: t; -*-
;;; code:
;;; Commentary:
(my-add-package 'eshell-z)
(my-add-package 'exec-path-from-shell)
(my-add-package 'load-bash-alias)
(add-hook 'eshell-mode-hook
		  (lambda ()
			(evil-collection-define-key 'insert 'eshell-mode-map
			  (kbd "C-a") 'my-eshell-maybe-bol
			  (kbd "C-w") 'my-kill-word-backward
			  (kbd "C-d") 'my-eshell-quit-or-delete-char
			  (kbd "C-p") 'eshell-previous-input
			  (kbd "C-n") 'eshell-next-input
			  (kbd "C-r") 'my-esh-history)
			;; eshell-z
			(require 'eshell-z)
			))

(setq-default eshell-history-size 1000000) ; history size

;; copy from https://github.com/stanhe/pop-eshell
(defvar my-eshell "*my-eshell*")

(defun my-eshell-pop ()
  "Pop or hide bottom eshell side window."
  (interactive)
  (if (get-buffer-window my-eshell)
	  (delete-windows-on my-eshell)
	    (my-eshell-pop-create)))

(defun my-eshell-pop-create()
  "Pop eshell at bottom."
  (let* ((pos-buffer (current-buffer))
		 (tmp-eshell (get-buffer my-eshell))
		 (dir (my-get-current-directory))
		 (pre-path nil)
		 (window nil)
		 )
	;; check if my-eshell exist,if not create one.
	(unless tmp-eshell
	  (setq tmp-eshell (eshell 100))
	  (with-current-buffer tmp-eshell
		(eshell/clear-scrollback)
		(rename-buffer my-eshell)
		(switch-to-buffer pos-buffer)))
	(setq window
		  (select-window
		   (display-buffer-in-side-window tmp-eshell '((side . bottom))) t))
	(set-window-dedicated-p window t)
	(when (not (equal pre-path dir))
	  (eshell/cd dir)
	  (eshell-send-input)
	  (setq pre-path dir))))

(with-eval-after-load 'eshell
  (defun eshell-prompt ()
    "Prompt for eshell."
    (concat
     (propertize user-login-name 'face 'font-lock-keyword-face)
     "@"
     "tmsksuki"
	 " "
     (if (equal (eshell/pwd) "~")
         "~"
       (abbreviate-file-name (my-fish-path (eshell/pwd) 25)))
     " "
     (if-let* ((vc (ignore-errors (vc-responsible-backend default-directory)))
               (br (car (vc-git-branches))))
         (concat (propertize "(" 'face 'success)
                 (format "%s" vc)
                 (propertize ")" 'face 'success)
                 (propertize "-" 'face 'font-lock-string-face)
                 (propertize "[" 'face 'success)
                 (propertize br 'face 'font-lock-constant-face)
                 (propertize "]" 'face 'success)
                 " ")
       "")
     "% "))

  (unless *win64* ; windows's shell is bad
	(setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx" "bash" "zsh"
								   "ncftp" "pine" "tin" "trn" "elm" "vim"
								   "nmtui" "alsamixer" "htop" "el" "elinks")
		  eshell-visual-subcommands '(("git" "log" "diff" "show" "push" "pull" "clone"))))
  (setq eshell-modules-list (push 'eshell-tramp eshell-modules-list)
		
		eshell-destroy-buffer-when-process-dies t ; auto destroy visual trem buffer
		eshell-scroll-to-bottom-on-input 'nil
		eshell-scroll-to-bottom-on-output 'nil
		eshell-kill-on-exit nil
		eshell-kill-processes-on-exit nil
		;; Don't record command in history if starts with whitespace
		eshell-input-filter 'eshell-input-filter-initial-space
		eshell-error-if-no-glob t
		eshell-hist-ignoredups t ; ignore repeat
		eshell-glob-case-insensitive t
		eshell-highlight-prompt t ; apart from highlight,it also make prompt read-only
		eshell-cmpl-ignore-case t ; ignore case when cmpl filename

		eshell-prompt-function 'eshell-prompt
		eshell-prompt-regexp "^[^@]+@[^ ]+ [^ ]+ \\(([a-zA-Z]+)-\\[[a-zA-Z]+\\] \\)?% "
		)

  ;; Copy from doom
  (defun my-eshell-quit-or-delete-char (arg)
	"Delete a character or quit eshell if there's nothing to delete."
	(interactive "p")
	(if (and (eolp) (looking-back eshell-prompt-regexp nil))
		(eshell-life-is-too-much)
	  (delete-char arg)))

  (defun my-eshell-maybe-bol ()
	"Go to the beginning of command line,or begining of line."
	(interactive)
	(let ((p (point)))
	  (eshell-bol)
	  (if (= p (point))
		  (beginning-of-line))))

  (defun eshell/unpack (file &rest args)
	"Unpack FILE with ARGS using default command."
	(let ((command (cl-some (lambda (x)
							  (if (string-match-p (car x) file)
								  (cadr x)))
							'((".*\.tar.bz2" "tar xjf")
							  (".*\.tar.gz" "tar xzf")
							  (".*\.bz2" "bunzip2")
							  (".*\.rar" "unrar x")
							  (".*\.gz" "gunzip")
							  (".*\.tar" "tar xf")
							  (".*\.tbz2" "tar xjf")
							  (".*\.tgz" "tar xzf")
							  (".*\.zip" "unzip")
							  (".*\.Z" "uncompress")
							  (".*" "echo 'Could not unpack the file:'")))))
	  (let ((unpack-command (concat command " " file " " (mapconcat 'identity args " "))))
		(eshell/printnl "Unpack command: " unpack-command)
		(eshell-command-result unpack-command))
	  ))


  (defun my-kill-word-backward ()
	"Let Eshell kill word acting like zsh."
	(interactive)
	(set-mark-command nil)
	(backward-word)
	(call-interactively 'kill-region))

  ;; Inspire by http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  (defun my-parse-bash-history ()
	"Parse the bash history."
	(interactive)
	(when (file-exists-p "~/.bash_history")
	  (let (collection bash_history)
		;; (shell-command "history -r") ; reload history
		(setq collection
			  (nreverse
			   (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
											   (buffer-string))
							 "\n"
							 t)))
		(when (and collection (> (length collection) 0)
				   (setq bash_history collection))
		  bash_history))))

  (defun my-parse-zsh-history ()
	"Parse the bash history."
	(interactive)
	(when (file-exists-p "~/.zsh_history")
	  (let (collection zsh_history)
		(setq collection
			  (nreverse
			   (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
											   (replace-regexp-in-string "^:[^;].*;" "" (buffer-string)))
							 "\n"
							 t)))
		(when (and collection (> (length collection) 0)
				   (setq zsh_history collection))
		  zsh_history))))

  (defun my-esh-history ()
	"Interactive search eshell history."
	(interactive)
	(my-ensure 'em-hist)
	(save-excursion
	  (let* ((start-pos (eshell-beginning-of-input))
			 (input (eshell-get-old-input))
			 (esh-history (when (> (ring-size eshell-history-ring) 0)
							(ring-elements eshell-history-ring)))
			 (all-shell-history (append esh-history (my-parse-zsh-history) (my-parse-bash-history))))
		(eshell-kill-input)
		(let* ((command (my-completing-read "Command: "
											(delete-dups all-shell-history)
											:require-match t
											:action 'insert
											)))
		  )))
	;; move cursor to eol
	(end-of-line))

  ;; exec-path-from-shell
  (when *darwin* ; we need this only in `*darwin*'
	(setq exec-path-from-shell-variables '("JAVA_HOME"
										   "PATH"
										   "WORKON_HOME"
										   "MANPATH"
										   "LANG"))
	(exec-path-from-shell-initialize))

  ;; load alias from bash
  (autoload 'load-bash-alias-into-eshell "load-bash-alias" "" t)

  (with-eval-after-load 'load-bash-alias
	(setq load-bash-alias-bashrc-file "~/.shellrc")
	(setq load-bash-alias-exclude-aliases-regexp "^alias magit\\|^alias oc"))
  )

;;; term-mode and ansi-term and shell-mode
(my-key-def-preset :term-raw-map :keymaps 'term-raw-map) ; char-mode
(my-key-def-preset :term-mode-map :keymaps 'term-raw-map) ; line-mode
(my-key-def-preset :shell-mode-map :keymaps 'term-raw-map)
(my-def-key
 :term-raw-map
 "M-x" nil ; never bind my M-x
 )

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook
		  (lambda ()
			(ansi-color-for-comint-mode-on)
			(evil-collection-define-key 'insert 'shell-mode-map (kbd "C-r") 'my-esh-history)))

(with-eval-after-load 'comint
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(provide 'init-shell)
;;; init-shell.el ends here.
