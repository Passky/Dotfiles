;;{{ Make,compile,run
(with-eval-after-load 'compile
  (setq compilation-ask-about-save nil         ; do not ask me to save buffer
		compilation-auto-jump-to-first-error nil ; no auto jump to error
		compilation-read-command nil ; no ask for command
		compilation-always-kill t    ; auto kill undone job
		compilation-disable-input nil
		compilation-scroll-output t)
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (defun colorize-compilation-buffer ()
	"ANSI coloring in compilation buffers."
	(when (eq major-mode 'compilation-mode)
	  (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (when (functionp 'notify-send)
	(add-to-list 'compilation-finish-functions 'notify-compilation-result)
	(defun notify-compilation-result (_comp-buffer exit-string)
	  "Notify after the compilation is done."
	  (if (string-match "^finished" exit-string)
		  (notify-send :title "Compilation"
					   :body "Compilation successful :D"
					   :timeout 5000
					   :urgency 'normal)
		(notify-send :title "Compilation"
					 :body "Compilation failed :("
					 :timeout 5000
					 :urgency 'critical)))))

;; {{ Quickrun
(my-add-package 'quickrun)
(setq-default quickrun--eshell-buffer-name "*my-eshell*")
(with-eval-after-load 'quickrun
  (setq quickrun-focus-p nil) ; not to focus
  (setq-default quickrun--eshell-map ; do not bind my key!
				(let ((map (make-sparse-keymap)))
				  (set-keymap-parent map eshell-mode-map)
				  (define-key map (kbd "C-c C-c") 'eshell-kill-process)
				  (define-key map (kbd "C-c C-k") 'eshell-kill-process)
				  map))
  (setq-local eshell-kill-on-exit t
			  eshell-kill-processes-on-exit t)

  (defun my-quickrun-hack (fnc &rest args)
	(save-buffer) ; always save buffer
	;; see https://github.com/emacsorphanage/quickrun/issues/108
	(let ((even-window-heights nil)) ; do not resize
	  (apply fnc args)))

  (advice-add 'quickrun :around #'my-quickrun-hack)

  ;; it will remove vlang support, but I don't really care.
  (push '("\\.v\\'" . "verilog") quickrun-file-alist)
  (quickrun-add-command "verilog"
	'((:command . "iverilog")
	  (:exec    . ("%c -o %e %s"
				   "%e %a"))
	  (:remove  . ("%e")))
	:mode 'verilog-mode))
;;}}

;; {{ Find-file-in-project
(my-add-package 'find-file-in-project)
(after! find-file-in-project
  (when (executable-find "fd")
	(setq ffip-use-rust-fd t))
  (when (executable-find "fdfind") ; apt/yum/dnf named fd strangely for historical reason
	(setq ffip-use-rust-fd t)
	(setq ffip-find-executable (executable-find "fdfind"))))

;; {{ Auto-save
(my-add-package 'super-save)
(my-delay-eval #'(lambda () (super-save-mode t)) 0.7)

(with-eval-after-load 'super-save
  (setq super-save-exclude '(".gpg"))
  ;; add integration with ace-window
  ;; (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  ;; (add-to-list 'super-save-hook-triggers 'find-file-hook) ; against lsp-mode,so close it.
  (setq super-save-auto-save-when-idle t
		super-save-remote-files nil))

;; {{ wgrep
(my-add-package 'wgrep) ; super weapon!
(autoload 'wgrep-change-to-wgrep-mode "wgrep" "" t)
(with-eval-after-load 'grep
  (define-key grep-mode-map
	(kbd "C-x C-o") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map
	(kbd "C-c C-c") 'wgrep-finish-edit))
(with-eval-after-load 'wgrep
  (setq wgrep-auto-save-buffer t
		wgrep-change-readonly-file t))

;; {{ Debugger
(add-hook 'gud-mode-hook 'gud-tooltip-mode)
(with-eval-after-load 'gud
  ;; Add color to the current GUD line
  ;; From https://kousik.blogspot.com/2005/10/highlight-current-line-in-gdbemacs.html
  (defconst gud-highlight-face 'secondary-selection)

  (defvar gud-overlay
	(let ((overlay (make-overlay (point) (point))))
	  (overlay-put overlay 'face gud-highlight-face)
	  overlay)
	"Overlay variable for GUD highlighting.")

  (define-advice gud-display-line (:after (true-file _line))
	"Highlight gud current line."
	(when-let* ((buffer (gud-find-file true-file)))
	  (with-current-buffer buffer
		(move-overlay gud-overlay (line-beginning-position) (line-end-position)
					  (current-buffer)))))

  (define-advice gud-kill-buffer-hook (:after nil)
	"Remove highlight overlay."
	(delete-overlay gud-overlay))

  (define-advice gud-sentinel (:after (_1 _2))
	"Remove highlight overlay when user quit gud."
	(delete-overlay gud-overlay)))

(with-eval-after-load 'gdb-mi
  (setq gdb-show-main t
		gdb-display-io-nopopup t
		gdb-show-changed-values t
		gdb-delete-out-of-scope t
		gdb-use-colon-colon-notation t
		gdb-restore-window-configuration-after-quit t))

;; {{ windows and jumper
;; winner-mode ; undo&redo for window manager
(my-add-package 'transient) ;; TODO: remove this after merged into master.
(add-hook 'after-init-hook 'winner-mode)
(setq winner-boring-buffers '("*Completions*"
							  "*Compile-Log*"
							  "*inferior-lisp*"
							  "*Fuzzy Completions*"
							  "*Apropos*"
							  "*Help*"
							  "*cvs*"
							  "*Buffer List*"
							  "*Ibuffer*"
							  "*esh command on file*"))
(after! winner
  (defun my-transient-winner-undo ()
	"Transient version of `winner-undo'."
	(interactive)
	(let ((echo-keystrokes nil))
	  (winner-undo)
	  (message "Winner: [u]ndo [r]edo")
	  (set-transient-map
	   (let ((map (make-sparse-keymap)))
		 (define-key map [?u] #'winner-undo)
		 (define-key map [?r] #'winner-redo)
		 map)
	   t)))

  (defun my-transient-winner-redo ()
	"Transient version of `winner-redo'."
	(interactive)
	(let ((echo-keystrokes nil))
	  (winner-redo)
	  (message "Winner: [u]ndo [r]edo")
	  (set-transient-map
	   (let ((map (make-sparse-keymap)))
		 (define-key map [?u] #'winner-undo)
		 (define-key map [?r] #'winner-redo)
		 map)
	   t))))

;; Jump to definition, used as a fallback of lsp-find-definition
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(with-eval-after-load 'dumb-jump
  (setq dumb-jump-quiet t
		dumb-jump-aggressive nil
		dumb-jump-selector 'completing-read))

(provide 'init-workflow)
;;; init-workflow.el ends here
