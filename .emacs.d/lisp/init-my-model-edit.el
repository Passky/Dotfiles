;;; This is copy from meow and litte rewrite
;;; See it at https://github.com/DogLooksGood/meow

;;; Commentary:
;; I don't need much history items.
;; Also I'd like it to be a evil like but more lightweight model edit.
(require 'cl-lib)

(defvar-local meow--space-command nil
  "Current command on SPC in special mode buffer.")


(defvar-local meow--keymap-loaded nil
  "If keymap is loaded in this buffer.")

(defvar meow-keymap
  (let ((keymap (make-sparse-keymap)))
	keymap)
  "Global keymap for Meow.")

(defvar meow-insert-state-keymap
  (let ((keymap (make-keymap)))
	(define-key keymap (kbd "<escape>") 'meow-insert-exit)
	(define-key keymap (kbd "ESC") 'meow-insert-exit)
	keymap)
  "Keymap for Meow insert state.")

(defvar meow-normal-state-keymap
  (let ((keymap (make-keymap)))
	keymap)
  "Keymap for Meow normal state.")

(defvar meow-motion-state-keymap
  (let ((keymap (make-keymap)))
	keymap)
  "Keymap for Meow motion state.")

(defvar meow-normal-state-mode-list
  '(fundamental-mode
	text-mode
	prog-mode
	conf-mode
	cider-repl-mode
	eshell-mode
	vterm-mode
	json-mode
	wdired-mode
	deft-mode
	pass-view-mode
	telega-chat-mode
	restclient-mode)
  "A list of modes should enable normal state.")

(defvar meow-switch-state-hook nil
  "Hooks run when switching state.")

(defun meow-insert-mode-p ()
  "If insert mode is enabled."
  (bound-and-true-p meow-insert-mode))

(defun meow-motion-mode-p ()
  "If motion mode is enabled."
  (bound-and-true-p meow-motion-mode))

(defun meow-normal-mode-p ()
  "If normal mode is enabled."
  (bound-and-true-p meow-normal-mode))

(defun meow-insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-insert-mode-p)
	(when overwrite-mode
	  (overwrite-mode -1))
	(meow--switch-state 'normal))))


(defun meow--switch-state (state)
  "Switch to STATE."
  (cl-case state
	('insert
	 (meow-insert-mode 1))
	('normal
	 (meow-normal-mode 1))
	('motion
	 (meow-motion-mode 1)))
  (run-hook-with-args 'meow-switch-state-hook state))

;;;###autoload
(define-minor-mode meow-insert-mode
  "Meow Insert state."
  :init-value nil
  :lighter " [I]"
  :keymap meow-insert-state-keymap
  (meow--insert-init))

;;; {{ Handle state
;;;###autoload
(define-minor-mode meow-normal-mode
  "Meow Normal state."
  :init-value nil
  :lighter " [N]"
  :keymap meow-normal-state-keymap
  (meow--normal-init))

;;;###autoload
(define-minor-mode meow-motion-mode
  "Meow motion state."
  :init-value nil
  :lighter " [M]"
  :keymap meow-motion-state-keymap
  (meow--motion-init))

;;;###autoload
(define-minor-mode meow-mode
  "Meow minor mode.
This minor mode is used by global-meow-mode, should not be enabled directly."
  :init-value nil
  :lighter nil
  :keymap meow-keymap
  (if meow-mode
	  (meow--enable)
	(meow--disable)))


(defun meow-insert ()
  "Move to the begin of selection, switch to INSERT state."
  (interactive)
  (meow--direction-backward)
  (meow--switch-state 'insert))

(defun meow-append ()
  "Move to the end of selection, switch to INSERT state."
  (interactive)
  (meow--direction-forward)
  (meow--switch-state 'insert))

(defun meow-open ()
  "Open a newline below and switch to INSERT state."
  (interactive)
  (goto-char (line-end-position))
  (newline-and-indent)
  (meow--switch-state 'insert))

(define-global-minor-mode global-meow-mode meow-mode
  (lambda ()
	(unless (minibufferp)
	  (meow-mode 1)))
  (if meow-mode
	  (meow--global-enable)
	(meow--global-disable)))

(defun meow-indicator ()
  "Indicator show current mode."
  (interactive)
  (when (bound-and-true-p global-meow-mode)
	(cond
	 (meow-normal-mode
	  (propertize
	   (concat
		" NORMAL "))
	  'face 'meow-normal-indicator)
	 (meow-motion-mode
	  (propertize " MOTION " 'face 'meow-motion-indicator))
	 (meow-insert-mode
	  (cond
	   ;; Vterm's vterm-mode is read-only.
	   ((and buffer-read-only (not (equal major-mode 'vterm-mode)))
		(propertize " READONLY " 'face 'meow-insert-indicator))
	   ((bound-and-true-p overwrite-mode)
		(propertize " OVERWRITE " 'face 'meow-insert-indicator))
	   (t (propertize " INSERT " 'face 'meow-insert-indicator))))
	 (t
	  ""))
	))

(defun meow--normal-init ()
  "Init normal state."
  (when meow-normal-mode
	(meow-insert-mode -1)
	(meow-motion-mode -1)
	(unless meow--keymap-loaded
	  (setq-local meow--keymap-loaded t))))

(defun meow--insert-init ()
  "Init insert state."
  (when meow-insert-mode
	(meow-normal-mode -1)
	(meow-motion-mode -1)))

(defun meow--motion-init ()
  "Init motion state."
  (when meow-motion-mode
	(meow-normal-mode -1)
	(meow-insert-mode -1)
	(unless meow--keymap-loaded
	  (setq meow--keymap-loaded t))))

(defun meow--enable ()
  "Enable Meow.
We will save command on SPC to variable `meow--space-command'
before activate any state.
then SPC will be bound to LEADER."
  (unless meow--space-command
	(let ((cmd (key-binding (read-kbd-macro "SPC"))))
	  (when (and (commandp cmd)
				 (not (equal cmd 'undefined)))
		(setq-local meow--space-command cmd))))
  (if (apply #'derived-mode-p meow-normal-state-mode-list)
	  (meow--switch-state 'normal)
	(meow--switch-state 'motion)))

(defun meow--disable ()
  "Disable Meow."
  (meow-normal-mode -1)
  (meow-insert-mode -1)
  (meow-motion-mode -1))

(defun meow--global-enable ()
  "Enable meow globally."
  (global-set-key (kbd "<escape>") 'meow-escape-or-normal-modal)
  (global-set-key (kbd "ESC") 'meow-escape-or-normal-modal)
  (setq delete-active-region nil)
  )

(defun meow--global-disable ()
  "Disable Meow globally."
  (global-unset-key (kbd "<escape>"))
  (global-unset-key (kbd "ESC"))
  )

(defun meow-escape-or-normal-modal ()
  "Keyboard escape quit or switch to normal state."
  (interactive)
  (cond
   ((minibufferp)
	(if (fboundp 'minibuffer-keyboard-quit)
		(call-interactively #'minibuffer-keyboard-quit)
	  (call-interactively #'abort-recursive-edit)))
   ((meow-insert-mode-p)
	(when overwrite-mode
	  (overwrite-mode -1))
	(meow--switch-state 'normal))
   ((eq major-mode 'fundamental-mode)
	(meow--switch-state 'normal))))
;;; }}

(defun meow-insert-in ()
  (interactive)
  (meow--switch-state 'insert)
  )

(defun meow-string-replace ()
  (interactive)
  (if (region-active-p)
	  (call-interactively #'r)
	(call-interactively #'set-mark-command)))


;; {{ keymaps configration
(my-key-def-preset :meow-normal :keymaps 'meow-normal-state-keymap)
(my-key-def-preset :meow-insert :keymaps 'meow-insert-state-keymap)
(my-key-def-preset :meow-action
  :prefix "\:"
  :keymaps 'meow-insert-state-keymap)
(my-def-key
 :keymaps 'override
 "M-x" 'execute-extended-command
 :meow-normal
 "gd" 'my-jump-to-tag
 "gD" 'xref-find-definitions-other-window
 "gr" 'xref-find-references
 "[escape]" 'keyboard-quit
 "<esc>" 'keyboard-quit
 "C-o" 'previous-buffer
 "C-i" 'next-buffer
 "C-d" 'scroll-up ; it looks strange, but in emacs it exactly is.
 "C-u" 'scroll-down

 "p" 'yank
 "y" 'kill-ring-save
 "u" 'undo
 "C-r" 'undo-redo

 "a" 'beginning-of-line
 "e" 'end-of-line
 "w" 'forward-word
 "b" 'backward-word
 "h" 'backward-char
 "l" 'forward-char
 "j" 'next-line
 "k" 'previous-line


 "d" 'my-delete-selected-region
 "x" 'delete-char

 "v" 'set-mark-command
 "i" 'meow-insert-in
 "/" 'isearch-forward
 "?" 'isearch-backward
 :meow-insert
 "[escape]" 'meow-escape-or-normal-modal
 "ESC" 'meow-escape-or-normal-modal
 :meow-action
 "vsp" 'my-vsplit-window
 "sp" 'my-split-window
 )

(my-key-def-preset
	:meow-space-leader
  :prefix "SPC"
  :keymaps 'meow-normal-state-keymap)

(my-key-def-preset
	:meow-backslash-leader
  :prefix "\\"
  :keymaps 'meow-normal-state-keymap)

(my-def-key
 :meow-space-leader
 ;;{{ window move
 "jj" 'scroll-other-window
 "kk" 'scroll-other-window-up
 "wh" 'windmove-left
 "wj" 'windmove-down
 "wk" 'windmove-up
 "wl" 'windmove-right
 "wo" 'delete-other-windows
 "ws" 'window-swap-states
 "wu" 'winner-undo
 "wr" 'winner-redo
 "wc" 'delete-window
 "wt" 'my-trans-window-split
										; "wb" ' ; vertical split window
 ;;{{ project
 "ps" 'project-async-shell-command
 "pt" 'project-eshell
 "pm" 'project-compile
 "pg" 'project-vc-dir
 "pp" 'project-switch-project
 ;;{{ Find && Grep
 ;; "[" 'flycheck-previous-error
 ;; "]" 'flycheck-next-error
 "[" 'flymake-goto-prev-error
 "]" 'flymake-goto-next-error
 "{" 'evil-prev-flyspell-error
 "}" 'evil-next-flyspell-error
 "fd" 'counsel-dired-jump
 "fD" 'counsel-dired
 "ff" 'counsel-find-file
 "fF" 'find-file-other-window
 "fp" 'project-find-file
 "fP" 'my-find-file-in-current-directory
 "fg" 'my-counsel-etags-grep
 "fG" 'my-counsel-etags-grep-at-current-directory ; grep current directory
 "fm" 'recentf-open-files
 "fl" 'counsel-grep-or-swiper
 "fL" 'swiper-all
 "fb" 'counsel-ibuffer
 "fB" 'counsel-switch-buffer-other-window
 "fs" 'counsel-semantic-or-imenu
 "fw" 'ace-window
 ;;{{ Tab
 "tt" 'tab-bar-new-tab
 "tn" 'tab-bar-switch-to-next-tab
 "tp" 'tab-bar-switch-to-prev-tab
 "ts" 'tab-bar-switch-to-tab
 "tc" 'tab-bar-close-tab
 "tk" 'tab-bar-close-tab
 "tC" 'tab-bar-close-tab-by-name
 "to" 'tab-bar-close-other-tabs
 ;;{{ Code Action
 "lf" 'lsp-execute-code-action
 "ls" 'yas-insert-snippet
 "lm" 'lsp-format-buffer
 "lr" 'lsp-rename
 "ld" 'lsp-describe-thing-at-point
 "lle" 'lsp-treemacs-errors-list
 ;;{{ Git use vc-git, not magit
 ;; vc-log-outgoing vc-log-comming
 "gll" 'vc-print-log ; git log
 "glr" 'vc-print-root-log ; git root log
 "glc" 'counsel-git-log ; git log counsel
 "glu" 'vc-update-change-log ; git update changelog
 "gdd" 'vc-diff ; git diff
 "gdm" 'vc-resolve-conflicts ; use ediff to resolve conflicts
 "gpl" 'vc-update ; git pull
 "gpp" 'vc-push ; git push
 "gm" 'vc-merge ; git merge
 "gh" 'vc-insert-headers ; insert head into file
 "gcc" 'vc-checkout ; git checkout
 "gcu" 'vc-revert ; undo checkout
 "gsd" 'vc-dir ; show all files which are not up to date
 "gsa" 'vc-annotate ; show when each line in a tracked file was added and by whom
 "gbc" 'vc-revision-other-window ; change branch with info
 "gbb" 'vc-create-tag ;; ; create a new branch
 "gbs" 'vc-retrieve-tag ; switch to a branch
 "gn" 'vc-next-action ; perform the next logical control operation on file
 "ga" 'vc-register ; add a new file to version control
 ;;{{ counsel
 "cp" 'counsel-yank-pop
 "ca" 'counsel-linux-app
 "cgl" 'counsel-git-log
 "cgb" 'counsel-git-change-worktree
 "cgc" 'counsel-git-checkout
 "cgs" 'counsel-git-stash
 ;;{{ Gdb
 "dw" 'gud-watch
 "dr" 'gud-remove
 "db" 'gud-break
 "du" 'gud-run
 "dp" 'gud-print
 "dn" 'gud-next
 "ds" 'gud-step
 "di" 'gud-stepi
 "dc" 'gud-cont
 "df" 'gud-finish
 ;;{{ QuickAction
 "qQ" 'quickrun
 "qq" 'quickrun-shell
 "qd" 'bing-dict-brief
 "qt" 'my-eshell-pop
 "qs" 'async-shell-command
 "qk" 'which-key-show-top-level ; show all keymap
 ;;{{ iinteresting builtin package
 "sss" 'my-sudo-save
 "ssc" 'flyspell-buffer
 "sm" 'proced ; quick monitor
 "swb" 'browse-url
 "scm" 'align
 "swr" 'shr-render-region ; simple html renderer
 "srr" 're-builder ; build regexp with feedback
 "st" 'my-eshell-pop
 ;;{{ buffer actions
 "bc" 'kill-current-buffer
 "bk" 'buf-move-up
 "bj" 'buf-move-down
 "bh" 'buf-move-left
 "bl" 'buf-move-right
 "bb" 'counsel-ibuffer
 "bo" 'counsel-switch-buffer-other-window
 "bd" 'kill-buffer
 ;;{{ ace-jump
 "ac" 'avy-goto-char-2-below
 "aC" 'avy-goto-char-2-above
 "aw" 'avy-goto-word-1-below
 "aW" 'avy-goto-word-1-above
 "al" 'avy-goto-line-below
 "aL" 'avy-goto-line-above
 ;;{{ Help yourself
 "hf" 'describe-function
 "hv" 'describe-variable
 "hk" 'describe-key
 ;;{{ org
 "ot" 'org-todo
 "od" 'org-deadline
 "or" 'org-refile
 "oh" 'org-backward-heading-same-level
 "oj" 'org-next-visible-heading
 "ok" 'org-previous-visible-heading
 "ol" 'org-forward-heading-same-level
 "ost" 'org-sparse-tree
 "osc" 'org-set-tags-command
 "osd" 'org-schedule
 "oc" 'org-ctrl-c-ctrl-c
 "o," 'org-priority
 "o." 'org-time-stamp
 :meow-backslash-leader
 ;;{{ file-action
 "frd" 'my-remove-dos-eol
 "fd" 'my-delete-this-file
 "frf" 'my-rename-this-file-and-buffer
 "ss" 'my-reload-emacs
  ;;;{{ tools
 "y" 'my-system-copy
 "Y" 'my-kill-ring-to-clipboard
 "p" 'my-system-paste
 "P" 'my-clipboard-to-kill-ring
 "q" 'my-force-quit-without-asking
 "ef" 'eval-defun
 "er" 'eval-region ;; eval text selection
 "eb" 'eval-buffer ;; eval buffer
 "ee" 'eval-expression ;;
 )
;; }}

;; (global-meow-mode)

(provide 'init-my-model-edit)
;;; init-my-model-edit.el ends here
