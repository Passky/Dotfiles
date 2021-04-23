;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:
(my-add-package 'evil)
(my-add-package 'avy)
(my-add-package 'evil-avy)
(my-add-package 'evil-mark-replace)
(my-add-package 'evil-matchit)
(my-add-package 'evil-collection)
(my-add-package 'evil-nerd-commenter)
(my-add-package 'evil-surround)
(my-add-package 'key-chord)
(my-add-package 'general)
(my-add-package 'ace-window) ; quick window move
(unless (functionp 'undo-redo)
  (my-add-package 'undo-fu))

;; use subWord as example
;; `w' will move cursor to sub|Word instead of subWord|
(global-subword-mode)

;; for evil-collection
(setq-default evil-want-keybinding nil) ; set it to nil before loading evil
;; use evil everywhere
(setq-default evil-overriding-maps nil ; do not override my-leader
			  evil-insert-state-modes '(eshell-mode) ; do not enter some mode with insert state
			  evil-motion-state-modes nil ; do not enter some mode with motion state
			  ) ;

(add-hook 'after-init-hook 'evil-mode)
;; {{ evil-self
(with-eval-after-load 'evil
  (if (not (functionp 'undo-redo))
	  (evil-set-undo-system 'undo-fu)
	(evil-set-undo-system 'undo-redo))
  (setq-default evil-split-window-below t
				evil-vsplit-window-right t
				evil-ex-complete-emacs-commands nil
				evil-ex-interactive-search-highlight 'selected-window
				evil-disable-insert-state-bindings t
				evil-respect-visual-line-mode t ; j&k operate via visual line or not
				evil-want-integration t
				evil-want-fine-undo t ; native undo system
				evil-want-C-g-bindings nil
				evil-move-cursor-back nil ; not like vim
				evil-want-Y-yank-to-eol nil ; not include eof
				evil-want-abbrev-expand-on-insert-exit nil
				evil-show-paren-range 0 ; distance to show paren
				evil-symbol-word-search t
				)
  ;; strange bug in evil-collections
  (defvar org-agenda-diary-file nil)

  (setq evil-collection-calendar-want-org-bindings t
		evil-collection-company-use-tng t
		evil-collection-outline-bind-tab-p t
		evil-collection-term-sync-state-and-mode-p nil
		evil-collection-setup-minibuffer nil
		evil-collection-setup-debugger-keys nil
		;; I use space for leader key
		evil-collection-key-blacklist '("<SPC>"))
  (evil-collection-init)
  (dolist (ig-mode '(realgud calendar)) ; remove specific mode
	(setq evil-collection-mode-list (remove ig-mode evil-collection-mode-list)))
  (evil-collection-define-key 'normal 'occur-mode-map
	;; Keybindings tweaks
	;; consistent with ivy
	(kbd "C-x C-o") 'occur-edit-mode)

  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
  (evil-ex-define-cmd "Q[uit]" 'quit-window)

  (general-define-key
   :states 'visual
   "j" 'next-line
   "k" 'previous-line
   "C-c" 'keyboard-quit)
  (general-define-key
   :states 'normal
   "gd" 'xref-find-definitions
   "gD" 'xref-find-definitions-other-window
   "gr" 'xref-find-references
   "gl" 'evil-avy-goto-line
   "gs" 'evil-avy-goto-word-1
   "gh" 'beginning-of-defun
   "ge" 'end-of-defun
   "C-d" 'scroll-up ; it looks strange, but in emacs it exactly is.
   "C-u" 'scroll-down
   "j" 'next-line
   "k" 'previous-line
   "J" 'nil
   )
  (general-define-key
   :states 'ex
   "C-a" 'move-beginning-of-line
   "C-b" 'backward-char
   "M-p" 'previous-complete-history-element
   "M-n" 'next-complete-history-elemen)

  ;; {{ evil-surround
  ;; left tag will comes with an extra space, like `[' 
  ;; right tag won't do that,like `]'
  (global-evil-surround-mode)

  ;; evil-nerd-commenter
  (autoload 'evilnc-comment-operator "evil-nerd-commenter")
  (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-motion-state-map "gC" 'evilnc-comment-operator)

  ;; matchit
  (global-evil-matchit-mode t)
  (define-key evil-normal-state-map "%" 'evilmi-jump-items)
  )
;; }}

(with-eval-after-load 'evil
  ;; set bindings for doc-mode
  (after! org
	(evil-declare-key 'normal org-mode-map
	  "gh" 'outline-up-heading
	  "$" 'org-end-of-line ; smarter behavior on headlines etc.
	  "^" 'org-beginning-of-line ; ditto
	  "<" (lambda () (interactive) (org-demote-or-promote 1)) ; out-dent
	  ">" 'org-demote-or-promote ; indent
	  (kbd "TAB") 'org-cycle) )

  (after! markdown-mode
	(evil-declare-key 'normal markdown-mode-map
	  "gh" 'outline-up-heading
	  (kbd "TAB") 'markdown-cycle))

  ;;{{ My leader
  (general-create-definer my-leader-def
	:states '(normal visual)
	;; :prefix my-leader
	:prefix "SPC")

  (my-leader-def
   "jj" 'scroll-other-window
   "kk" 'scroll-other-window-up
   "wh" 'windmove-left
   "wj" 'windmove-down
   "wk" 'windmove-up
   "wl" 'windmove-right
   "wo" 'delete-other-windows
   "ws" 'window-swap-states
   "wu" 'my-transient-winner-undo
   "wr" 'my-transient-winner-redo
   "wc" 'delete-window
   "wt" 'my-trans-window-split
   "wb" 'my-vsplit-window ; vertical split window

   ;;{{ project
   "ps" 'project-async-shell-command
   "pt" 'project-eshell
   "pm" 'project-compile
   "pg" 'project-vc-dir
   "pp" 'project-switch-project

   ;;{{ Find && Grep
   "[" 'flymake-goto-prev-error
   "]" 'flymake-goto-next-error
   "{" 'evil-prev-flyspell-error
   "}" 'evil-next-flyspell-error
   "ff" 'find-file
   "fe" 'consult-flymake
   "fF" 'find-file-other-window
   "fp" 'find-file-in-project
   "fP" 'find-file-in-current-directory
   "fg" 'my-consult-grep ; grep at project
   "fG" 'my-consult-grep-at-current-dir
   "fm" 'my-recentf
   "fM" 'my-recentf-the-other-window
   "fl" 'consult-line
										; "fL" 'consult-line-all-buffer
   "fb" 'switch-to-buffer
   "fB" 'switch-to-buffer-other-window
   "fs" 'imenu
   "fw" 'ace-window
   "ft" 'tab-bar-select-tab-by-name
   "fo" 'consult-outline

   ;;{{ Tab
   "tt" 'tab-bar-new-tab
   "tn" 'tab-bar-switch-to-next-tab
   "tp" 'tab-bar-switch-to-prev-tab
   "ts" 'tab-bar-switch-to-tab
   "tc" 'tab-bar-close-tab
   "tk" 'tab-bar-close-tab
   "tC" 'tab-bar-close-tab-by-name
   "to" 'tab-bar-close-other-tabs
   "1" 'my-select-tab-1
   "2" 'my-select-tab-2
   "3" 'my-select-tab-3
   "4" 'my-select-tab-4
   "5" 'my-select-tab-5
   "6" 'my-select-tab-6
   "7" 'my-select-tab-7
   "8" 'my-select-tab-8
   "9" 'my-select-tab-9

   ;;{{ Code Action
   "lf" 'lsp-execute-code-action
   "ls" 'yas-insert-snippet
   "lm" 'lsp-format-buffer
   "lr" 'lsp-rename
   "ld" 'lsp-describe-thing-at-point
   "lle" 'lsp-treemacs-errors-list

   ;;{{ Builtin version-control
   ;; vc-log-outgoing vc-log-comming
   "gll" 'vc-print-log ; git log
   "glr" 'vc-print-root-log ; git root log
   "glu" 'vc-update-change-log ; git update changelog
   "gdd" 'vc-diff ; git diff
   "gdm" 'smerge-ediff ; use ediff to resolve conflicts
   "gpl" 'vc-pull ; git pull
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
   "cp" 'consult-yank-pop
   "cP" 'consult-yank

   ;;{{ Gdb
   "dw" 'gud-watch
   "dr" 'gud-remove
   "db" 'gud-break
   "dB" 'gud-tbreak ; temporary break point
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

   ;; code
   "ca" 'add-change-log-entry-other-window
   "cd" 'rmsbolt-compile
   "cc" 'compile
   "cC" 'recompile
   "ck" 'kill-compilation
   "cX" 'quickrun
   "cx" 'quickrun-shell

   ;;{{ interesting builtin work
   "sss" 'my-sudo-save
   "ssc" 'flyspell-buffer
   "sm" 'proced ; top like system monitor
   "swj" 'webjump
   "swb" 'browse-url
   "scm" 'align
   "ssr" 'shr-render-region ; simple html renderer
   "srb" 're-builder ; build regexp with feedback
   "st" 'my-eshell-pop

   ;;{{ buffer actions
   "bc" 'kill-current-buffer
   "bk" 'buf-move-up
   "bj" 'buf-move-down
   "bh" 'buf-move-left
   "bl" 'buf-move-right
   "bb" 'switch-to-buffer
   "bo" 'switch-to-buffer-other-window
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
   "dp" 'luna-hungry-delete ; or will cause paren error
   )

  (general-create-definer my-backslash-leader
	:states '(normal visual)
	;; :prefix my-leader
	:prefix "\\")

  (my-backslash-leader
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
	))

(provide 'init-evil)
;;; init-evil.el ends here.
