
(require 'package)
;; load autoload files and populate load-path’s
(package-initialize)

(message "\n|Dumping start at %s|\n" (current-time-string))

(global-font-lock-mode) ; enable color theme

(load-file (concat (file-name-as-directory user-emacs-directory) "init.el"))
(setq evil-want-keybinding nil)

(dolist (package '(icomplete
					   company company-capf company-files company-dabbrev company-dabbrev-code company-yasnippet company-prescient prescient yasnippet-snippets yasnippet 
					   which-key evil evil-matchit evil-collection evil-surround evil-nerd-commenter evil-surround evil-avy bing-dict htmlize
					   hl-todo buffer-move wgrep hydra xclip page-break-lines package elpy posframe dumb-jump
					   rainbow-delimiters elpy diff-hl quickrun 
					   lsp-ui lsp-ui-doc lsp-completion lsp-diagnostics lsp lsp-lens lsp-mode lsp-protocol lsp-hack lsp-java lsp-pyright dap-mode 
					   rust-mode cargo racket-doc racket-eldoc racket-mode racket-xp-complete racket-xp geiser 
					   prog-mode cc-mode cpp python sh-script js thingatpt lisp-mode scheme elisp-mode 
					   flymake elec-pair project xref paren midnight dash text-mode pcomplete json 
					   recentf saveplace woman man dired proced simple vc-git
					   eshell-z em-script esh-cmd esh-io em-script em-xtra em-glob em-tramp em-basic eshell em-hist em-unix em-cmpl em-ls em-term em-alias em-banner em-pred em-prompt
					   org org-table org-timer org-list org-protocol org-archive org-clock org-agenda org-attach org-capture org-compat org-faces org-feed org-goto ol-irc ob-C ob-js ob-J ob-abc ob-calc ob-clojure ob-comint ob-core ob-emacs-lisp ob-eshell ob-haskell ob-java ob-lisp ob-lua ob-makefile ob-octave ob-org ob-perl ob-python ob-ruby ob-scheme ob-shell ob-sql
					   doom-modeline doom-modeline-core doom-modeline-env
					   ))
  (require package))

(defvar *dump* t
  "Using dumping now.")

;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")

(message "\n|Dumping finished at %s|\n" (current-time-string))
