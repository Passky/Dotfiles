;; -*- coding: utf-8; lexical-binding: t; -*-
;; {{ yas-snippet
(my-add-package 'yasnippet)
(my-add-package 'yasnippet-snippets)
(defun my-enable-yas-minor-mode ()
  "Enable `yas-minor-mode'."
  (unless (my-is-buffer-file-temp) (yas-minor-mode 1)))

(add-hook 'prog-mode-hook 'my-enable-yas-minor-mode)
(add-hook 'text-mode-hook 'my-enable-yas-minor-mode)
;; non prog/text-mode
(add-hook 'cmake-mode-hook 'my-enable-yas-minor-mode)
(add-hook 'web-mode-hook 'my-enable-yas-minor-mode)
(add-hook 'scss-mode-hook 'my-enable-yas-minor-mode)

(advice-add  #'yas-insert-snippet :before #'evil-insert-state) ; automatic switch to insert-state
(global-set-key (kbd "C-c s") 'yas-insert-snippet)

;; consistent with vim
(setq-default company-yasnippet-annotation-fn '(lambda (snip-name)
												 (format "%s (Snippet)" snip-name)))
;; }}

;; company-mode
(my-add-package 'company)
(my-add-package 'company-prescient)
;; TODO: add a tag based completion system(works out of box and async and compatable with capf)


(add-hook 'after-init-hook 'global-company-mode)
(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
		'(company-complete-common
		  company-select-next
		  company-select-previous
		  company-complete-selection
		  company-complete-number)))

(with-eval-after-load 'company
  ;; (company-ctags-auto-setup)
  (general-define-key
   :keymaps 'company-active-map
   "C-p"     'company-select-previous
   "C-n"     'company-select-next
   "C-s"     'company-filter-candidates
   "<esc>" 'company-abort)

  (general-define-key
   :keymaps 'company-search-map
   "C-p"    'company-select-previous
   "C-n"    'company-select-next
   "<esc>" 'company-abort)

  (setq company-global-modes '(not eshell-mode shell-mode comint-mode erc-mode rcirc-mode gud-mode minibuffer-inactive-mode ;; emacs-lisp-mode elisp-mode
								   )
		;; NOT to enable company-mode for certain major modes, too slow.
		company-selection-wrap-around t
		company-auto-complete nil ; do not complete when tap space
		company-idle-delay 0.1
		company-echo-delay (if *gui* nil 0.1)
		company-show-numbers t ; Easy navigation to candidates with M-<n>
		company-require-match nil ; allow input string that do not match candidate words
		company-minimum-prefix-length 2 ; minimal prefix to triger completion
		company-tooltip-align-annotations t
		company-tooltip-limit 8 ; faster!
		completion-ignore-case t      ; ignore case for `company-capf'
        company-etags-ignore-case t
        company-etags-everywhere t
		company-dabbrev-other-buffers t
		company-dabbrev-ignore-case t
		company-dabbrev-downcase nil ; do not return downcase string
		;; make dabbrev-code non case-sensitive
		company-dabbrev-code-ignore-case t
		company-dabbrev-code-everywhere nil ; complete `abbrev' words only in current buffer
		company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\|!\\|?\\)" ; dabbrev will accept '- ! ? into candidates too.
		company-format-margin-function #'company-detect-icons-margin ; add icon for lisp edit
		;; company-transformers '(company-sort-by-backend-importance)
		company-backends '((company-files)
						   (company-capf)
						   ;; (company-dabbrev-code)
						   (company-dabbrev)
						   (company-etags)
						   (company-dabbrev)
						   (company-keywords)
						   (company-yasnippet)))

  ;; better fuzzy match
  (company-prescient-mode 1)

  ;; like vim
  (company-tng-mode)

  ;; Integrate company-dabbrev with company-capf
  ;; https://emacs-china.org/t/tabnine/9988/40
  ;; I delete this, but still hold link for review

  ;; The free version of TabNine is good enough,
  ;; and below code is recommended that TabNine not always
  ;; prompt me to purchase a paid version in a large project.
  ;; https://github.com/manateelazycat/lazycat-emacs
  (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
	(let ((company-message-func (ad-get-arg 0)))
	  (when (and company-message-func
				 (stringp (funcall company-message-func)))
		(unless (or
				 (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
				 (string-match "TabNine::sem to enable semantic completion for" (funcall company-message-func))
				 )
		  ad-do-it)
		))))

;; in case evil is disabled
(defun my-company-ispell-available-hack (orig-func &rest args)
  (my-ensure 'evil-nerd-commenter)
  (cond
   ((and (derived-mode-p 'prog-mode)
		 (or (not (company-in-string-or-comment)) ; respect advice in `company-in-string-or-comment'
			 (not (evilnc-is-pure-comment (point))))) ; auto-complete in comment only
	;; only use company-ispell in comment when coding
	nil)
   (t
	(apply orig-func args))))
(advice-add 'company-ispell-available :around #'my-company-ispell-available-hack)

(defun my-add-ispell-to-company-backends ()
  "Add ispell to the last of `company-backends'."
  (setq company-backends
		(add-to-list 'company-backends 'company-ispell)))

;; {{ setup company-ispell
(defun my-toggle-company-ispell ()
  "Toggle company-ispell."
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
	(setq company-backends (delete 'company-ispell company-backends))
	(message "company-ispell disabled"))
   (t
	(my-add-ispell-to-company-backends)
	(message "company-ispell enabled!"))))

(defun my-company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
	(make-local-variable 'company-backends)
	(my-add-ispell-to-company-backends)
	;; @see https://github.com/redguardtoo/emacs.d/issues/473
	(cond
	 ((and (boundp 'ispell-alternate-dictionary)
		   ispell-alternate-dictionary)
	  (setq company-ispell-dictionary ispell-alternate-dictionary))
	 (t
	  (setq company-ispell-dictionary (file-truename (concat my-emacs-d "misc/english-words.txt")))
	  ;; using https://github.com/dolph/dictionary
	  (setq ispell-alternate-dictionary company-ispell-dictionary)
	  ))))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
(add-hook 'org-mode-hook 'my-company-ispell-setup)
(add-hook 'markdown-mode-hook 'my-company-ispell-setup)

;;}}
(provide 'init-completion)
;;; init-completion.el ends here
