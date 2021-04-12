;; Many actions
(my-add-package 'consult)

(my-add-package 'iedit) ; multi cursor
(with-eval-after-load 'iedit
  (setq iedit-search-invisible nil))

(with-eval-after-load 'xref
  (when (executable-find "rg")
	(setq xref-search-program 'ripgrep))
  ;; TODO: No need to cond in emacs-28
  (when (functionp 'xref-show-definitions-completing-read)
	(setq xref-show-xrefs-function #'xref-show-definitions-completing-read ; it will affect all xref-based commands
		  xref-show-definitions-function #'xref-show-definitions-completing-read)))

;; {{ isearch
(with-eval-after-load 'isearch
  ;; One space can represent a sequence of whitespaces
  (setq isearch-lax-whitespace t ; in literal search, treat space like `search-whitespace-regexp'
		isearch-regexp-lax-whitespace t ; in regexp ...
		search-whitespace-regexp "[ \t\r\n]+"
		isearch-allow-scroll t
		isearch-lazy-count t
		isearch-yank-on-move t
		lazy-count-prefix-format nil
		lazy-count-suffix-format " [%s/%s]"
		lazy-highlight-cleanup t ; I hate highlight
		lazy-highlight-buffer nil)

  (define-advice isearch-occur (:after (_regexp &optional _nlines))
	(isearch-exit))
  (my-def-key
   :keymaps 'occur-mode-map
   "C-x C-o" 'occur-edit-mode
   :keymaps 'isearch-mode-map
   "M-<return>"                'isearch-repeat-forward
   "M-S-<return>"              'isearch-repeat-backward
   "C-o"                       'swiper-from-isearch
   ;; consistent with ivy-occur'
   "C-c C-o"                   'isearch-occur ; consist with wgrep
   [escape]                    'isearch-cancel
   ;; Edit the search string instead of jumping back
   [remap isearch-delete-char] 'isearch-del-char))

;; }}


;; {{ minibuffer completion
(setq enable-recursive-minibuffers nil)        ; do not use minibuffer in minibuffer, causes bad bugs
(setq history-delete-duplicates t)          ; remove repeat history
;; TODO: If we remove ivy,then uncomment codes below
(define-key minibuffer-local-map (kbd "M-o") 'hydra-minibuffer/body)

;; TODO: icomplete-vertical is merging into master,so we remove this later
(my-add-package 'icomplete-vertical)

;; In Emacs 27 there is also a flex style which you might like.
;; WARNING: This causes bugs in eshell and path expand,so drop this.
;; (setq completion-styles '(flex substring partial-completion))

(after! icomplete
  (setq use-native? t)
  (icomplete-vertical-mode)
  (my-def-key
   :keymaps 'icomplete-minibuffer-map
   "C-n"  'icomplete-forward-completions
   "C-p"  'icomplete-backward-completions
   "M-n"  'icomplete-forward-completions
   "M-p"  'icomplete-backward-completions
   "C-c C-o" 'embark-export
   "SPC" 'my-space
   )
  (setq icomplete-separator "\n" 
		icomplete-show-matches-on-no-input nil
		icomplete-hide-common-prefix nil
		icomplete-tidy-shadowed-file-names t
		icomplete-in-buffer nil)
  )

;; {{ embark
(my-add-package 'embark)
(with-eval-after-load 'embark
  (evil-collection-define-key 'normal 'embark-general-map
	"i" 'wgrep-change-to-wgrep-mode)
  (my-def-key
   :keymaps 'embark-general-map
   "i" 'wgrep-change-to-wgrep-mode)
  )


;; NOTE: May try to rebuild workflow with fido-mode + orderless + embark + consult instead of ivy, when it is time.
;;{{ ivy core|(minibuffer fuzzy)
;; ivy-occur
(my-add-package 'ivy)
(my-add-package 'counsel) ; counsel => swiper => ivy
(my-add-package 'ivy-posframe)

;; enable ivy-mode 0.5 seconds after init
(my-delay-eval #'(lambda ()
						(ivy-mode 1)
						(counsel-mode 1)) 0.5)

(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map (kbd "M-o") 'hydra-minibuffer/body)
  (define-key ivy-minibuffer-map (kbd "M-p") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "M-n") 'ivy-next-line)
  (setq ivy-display-style 'fancy           ; fancy style
		ivy-count-format "%d/%d "          ; better counts
		ivy-use-virtual-buffers t          ; show recent files
		ivy-extra-directories '("./")      ; no ".." directories
		ivy-fixed-height-minibuffer t      ; fixed height
		ivy-height 10
		ivy-on-del-error-function 'ignore)
  ;; ivy-posframe
  (when *gui*
	;; Different command can use different display function.
	(setq ivy-posframe-display-functions-alist
		  '(
			(swiper          . ivy-posframe-display-at-window-bottom-left)
			(complete-symbol . ivy-posframe-display-at-point)
			(counsel-M-x     . ivy-posframe-display-at-frame-center)
			(t               . ivy-posframe-display)))
	(ivy-posframe-mode 1)
	)

  (defun ivy-occur-grep-mode-hook-setup ()
	"Set up ivy occur grep mode."
	;; no syntax highlight, I only care performance when searching/replacing
	(font-lock-mode -1)
	;; @see https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
	(column-number-mode -1)
	(local-set-key (kbd "RET") #'ivy-occur-press-and-switch))

  (add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-grep-mode-hook-setup))
;;}}

(with-eval-after-load 'counsel
  (my-def-key
   :keymaps 'override
   ;; [remap find-file] 'counsel-find-file
   ;; [remap find-library] 'counsel-M-x
   ;; [remap org-capture]         'counsel-org-capture
   ;; [remap describe-function]   'counsel-describe-function
   ;; [remap describe-variable]   'counsel-describe-variable
   [remap imenu]               'counsel-imenu
   [remap describe-face]       'counsel-describe-face
   )
  (setq ivy-initial-inputs-alist nil) ;; It will change automaticly , so set there instead of ivy
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
		counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
		counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
		counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s")
  )
;;}}

;; hydra-ivy
;; about configration see https://github.com/abo-abo/swiper/commit/28e88ab23a191420a93a4f920ca076674ee53f94
;; "M-o"
(with-eval-after-load 'ivy
  (defhydra hydra-minibuffer (:hint nil :color pink)
	"
						^minibuffer action^
--------------------------------------------------------------------------
[_m_] mark            [_TAB_] Done        [_h_] Actions
[_M_] unmark          [_gg_] Go-Begin     [_G_] Go-end
[_u_] Scroll-up       [_d_] Scroll-down   [_o_] ivy-occur
[_f_] Call                                [_c_] ivy-toggle-calling
[_q_] quit                                [<esc>] quit
[_sf_] embark-act     [_ss_] embark-export 
"
	;; arrows
	;; ("h" ivy-backward-delete-char)
	("h" ivy-dispatching-done)
	("gg" ivy-beginning-of-buffer)
	("G" ivy-end-of-buffer)
	("d" ivy-scroll-up-command)
	("u" ivy-scroll-down-command)
	;; actions
	("q" keyboard-escape-quit :exit t)
	("<escape>" keyboard-escape-quit :exit t)
	("TAB" ivy-alt-done :exit nil)
	("RET" ivy-done :exit t)
	;; ("C-SPC" ivy-call-and-recenter :exit nil)
	("f" ivy-call)
	("c" ivy-toggle-calling)
	("m" ivy-mark)
	("M" ivy-unmark)
	;; ("t" (setq truncate-lines (not truncate-lines)))
	("sf" embark-act)
	("ss" embark-export) 
	("o" ivy-occur :exit t)))


(provide 'init-search)
;;; init-search.el ends here
