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
   ;; Edit the search string instead of jumping back
   [remap isearch-delete-char] 'isearch-del-char))
;; }}


;; {{ minibuffer completion
(setq enable-recursive-minibuffers nil)        ; do not use minibuffer in minibuffer, causes bad bugs
(setq history-delete-duplicates t)          ; remove repeat history
(define-key minibuffer-local-map (kbd "M-o") 'hydra-minibuffer/body)
(define-key minibuffer-local-map (kbd "C-c C-o") 'embark-export)


(my-add-package 'orderless)
(setq completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion))))
	  completion-styles '(orderless basic partial-completion substring))

;; TODO: icomplete-vertical is merging into master,so we remove this later
(my-add-package 'icomplete-vertical)
(after! icomplete
  (icomplete-vertical-mode)
  (my-def-key
   :keymaps 'icomplete-minibuffer-map
   "C-n"  'icomplete-forward-completions
   "C-p"  'icomplete-backward-completions
   "M-n"  'icomplete-forward-completions
   "M-p"  'icomplete-backward-completions
   "C-c C-o" 'embark-export
   )
  (setq icomplete-separator "\n" 
		icomplete-show-matches-on-no-input nil
		icomplete-hide-common-prefix nil
		icomplete-tidy-shadowed-file-names t
		icomplete-in-buffer nil))

(my-delay-eval #'(lambda ()
				   (icomplete-mode)))

;; {{ embark
(my-add-package 'embark)
(my-add-package 'embark-consult)
(with-eval-after-load 'embark
  (my-ensure 'embark-collect)
  (evil-collection-define-key 'normal 'embark-general-map
	"i" 'wgrep-change-to-wgrep-mode)
  (my-def-key
   :keymaps 'embark-general-map
   "i" 'wgrep-change-to-wgrep-mode)
  )
(add-hook #'embark-collect-mode #'embark-consult-preview-minor-mode)


;; about configration see https://github.com/abo-abo/swiper/commit/28e88ab23a191420a93a4f920ca076674ee53f94
;; "M-o"
(with-eval-after-load 'icomplete
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
