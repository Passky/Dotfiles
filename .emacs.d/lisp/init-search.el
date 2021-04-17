;; At least we can have icomplete with evil,which makes debug easier
;; completion-styles
(setq completion-category-defaults nil
	  completion-category-overrides '((file (styles . (partial-completion)))) ; NOTE: file path expand need this
	  completion-styles '(flex basic substring partial-completion))

(after! icomplete
  ;; Should we use `icomplate-vertical' package?
  (my-def-key
   :keymaps 'icomplete-minibuffer-map
   "C-n"  'icomplete-forward-completions
   "C-p"  'icomplete-backward-completions
   "M-n"  'icomplete-forward-completions
   "M-p"  'icomplete-backward-completions
   "C-r"  'previous-matching-history-element
   [?\t] 'icomplete-force-complete ; keep up with ivy or selectrum
   "C-c C-o" 'embark-export
   )
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
  (define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir)
  (define-key icomplete-minibuffer-map (kbd "M-m") 'icomplete-ret)

  (setq icomplete-separator "\n" ;; (propertize " ☯" 'face  '(foreground-color . "SlateBlue1")) ; using icomplete-vertical
		icomplete-delay-completions-threshold 2000
		icomplete-compute-delay 0
		icomplete-show-matches-on-no-input t
		icomplete-hide-common-prefix nil
		icomplete-tidy-shadowed-file-names t
		icomplete-in-buffer t
		icomplete-in-buffer nil))

(my-delay-eval #'(lambda ()
				   (icomplete-mode)))

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
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)
(setq history-delete-duplicates t)          ; remove repeat history
(define-key minibuffer-local-map (kbd "M-o") 'hydra-minibuffer/body)
(define-key minibuffer-local-map (kbd "C-c C-o") 'embark-export)

;; search actions
(my-add-package 'consult)
(after! consult
  ;; Or use `my-project-root'
  (setq consult-project-root-function #'vc-root-dir)
  (defun my-minibuffer-space ()
	(interactive)
	(if (and (string-prefix-p consult-async-default-split (minibuffer-contents))
			 (= 2 (length (split-string (minibuffer-contents) consult-async-default-split))))
		(insert consult-async-default-split)
	  (when (looking-back consult-async-default-split) (delete-char -1))
	  (insert " ")))
  (define-key icomplete-minibuffer-map (kbd "SPC") 'my-minibuffer-space))

(defun my-consult-grep ()
  "Use ripgrep first then fallback to grep."
  (interactive)
  (if
	  (executable-find "rg")
	  (call-interactively #'consult-ripgrep)
	(call-interactively #'consult-grep)))

(defun my-consult-grep-at-current-dir ()
  "Use ripgrep first then fallback to grep,
search in current directory."
  (interactive)
  (if
	  (executable-find "rg")
	  (call-interactively #'consult-ripgrep default-directory)
	(call-interactively #'consult-grep default-directory)))

(my-add-package 'iedit) ; multi cursor
(with-eval-after-load 'iedit
  (setq iedit-search-invisible nil))
;; {{ embark
(my-add-package 'embark)
(my-add-package 'embark-consult)
(with-eval-after-load 'embark
  (my-ensure 'embark-consult)
  (my-def-key
   :keymaps 'embark-general-map
   "i" 'wgrep-change-to-wgrep-mode)
  )
(add-hook #'embark-collect-mode #'embark-consult-preview-minor-mode)

;; from vmacs
(my-add-package 'orderless)
(my-delay-eval
 #'(lambda ()
	 (require 'orderless)))

(after! orderless
  (setq completion-styles (cons 'orderless completion-styles)) ;把orderless放到completion-styles 开头
  ;; 默认按空格开隔的每个关键字支持regexp/literal/initialism 3种算法
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
  (defun without-if-$! (pattern _index _total)
    (when (or (string-prefix-p "$" pattern) ;如果以! 或$ 开头，则表示否定，即不包含此关键字
              (string-prefix-p "!" pattern))
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun flex-if-comma (pattern _index _total) ;如果以逗号结尾，则以flex 算法匹配此组件
    (when (string-suffix-p "," pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun literal-if-= (pattern _index _total) ;如果以=结尾，则以literal  算法匹配此关键字
    (when (or (string-suffix-p "=" pattern)
              (string-suffix-p "-" pattern)
              (string-suffix-p ";" pattern))
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(literal-if-= flex-if-comma without-if-$!)))

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
