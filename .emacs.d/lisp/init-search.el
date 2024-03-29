;; {{ minibuffer completion
;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
	  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; minibuffer
(setq enable-recursive-minibuffers t
	  minibuffer-eldef-shorten-default t
	  history-delete-duplicates t)
(minibuffer-depth-indicate-mode)
(minibuffer-electric-default-mode)

(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;; completion-styles
(setq completion-auto-help 'lazy
	  completion-category-defaults nil
	  ;; let filepath expend correctly
	  completion-category-overrides '((file (styles . (partial-completion))))
	  completion-styles '(flex basic substring partial-completion)
	  completion-ignore-case t
	  completion-cycle-threshold 3
	  read-file-name-completion-ignore-case t
	  read-buffer-completion-ignore-case t
	  )
;; }}

(after! icomplete
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
  (define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir)
  (define-key icomplete-minibuffer-map (kbd "M-m") 'minibuffer-complete-and-exit)

  (setq icomplete-separator (propertize "☆\n" 'face  '(face-foreground . 'warning)) ; using icomplete-vertical
		icomplete-delay-completions-threshold 2000
		icomplete-compute-delay 0
		icomplete-show-matches-on-no-input t
		icomplete-hide-common-prefix nil
		icomplete-tidy-shadowed-file-names t
		icomplete-scroll t ; like ivy!
		;; in-buffer completion is buggy now.
		icomplete-in-buffer t
		icomplete-prospects-height 10
		)
  ;; highlight current selected
  (custom-set-faces '(icomplete-first-match ((t (:inherit highlight))))) ; NOTE: REMOVE this line in emacs28
  )

(my-delay-eval #'(lambda ()
				   (icomplete-mode)) 0) 

(unless (functionp 'icomplete-vertical-mode)
  (my-add-package 'icomplete-vertical))

(after! icomplete
  ;; it will be in emacs core soon!
  (icomplete-vertical-mode)
  (general-define-key
   :keymaps 'icomplete-minibuffer-map
   "C-n"  'icomplete-forward-completions
   "C-p"  'icomplete-backward-completions
   "M-n"  'icomplete-forward-completions
   "M-p"  'icomplete-backward-completions
   "C-r"  'previous-matching-history-element
   "<up>" 'icomplete-backward-completions
   "<down>" 'icomplete-forward-completions
   [?\t] 'icomplete-force-complete ; keep up with ivy or selectrum
   "C-c C-o" 'embark-export
   ))

(after! marginalia
  ;; REVIEW: will there be performance issue?
  ;; use heavy mode by default
  ;; (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)) 
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  ;; use `marginalia-cycle' to switch from light or heavy mode
  (define-key icomplete-minibuffer-map (kbd "M-a") 'marginalia-cycle))


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
  (general-define-key
   :keymaps 'occur-mode-map
   "C-x C-o" 'occur-edit-mode)

  (general-define-key
   :keymaps 'isearch-mode-map
   "M-<return>"                'isearch-repeat-forward
   "M-S-<return>"              'isearch-repeat-backward
   ;; consistent with ivy-occur'
   "C-c C-o"                   'isearch-occur ; consist with wgrep
   ;; Edit the search string instead of jumping back
   [remap isearch-delete-char] 'isearch-del-char))
;; }}

;; {{ search actions
(my-add-package 'consult)
(my-add-package 'consult-lsp)

(after! consult
  (setq consult-async-refresh-delay 0.1
		consult-preview-key nil))

(defun my-consult-grep ()
  "Use ripgrep first or fallback to grep."
  (interactive)
  (if (executable-find "rg")
	  (call-interactively #'consult-ripgrep)
	(call-interactively #'consult-grep)))

(defun my-consult-grep-at-current-dir ()
  "Use ripgrep first or fallback to grep,
search in current directory."
  (interactive)
  (let ((consult-project-root-function nil))
	(if (executable-find "rg")
		(call-interactively #'consult-ripgrep default-directory)
	  (call-interactively #'consult-grep default-directory))))

(my-add-package 'iedit) ; multi cursor
(with-eval-after-load 'iedit
  (setq iedit-search-invisible nil))
;; {{ embark
(my-add-package 'embark)
(my-add-package 'embark-consult)
(define-key minibuffer-local-map (kbd "C-c C-o") 'embark-export)
(define-key minibuffer-local-map (kbd "C-c C-o") 'embark-export)
(with-eval-after-load 'embark
  (my-ensure 'embark-consult)
  (general-define-key
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

(my-add-package 'pinyinlib) ; pinyin regex match by 首字母
(after! orderless
  (require 'pinyinlib)
  (defun completion--regex-pinyin (str)
		 (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

;; "M-o"
(with-eval-after-load 'icomplete
  ;; TODO: replace this with transient
  (defun my-transient-minibuffer ()
	"Use transient fo minibuffer actions."
	(interactive)
	(let ((echo-keystrokes nil)
		  (minibuffer-mes "[ea] embark-act  [ee] embark-export  [ec] embark-collect-live
[ei] embark-isearch [es] embark-eshell"
						  ))
	  (message minibuffer-mes)
	  (set-transient-map
	   (let ((map (make-sparse-keymap)))
		 (define-key map (kbd "ea") #'embark-act)
		 (define-key map (kbd "ec") #'embark-collect-live)
		 (define-key map (kbd "ee") #'embark-export)
		 (define-key map (kbd "ei") #'embark-isearch)
		 (define-key map (kbd "es") #'embark-eshell)
		 map)
	   t))
	))

(define-key minibuffer-local-map (kbd "M-o") #'my-transient-minibuffer)

(provide 'init-search)
;;; init-search.el ends here
