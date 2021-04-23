;;; -*- lexical-binding: t; -*-

;;; clojure
(my-add-package 'clojure-mode)
(my-add-package 'cider)

;; Racket
(my-add-package 'racket-mode)
(setq auto-mode-alist (delete '("\\.rkt\\'" . scheme-mode) auto-mode-alist)) ; not use default `scheme-mode'
(add-auto-mode 'racket-mode
			   "\\.rkt\\'")
(add-hook 'racket-mode-hook #'(lambda ()
								;; TODO: Wait till lsp-racket become a mature one
								(if nil
									(progn
									  (require 'lsp-racket)
									  (lsp))
								  (progn
									(general-define-key
									 :keymaps 'racket-mode-map
									 [remap evil-lookup] 'racket-xp-describe
									 [remap flymake-goto-next-error] 'racket-xp-next-error
									 [remap flymake-goto-prev-error] 'racket-xp-previous-error)
									"lr" 'racket-xp-rename
									(racket-xp-mode))
								  ))) ; provide check documentation describe
(after! racket-mode
  (general-define-key
   :keymaps 'racket-mode-map
   [remap quickrun-shell] 'racket-run-with-errortrace
   [remap quickrun] 'racket-run-with-debugging
   ;; [backspace] 'luna-hungry-delete
   ;; [?\d] 'luna-hungry-delete
   )
  
  (add-hook 'racket-xp-mode-hook ; improve performance
			(lambda ()
			  (remove-hook 'pre-redisplay-functions
						   #'racket-xp-pre-redisplay
						   t))))

;; Common lisp
;; (add-hook 'lisp-mode 'sly-mode)
(my-add-package 'sly)
(with-eval-after-load 'sly
  (setq sly-net-coding-system 'utf-8-unix)
  (let ((sbcl-bin-path (concat (getenv "HOME") "/lib/sbcl")))
	(when (file-exists-p sbcl-bin-path)
	  (setenv "SBCL_HOME" sbcl-bin-path)))
  (setq sly-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "2048"))
								   (ecl ("ecl"))
								   (clisp ("clisp" "-ansi"))
								   (chicken ("csi"))
								   (abcl ("abcl")))))

;; Scheme
;; (add-hook 'scheme-mode-hook 'geiser-mode)
(my-add-package 'geiser)
(with-eval-after-load 'geiser
  (progn
	(setq geiser-mode-smart-tab-p t)
	))

;; elisp
;; hack company-backends in emacs-lisp-mode
;; (add-hook 'emacs-lisp-mode-hook (lambda ()
;; 								  ;; This slows emacs down
;; 								  (set (make-local-variable 'company-backends)
;; 									   '(
;; 										 (company-files) 
;; 										 (company-keywords)
;; 										 (company-dabbrev)
;; 										 (company-capf company-etags)
;; 										 ))
;; 								  ))
(autoload 'flymake-start "flymake" "Enable flymake" t)
(add-hook 'emacs-lisp-mode-hook 'flymake-start)

(provide 'init-lisp)
;;; init-lisp.el ends here
