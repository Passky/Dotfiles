;;; commenary
;;; {{ spell checker (requires aspell or hunspell)
(my-add-package 'wucuo)

;;;{{ Input method.
; 1. For where without pyim,you can use builtin pinyin `chinese-py'
; 2. You can use builtin japanese input method,which named `japanese', smooth enough.

;; Better rime
(my-add-package 'rime)
(after! rime
  (setq rime-show-candidate 'posframe))

;;; Stop using pyim,since it still needs rime.
;;; But just keep these configration
;; (my-add-package 'pyim)
;; (my-add-package 'pyim-basedict)
;; (my-add-package 'liberime)
;; (defun my-toggle-pyim-rime()
;;   "Using rime as pyim's backend."
;;   (interactive)
;;   (setq default-input-method "pyim")
;;   (set-input-method 'pyim)
;;   (let ((liberime-auto-build t))
;; 	(require 'pyim-liberime))
;;   (setq pyim-default-scheme 'rime-quanpin))

;; (with-eval-after-load 'pyim
;;   (setq pyim-fuzzy-pinyin-alist
;; 		'(("en" "eng")
;; 		  ("in" "ing")))

;;   ;; auto choose pyim after chosen it once
;;   (setq default-input-method "pyim")

;;   ;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
;;   ;; enable basedict
;;   (pyim-basedict-enable)

;;   ;; 全拼
;;   ;; use western punctuation
;;   (setq pyim-punctuation-dict nil)
;;   ;; (setq pyim-default-scheme 'quanpin)

;;   ;; use memory efficient pyim engine
;;   (setq pyim-dcache-backend 'pyim-dregcache)
;;   ;; don's use shortcode2word
;;   (setq pyim-enable-shortcode nil)

;;   ;; 选词框显示5个候选词
;;   (setq pyim-page-length 5)
;;   (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。

;;   ;; 开启拼音搜索功能
;;   (pyim-isearch-mode 1)

;; ;; 使用 popup-el 来绘制选词框, 如果用 emacs26, 建议设置
;; ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
;; ;; 手动安装 posframe 包。
;; (if *gui* ;;for gui
;; 	  (setq pyim-page-tooltip 'posframe)
;; 	(setq pyim-page-tooltip 'popup)
;; 	))

;;; {{ language learn
;; japanese
(autoload 'kana "kana" "" t)
(my-add-package 'kana)

(provide 'init-lang)
;;; init-lang.el ends here
