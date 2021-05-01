;;; commenary
;;; {{ spell checker (requires aspell or hunspell)
(my-add-package 'wucuo)

;;;{{ Input method.
; 1. For where without rime,you can use builtin pinyin `chinese-py'
; 2. You can use builtin japanese input method,which named `japanese', smooth enough.
(set-input-method 'chinese-tonepy) ; 带声调的拼音

;; Better rime
(my-add-package 'rime)
(after! rime
  (setq rime-show-candidate 'posframe))

;;; {{ language learn
;; japanese
(autoload 'kana "kana" "" t)
(my-add-package 'kana)

(provide 'init-lang)
;;; init-lang.el ends here
