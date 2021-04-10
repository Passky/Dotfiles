;; eieio provides a non complete CLOS typeystem
;;

(require 'cl-lib)
(cl-defstruct myFuzzyMatch (pattern "")
			  (lower-pattern ""))

(defun my-fuzzyMatch-evaluate (text pattern text_mask j pattern_mask k val)
  "
"
  ;;
  (setq my-evaluate-return (make-vector 3))
  ;; right shift 1
  (setq x (ash text_mask -1) )
  ;; e.g., text = 'a~bc~d~~ab~~d~', pattern = 'abcd'
  (when (= x 0) ; x = 0 ,return a vector[0 0 0]
	[0 0 0])
  (setq my-special 0)
  ;; e.g., text = '~abc~~AbcD~~', pattern = 'abcd'
  (if (and (not (= -1 (cl-position k val))) ; if k in val
			 (>= (nth 1 (nth k val)) j))

	)
  )


;; and `make-myFuzzyMatch'-> constructor and `myFuzzyMatch-p' function was there







(provide 'myFuzzyMatch)
;;; myFuzzyMatch.el ends here
