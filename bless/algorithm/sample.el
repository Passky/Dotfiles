(require'cl-lib)
(setq alist (list
			 1 2 3 4 5 6))
(setq changedlist (cl-remove-if
				   (lambda (x) (= (% x 2) 0))
	  alist ))

(dolist (i changedlist)
  (message "%d " i))
(cl-defstruct liso (name 1)
			 (fuck 2) )

(setq astruct (make-liso :name 1 :fuck 2))
(print (liso-name astruct ))


