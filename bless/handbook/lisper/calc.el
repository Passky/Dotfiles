(require 'pcase)
(require 'cl-lib)

(defun tree-sum (exp)
  "Return the sum of EXP tree."
  (pcase exp ; pcase is a `macro'
	(`(,left ,right) ; check if it is a tree
	(let ((r1 (tree-sum left))
		  (r2 (tree-sum right)))
	  (+ r1 r2)))
	((pred numberp) ; check if is a number
	 exp)))

(message "%s" (tree-sum '(1 (1 (1 2)))))

(defun calc (exp)
  (pcase exp
	(`(,op ,e1 ,e2)
	 (let ((r1 (calc e1))
		   (r2 (calc e2)))
	   (pcase op
		 ('+ (+ r1 r2))
		 ('- (- r1 r2))
		 ('* (* r1 r2))
		 ('/ (/ r1 r2)))))
	((pred numberp) exp)))

(defun ext-env (bind env)
  (cons bind env))

(defun ext-envs (bind-list env)
  (append bind-list env))

(defun lookup (var env)
  (cl-loop for bind in env
		   for vr = (car bind)
		   for va = (cdr bind)
		   if (eq var vr)
		   return va
		   finally return nil))

;; (closure f env)
;; (lambda (x) body)
;; (let (var val) body)
;; (function arg)

(defun interp (exp env)
  (pcase exp
	;; variable
	((pred (lambda (e) (and (symbolp e) (not (booleanp e)))))
	 (interp (lookup exp env) env))
	;; single lambda / closure
	(`(lambda . ,_) `(closure ,exp ,env))
	(`(closure . ,_) exp)
	;; lambda / closure application
	(`((lambda . ,rest) ,arg)
	 (interp (list (interp `(lambda . ,rest) env) (interp arg env)) env))
	(`((closure (lambda (,x) ,body) ,e) ,arg)
	 (interp body (ext-env (cons x (interp arg (ext-envs e env)))
						   (ext-envs e env))))
	;; symbol application
	(`(,f ,a) (when (and (symbolp f) (not (booleanp f)))
				(interp `(,(interp f env) ,(interp a env)) env)))
	;; let
	(`(let (,var ,val) ,body)
	 (interp body (ext-env (cons var (interp val env)) env)))
	;; arithmetic
	;; op e1 e2 can be confused with let bind body
	(`(+ ,e1 ,e2) (+ (interp e1 env) (interp e2 env)))
	(`(- ,e1 ,e2) (- (interp e1 env) (interp e2 env)))
	(`(* ,e1 ,e2) (* (interp e1 env) (interp e2 env)))
	(`(/ ,e1 ,e2) (/ (interp e1 env) (interp e2 env)))
	;; number
	((pred numberp) exp)))
