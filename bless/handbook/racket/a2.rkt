#lang racket
(define my-list-ref
  (lambda
    (ls n)
    (letrec
        ([nth-cdr
          (lambda
            (n)
            (cond
              ;; fill up the function body
              ([zero? n]
               ls)
              (else
               (cdr
                (nth-cdr
                 (- n 1)))))
            )])
      (car
       (nth-cdr n)))))
;; (my-list-ref '(1 2 3) 2)
;; 3
(define contained-p
  ;; if a contained in b list
  (lambda
    (a b)
    (cond
      [(null? b)
       #f]
      [(equal? a
               (car b))
       #t]
      [else
       (contained-p a
                    (cdr b))])
    ))

;; It could be orderless,so let it be
(define my-union
  (lambda
    (a b)
    (cond
      [(empty? b) a]
      [else
       (cond
         [(contained-p (car b) a) (my-union a (cdr b))]
         [else
          (cons (car b)
                (my-union a (cdr b)))])])))

;; (my-union '(x y) '(x z))
;; '(z x y)

(define my-extend
  (λ
    (x pred)
    (lambda
      (a)
      (or
       (equal? a x)
       (pred a))
      )))
;; (filter (my-extend 1 even?) '(0 1 2 3 4 5))
;; '(0 1 2 4)
(define walk-symbol
  (lambda
    (a asl)
    (cond
      [(null? asl)
       '()]
      [(equal? a
               (car
                (car asl)))
       (cdr
        (car asl))]
      [else
       (walk-symbol a
                    (cdr asl))])
    ))
;; (walk-symbol 'a '((b . c) (a . b)))
;; b
(define lambda->lumbda
  (lambda
    (exp)
    ;; just like pcace in elisp
    (match exp
      [`(lambda (,m)
          ,y)
       `(lumbda (,m)
                ,(lambda->lumbda y))]
      [`(,rator ,rand)
       `(,(lambda->lumbda rator)
         ,(lambda->lumbda rand))]
      [`,m (not
            (pair? m)) m])))

(define var-occurs?
  (lambda
    (var-to-match exp)
    (match exp
      [`(lambda (,m)
          ,n)
       (var-occurs? var-to-match n)]
      [`(,m ,n)
       (or
        (var-occurs? var-to-match m)
        (var-occurs? var-to-match n))]
      [`,m ;; (not (pair? m))
       (eq? var-to-match m)
       ])))
;; (var-occurs? 'x '(x y))
;; #t

;; extract all occur vars
(define vars
  (lambda (exp)
    ;; match is just like pcace in elisp
    (match exp
      [`(lambda (,m) ,y)
       (vars y)]
      [`(,rator ,rand)
       (append (vars rator)
               (vars rand))]
      [`,m (not (pair? m)) `(,m)])))


;; The only difference is `union' with `append'
(define unique-vars
  (lambda (exp)
    ;; just like pcace in elisp
    (match exp
      [`(lambda (,m) ,y)
       (unique-vars y)]
      [`(,rator ,rand)
       (my-union (unique-vars rator)
                 (unique-vars rand))]
      [`,m (not (pair? m)) `(,m)])))


(define var-occurs-free
  (lambda (var exp)
    (match exp
      [`(lambda (,m) ,y)
       (if (eq? m var)
           #f
           (var-occurs-free var y))]
      [`(,rator ,rand)
       (or (var-occurs-free var rator)
           (var-occurs-free var rand))]
      [`,m (not (pair? m))
           (eq? m var)]
      )))

;; (var-occurs-free 'y '((lambda (y) (x y)) (lambda (x) (x y))))
;; #t

(define var-occurs-bound?
  (lambda (var exp)
    (match exp
      [`(,rator ,rand)
       (or (var-occurs-bound? var rator)
           (var-occurs-bound? var rand))]
      [`(lambda (,m) ,y)
       (cond
         [(memv var (vars y))
          (cond
            [(eq? m var) #t]
            [else (var-occurs-bound? var y)])]
         (else #f)
         )]
      [`,m (symbol? m) #f]
      )))

;; (var-occurs-bound? 'x 'x)
;; (var-occurs-bound? 'x '(lambda (x) x))
;; (var-occurs-bound? 'y '(lambda (x) x))
;; (var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
;; (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
;; (var-occurs-bound? 'z '(lambda (y) (lambda (z) (y z))))
;; (var-occurs-bound? 'x '(lambda (x) y))
;; (var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))
;; #t+#f * 4

(define unique-free-vars
  (lambda (l)
    (match l
      [`(,rator ,rand)
       (my-union (unique-free-vars rator)
                 (unique-free-vars rand))]
      [`(lambda (,m) ,y) (remv m (unique-free-vars y))]
      [`,m (symbol? m)
           `(,m)])
    ))

;; (unique-free-vars 'x)
;; (unique-free-vars '(lambda (x) (x y)))
;; (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;; '(x)
;; '(y)
;; '(x e y)

(define unique-bound-vars
  (lambda (l)
    (match l
      [`(,rator ,rand)
       (my-union (unique-bound-vars rator)
                 (unique-bound-vars rand))]
      [`(lambda (,m) ,y) 
       (cond [(memv m (vars y)) 
              (cons m (unique-bound-vars y))]
             [else (unique-bound-vars y)])
       ]
      [`,m (symbol? m)
           '()])
    ))

;; (unique-bound-vars 'x)
;; (unique-bound-vars '(lambda (x) y))
;; (unique-bound-vars '(lambda (x) (x y)))
;; (unique-bound-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;; '()
;; '()
;; '(x)
;; '(c x)

(define lex
  (λ (l len)
    (match l
      [`(,rator ,rand)
       (list (lex rator)
             (lex rand))]
      [`(lambda (,m) ,y)
       `(lambda ,(lex y (cons m len)))]
      [`,m (symbol? m)
           `(var ,)]
      )
    ))
