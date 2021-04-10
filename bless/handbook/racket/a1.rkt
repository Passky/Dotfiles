#lang racket

;;; NOTE
;; function named *-impr means using recursive, it does not mean to be faster,but more proper.

;; (: mes! (-> Any Void))
(define mes!
  (lambda (arg [split "\n"])
    (printf "~a~a" arg split)))


;; (: countdown (-> Integer String))
(define countdown
  (lambda (num)
    (let ([i (range num -1 -1)]
          [outstring ""])
      (for ([in i])
        (if (equal? in 0)
            (set! outstring (string-append outstring (format "~a" in)))
            (set! outstring (string-append outstring (format "~a " in)))))
      outstring)))

;; (: countdown-impr (-> Number (Listof Number)))
(define countdown-impr
  (lambda (num)
    (cond [(zero? num)
           (list 0)]
          [else (cons num (countdown-impr (sub1 num)))])))


;; (: insertR (-> Any Any Any (Listof Symbol)))
(define insertR
  (lambda (pos-item after-item plist)
    (reverse (foldl (lambda (from-list x)
                      (if (equal? pos-item from-list)
                          (begin ; just like progn in common lisp
                            (cons after-item (cons from-list x)))
                          (cons from-list x)))
                    '() plist))))

;; (: insertR-impr (-> (Listof Any) (Listof Any) (Listof Any)))
(define insertR-impr
  (lambda (s1 s2 l)
    (cond [(null? l)
           '()]
          [(eqv? (car l) s1)
           (cons s1 (cons s2 (insertR-impr s1 s2 (cdr l))))]
          [else (cons (car l) (insertR-impr s1 s2 (cdr l)))]
          )
    ))


;; (insertR-impr 'x 'y '(x z z x y x))
;; '(x y z z x y y x y)

;; (: remv-1st)
(define remv-1st
  (lambda (s l)
    (cond
      [(null? l) '()]
      [(eqv? (car l) s) (cdr l)]
      [else (cons (car l) (remv-1st s (cdr l)))])))

;; (remv-1st 'x '(x y z x))
;; '(y z x)

(define list-index-ofv
  (lambda (s l)
    (cond
      [(eqv? s (car l)) 0]
      [else (+ 1 (list-index-ofv s (cdr l)))]
      )))

;; (list-index-ofv 'x '(y z x x))
;; 2


(define my-filter
  (lambda (pre l)
    ;; pre = predicate
    (cond
      [(null? l) '()]
      [(pre (car l)) (cons (car l) (my-filter pre (cdr l)))]
      [else (my-filter pre (cdr l))]
      )
    ))

;; (my-filter even? '(1 2 3 4 5 6))
;; '(2 4 6)

(define zip
  (lambda (l1 l2)
    (cond
      [(or (null? l1) (null? l2)) '()]
      [(cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]
      )))

;; (zip '(1 2 3 4 5 6) '(a b c))
;; '((1 . a) (2 . b) (3 . c))

(define my-map
  (lambda (func l)
    (cond
      [(null? l) '()]
      [else (cons (func (car l) (my-map func (cdr l))))])))

;; (map add1 '(1 2 3 4))
;; '(2 3 4 5)

;; (cons '(1 2) '(1 3))
;; '((1 2) 1 3)
;; This is different with `append'
(define my-append
  (lambda (l1 l2)
    (cond
      [(null? l1)
       (cond [(null? l2) '()]
             [else (cons (car l2) (my-append l1 (cdr l2)))]
             )]
      [else (cons (car l1) (my-append (cdr l1) l2))])
    ))

;; (my-append '(1 2 3) '(a b c))
;; '(1 2 3 a b c)

;; TODO: I do not understand this very much,
;; REVIEW IT later
(define my-reverse
  (lambda (l)
    (cond
      [(null? l) '()]
      [else (my-append (my-reverse (cdr l)) `(,(car l)))])))
;; NOTE: ` , '
;; ` means construct a list and , means eval s-exp in list

;; (my-reverse '(1 2 3))
;; '(3 2 1)

(define same-lists
  (lambda (l1 l2)
    (cond
      [(null? l1)
       (cond
         [(null? l2) #t]
         [else #f])]
      [(equal? (car l1) (car l2)) (same-lists (cdr l1) (cdr l2))]
      [else #f]
      )))

;; (same-lists '(a b c d) '(a b c d))
;; -> #t
;; (same-lists '(1 2 3 4) '(1 2 3 4 5))
;; -> #f

(define binary->natural
  (lambda (l)
    (cond
      [(null? l) 0]
      [else (+ (* 2 (binary->natural (cdr l))) (car l))])))

(define minus
  (lambda (a b)
    (cond
      [(zero? b) a]
      [else (sub1 (minus a (sub1 b)))])))

(define divide
  (lambda (a b)
    (cond
      [(zero? a) 0]
      [else (+ 1 (divide (- a b) b))])
    ))

(define my-append-map
  (lambda (func prol)
    (cond 
      [(null? prol) '()]
      [else (my-append (func (car prol)) (my-append-map func (cdr prol)))])))

;; (my-append-map countdown-impr (countdown-impr 5))
;; '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)

(define contained-p
  (lambda (a b)
    (cond
      [(null? b) #f]
      [(equal? a (car b)) #t]
      [else (contained-p a (cdr b))])
    ))

(define set-same ; different is simply the same way
  (lambda (l1 l2)
    (cond
      [(null? l1) '()]
      [(contained-p (car l1) l2) (cons (car l1) (set-same (cdr l1) l2))]
      [else (set-same (cdr l1) l2)]
      )
    ))

;; (set-same '(1 2 3 4 5) '(2 4 6 8))
;; '(2 4)
