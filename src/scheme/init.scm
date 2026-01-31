;
;
;
; The Schem Porgramming language, 2nd. end R. Kent Dybvig [x](https://www.scheme.com/tspl2d/)
; The three implementation model for scheme https://www.cs.unc.edu/xcms/wpfiles/dissertations/dybvig.pdf
; Norvig PAIP https://norvig.github.io/paip-lisp/#/chapter22
;

;; possible match impl:
;; https://github.com/shirok/Gauche/blob/master/libsrc/util/match-impl.scm
;; pattern matching ref: https://docs.racket-lang.org/reference/match.html

;; Optimizations
;; * Lexical addressing? (De-Brujin inexes)
;; * Tagged pointers
;; * Flat closures?
;; * Hash consing?

;; GC
;; An Efﬁcient Non-Moving Garbage Collector for Functional Languages Katsuhiro Ueno Atsushi Ohori Toshiaki Otomo 

;; Some HAMT implemenations:
;; - https://github.com/SimonLSchlee/hamt (zig)
;; - https://github.com/paoda/hamt (zig)
;; - https://github.com/mkirchner/hamt (c)
;; also clojure and scala have hamt dictionaries
;; https://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice.html

;; (define (caar x) (car (car x)))
;; (define (cadr x) (car (cdr x)))
;; (define (cdar x) (cdr (car x)))
;; (define (cddr x) (cdr (cdr x)))
;;
;; (define (caaar x) (car (car (car x))))
;; (define (caadr x) (car (car (cdr x))))
;; (define (cadar x) (car (cdr (car x))))
;; (define (caddr x) (car (cdr (cdr x))))
;; (define (cdaar x) (cdr (car (car x))))
;; (define (cdadr x) (cdr (car (cdr x))))
;; (define (cddar x) (cdr (cdr (car x))))
;; (define (cdddr x) (cdr (cdr (cdr x))))
;; (define (caaaar x) (car (car (car (car x)))))
;; (define (caaadr x) (car (car (car (cdr x)))))
;; (define (caadar x) (car (car (cdr (car x)))))
;; (define (caaddr x) (car (car (cdr (cdr x)))))
;; (define (cadaar x) (car (cdr (car (car x)))))
;; (define (cadadr x) (car (cdr (car (cdr x)))))
;; (define (caddar x) (car (cdr (cdr (car x)))))
;; (define (cadddr x) (car (cdr (cdr (cdr x)))))
;; (define (cdaaar x) (cdr (car (car (car x)))))
;; (define (cdaadr x) (cdr (car (car (cdr x)))))
;; (define (cdadar x) (cdr (car (cdr (car x)))))
;; (define (cdaddr x) (cdr (car (cdr (cdr x)))))
;; (define (cddaar x) (cdr (cdr (car (car x)))))
;; (define (cddadr x) (cdr (cdr (car (cdr x)))))
;; (define (cdddar x) (cdr (cdr (cdr (car x)))))
;; (define (cddddr x) (cdr (cdr (cdr (cdr x)))))


;; and how to mark currently executed lambda in GC? (it shoould be in stack!)
;; ((lambda (x . xs) xs) 3 4 5)

;; (set! foo (lambda (x . xs) xs))

;; (if #t (+ 1 2 3) (+ 1 2 3))

;; '(1 2 (+ 2 3) "" '(3 4) 5 (6 . 7))
;; (set! d 5)
;; (set! f (lambda (a ) ((lambda (a c) (+ a c d)) (+ a 1) 4)))
;; (f 3)
;; (define plus +) // TODO: sort this out..

;; (define foo 3)
;; (define (g x) (begin (+ 1 6) (+ 1 6) (+ 1 x)))
;; (g 3)
;; (define (+ . args) (+ ,@args))

(define fooz (let ((r 1)) r))
         (define (myadd n r)
           (if (zero? n)
              r
             (myadd (- n 1) (+ r 1))
           )
         )
         (define (myadd1 n r)
           (cond 
             [(zero? n) r]
             [ else (myadd (- n 1) (+ r 1))]))

;; "foo"
;;
(define (first x) (car x))
(define (second x) (car (cdr x)))
(define (third x) (car (cdr (cdr x))))
(define (third1 x) (head (tail (tail x))))
(define (head x) (car x))
(define (tail x) (cdr x))
(define (rest x) (cdr x))
(define nil '())
(define empty '())
(define (empty? x) (null? x))
(define (nil? x) (null? x))
; cool, but we need variable number of lists
(define (append list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))
;;

(define (map0 f lst) '())
;; (define (map1 f lst) ; panic incorrect pos 1
;;     (cons (f (car lst)) (cdr lst)))
(define (map1 f lst) ; panic incorrect pos 1
    (begin (f 3) (cdr lst)))

(define (map f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst))
                (map f (rest lst)))]))
;;
;;
(define (memv x xs) 
  (cond 
    [(empty? xs) #f]
    [(eqv? x (head xs)) xs]
    [else (memv x (tail xs))]))

(define (memq x xs) 
  (cond 
    [(empty? xs) #f]
    [(eq? x (head xs)) xs]
    [else (memq x (tail xs))]))

(define (member x xs) 
  (cond 
    [(empty? xs) #f]
    [(equal? x (head xs)) xs]
    [else (member x (tail xs))]))

(define (assoc x xs) 
  (cond 
    [(empty? xs) #f]
    [(equal? x (head (head xs))) (head xs)]
    [else (assoc x (tail xs))]))
;;
(define (assq x xs) 
  (cond 
    [(empty? xs) #f]
    [(eq? x (head (head xs))) (head xs)]
    [else (assq x (tail xs))]))

(define (assv x xs) 
  (cond 
    [(empty? xs) #f]
    [(eqv? x (head (head xs))) (head xs)]
    [else (assv x (tail xs))]))

(define (flatten xs)
  (cond
    [(empty? xs) empty]
    [else (append (first xs)
                (flatten (rest xs))])))

;; ;; I prey for the gods of tco
;; (define (flatMap f lst)
;;    (flatten (map f lst)))
;;
(define (add1 x) (+ 1 x))
(define (sub1 x) (- x 1))
(define list (lambda x x))
;;
;;
;;
;; (define (pos x xs) 
;;     (define (pos-rec n x xs) 
;;       (cond 
;;         [(empty? xs) -1]
;;         [(equal? x (head xs)) n]
;;         [else (pos-rec (+ n 1) x (tail xs))]))
;;     (pos-rec 0 x xs))
;; ;;; -------------------------------------------
;; (define read #f) ;; TODO: actually add read
;;
;;
;; ;;; yeah, simpler to add special form..
(define-macro (def name . body)
  (if (symbol? name)
    (append (list 'set! name) body)

    (list 'set! (head name) (flatten (list (list  'lambda) (list (tail  name)) body '())))
             ))
;;
;; ;;; ------------- pattern matching ------------
;; ;;; -------------------------------------------
;; (define-macro (foo expr)
;;   ;; (display expr)
;;   (if (equal? '+ (car expr))
;;     (set-car! expr '-)
;;     (set-car! expr '+))
;;   expr)
;;
;; ;; TODO: macroexpand?
;;
;; ;; (define-macro (quasiquote xs)
;; ;;     (define (iter x)
;; ;;         (cond 
;; ;;             [(and (pair? x) (equal? 'unquote (head x))) (list (eval (head (tail x))))]
;; ;;             [(and (pair? x) (equal? 'unquote-splicing (head x))) (eval (head (tail x)))]
;; ;;             [(pair? x) (append (iter (head x)) (iter (tail x)))]
;; ;;             [(nil? x) '()]
;; ;;             [else (list x)]))
;; ;;     (list 'quote (iter xs)))
;; (define (id x) x)
;; ;; almost working version
;; ;; (define-macro (quasiquote xs)
;; ;;     (define (qq-expand x)
;; ;;       (cond 
;; ;;             [(and (pair? x) (equal? 'unquote (head x)))  (list (eval (head (tail x))))]
;; ;;             [(and (pair? x) (equal? 'unquote-splicing (head x))) (eval (head (tail x)))]
;; ;;             [else (list (qq-iter x))]))
;; ;;     (define (qq-iter x)
;; ;;         (cond 
;; ;;             [(and (pair? x) (equal? 'unquote (head x)))  ( eval (head (tail x)))]
;; ;;             [(and (pair? x) (equal? 'unquote-splicing (head x))) (error "invalid context within quasiquote")]
;; ;;             [(pair? x) (flatten (map qq-expand x))]
;; ;;             [else x]))
;; ;;     (list 'quote  (qq-iter xs)))
;; ;;
;;
;; ;; and it's broken...
;; (define-macro (quasiquote xs)
;;     (define env (callerEnv))
;;     (define (qq-expand x)
;;       (cond 
;;             [(and (pair? x) (equal? 'unquote (head x)))  (list (eval1 (head (tail x)) env))]
;;             [(and (pair? x) (equal? 'unquote-splicing (head x))) (eval1 (head (tail x)) env)]
;;             [else (list (qq-iter x))]))
;;     (define (qq-iter x)
;;         (cond 
;;             [(and (pair? x) (equal? 'unquote (head x)))  ( eval1 (head (tail x)) env)]
;;             [(and (pair? x) (equal? 'unquote-splicing (head x))) (error "invalid context within quasiquote")]
;;             [(pair? x) (flatten (map qq-expand x))]
;;             [else x]))
;;     (list 'quote  (qq-iter xs)))
;;
;; ;;     (let ([id val-expr] ...) body ...+)
;; ;; (let proc-id ([id init-expr] ...) body ...+)
;;
;; (define-macro (let bindings . body)
;;   (if (symbol? bindings)
;;       `(begin (define ,bindings (lambda ,(map first (head body)) ,@(tail body)))
;;         (,bindings ,@(map second (head body))))
;;       `((lambda ,(map first bindings) ,@body)
;;         ,@(map second bindings)) ))
;;
(define-macro (let bindings . body)
    (append   (list (flatten (list (list  'lambda) (list (map first bindings)) body (list ))))
            (map second bindings) ))

