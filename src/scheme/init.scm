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
(define (bar x) 
  (cond 
    [(zero? x) 0]
    [(eq? 5 x) 5]
    [else 1]
  ))

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
;; ;; (define-macro (let bindings . body)
;; ;;   (if (symbol? bindings)
;; ;;       `(begin (define ,bindings (lambda ,(map first (head body)) ,@(tail body)))
;; ;;         (,bindings ,@(map second (head body))))
;; ;;       `((lambda ,(map first bindings) ,@body)
;; ;;         ,@(map second bindings)) ))
;;
;; (define-macro (let bindings . body)
;;     (append   (list (flatten (list (list  'lambda) (list (map first bindings)) body (list ))))
;;             (map second bindings) ))
;;
;;
;;
;;       ;; `((lambda ,(map first bindings) ,@body)
;;       ;;   ,@(map second bindings)) )
;; ;; TODO: let*, letrec
;;
;; ;; (def-scheme-macro let* (bindings &rest body)
;; ;;   (if (null bindings)
;; ;;               '(begin .,body)
;; ;;               '(let (,(first bindings))
;; ;;          (let* ,(rest bindings) . ,body))))
;;
;; ;; (def-scheme-macro letrec (bindings &rest body)
;; ;;  '(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
;; ;;     ,@(mapcar #'(lambda (v) '(set! . ,v)) bindings)
;; ;;    .,body))
;;
;; ;; (expand-macro '(let ((r 1)) r))
;; ;; (let x ((a 1)) a)
;;
;; ;; (let fac ([n 10])
;; ;;         fac
;; ;;         (if (zero? n)
;; ;;             1
;; ;;             (* n (fac (sub1 n)))))
;;
;; (define gensym-id 0)
;;
;; (define (gensym) 
;;   (let ((i gensym-id))
;;     (begin 
;;       (set! gensym-id (+ 1 i))
;;       (string->atom (string-append "gensym-" (number->string i))))))
;; ;; ;;; --------------------------------------------------
;; ;;
;; ;; ;;     (match case-expr (pattern body-expr) ...)
;; ;; ;;
;; ;; ;;  
;; ;; ;; pattern	 	=	 	name
;; ;; ;;  	 	|	 	literal-constant
;; ;; ;;  	 	|	 	(cons pattern pattern)
;; ;; ;;  	 	|	 	(list pattern ...)
;; ;; ;;  	 	|	 	(name pattern ...)
;; ;; ;;  	 	|	 	(? name)
;; ;;
;;
;; ;; (displaynl "hello")
;;
;; ;; (define (matches-internal? expr pattern bindings)
;; ;;   ;; (displaynl pattern)
;; ;;   (cond 
;; ;;     ;; [(null? pattern) (list #t bindings)]
;; ;;     [(and (or (number? pattern) (null? pattern) (string? pattern) (boolean? pattern)) (equal? pattern expr)) 
;; ;;          (list #t bindings)]
;; ;;     [(and (pair? pattern) (equal? 'quote (head pattern)) (equal? (head (tail pattern)) expr)) (list #t bindings)]
;; ;;     [(and (pair? pattern) (equal? 'cons (head pattern)) (pair? expr))
;; ;;         (let ((res (matches-internal? (head expr) (head (tail pattern)) bindings )))
;; ;;           (if (head res) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail res)) ) (list #f .()) ))]
;; ;;     [(and (pair? pattern) (equal? '? (head pattern))) 
;; ;;       (if ((eval (head (tail pattern))) expr) (list #t bindings)   (list #f .()))]
;; ;;     [(symbol? pattern) (list #t (cons (list pattern expr) bindings))]
;; ;;     [else (list #f .())]))
;; ;;
;; ;; (define (matches? expr _p) ;; right, <--- we can't eval here as it has it's own env. and won't capture free vars
;; ;;   (let ((pattern (head _p))
;; ;;         (body-expr (head (tail _p))))
;; ;;   (let ((res (matches-internal? expr pattern '())))
;; ;;     (if (head res) (begin (addToEnv (second res)) (list #t (eval body-expr))) (list #f .()) ))))
;; ;;   
;; ;; ;;; hmmm, use displaynl luke
;; ;; (define-macro (match case-expr . patterns) 
;; ;;     (define (gen-match-rec expr patterns)
;; ;;       (cond 
;; ;;         [(null? patterns) `(error "no matching clause")]
;; ;;         [else `(let ((res (matches? ,expr ,(list 'quote (head patterns))))) 
;; ;;                  (if (head res) (second res) ,(gen-match-rec expr (tail patterns)))) ]))
;; ;;         ;; [else (cons (gen-match expr (head patterns)) (gen-match-rec expr (tail patterns)))]))
;; ;;     (if (null? patterns) (error "patterns are empty")
;; ;;       (gen-match-rec case-expr patterns)))
;;
;;
;;
;; (define (let-test1 x bindings) 
;;   (cond 
;;     [(zero? x) (cons 1 bindings)]
;;     [else (let ((rs (let-test1 (sub1 x) bindings))) rs ) 
;;       ]))
;; ;; this one is workingv
;; (define (let-test2 x bindings)
;; (begin 
;;   (cond [(zero? x ) (cons 1 bindings ) ] 
;;         [else ((lambda (rs ) rs ) (let-test2 (sub1 x ) bindings ) ) ] ) ))
;;
;;
;; (define (let-test3)  
;;     (let ((x (let ((x 3)) x))) (let ((x x)) x) )) 
;;       
;;
;; (define (let-test4 x bindings) 
;;   (if  (zero? x) (cons 1 bindings)
;;      (let ((rs (let-test1 (sub1 x) bindings))) rs ) 
;;       ))
;;
;; (define (let-test5 x bindings) 
;;   (if  (zero? x) (cons 1 bindings)
;;      (let ((rs (cons 0 bindings))) rs ) 
;;       ))
;;
;; (define (let-test6 x bindings) 
;;   (if  (zero? x) (cons 1 bindings)
;;      (let ((rs  bindings)) rs ) 
;;       ))
;; (define (let-test7 x brr) 
;;      (let ((rs  brr)) rs ) 
;;       )
;; (define (matches-internal? expr pattern bindings)
;;   ;; (displaynl pattern)
;;   (cond 
;;     ;; [(null? pattern) (list #t bindings)]
;;     [(and (or (number? pattern) (null? pattern) (string? pattern) (boolean? pattern)) (equal? pattern expr)) 
;;          (list #t bindings)]
;;     [(and (pair? pattern) (equal? 'quote (head pattern)) (equal? (head (tail pattern)) expr)) (list #t bindings)]
;;     [(and (pair? pattern) (equal? 'cons (head pattern)) (pair? expr))
;;         (begin (define rs (matches-internal? (head expr) (head (tail pattern)) bindings ))
;;           (if (head rs) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail rs)) ) (list #f .()) ))]
;;         ;; (let ((rs (matches-internal? (head expr) (head (tail pattern)) bindings )))
;;         ;;   (if (head rs) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail rs)) ) (list #f .()) ))]
;;     [(and (pair? pattern) (equal? '? (head pattern))) 
;;       (if ((eval (head (tail pattern))) expr) (list #t bindings)   (list #f .()))]
;;     [(symbol? pattern) (list #t (cons (list pattern expr) bindings))]
;;     [else (list #f .())]))
;;
;; (define (matches? expr _p) ;; right, <--- we can't eval here as it has it's own env. and won't capture free vars
;;   (let ((pattern (head _p))
;;         (body-expr (head (tail _p))))
;;   (let ((res (matches-internal? expr pattern '())))
;;     (if (head res) (list #t (second res) body-expr) (list #f .()) ))))
;;   
;; ;;; hmmm, use displaynl luke
;; (define-macro (match case-expr . patterns) 
;;     (define _res (gensym))
;;     (define (gen-match-rec expr patterns)
;;       (cond 
;;         [(null? patterns) `(error "no matching clause")]
;;         [else `(let ((,_res (matches? ,expr ,(list 'quote (head patterns))))) 
;;                  (if (head ,_res) (begin (addToEnv (second ,_res)) (eval (third ,_res))) ,(gen-match-rec expr (tail patterns)))) ]))
;;         ;; [else (cons (gen-match expr (head patterns)) (gen-match-rec expr (tail patterns)))]))
;;     (if (null? patterns) (error "patterns are empty")
;;       (gen-match-rec case-expr patterns)))
;; ;;
;; ;; (match '(1 2)
;; ;;     ['four  1]
;; ;;     ["four" 2]
;; ;;     [#t  3]
;; ;;     [(cons a (cons b '())) (+ a b)]
;; ;;     [(cons 1 (cons 2 '())) 4]
;; ;;     [4      "hello world"])
;; ;; ;; ;;
;; (define (last-item l)
;;     (match l
;;       [(cons lst '()) lst]
;;       [(cons fst rst) (last-item rst)]))
;; ;;; --------------------------------------------------
;; ;; 
;; ;; FIXME: quasiquote
;;
;; (define (expand-macro expr)
;;     (define hasExpanded #f)
;;     (define (em-iter x)
;;         (cond 
;;             [(and (pair? x) (equal? 'quasiquote (head x))) x]
;;             [(and (pair? x) (macro? (head x))) (begin (set! hasExpanded #t) (expand (head x) (tail x)))]
;;             [(pair? x) (map em-iter x)]
;;             [else x]))
;;     (list (em-iter expr) hasExpanded))
;;     ;; (list (list 'quote (em-iter expr)) hasExpanded))
;;
;; (define (expand-macro-rec expr)
;;   (let ((res (expand-macro expr)))
;;     (if (head (tail res)) (expand-macro-rec  (head res)) (head res))))
;; ;; (define (expand-macro-rec expr)
;; ;;   (let ((res (expand-macro expr)))
;; ;;     (if (head (tail res)) (expand-macro-rec (head (tail (head res)))) (head res))))
;; ;; (expand-macro (body mylambda))
;; ;; (arg1 arg2 ...) => ({current} {next})
;; ;;                    (((arg1 (1 1) (arg2 (1 2)) ...)) {next})
;; ;; then lookup-arg param-map name => (pos, level) => add level to pos...
;;
;; (define (lookup-arg param-map name lvl)
;;   (if (null? param-map) '() 
;;     (let ((maybeIdx (assoc name (head param-map) )))
;;       (if (not maybeIdx) 
;;          (lookup-arg (tail param-map) name (+ 1 lvl)) 
;;          (list lvl (tail maybeIdx))  ))))
;;
;; (define sample-map '(((a 0) (b 1)) ((c 0))))
;;
;; (define (enumerate xs)
;;   (define (enum-inner i xs)
;;     (if (null? xs) '()
;;       (if (pair? xs)
;;           (cons (list (head xs) i) (enum-inner (add1 i) (tail xs)))
;;           (list (list xs i))
;;       )
;;     ))
;;   (enum-inner 0 xs))
;;
;;
;;     (define (do-symbol pmap x)
;;       (let ((arg-pos (lookup-arg pmap x 0))
;;             (env-ref (get-env-ref x)))
;;         (cond 
;;           [(not (null? arg-pos)) 
;;                 (let ((lvl (head arg-pos))(pos (head (head (tail arg-pos))))) 
;;                      (list __retrieveArg (+ 1 (* 2 lvl)) pos))]
;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;;           [else x])
;;       ))
;;
;;     (define (do-lambda param-list bs pmap)
;;       (let ((pmap1 (cons (enumerate param-list) pmap))) 
;;           ;; (flatten (list (list  'lambda) (list param-list) (do-iter pmap1 bs) ))))
;;         ;; (do-iter pmap1 bs)))
;;       `(lambda ,param-list ,@(do-iter pmap1 bs))))
;;
;;     (define (do-iter pmap x)
;;         (cond 
;;             [(symbol? x) (do-symbol pmap x)]
;;             [(and (pair? x) (equal? 'quasiquote (head x))) `(quasiquote ,(second x))]
;;             [(and (pair? x) (equal? 'quote (head x))) `(quote ,(second x))]
;;             [(and (pair? x) (equal? 'set! (head x))) `(set! ,(second x) ,(do-iter pmap (third x)))]
;;             [(and (pair? x) (equal? 'put! (head x))) `(put! ,(second x) ,(do-iter pmap (third x)))]
;;             [(and (pair? x) (equal? 'lambda (head x))) (do-lambda (second x) (tail (tail x)) pmap)]
;;             ;; [(and (pair? x) (increments-level? (head x))) (map (lambda (y) (do-iter (+ 1 level) y)) x)]
;;             [(pair? x) (map (lambda (y) (do-iter pmap y)) x)]
;;             [else x]))
;;
;; (define (do-lexical-scoping proc)
;;     (let ((expanded (expand-macro-rec (body proc)))
;;           (param-map (list (enumerate (args proc)))))
;;     (do-iter param-map expanded)))
;;
;;
;; (define (foo2 x . xs) 
;;     xs)
;; (define (foo1 x y) 
;;     ((lambda (x) (+ x y)) (+ y x)))
;;
;; (define (testquote)
;;   '(+ foo1 b))
;;
;; (define (testquasi)
;;   `(+ ,(foo1 1 2) b))
;;
;; ;; (define (do-lexical-scoping proc)
;; ;;     (define expanded (expand-macro-rec (body proc)))
;; ;;     (define _args (args proc))
;; ;;     (define (increments-level? x) 
;; ;;       (or (equal? x 'define) (equal? x 'let)))
;; ;;     (define (do-symbol level x)
;; ;;       (let ((arg-pos (pos x _args))
;; ;;             (env-ref (get-env-ref x)))
;; ;;         (cond 
;; ;;           [(not (eq? -1 arg-pos)) (list __retrieveArg level arg-pos)]
;; ;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;; ;;           [else x])
;; ;;       ))
;; ;;     (define (do-lambda param-list bs) `(lambda ,param-list ,@(iter 1 bs)))
;; ;;     (define (iter level x)
;; ;;         (cond 
;; ;;             [(symbol? x) (do-symbol level x)]
;; ;;             [(and (pair? x) (equal? 'lambda (head x))) (do-lambda (second x) (tail (tail x)))]
;; ;;             [(and (pair? x) (increments-level? (head x))) (map (lambda (y) (iter (+ 1 level) y)) x)]
;; ;;             [(pair? x) (map (lambda (y) (iter level y)) x)]
;; ;;             [else x]))
;; ;;     (iter 1 expanded))
;;
;; ;; (define (do-lexical-scoping proc)
;; ;;     (define expanded (expand-macro-rec (body proc)))
;; ;;     (define args (args proc))
;; ;;     (define (increments-level? x) 
;; ;;       (or (equal? x 'define) ))
;; ;;     (define (do-symbol level x)
;; ;;       (let ((arg-pos (pos x args))
;; ;;             (env-ref (get-env-ref x)))
;; ;;         (cond 
;; ;;           [(not (eq? -1 arg-pos)) (list __retrieveArg level arg-pos)]
;; ;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;; ;;           [else x])
;; ;;       ))
;; ;;     (define (iter level x)
;; ;;         (match x 
;; ;;             ;; [(and (pair? x) (macro? (head x))) (begin (set! hasExpanded #t) (expand (head x) (tail x)))]
;; ;;             [(? symbol?) (do-symbol level x)]
;; ;;             ;; [(and (pair? x) (increments-level? (head x))) (map (lambda (y) (iter (+ 1 level) y)) x)]
;; ;;             [(? pair?) (map (lambda (y) (iter level y)) x)]
;; ;;             [x x]))
;; ;;     (iter 1 expanded))
;;
;;
;; ;; (define (last-item l)
;; ;;     (match l
;; ;;       [(cons lst '()) lst]
;; ;;       [(cons fst rst) (last-item rst)]))
;;
;; (define (optimize proc) 
;;   (set-body proc (do-lexical-scoping proc)))
;;
;;
;; (define (foreach f lst)
;;   (cond
;;     [(empty? lst) empty]
;;     [else (begin (f (first lst))
;;                 (foreach f (rest lst)))]))
;;
;; (displaynl "reached stage 1. optimizing...")
;;
;;
;; (time (let ((procs (getAllProcs)))
;;         (foreach optimize procs)))
;;
;; (set! OptimizerAvailable #t)
;; (set! hook-optimize optimize)
;; (displaynl "...done")
;;
;; (define-macro (foo expr)
;;   (case (car expr)
;;     ((+) (set-car! expr '-))
;;     ((-) (set-car! expr '+)))
;;   expr)
;;
;; (define-macro (foo expr)
;;   (list (case (car expr) ((+) '-) ((-) '+))
;;         (cadr expr)
;;         (caddr expr)))
;;
;; (define-macro (when expr body)
;;   (let ((tmp (gensym)))
;;     `(let ((,tmp ,expr))
;;        (if ,tmp
;;            (begin
;;              ,@body) ))))
;;
;; (define-macro (aif test true false)
;;   `(let ((it ,test))
;;      (if it
;;          ,true
;;          ,false)))
;; ;;; --------------------------------------------------
;; ( define test1
;;     (lambda (x) 
;;       (let ((y 3)) x)))
;;
;; ( define test2
;;     (lambda (x) 
;;       (let ((x 3)) x)))
;;
;; ;; ( define test3
;; ;;     (lambda (x) 
;; ;;       (let ((x 3)) (+ x z))))
;; ;;
;; ( define test4
;;     (lambda (x) 
;;       (let ((x 3)) (+ x 1))
;;       x))
;;
;; ;; also nested lambdas...
;;
;; ;; (define mylambda 
;; ;;   (lambda (x) (+ 1 1) (aif (zero? x) (add1 x) (aif (zero? x) 1 3)) ))
;; ;; (let ((alist '((a . 10) (b . 20) (c . 30))))
;; ;;   (aif (assoc 'a alist)
;; ;;        (begin
;; ;;          (display (cdr it))
;; ;;          (newline)) (begin)))
;; ;; (define-macro (alist . body)
;; ;;   (if (null? body)
;; ;;       ()
;; ;;       `(cons (cons ,(car body) ,(cadr body)) (alist ,@(cddr body)))))
;;
;; ;; (alist "foo" 10 "bar" 20 "baz" 30)
;;
;; ;; (define-macro (when expr body)
;; ;;   `(let ((tmp ,expr))
;; ;;      (if tmp
;; ;;          (begin
;; ;;            ,@body)
;; ;;          (begin))))
;; ;;
;; ;; (let ((tmp 1000))
;; ;;   (when (= tmp 1000)
;; ;;     (display tmp)
;; ;;     (newline)))
;; ;; (define-macro (when test . body)
;; ;;   `(if ,test 
;; ;;        (begin
;; ;;          ,@body) (begin)))
;; ;;
;; ;; (when (zero? 0) (display "x"))
;; ;; (if ((zero? x ) ) (begin ((display "x" ) (display " = " ) (display "zero" ) (newline ) ) ) )
;; ;;; -------------------------------------------
;; ;; (define __transform 
;; ;;   (lambda () 
;; ;;     (foreach patterns if matches syntax then return template)))
;; ;;
;; ;; (define-syntax while
;; ;;   (syntax-rules (=> ? )
;; ;;     ((while condition body ...) <template>)
;; ;;     ((while condition body ...) <template1>)))
;; ;;
;; ;; (define-syntax while
;; ;;   (syntax-rules ()
;; ;;     ((while condition body ...)
;; ;;      (let loop ()
;; ;;        (if condition
;; ;;            (begin
;; ;;              body ...
;; ;;              (loop))
;; ;;            #f)))))
;; ;;; -------------------------------------------
;;
;;
;;
;; ;; (define (call/cc x) (call/cc x))
;; ;; (define (call-with-current-continuation x) (call/cc x))
;;
;;
;; (define (my-length lst)
;;   ; local function iter:
;;   (define (iter lst len)
;;     (cond
;;       [(empty? lst) len]
;;       [else (iter (rest lst) (+ len 1))]))
;;   ; body of my-length calls iter:
;;   (iter lst 0))
;;
;; (define (reverse l)
;;   (define (iter in out)
;;     (if (pair? in)
;;         (iter (cdr in) (cons (car in) out))
;;         out))
;;   (iter l '()))
;;
;; ;; (define (sub1 n) (- n 1))
;; ;; (let fac ([n 10])
;; ;;     (if (zero? n)
;; ;;         1
;; ;;         (* n (fac (sub1 n)))))
;;
;; ;(append '(1 2 3) '(4 5 6))
;; ;; (+ 2 3 4 5)
;; (define (length xs)
;;         (if (null? xs) 0
;;             (+ 1 (length (cdr xs)))))
;;
;; (define (member? x list)
;;      (if (null? list) #f                                ;(1)
;;          (if (equal? x (car list)) #t                   ;(2)
;;               (member? x (cdr list)))))                 ;(3)
;;
;;
;; (define (make-list n . f) 
;;      (if (= 0 n) '()
;;        (if (null? f) (cons 0 (make-list (- n 1)))
;;             (cons (car f) (make-list (- n 1) (car f)))
;;          )))
;;
;; ;; (define (remove-dups l)
;; ;;   (cond
;; ;;     [(empty? l) empty]
;; ;;     [(empty? (rest l)) l]
;; ;;     [else
;; ;;      (let ([i (first l)])
;; ;;        (if (equal? i (first (rest l)))
;; ;;            (remove-dups (rest l))
;; ;;            (cons i (remove-dups (rest l)))))]))
;; ;; (remove-dups (list "a" "b" "b" "b" "c" "c"))
;;
;;
;;      ; (define retry #f)
;;      ; (define factorial
;;      ;   (lambda (x)
;;      ;     (if (zero? x)
;;      ;         (call/cc (lambda (k) (set! retry k) 1))
;;      ;         (* x (factorial (- x 1))))))
;;      ; (define (factorial x)
;;      ;     (if (zero? x)
;;      ;         (call/cc (lambda (k) (set! retry k) 1))
;;      ;         (* x (factorial (- x 1)))))
;;      ; (factorial 5)
;;
;;         ; (define (fact n r)
;;         ;   (cond
;;         ;     [(zero? n) r]
;;         ;     [else (fact (- n 1) (* r n))]
;;         ;   )
;;         ; )
;;         ; (fact 12 1)
;;     ; (define (foo x . xs) xs)
;;     ; (foo 3 4 5 6 7 8)
;;     ; ((lambda xs xs) 1 2 3 4 5)
;;
;;         ; (define (add1 a) (+ a 1))
;;         ; (fact 3000 1)
;;         ; (fact 50 1)
;;
;;
;; ;; (define (myadd n r)
;; ;;   (cond
;; ;;     [(zero? n) r]
;; ;;     [else (myadd (- n 1) (* r n))]
;; ;;   )
;; ;; )
;; ; (myadd 5 1)
;; ; (+ 1 (call/cc (lambda (cc) (+ 20 300))))
;; ; (+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)) 1)))
;; ; (define foo (+ 1 (call/cc (lambda (cc) (+ 20 (cc 300)))))
;; ; (+ 1  ((lambda (cc) (+ 20 300)) 5))
;; ; (((call/cc (lambda (k) k)) (lambda (x) x)) "HEY!")
;; ; (define x 12)
;; ; (set! x (+ 1 x))
;; ; x
;; ; (set! i-am-not-defined 10)
;; ; '(+ 2 3) '(quote (1 2 . (3))) '(you can 'me) '(+ 2 3)
;; ; (+ 2 3)
;; ; (define (add1 a) (+ a 5) (+ a 1)) (add1 3)
;; ; ((lambda (a b) (+ b a) ) 3 4)
;; ; (define pi 3) (define (add a b) (+ a (add1 b))) (add pi pi)
;; ; (define pi 3) (+ pi 2)
;; ; (+ 2 3) (+) (+ 1) (+ (+ 2 3) 3)
;; ; (+ 2 3) (define (pi x y) (+ x y)) (pi 0 3) (1 2) ()
;; ; (define (pi x y) (+ x y)) (pi 2 3)
;; ; (define (cond . xs) (
;; ;    (if (head (head xs))
;; ;        (tail (head xs))
;; ;        (cond (tail xs))
;; ;    )
;; ; ))
;; ; (cond '[(zero? 0) 1] '[else 5])
;; ; 1 2 (begin 1 2 5)
;; ;(begin
;; ;(define x 10)
;; ;x)
;; ; (+ 1 2 3 4 5)
;; ; (zero? 0)
;; ; (add1  3)  (add1 5) )
;; ; ((lambda (x y) (+ x y)) 3 4)
;; ; (if #f 1 2)
;; ; (add1 5)
;; ; (define (add2 a) (add1 a))
;; ; (cond [#f 0] [else 3])
;; ; (cond [else 1] )
;; ; (define (add a b) (+ a b)) (add 2 3)
;; ; (cond [(zero? 0) 1] [else 5])
;;  (define (factorial n)
;;       (if (zero? n)
;;          1
;;         (* n (factorial (- n 1)))
;;       )
;;     )
;; ;  (factorial 5)
;; ; (define (mysumm n)
;; ;      (cond
;; ;        [(zero? n) 1]
;; ;        [else (* 1 (add1 (- n 1)))] ; Recursive step: n * factorial of (n-1)
;; ;      )
;; ;    )
;; ;  (mysumm 0)
;;
;; ; FIXME: let begin ...
;;
;; ; TODO: make the interpereter work
;;
;; ;;; (define interpret #f)
;; ;;; (let ()
;; ;;;   (begin
;; ;;;   ;; primitive-environment is an environment containing a small
;; ;;;   ;; number of primitive procedures; it can be extended easily
;; ;;;   ;; to include additional primitives.
;; ;;;   (define primitive-environment
;; ;;;     (list (cons 'apply apply)
;; ;;;           (cons 'assq assq)
;; ;;;           (cons 'call/cc call/cc)
;; ;;;           (cons 'car car)
;; ;;;           (cons 'cadr cadr)
;; ;;;           (cons 'caddr caddr)
;; ;;;           (cons 'cadddr cadddr)
;; ;;;           (cons 'cddr cddr)
;; ;;;           (cons 'cdr cdr)
;; ;;;           (cons 'cons cons)
;; ;;;           (cons 'eq? eq?)
;; ;;;           (cons 'list list)
;; ;;;           (cons 'map map)
;; ;;;           (cons 'memv memv)
;; ;;;           (cons 'null? null?)
;; ;;;           (cons 'pair? pair?)
;; ;;;           (cons 'read read)
;; ;;;           (cons 'set-car! set-car!)
;; ;;;           (cons 'set-cdr! set-cdr!)
;; ;;;           (cons 'symbol? symbol?)))
;; ;;; 
;; ;;;   ;; new-env returns a new environment from a formal parameter
;; ;;;   ;; specification, a list of actual parameters, and an outer
;; ;;;   ;; environment.  The symbol? test identifies "improper"
;; ;;;   ;; argument lists.  Environments are association lists,
;; ;;;   ;; associating variables with values.
;; ;;;   (define new-env
;; ;;;     (lambda (formals actuals env)
;; ;;;       (cond
;; ;;;         ((null? formals) env)
;; ;;;         ((symbol? formals) (cons (cons formals actuals) env))
;; ;;;         (else
;; ;;;          (cons (cons (car formals) (car actuals))
;; ;;;                (new-env (cdr formals) (cdr actuals) env))))))
;; ;;; 
;; ;;;   ;; lookup finds the value of the variable var in the environment
;; ;;;   ;; env, using assq.  Assumes var is bound in env.
;; ;;;   (define lookup
;; ;;;     (lambda (var env)
;; ;;;       (cdr (assq var env))))
;; ;;; 
;; ;;;   ;; assign is similar to lookup but alters the binding of the
;; ;;;   ;; variable var in the environment env by changing the cdr of
;; ;;;   ;; association pair
;; ;;;   (define assign
;; ;;;     (lambda (var val env)
;; ;;;       (set-cdr! (assq var env) val)))
;; ;;; 
;; ;;;   ;; exec evaluates the expression, recognizing all core forms.
;; ;;;   (define exec
;; ;;;     (lambda (exp env)
;; ;;;       (cond
;; ;;;         ((symbol? exp) (lookup exp env))
;; ;;;         ((pair? exp)
;; ;;;          (case (car exp)
;; ;;;            ((quote) (cadr exp))
;; ;;;            ((lambda)
;; ;;;             (lambda vals
;; ;;;               (let ((env (new-env (cadr exp) vals env)))
;; ;;;                 (let loop ((exps (cddr exp)))
;; ;;;                    (if (null? (cdr exps))
;; ;;;                        (exec (car exps) env)
;; ;;;                        (begin
;; ;;;                           (exec (car exps) env)
;; ;;;                           (loop (cdr exps))))))))
;; ;;;            ((if)
;; ;;;             (if (exec (cadr exp) env)
;; ;;;                 (exec (caddr exp) env)
;; ;;;                 (exec (cadddr exp) env)))
;; ;;;            ((set!)
;; ;;;             (assign (cadr exp)
;; ;;;                     (exec (caddr exp) env)
;; ;;;                     env))
;; ;;;            (else
;; ;;;             (apply (exec (car exp) env)
;; ;;;                    (map (lambda (x) (exec x env))
;; ;;;                         (cdr exp))))))
;; ;;;         (else exp))))
;; ;;; 
;; ;;;   ;; interpret starts execution with the primitive environment.
;; ;;; (set! interpret
;; ;;;     (lambda (exp)
;; ;;;       (exec exp  primitive-environment)))))
;;
;;
