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

;; syntax:
;; https://www.scheme.com/syntax-case/old-psyntax.html

;; Nice to have:
;; https://docs.racket-lang.org/srfi/srfi-std/srfi-78.html (lightweight testing)

;; (define (call-with-current-continuation f) (call/cc f))
;; (define (call/cc f) (call/cc f))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
;; ;;
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define *current-output-port* (createStdout))
(define (current-output-port) *current-output-port*)


;; (write-string string port start) procedure
;; (write-string string port start end ) procedure
(define writeString
  (case-λ 
    [(s) (writeString2 s (current-output-port))]
    [(s p) (writeString2 s p)]))

(define newline 
  (case-λ 
    [() (newline1 (current-output-port))]
    [(p) (newline1 p)]))

(define display
  (case-λ 
    [(x) (display2 x (current-output-port))]
    [(x p) (display2 x p)]))

(define write
  (case-λ 
    [(x) (write2 x (current-output-port))]
    [(x p) (write2 x p)]))

(define write-u8
  (case-λ 
    [(x) (writeU82 x (current-output-port))]
    [(x p) (writeU82 x p)]))

(define displaynl
  (case-λ 
    [(x) (display2 x (current-output-port)) (newline)]
    [(x p) (display2 x p) (newline p)]))

(define flush-output-port
  (case-λ 
    [() (flush-output-port1 (current-output-port))]
    [(p) (flush-output-port1 p)]))

(define (read x) (read x))

;; temprorary define only binary functions
(define (+ a b) (+ a b)) 
(define (- a b) (- a b)) 
(define (/ a b) (/ a b)) 
(define (* a b) (* a b)) 
(define (= a b) (= a b)) 
(define (error x) (error x))

(define (zero? x) (zero? x))
(define (pair? x) (pair? x))
(define (null? x) (null? x))
(define (list? x) (list? x))
(define (positive? x) (positive? x))
(define (boolean? x) (boolean? x))
(define (symbol? x) (symbol? x))
(define (integer? x) (integer? x))
(define (string? x) (string? x))
(define (real? x) (real? x))
(define (not x) (not x))


;; (define (displaynl x) (displaynl x))
;; (define (print x) (print x))

(define (car a ) (car a ))
(define (cdr a ) (cdr a ))
(define (number->string a ) (number->string a ))
(define (string->symbol a ) (string->symbol a ))
(define string-append 
  (case-lambda 
    [() ""]
    [(x) (string-append x)]
    [(x y) (string-append x y)]
    [(x y z) (string-append x y z)]
    [(x y z v) (string-append x y z v)]
    [(x y z v a) (string-append x y z v a)]
    [(x y z v a b) (string-append x y z v a b)]
    ))


(define (eq? a b) (eq? a b))
(define (eqv? a b) (eqv? a b))
(define (equal? a b) (equal? a b))
(define (cons a b) (cons a b))
(define (set-car! a b) (set-car! a b))
(define (set-cdr! a b) (set-cdr! a b))
(define (apply a b) (apply a b))



;; and how to mark currently executed λ in GC? (it shoould be in stack!)
;; ((λ (x . xs) xs) 3 4 5)

;; (set! foo (λ (x . xs) xs))

;; (if #t (+ 1 2 3) (+ 1 2 3))

;; '(1 2 (+ 2 3) "" '(3 4) 5 (6 . 7))
;; (set! d 5)
;; (set! f (λ (a ) ((λ (a c) (+ a c d)) (+ a 1) 4)))
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
(define (head x) (car x))
(define (tail x) (cdr x))
(define (rest x) (cdr x))
(define nil '())
(define empty '())
(define (empty? x) (null? x))
(define (nil? x) (null? x))

(define (append2 list1 list2)
        (if (null? list1) list2
            (cons (car list1) (append (cdr list1) list2))))

(define (append . xs)
        (if (null? xs) '()
            (if (null? (tail xs)) (head xs)
            (append2 (head xs) (apply append (tail xs))))))

;; (define (map f lst)
;;   (cond
;;     [(empty? lst) empty]
;;     [else (cons (f (first lst))
;;                 (map f (rest lst)))]))

(define (map1 f lst)
  (cond
    [(empty? lst) empty]
    [else (cons (f (first lst))
                (map1 f (rest lst)))]))
(define (map f . xs)
  (cond 
    [(empty? (head xs)) empty]
    [else (cons (apply f (map1 car xs)) 
                (apply map (cons f (map1 cdr xs))))]
    ))

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
(define list (λ x x))

(define (list-ref xs i) 
  (if (zero? i) (head xs) (list-ref (tail xs) (- i 1))))
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

;; (define-macro (foo expr)
;;   ;; (display expr)
;;   (begin (if (equal? '+ (car expr))
;;     (set-car! expr '-)
;;     (set-car! expr '+))
;;   expr))

;; ;;
;; ;;
;; ;; (define (id x) x)
;; ;; ;; almost working version
;; ;; ;;
;; ;;
;; ;;
;; ;; ;;     (let ([id val-expr] ...) body ...+)
;; ;; ;; (let proc-id ([id init-expr] ...) body ...+)
;; ;;

;; FIXME: named λ is probably wrong..

(define-macro (let bindings . body)
  (if (symbol? bindings)
      `(letrec ((,bindings (λ ,(map first (head body)) ,@(tail body))))
        (,bindings ,@(map second (head body))))
      `((λ ,(map first bindings) ,@body)
        ,@(map second bindings)) ))

;; (define-macro (let bindings . body)
;;   (if (symbol? bindings)
;;       `(begin (define ,bindings (λ ,(map first (head body)) ,@(tail body)))
;;         (,bindings ,@(map second (head body))))
;;       `((λ ,(map first bindings) ,@body)
;;         ,@(map second bindings)) ))
;;
;; like let but evaluates val-exprs one-by-one
(define-macro (let* bindings . body)
        (if (null? bindings)
              `(begin ,@body)
              `(let (,(head bindings))
                    (let* ,(tail bindings) ,@body))))
;; ;; ;; example:
;; (let* ([x 1]
;;          [y (+ x 1)])
;;     (list y x))
;;
(define-macro (letrec bindings . body)
    `(let ,(map (λ (v) (list (head v) #f))  bindings)
       ,@(map (λ (v) `(set! ,(head v) ,(head (tail v)))) bindings)
         (let () ,@body)))


(define-macro (letrec* bindings . body)
    `(let ,(map (λ (v) (list (head v) #f))  bindings)
       ,@(map (λ (v) `(set! ,(head v) ,(head (tail v)))) bindings)
         (let () ,@body)))

    ;; `(let ,(map (λ (v) (list (head v) #f))  bindings) ,@(map (λ (v) `(set! ,(head v) ,(head (tail v)))) bindings) (let () ,@body))
;;
;; ;; ;; (def-scheme-macro letrec (bindings &rest body)
;; ;; ;;  '(let ,(mapcar #'(λ (v) (list (first v) nil)) bindings)
;; ;; ;;     ,@(mapcar #'(λ (v) '(set! . ,v)) bindings)
;; ;; ;;    .,body))
;;
;; ;; example usage:
 ;; (letrec ([is-even? (λ (n)
 ;;                       (or (zero? n)
 ;;                           (is-odd? (sub1 n))))]
 ;;           [is-odd? (λ (n)
 ;;                      (and (not (zero? n))
 ;;                           (is-even? (sub1 n))))])
 ;;    (is-odd? 11))
;;
;; ;; (define-macro (gen-fun pr)
;; ;;         `set! ,pr (λ xs `(,pr ,@xs)))
;; ;; (define (+ . xs)  `(+ ,@xs))
;;

(define (foobaz . xs) xs)
(define fooball (lambda xs xs))

(define-macro (and . xs) 
    (cond [(null? xs) #t]
          [(null? (tail xs)) (head xs)]
          [else `(if ,(head xs) (and ,@(tail xs)) #f)]))
;;
(define-macro (or . xs) 
    (let ((x (gensym)))
        (cond [(null? xs) #f]
              [(null? (tail xs)) (head xs)]
              [else 
                `(let ((,x ,(head xs))) (if ,x ,x (or ,@(tail xs))) )])))
;; ;;
;;
(define-macro (case x . xs) 
    (if (null? xs) `(begin)
      (if (equal? 'else (head (head xs))) `(begin ,@(tail (head xs)))
        (let ((y (gensym)))
        `((λ (,y) (if (member ,y (quote ,(head (head xs)))) (begin ,@(tail (head xs))) (case ,x ,@(tail xs))) ) ,x)))))
;;
;; ;; (define (tq a b) `(1 2 ,a ,b))
;; ;; (define-macro (let bindings . body)
;; ;;     (append   (list (flatten (list (list  'lambda) (list (map first bindings)) body (list ))))
;; ;;             (map second bindings) ))
;;
;; ;;
;; ;;
;; ;;       ;; `((λ ,(map first bindings) ,@body)
;; ;;       ;;   ,@(map second bindings)) )
;; ;; ;; TODO: let*, letrec
;; ;;
;; ;;
;; ;; ;; (expand-macro '(let ((r 1)) r))
;; ;; ;; (let x ((a 1)) a)
;; ;;
;; doesn't work, but should.. named let not working... 
;; maybe expand to letrec?
;; (let fac ([n 10])
;;         (if (zero? n)
;;             1
;;             (* n (fac (sub1 n)))))

(define gensym-id 0)
;;
;;
(define (gensym) 
  (let ((i gensym-id))
    (begin 
      (set! gensym-id (+ 1 i))
      (string->symbol (string-append "gensym-" (number->string i))))))



;; 
(define dynamic-wind #f)
(define call/cc #f)
(define call-with-current-continuation call/cc)
(let ((winders '()))
  (define common-tail
    (λ (x y)
      (let ((lx (length x)) (ly (length y)))
        (do ((x (if (> lx ly) (list-tail x (- lx ly)) x) (cdr x))
             (y (if (> ly lx) (list-tail y (- ly lx)) y) (cdr y)))
            ((eq? x y) x)))))
  (define do-wind
    (λ (new)
      (let ((tail (common-tail new winders)))
        (let f ((l winders))
          (if (not (eq? l tail))
              (begin
                (set! winders (cdr l))
                ((cdar l))
                (f (cdr l)))))
        (let f ((l new))
          (if (not (eq? l tail))
              (begin
                (f (cdr l))
                ((caar l))
                (set! winders l)))))))
  (set! call/cc
      (λ (f)
        (raw-call/cc (λ (k)
             (f (let ((save winders))
                  (λ (x)
                    (if (not (eq? save winders)) (do-wind save))
                    (k x))))))))
  (set! call-with-current-continuation call/cc)
  (set! dynamic-wind
    (λ (in body out)
      (in)
      (set! winders (cons (cons in out) winders))
      (let ((ans (body)))
        (set! winders (cdr winders))
        (out)
        ans))))





;; fixme: looks like cc should accept N arguments
;; (define (values . things)
;;    (call-with-current-continuation
;;        (λ (cont) (apply cont things))))

;; it's working!!!
;; (let* ((yin
;;          ((λ (cc) (display #\@) cc) (call/cc (λ (c) c))))
;;        (yang
;;          ((λ (cc) (display #\*) cc) (call/cc (λ (c) c)))))
    ;; (yin yang))

;; (do ((〈variable1〉 〈init1〉 〈step1〉) syntax
;; . . . )
;; (〈test〉 〈expression〉 . . . )
;; 〈command〉 . . . )
;; (define-macro (do init test . cmds)) 


;; (define-syntax do
;; (syntax-rules ()
;; ((do ((var init step ...) ...)
;; (test expr ...)
;; command ...)
;; (letrec
;; ((loop
;; (λ (var ...)
;; (if test
;; (begin
;; (if #f #f)
;; expr ...)
;; (begin
;; command
;; ...
;; (loop (do "step" var step ...)
;; ...))))))
;; (loop init ...)))
;; ((do "step" x)
;; x)
;; ((do "step" x y)
;; y)))

;; (define (simple-do) 
;;   (letrec* ))

;; (define (or . xs) 
;;     (let ((x (gensym)))
;;         (cond [(null? xs) #f]
;;               [(null? (tail xs)) (head xs)]
              ;; [else 
              ;;   `(let ((,x ,(head xs))) (if ,x ,x (or ,@(tail xs))) )])))


;; (define-macro (
 ;; (let ((x 5)) 
 ;;   (letrec* ((foo (λ (y ) (bar x y ) ) ) 
 ;;             (bar (λ (a b ) (+ (* a b ) a ) ) ) ) 
 ;;     (foo (+ x 3 ) ) ))

;;'(if (null? (tail xs ) ) (begin #t ) 5 )
;; ((λ (ys)  (if (null? (tail ys ) ) #t  5)  ) '(1 2 3 4 5)) 

;; test case for ap and tailcals and closures..
;; ((λ (zs) ((λ (x ) (displaynl zs) ) 5 )) 8) 

;; works ok -> ((λ (zs) ((λ (x ) (displaynl zs) ) 5 ) (+ 1 1)) 8) 


;; (set! foobar (λ (zs) (λ (x ) (displaynl zs) )  ) )
;; ;; ;; ;;; --------------------------------------------------
;; ;; ;;
;; ;; ;; ;;
;; ;; ;; ;;  
;; ;; ;; ;; pattern	 	=	 	name
;; ;; ;; ;;  	 	|	 	literal-constant
;; ;; ;; ;;  	 	|	 	(cons pattern pattern)
;; ;; ;; ;;  	 	|	 	(list pattern ...)
;; ;; ;; ;;  	 	|	 	(name pattern ...)
;; ;; ;; ;;  	 	|	 	(? name)
;; ;; ;;
;; ;;
;; ;; ;; (displaynl "hello")
;; ;;
;; ;; ;; (define (matches-internal? expr pattern bindings)
;; ;; ;;   ;; (displaynl pattern)
;; ;; ;;   (cond 
;; ;; ;;     ;; [(null? pattern) (list #t bindings)]
;; ;; ;;     [(and (or (number? pattern) (null? pattern) (string? pattern) (boolean? pattern)) (equal? pattern expr)) 
;; ;; ;;          (list #t bindings)]
;; ;; ;;     [(and (pair? pattern) (equal? 'quote (head pattern)) (equal? (head (tail pattern)) expr)) (list #t bindings)]
;; ;; ;;     [(and (pair? pattern) (equal? 'cons (head pattern)) (pair? expr))
;; ;; ;;         (let ((res (matches-internal? (head expr) (head (tail pattern)) bindings )))
;; ;; ;;           (if (head res) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail res)) ) (list #f .()) ))]
;; ;; ;;     [(and (pair? pattern) (equal? '? (head pattern))) 
;; ;; ;;       (if ((eval (head (tail pattern))) expr) (list #t bindings)   (list #f .()))]
;; ;; ;;     [(symbol? pattern) (list #t (cons (list pattern expr) bindings))]
;; ;; ;;     [else (list #f .())]))
;; ;; ;;
;; ;; ;; (define (matches? expr _p) ;; right, <--- we can't eval here as it has it's own env. and won't capture free vars
;; ;; ;;   (let ((pattern (head _p))
;; ;; ;;         (body-expr (head (tail _p))))
;; ;; ;;   (let ((res (matches-internal? expr pattern '())))
;; ;; ;;     (if (head res) (begin (addToEnv (second res)) (list #t (eval body-expr))) (list #f .()) ))))
;; ;; ;;   
;; ;; ;; ;;; hmmm, use displaynl luke
;; ;; ;; (define-macro (match case-expr . patterns) 
;; ;; ;;     (define (gen-match-rec expr patterns)
;; ;; ;;       (cond 
;; ;; ;;         [(null? patterns) `(error "no matching clause")]
;; ;; ;;         [else `(let ((res (matches? ,expr ,(list 'quote (head patterns))))) 
;; ;; ;;                  (if (head res) (second res) ,(gen-match-rec expr (tail patterns)))) ]))
;; ;; ;;         ;; [else (cons (gen-match expr (head patterns)) (gen-match-rec expr (tail patterns)))]))
;; ;; ;;     (if (null? patterns) (error "patterns are empty")
;; ;; ;;       (gen-match-rec case-expr patterns)))
;; ;;
;; ;;
;; ;;
;; ;; (define (let-test1 x bindings) 
;; ;;   (cond 
;; ;;     [(zero? x) (cons 1 bindings)]
;; ;;     [else (let ((rs (let-test1 (sub1 x) bindings))) rs ) 
;; ;;       ]))
;; ;; ;; this one is workingv
;; ;; (define (let-test2 x bindings)
;; ;; (begin 
;; ;;   (cond [(zero? x ) (cons 1 bindings ) ] 
;; ;;         [else ((λ (rs ) rs ) (let-test2 (sub1 x ) bindings ) ) ] ) ))
;; ;;
;; ;;
;; ;; (define (let-test3)  
;; ;;     (let ((x (let ((x 3)) x))) (let ((x x)) x) )) 
;; ;;       
;; ;;
;; ;; (define (let-test4 x bindings) 
;; ;;   (if  (zero? x) (cons 1 bindings)
;; ;;      (let ((rs (let-test1 (sub1 x) bindings))) rs ) 
;; ;;       ))
;; ;;
;; ;; (define (let-test5 x bindings) 
;; ;;   (if  (zero? x) (cons 1 bindings)
;; ;;      (let ((rs (cons 0 bindings))) rs ) 
;; ;;       ))
;; ;;
;; ;; (define (let-test6 x bindings) 
;; ;;   (if  (zero? x) (cons 1 bindings)
;; ;;      (let ((rs  bindings)) rs ) 
;; ;;       ))
;; ;; (define (let-test7 x brr) 
;; ;;      (let ((rs  brr)) rs ) 
;; ;;       )
;; ;; (define (matches-internal? expr pattern bindings)
;; ;;   ;; (displaynl pattern)
;; ;;   (cond 
;; ;;     ;; [(null? pattern) (list #t bindings)]
;; ;;     [(and (or (number? pattern) (null? pattern) (string? pattern) (boolean? pattern)) (equal? pattern expr)) 
;; ;;          (list #t bindings)]
;; ;;     [(and (pair? pattern) (equal? 'quote (head pattern)) (equal? (head (tail pattern)) expr)) (list #t bindings)]
;; ;;     [(and (pair? pattern) (equal? 'cons (head pattern)) (pair? expr))
;; ;;         (begin (define rs (matches-internal? (head expr) (head (tail pattern)) bindings ))
;; ;;           (if (head rs) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail rs)) ) (list #f .()) ))]
;; ;;         ;; (let ((rs (matches-internal? (head expr) (head (tail pattern)) bindings )))
;; ;;         ;;   (if (head rs) (matches-internal? (tail expr) (head (tail (tail pattern))) (head (tail rs)) ) (list #f .()) ))]
;; ;;     [(and (pair? pattern) (equal? '? (head pattern))) 
;; ;;       (if ((eval (head (tail pattern))) expr) (list #t bindings)   (list #f .()))]
;; ;;     [(symbol? pattern) (list #t (cons (list pattern expr) bindings))]
;; ;;     [else (list #f .())]))
;; ;;
;; ;; (define (matches? expr _p) ;; right, <--- we can't eval here as it has it's own env. and won't capture free vars
;; ;;   (let ((pattern (head _p))
;; ;;         (body-expr (head (tail _p))))
;; ;;   (let ((res (matches-internal? expr pattern '())))
;; ;;     (if (head res) (list #t (second res) body-expr) (list #f .()) ))))
;; ;;   
;; ;; ;; ;;; hmmm, use displaynl luke
;; ;; (define-macro (match case-expr . patterns) 
;; ;;     (define _res (gensym))
;; ;;     (define (gen-match-rec expr patterns)
;; ;;       (cond 
;; ;;         [(null? patterns) `(error "no matching clause")]
;; ;;         [else `(let ((,_res (matches? ,expr ,(list 'quote (head patterns))))) 
;; ;;                  (if (head ,_res) (begin (addToEnv (second ,_res)) (eval (third ,_res))) ,(gen-match-rec expr (tail patterns)))) ]))
;; ;;         ;; [else (cons (gen-match expr (head patterns)) (gen-match-rec expr (tail patterns)))]))
;; ;;     (if (null? patterns) (error "patterns are empty")
;; ;;       (gen-match-rec case-expr patterns)))
;; ;; ;;
;; ;; ;; (match '(1 2)
;; ;; ;;     ['four  1]
;; ;; ;;     ["four" 2]
;; ;; ;;     [#t  3]
;; ;; ;;     [(cons a (cons b '())) (+ a b)]
;; ;; ;;     [(cons 1 (cons 2 '())) 4]
;; ;; ;;     [4      "hello world"])
;; ;; ;; ;; ;;
;; ;; (define (last-item l)
;; ;;     (match l
;; ;;       [(cons lst '()) lst]
;; ;;       [(cons fst rst) (last-item rst)]))
;; ;; ;;; --------------------------------------------------
;; ;; ;; 
;; ;;
;; ;;
;; ;;
;; ;; (define (enumerate xs)
;; ;;   (define (enum-inner i xs)
;; ;;     (if (null? xs) '()
;; ;;       (if (pair? xs)
;; ;;           (cons (list (head xs) i) (enum-inner (add1 i) (tail xs)))
;; ;;           (list (list xs i))
;; ;;       )
;; ;;     ))
;; ;;   (enum-inner 0 xs))
;; ;;
;; ;;
;; ;;     (define (do-symbol pmap x)
;; ;;       (let ((arg-pos (lookup-arg pmap x 0))
;; ;;             (env-ref (get-env-ref x)))
;; ;;         (cond 
;; ;;           [(not (null? arg-pos)) 
;; ;;                 (let ((lvl (head arg-pos))(pos (head (head (tail arg-pos))))) 
;; ;;                      (list __retrieveArg (+ 1 (* 2 lvl)) pos))]
;; ;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;; ;;           [else x])
;; ;;       ))
;; ;;
;; ;;     (define (do-λ param-list bs pmap)
;; ;;       (let ((pmap1 (cons (enumerate param-list) pmap))) 
;; ;;           ;; (flatten (list (list  'lambda) (list param-list) (do-iter pmap1 bs) ))))
;; ;;         ;; (do-iter pmap1 bs)))
;; ;;       `(λ ,param-list ,@(do-iter pmap1 bs))))
;; ;;
;; ;;     (define (do-iter pmap x)
;; ;;         (cond 
;; ;;             [(symbol? x) (do-symbol pmap x)]
;; ;;             [(and (pair? x) (equal? 'quasiquote (head x))) `(quasiquote ,(second x))]
;; ;;             [(and (pair? x) (equal? 'quote (head x))) `(quote ,(second x))]
;; ;;             [(and (pair? x) (equal? 'set! (head x))) `(set! ,(second x) ,(do-iter pmap (third x)))]
;; ;;             [(and (pair? x) (equal? 'put! (head x))) `(put! ,(second x) ,(do-iter pmap (third x)))]
;; ;;             [(and (pair? x) (equal? 'λ (head x))) (do-λ (second x) (tail (tail x)) pmap)]
;; ;;             ;; [(and (pair? x) (increments-level? (head x))) (map (λ (y) (do-iter (+ 1 level) y)) x)]
;; ;;             [(pair? x) (map (λ (y) (do-iter pmap y)) x)]
;; ;;             [else x]))
;; ;;
;; ;; (define (do-lexical-scoping proc)
;; ;;     (let ((expanded (expand-macro-rec (body proc)))
;; ;;           (param-map (list (enumerate (args proc)))))
;; ;;     (do-iter param-map expanded)))
;; ;;
;; ;;
;; ;; (define (foo2 x . xs) 
;; ;;     xs)
;; ;; (define (foo1 x y) 
;; ;;     ((λ (x) (+ x y)) (+ y x)))
;; ;;
;; ;; (define (testquote)
;; ;;   '(+ foo1 b))
;; ;;
;; ;; (define (testquasi)
;; ;;   `(+ ,(foo1 1 2) b))
;; ;;
;; ;; ;; (define (do-lexical-scoping proc)
;; ;; ;;     (define expanded (expand-macro-rec (body proc)))
;; ;; ;;     (define _args (args proc))
;; ;; ;;     (define (increments-level? x) 
;; ;; ;;       (or (equal? x 'define) (equal? x 'let)))
;; ;; ;;     (define (do-symbol level x)
;; ;; ;;       (let ((arg-pos (pos x _args))
;; ;; ;;             (env-ref (get-env-ref x)))
;; ;; ;;         (cond 
;; ;; ;;           [(not (eq? -1 arg-pos)) (list __retrieveArg level arg-pos)]
;; ;; ;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;; ;; ;;           [else x])
;; ;; ;;       ))
;; ;; ;;     (define (do-λ param-list bs) `(λ ,param-list ,@(iter 1 bs)))
;; ;; ;;     (define (iter level x)
;; ;; ;;         (cond 
;; ;; ;;             [(symbol? x) (do-symbol level x)]
;; ;; ;;             [(and (pair? x) (equal? 'λ (head x))) (do-λ (second x) (tail (tail x)))]
;; ;; ;;             [(and (pair? x) (increments-level? (head x))) (map (λ (y) (iter (+ 1 level) y)) x)]
;; ;; ;;             [(pair? x) (map (λ (y) (iter level y)) x)]
;; ;; ;;             [else x]))
;; ;; ;;     (iter 1 expanded))
;; ;;
;; ;; ;; (define (do-lexical-scoping proc)
;; ;; ;;     (define expanded (expand-macro-rec (body proc)))
;; ;; ;;     (define args (args proc))
;; ;; ;;     (define (increments-level? x) 
;; ;; ;;       (or (equal? x 'define) ))
;; ;; ;;     (define (do-symbol level x)
;; ;; ;;       (let ((arg-pos (pos x args))
;; ;; ;;             (env-ref (get-env-ref x)))
;; ;; ;;         (cond 
;; ;; ;;           [(not (eq? -1 arg-pos)) (list __retrieveArg level arg-pos)]
;; ;; ;;           [(not (eq?  0 env-ref)) (list __retrieve env-ref)]
;; ;; ;;           [else x])
;; ;; ;;       ))
;; ;; ;;     (define (iter level x)
;; ;; ;;         (match x 
;; ;; ;;             ;; [(and (pair? x) (macro? (head x))) (begin (set! hasExpanded #t) (expand (head x) (tail x)))]
;; ;; ;;             [(? symbol?) (do-symbol level x)]
;; ;; ;;             ;; [(and (pair? x) (increments-level? (head x))) (map (λ (y) (iter (+ 1 level) y)) x)]
;; ;; ;;             [(? pair?) (map (λ (y) (iter level y)) x)]
;; ;; ;;             [x x]))
;; ;; ;;     (iter 1 expanded))
;; ;;
;; ;;
;; ;; ;; (define (last-item l)
;; ;; ;;     (match l
;; ;; ;;       [(cons lst '()) lst]
;; ;; ;;       [(cons fst rst) (last-item rst)]))
;; ;;
;; ;;
;; ;;
;; ;; (define (foreach f lst)
;; ;;   (cond
;; ;;     [(empty? lst) empty]
;; ;;     [else (begin (f (first lst))
;; ;;                 (foreach f (rest lst)))]))
;; ;;
;; ;; (displaynl "reached stage 1. optimizing...")
;; ;;
;; ;;
;; ;; (time (let ((procs (getAllProcs)))
;; ;;         (foreach optimize procs)))
;; ;;
;; ;; (set! OptimizerAvailable #t)
;; ;; (set! hook-optimize optimize)
;; ;; (displaynl "...done")
;; ;;
;; ;; (define-macro (foo expr)
;; ;;   (case (car expr)
;; ;;     ((+) (set-car! expr '-))
;; ;;     ((-) (set-car! expr '+)))
;; ;;   expr)
;; ;;
;; ;; (define-macro (foo expr)
;; ;;   (list (case (car expr) ((+) '-) ((-) '+))
;; ;;         (cadr expr)
;; ;;         (caddr expr)))
;; ;;
;; ;; (define-macro (when expr body)
;; ;;   (let ((tmp (gensym)))
;; ;;     `(let ((,tmp ,expr))
;; ;;        (if ,tmp
;; ;;            (begin
;; ;;              ,@body) ))))
;; ;;
;; ;; (define-macro (aif test true false)
;; ;;   `(let ((it ,test))
;; ;;      (if it
;; ;;          ,true
;; ;;          ,false)))
;; ;; ;;; --------------------------------------------------
;; ;; ( define test1
;; ;;     (λ (x) 
;; ;;       (let ((y 3)) x)))
;; ;;
;; ;; ( define test2
;; ;;     (λ (x) 
;; ;;       (let ((x 3)) x)))
;; ;;
;; ;; ;; ( define test3
;; ;; ;;     (λ (x) 
;; ;; ;;       (let ((x 3)) (+ x z))))
;; ;; ;;
;; ;; ( define test4
;; ;;     (λ (x) 
;; ;;       (let ((x 3)) (+ x 1))
;; ;;       x))
;; ;;
;; ;; ;; also nested lambdas...
;; ;;
;; ;; ;; (define myλ 
;; ;; ;;   (λ (x) (+ 1 1) (aif (zero? x) (add1 x) (aif (zero? x) 1 3)) ))
;; ;; ;; (let ((alist '((a . 10) (b . 20) (c . 30))))
;; ;; ;;   (aif (assoc 'a alist)
;; ;; ;;        (begin
;; ;; ;;          (display (cdr it))
;; ;; ;;          (newline)) (begin)))
;; ;; ;; (define-macro (alist . body)
;; ;; ;;   (if (null? body)
;; ;; ;;       ()
;; ;; ;;       `(cons (cons ,(car body) ,(cadr body)) (alist ,@(cddr body)))))
;; ;;
;; ;; ;; (alist "foo" 10 "bar" 20 "baz" 30)
;;
;; ;; right, displays #t because
;; ;; (define-macro (when expr . body)
;; ;;   `(let ((tmp ,expr))
;; ;;      (if tmp
;; ;;          (begin
;; ;;            ,@body)
;; ;;          )))
;; ;;
;; ;; (let ((tmp 1000))
;; ;;   (when (= tmp 1000)
;; ;;     (display tmp)
;; ;;     (newline)))
;;
(define-macro (when test . body)
  `(if ,test 
       (begin
         ,@body) (begin)))

(define-macro (unless test . branch)
    (list 'if
          (list 'not test)
          (cons 'begin branch) '(begin)))
;; ;; ;;
;; (when (zero? 0) (display "x"))
;; ;; ;; (if ((zero? x ) ) (begin ((display "x" ) (display " = " ) (display "zero" ) (newline ) ) ) )
;; ;;
;; ;;
;; ;;
;; ;; (define (my-length lst)
;; ;;   ; local function iter:
;; ;;   (define (iter lst len)
;; ;;     (cond
;; ;;       [(empty? lst) len]
;; ;;       [else (iter (rest lst) (+ len 1))]))
;; ;;   ; body of my-length calls iter:
;; ;;   (iter lst 0))
;; ;;
;; ;; (define (reverse l)
;; ;;   (define (iter in out)
;; ;;     (if (pair? in)
;; ;;         (iter (cdr in) (cons (car in) out))
;; ;;         out))
;; ;;   (iter l '()))
;; ;;
;; ;; ;; (define (sub1 n) (- n 1))
;; ;; ;; (let fac ([n 10])
;; ;; ;;     (if (zero? n)
;; ;; ;;         1
;; ;; ;;         (* n (fac (sub1 n)))))
;; ;;
;; ;; ;(append '(1 2 3) '(4 5 6))
;; ;; ;; (+ 2 3 4 5)
(define (length xs)
        (if (null? xs) 0
            (+ 1 (length (cdr xs)))))

;; ;; (define (member? x list)
;; ;;      (if (null? list) #f                                ;(1)
;; ;;          (if (equal? x (car list)) #t                   ;(2)
;; ;;               (member? x (cdr list)))))                 ;(3)
;; ;;
;; ;;
;; ;; (define (make-list n . f) 
;; ;;      (if (= 0 n) '()
;; ;;        (if (null? f) (cons 0 (make-list (- n 1)))
;; ;;             (cons (car f) (make-list (- n 1) (car f)))
;; ;;          )))
;; ;;
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
;; ;;
;; ;;
      ;;
      ;; (define retry #f)
      ;; (define (factorial x)
      ;;     (if (zero? x)
      ;;         (call/cc (λ (k) (set! retry k) 1))
      ;;         (* x (factorial (- x 1)))))
      ;; (factorial 5)

;; ;;
;; ;;         ; (define (fact n r)
;; ;;         ;   (cond
;; ;;         ;     [(zero? n) r]
;; ;;         ;     [else (fact (- n 1) (* r n))]
;; ;;         ;   )
;; ;;         ; )
;; ;;         ; (fact 12 1)
;; ;;     ; (define (foo x . xs) xs)
;; ;;     ; (foo 3 4 5 6 7 8)
;; ;;     ; ((λ xs xs) 1 2 3 4 5)
;; ;;
;; ;;         ; (define (add1 a) (+ a 1))
;; ;;         ; (fact 3000 1)
;; ;;         ; (fact 50 1)
;; ;;
;; ;;
;; ;; ;; (define (myadd n r)
;; ;; ;;   (cond
;; ;; ;;     [(zero? n) r]
;; ;; ;;     [else (myadd (- n 1) (* r n))]
;; ;; ;;   )
;; ;; ;; )
;; ;; ; (myadd 5 1)
;; ;; ; (+ 1 (call/cc (λ (cc) (+ 20 300))))
;; ;; ; (+ 1 (call/cc (λ (cc) (+ 20 (cc 300)) 1)))
;; ;; ; (define foo (+ 1 (call/cc (λ (cc) (+ 20 (cc 300)))))
;; ;; ; (+ 1  ((λ (cc) (+ 20 300)) 5))
;; ;; ; (((call/cc (λ (k) k)) (λ (x) x)) "HEY!")
;; ;; ; (define x 12)
;; ;; ; (set! x (+ 1 x))
;; ;; ; x
;; ;; ; (set! i-am-not-defined 10)
;; ;; ; '(+ 2 3) '(quote (1 2 . (3))) '(you can 'me) '(+ 2 3)
;; ;; ; (+ 2 3)
;; ;; ; (define (add1 a) (+ a 5) (+ a 1)) (add1 3)
;; ;; ; ((λ (a b) (+ b a) ) 3 4)
;; ;; ; (define pi 3) (define (add a b) (+ a (add1 b))) (add pi pi)
;; ;; ; (define pi 3) (+ pi 2)
;; ;; ; (+ 2 3) (+) (+ 1) (+ (+ 2 3) 3)
;; ;; ; (+ 2 3) (define (pi x y) (+ x y)) (pi 0 3) (1 2) ()
;; ;; ; (define (pi x y) (+ x y)) (pi 2 3)
;; ;; ; (define (cond . xs) (
;; ;; ;    (if (head (head xs))
;; ;; ;        (tail (head xs))
;; ;; ;        (cond (tail xs))
;; ;; ;    )
;; ;; ; ))
;; ;; ; (cond '[(zero? 0) 1] '[else 5])
;; ;; ; 1 2 (begin 1 2 5)
;; ;; ;(begin
;; ;; ;(define x 10)
;; ;; ;x)
;; ;; ; (+ 1 2 3 4 5)
;; ;; ; (zero? 0)
;; ;; ; (add1  3)  (add1 5) )
;; ;; ; ((λ (x y) (+ x y)) 3 4)
;; ;; ; (if #f 1 2)
;; ;; ; (add1 5)
;; ;; ; (define (add2 a) (add1 a))
;; ;; ; (cond [#f 0] [else 3])
;; ;; ; (cond [else 1] )
;; ;; ; (define (add a b) (+ a b)) (add 2 3)
;; ;; ; (cond [(zero? 0) 1] [else 5])
;; ;;  (define (factorial n)
;; ;;       (if (zero? n)
;; ;;          1
;; ;;         (* n (factorial (- n 1)))
;; ;;       )
;; ;;     )
;; ;; ;  (factorial 5)
;; ;; ; (define (mysumm n)
;; ;; ;      (cond
;; ;; ;        [(zero? n) 1]
;; ;; ;        [else (* 1 (add1 (- n 1)))] ; Recursive step: n * factorial of (n-1)
;; ;; ;      )
;; ;; ;    )
;; ;; ;  (mysumm 0)
;; ;;
;; ;;
;; ;; ; TODO: make the interpereter work
;; ;;
;; (define (test1 x)
;;   (displaynl x)
;;   (if (not x) #f 
;;   (if #t (begin (test1 #f) x) x))
;; )

(define interpret #f)
 (let ()
   (begin
   ;; primitive-environment is an environment containing a small
   ;; number of primitive procedures; it can be extended easily
   ;; to include additional primitives.
   (define primitive-environment
     (list (cons 'apply apply)
           (cons 'assq assq)
           (cons 'call/cc call/cc)
           (cons 'car car)
           (cons 'cadr cadr)
           (cons 'caddr caddr)
           (cons 'cadddr cadddr)
           (cons 'cddr cddr)
           (cons 'cdr cdr)
           (cons 'cons cons)
           (cons 'eq? eq?)
           (cons 'list list)
           (cons 'map map)
           (cons 'memv memv)
           (cons 'null? null?)
           (cons 'pair? pair?)
           (cons 'read read)
           (cons 'set-car! set-car!)
           (cons 'set-cdr! set-cdr!)
           (cons 'symbol? symbol?)))
 
   ;; new-env returns a new environment from a formal parameter
   ;; specification, a list of actual parameters, and an outer
   ;; environment.  The symbol? test identifies "improper"
   ;; argument lists.  Environments are association lists,
   ;; associating variables with values.
   (define new-env
     (λ (formals actuals env)
       (cond
         ((null? formals) env)
         ((symbol? formals) (cons (cons formals actuals) env))
         (else
          (cons (cons (car formals) (car actuals))
                (new-env (cdr formals) (cdr actuals) env))))))
 
   ;; lookup finds the value of the variable var in the environment
   ;; env, using assq.  Assumes var is bound in env.
   (define lookup
     (λ (var env)
       (cdr (assoc var env))))
 
   ;; assign is similar to lookup but alters the binding of the
   ;; variable var in the environment env by changing the cdr of
   ;; association pair
   (define assign
     (λ (var val env)
       (set-cdr! (assoc var env) val)))
 
   ;; exec evaluates the expression, recognizing all core forms.
   (define exec
     (λ (exp env) ;; is it so?
       (cond
         [(symbol? exp) (lookup exp env)]
         [(pair? exp)
          (case (car exp)
            ((quote) (cadr exp))
            ((lambda)
             (λ vals
               (let ((env (new-env (cadr exp) vals env)))
                 (let loop ((exps (cddr exp)))
                    (if (null? (cdr exps))
                        (exec (car exps) env)
                        (begin
                           (exec (car exps) env)
                           (loop (cdr exps))))))))
            ((if)
             (if (exec (cadr exp) env)
                 (exec (caddr exp) env)
                 (exec (cadddr exp) env)))
            ((set!)
             (assign (cadr exp)
                     (exec (caddr exp) env)
                     env))
            (else
             (apply (exec (car exp) env)
                    (map (λ (x) (exec x env))
                         (cdr exp)))))]
         (else exp))))
 
   ;; interpret starts execution with the primitive environment.
 (set! interpret
     (λ (exp)
       (exec exp  primitive-environment)))))



;; (defstruct tree height girth age
;;                 (leaf-shape 'frond)
;;                 (leaf-color 'green))
;;
;; (define palm (make-tree 'height 60))
;;
;; (tree.height palm) 
;; => 60
;;
;; (tree.leaf-shape palm) 
;; => frond
;;
;; (define plantain 
;;   (make-tree 'height 7
;;              'leaf-shape 'sheet))
;;
;; (tree.height plantain) 
;; => 7
;;
;; (tree.leaf-shape plantain) 
;; => sheet
;;
;; (tree.leaf-color plantain) 
;; => green
;;
;; 9.2  defstruct defined
;;
;; The defstruct macro definition follows:

(define-macro (defstruct s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i 
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (λ (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol 
                       (string-append "make-" s-s))
               (λ fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i 
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st 
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol 
                                         (string-append
                                          s-s "." f))
                                 (λ (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append 
                                           "set!" s-s "." f))
                                  (λ (x v) 
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (λ (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s)))))))))


;; (define-syntax fluid-let
;;   (syntax-rules ()
;;     ((_ ((x v)) e1 e2 ...)
;;      (let ((y v))
;;        (let ((swap (λ () (let ((t x)) (set! x y) (set! y t)))))
;;          (dynamic-wind swap (λ () e1 e2 ...) swap))))))

;; (define-macro (fluid-let xexe . body)
;;     (let ((xx (map car xexe))
;;           (ee (map cadr xexe))
;;           (old-xx (map (λ (ig) (gensym)) xexe))
;;           (result (gensym)))
;;       `(let ,(map (λ (old-x x) `(,old-x ,x)) 
;;                   old-xx xx)
;;          ,@(map (λ (x e)
;;                   `(set! ,x ,e)) 
;;                 xx ee)
;;          (dynamic-wind swap (λ () ,@body) swap ))))
;; (let ((,result (begin ,@body)))
;;            ,@(map (λ (x old-x)
;;                     `(set! ,x ,old-x)) 
;;                   xx old-xx)
;;            ,result)

;; incorrect in presense of continuations
(define-macro (fluid-let xexe . body)
    (let ((xx (map car xexe))
          (ee (map cadr xexe))
          (old-xx (map (λ (ig) (gensym)) xexe))
          (result (gensym)))
      `(let ,(map (λ (old-x x) `(,old-x ,x)) 
                  old-xx xx)
         ,@(map (λ (x e)
                  `(set! ,x ,e)) 
                xx ee)
         (let ((,result (begin ,@body)))
           ,@(map (λ (x old-x)
                    `(set! ,x ,old-x)) 
                  xx old-xx)
           ,result))))

(define-macro (coroutine x . body)
    `(letrec ((local-control-state
               (λ (,x) ,@body))
              (resume
               (λ (c v)
                 (call/cc
                  (λ (k)
                    (set! local-control-state k)
                    (c v))))))
       (λ (v)
         (local-control-state v))))

(define make-matcher-cor
  (λ (tree-cor-1 tree-cor-2)
    (coroutine dummy-init-arg
      (let loop ()
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))

(define make-leaf-gen-cor
  (λ (tree matcher-cor)
    (coroutine dummy-init-arg
      (let loop ((tree tree))
        (cond ((null? tree) 'skip)
              ((pair? tree)
               (loop (car tree))
               (loop (cdr tree)))
              (else
               (resume matcher-cor tree))))
      (resume matcher-cor '()))))

(define same-fringe?
  (λ (tree1 tree2)
    (letrec ((tree-cor-1
              (make-leaf-gen-cor
               tree1
               (λ (v) (matcher-cor v))))
             (tree-cor-2
              (make-leaf-gen-cor
               tree2
               (λ (v) (matcher-cor v))))
             (matcher-cor
              (make-matcher-cor
               (λ (v) (tree-cor-1 v))
               (λ (v) (tree-cor-2 v)))))
      (matcher-cor 'start-the-ball-rolling))))
(define *rain-prob* 0.4)
(define *max-num-walks* (* 365 2 5))
(define make-location-cor
  (λ (other-location-cor manager-cor)
    (coroutine v
      (let ((num-umbrellas 1))
        (let loop ((umbrella? (car v))
                   (walks-so-far (cadr v)))
          (when umbrella?
            (set! num-umbrellas (+ num-umbrellas 1)))
          (cond ((>= walks-so-far *max-num-walks*)
                 (resume manager-cor walks-so-far))
                ((< (random) *rain-prob*)
                 (cond ((> num-umbrellas 0)
                        (set! num-umbrellas
                          (- num-umbrellas 1))
                        (apply loop
                          (resume other-location-cor
                                  (list #t
                                        (+ walks-so-far 1)))))
                       (else
                         (apply loop
                           (resume manager-cor walks-so-far)))))
                (else
                  (apply loop
                    (resume other-location-cor
                            (list #f (+ walks-so-far 1)))))))))))

(define make-manager-cor
  (λ (home-cor)
    (coroutine dummy-init-arg
      (resume home-cor (list #f 0)))))






(define umbrella-trial
  (λ (rain-prob)
    (λ ()
      (when (number? rain-prob) (set! *rain-prob* rain-prob))
        (letrec ((home-cor (make-location-cor
                             (λ (v) (office-cor v))
                             (λ (v) (manager-cor v))))
                 (office-cor (make-location-cor
                               (λ (v) (home-cor v))
                               (λ (v) (manager-cor v))))
                 (manager-cor (make-manager-cor
                                (λ (v) (home-cor v)))))
          (manager-cor 'start-the-ball-rolling)
           )
      ; the letrec expression goes here
      )))

(define *num-trials* 1000)

(define monte-carlo
  (λ (experiment)
    (let loop ((i 0) (acc 0.0))
      (if (= i *num-trials*)
          (/ acc *num-trials*)
          (loop (+ i 1) (+ acc (experiment)))))))


(define *exception-handler* '())
(define (catch tag body)
  (call/cc
    (λ (k)
      ;; Save the 'catch' continuation (k) in a global/dynamic handler list
      ;; associated with 'tag'. For a simple example, we assume a single,
      ;; global handler for demonstration purposes. A real implementation
      ;; would use a dynamic environment or similar mechanism.
      (let ((old-handler *exception-handler*)) ; Pseudo-code for dynamic binding
        (set! *exception-handler* (cons (cons tag  k) old-handler))
        (let ((result (body)))
          ;; If body finishes normally, restore the old handler and return result
          (set! *exception-handler* old-handler)
          result)))))

(define (throw tag value)
  ;; Find the most recent handler associated with 'tag'
  (let ((handler (assoc tag *exception-handler*))) ; Pseudo-code
    (if handler
        ;; Invoke the captured continuation with the thrown value
        ((cdr handler) value)
        ;; No handler found, perhaps raise a real error
        (error "Uncaught exception" tag))))


(define (test-ex)
  (catch 5 (λ () (throw 5 3)))

  )

;; (with-exception-handler handler thunk )
;; (raise obj ) 

(define *current-exception-handler* #f)

;; wrong, should use continuation
(define (with-exception-handler handler thunk) 
  (call/cc 
    (λ (k) 
    (let ((old-handler *current-exception-handler*)) 
      (set! *current-exception-handler* handler)
      (let ((result (thunk))) 
        (set! *current-exception-handler* old-handler)
        result)))))

(define (raise obj)
  (let ((handler *current-exception-handler*)) 
    (if handler
        ;; Invoke the captured continuation with the thrown value
        (handler obj)
        ;; No handler found, perhaps raise a real error
        (error "Uncaught exception" obj))))


(define (test-raise)
  (with-exception-handler (λ (err) (displaynl err)) (λ () (raise "error")))

  )
;; https://code.call-cc.org/svn/chicken-eggs/release/5/simple-exceptions/trunk/simple-exceptions.scm
;; ;;; R6RS and R7RS high-level exception-handler
;; (define-syntax guard
;;   (syntax-rules ()
;;     ((_ (exn cond-clause . cond-clauses) xpr . xprs)
;;      (handle-exceptions exn (cond cond-clause . cond-clauses)
;;                         xpr . xprs))))

;; (define (with-exn-handler handler thunk)
;;   ((call-with-current-continuation
;;      (λ (k)
;;        (with-exception-handler ; Chicken's handler
;;          (λ (exn)
;;            (k (λ () (handler exn))))
;;          thunk)))))



(define-macro (delay expr)
     `(make-promise (λ () ,expr)))


(define make-promise
  (λ (p)
    (let ([val #f] [set? #f])
      (λ ()
        (unless set?
          (let ([x (p)])
            (unless set?
              (set! val x)
              (set! set? #t))))
        val))))


(define force
  (λ (promise)
    (promise)))


;; (define call/cc call/cc)
;; (define values #f)
;; (define call-with-values #f)
;; (let ((magic (cons 'multiple 'values)))
;;   (define magic?
;;     (λ (x)
;;       (and (pair? x) (eq? (car x) magic))))
;;
;;   (set! call/cc
;;     (let ((primitive-call/cc call/cc))
;;       (λ (p)
;;         (primitive-call/cc
;;           (λ (k)
;;             (p (λ args
;;                  (k (apply values args)))))))))
;;

(define magic "__magic")
(define (magic? x) (and (pair? x) (equal? (car x) "__magic")))

  (define values
    (λ args
      (if (and (not (null? args)) (null? (cdr args)))
          (car args)
          (cons magic args))))

  (define call-with-values
    (λ (producer consumer)
      (let ((x (producer)))
        (if (magic? x)
            (apply consumer (cdr x))
            (consumer x)))))

;; (define for-each
;;   (λ (f ls . more)
;;     (do ((ls ls (cdr ls)) (more more (map cdr more)))
;;         ((null? ls))
        ;; (apply f (car ls) (map car more)))))
(define (for-each f lst)
  (cond
    [(empty? lst) empty]
    [else (begin (f (first lst))
                (for-each f (rest lst)))]))




;; (define-syntax try
;;   (syntax-rules ()
;;         ((_ handler throw chunk)
;;          (call/cc (λ (catch)
;;                 (let ((throw (λ (exc) (catch (handler exc)))))
;;                   chunk))))))
;; (define (div p q)
;;   (try 
;;     ;; Error processing
;;     (λ (error) (printf "Error: ~s~n" error) error)
;;
;;     ;; Error my be thrown with keyword "throw"
;;     throw
;;
;;     ;;Actual code to run
;;      (if (= q 0)
;;     ;; Oh noes, error!
;;         (throw "Division by zero")
;;     ;; All ok, do the work
;;      (/ p q))))
;;
;; (printf "1/0: ~s~n" (div 1 0))
;; (printf "1/2: ~s~n" (div 1 2))
;; (include "test.scm")

(define sc-expand #f)
(define $syntax-dispatch #f)
(define identifier? #f)
(define datum->syntax #f)
(define syntax->datum #f)
(define generate-temporaries #f)
(define free-identifier=? #f)
(define bound-identifier=? #f)
(define syntax-error #f)
(define make-variable-transformer #f)


(define (reverse ls)
  (let loop ((remaining ls) (result '()))
    (if (null? remaining)
        result
        (loop (cdr remaining) (cons (car remaining) result))))) 


(define (void) (begin))

;; (define values (lambda x x))

;; (define call-with-values
;;     (lambda (producer consumer)
;; 	    (apply consumer (producer))))

;; (define command-line (lambda (x) x))
;; (include "psyntax-chicken.pp")
(include "syntax.pp")

(define *syntax-expand* sc-expand)

(define-syntax sequence
          (syntax-rules ()
            [(_ e0 e1 ...) (begin e0 e1 ...)]))

(define-syntax do1
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...)
       (with-syntax ([((var val update) ...)
                      (map (lambda (b)
                             (syntax-case b ()
                               [(var val) #'(var val var)]
                               [(var val update) #'(var val update)]))
                           #'(binding ...))])
         #'(let doloop ([var val] ...)
             (if test
                 (begin (if #f #f) res ...)
                 (begin expr ... (doloop update ...)))))])))

(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(_ e ...)
       #'(call/cc
           (lambda (break)
             (let f () e ... (f))))])))


;; (include "syntax.ss")

;; (do ((i 0 (+ i 1)))
;;     ((= i 5) i)
;;   (display i))

(define-syntax foo
    (lambda (stx)
      (syntax "I am foo")))

;; https://srfi.schemers.org/srfi-39/srfi-39.html
(define make-parameter
      (lambda (init . conv)
        (let ((converter
               (if (null? conv) (lambda (x) x) (car conv))))
          (let ((global-cell
                 (cons #f (converter init))))
            (letrec ((parameter
                      (lambda new-val
                        (let ((cell (dynamic-lookup parameter global-cell)))
                          (cond ((null? new-val)
                                 (cdr cell))
                                ((null? (cdr new-val))
                                 (set-cdr! cell (converter (car new-val))))
                                (else ; this case is needed for parameterize
                                 (converter (car new-val))))))))
              (set-car! global-cell parameter)
              parameter)))))

    (define-syntax parameterize
      (syntax-rules ()
        ((parameterize ((expr1 expr2) ...) body ...)
         (dynamic-bind (list expr1 ...)
                       (list expr2 ...)
                       (lambda () body ...)))))

    (define dynamic-bind
      (lambda (parameters values body)
        (let* ((old-local
                (dynamic-env-local-get))
               (new-cells
                (map (lambda (parameter value)
                       (cons parameter (parameter value #f)))
                     parameters
                     values))
               (new-local
                (append new-cells old-local)))
          (dynamic-wind
            (lambda () (dynamic-env-local-set! new-local))
            body
            (lambda () (dynamic-env-local-set! old-local))))))

    (define dynamic-lookup
      (lambda (parameter global-cell)
        (or (assq parameter (dynamic-env-local-get))
            global-cell)))

    (define dynamic-env-local '())

    (define dynamic-env-local-get
      (lambda () dynamic-env-local))

    (define dynamic-env-local-set!
      (lambda (new-env) (set! dynamic-env-local new-env)))
