#lang racket



; forces the expression to be of the form ($--> (seq <exp> ...) <body>)
(define (force-$* exp)
  (match exp
    [`($*--> ,pats ... ,body)
     ; =>
     (force-$* `($--> (seq ,@pats) ,body))]
    
    [`($--> (seq . ,exps) . ,body)
     ; =>
     exp]
    
    [`(seq . ,exps)
     ; =>
     `($--> ,exp $$)]
    
    [(or `(quote ,_)
         (? symbol?)
         (? string?)
         ; TODO: seq 
         `(rep ,_)
         `(rep+ ,_)
         `(or . ,_))
     `($--> (seq ,exp) ($ 1))]))

(define (force-or exp)
  (match exp
    [`(or . ,exps) exp]
    [else `(or ,exp)]))



(define (unzip/callback lst k)
  (match lst
    ['() (k '() '())]
    [(cons (list a b) tl)
     (unzip/callback tl (λ (as bs)
       (k (cons a as) (cons b bs))))]))


;; desugar
(define (desugar grammar)
  
  
  (define added-rules '())
  
  (define (add-rule! non-term rhs)
    (set! added-rules (cons `[,non-term ,rhs] added-rules)))
  
  (define (atomize! exp)
    (match exp

      ; Terminals:
      [`(quote ,(and aterm (? symbol?)))
       ;=>
       exp]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       exp]
      
      [else
       ;=>
       (define $nt (gensym 'nt))
       (add-rule! $nt exp)
       $nt]))
  
  ; opt
  ; rep
  ; rep+ 
  ; seq
  ; $*-->
  ; TODO: >-->
  ; TODO: @-->
  ; (cons <pat> <pat>)  ==>  ($*--> <pat> <pat> (cons ($ 1) ($ 2)))
  ; #'(<qq-pat> ...) where <qq-pat> ::= #,<pat> | <pat>
  ; TODO: (car <pat>)
  ; TODO: (cdr <pat>)
  ; TODO: (rep/sep <pat> <pat>)
  ; TODO: (rep/sep+ <pat> <pat>)
  (define (desugar-exp exp)
    (match exp
    
      ; Terminals:
      [`(quote ,(and aterm (? symbol?)))
       ;=>
       exp]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       exp]
      
      ; Reductions
      [`($--> ,exp . ,body)
       `($--> ,(desugar-exp exp) . ,body)]
      
      [`($*--> ,exps ... ,body)
       `($--> (seq ,@(map desugar-exp exps)) ,body)]
      
      [`(cons ,exp1 ,exp2)
       `($--> (seq ,(desugar-exp exp1) ,(desugar-exp exp2)) (cons ($ 1) ($ 2)))]
      
      [`(car ,exp)
       `($--> (seq ,(desugar-exp exp)) (car ($ 1)))]
      
      [`(syntax ,args)
       ;=>
       (define (desugar-quoted exp position)
         (match exp
           [`(unsyntax ,exp)
            `(,position ,(desugar-exp exp))]
           
           [else 
            `(#f ,(desugar-exp exp))]))
         
       (define desugared (map desugar-quoted args (for/list ([i (in-range 1 (+ 1 (length args)))]) i)))
       
       (unzip/callback
        desugared 
        (λ (positions exps)
          (desugar-exp 
           `($--> (seq ,@exps)
                  (list ,@(map (λ (p) `($ ,p)) (filter identity positions)))))))]
      
      ; Core forms:
      [`(seq . ,exps)
       ; =>
       `(seq ,@(map desugar-exp exps))]
      
      [`(or . ,exps)
       ; =>
       `(or ,@(map desugar-exp exps))]
      
      
      ; Complex constructs:      
      [`(rep ,exp)
       ; =>
       `(rep ,(desugar-exp exp))]
      
      [`(rep+ ,exp)
       ; =>
       `(rep+ ,(desugar-exp exp))]

      [`(rep+/sep ,sep ,exp ,sep-tail?)
       ; => 

       (define $exp (atomize! (desugar-exp exp)))
       (define $sep (atomize! (desugar-exp sep)))
       
       (define $rep (gensym '$rep))
       
       (add-rule! $rep `(or ($--> (seq) '())
                            ($--> (seq ,$sep ,$exp ,$rep)
                                  (cons ($ 2) ($ 3)))))
       
       (define $tail (if sep-tail?
                         `((opt ,$sep))
                         `()))
        
       `(or 
            ($--> (seq ,$exp)       (list ($ 1)))
            ($--> (seq ,$exp ,$rep . ,$tail) (cons ($ 1) ($ 2))))]

      
      [`(rep/sep ,sep ,exp ,sep-tail?)
       
       (define $exp (atomize! (desugar-exp exp)))
       (define $sep (atomize! (desugar-exp sep)))
       
       (define $rep (gensym '$rep))
       
       (add-rule! $rep `(or ($--> (seq) '())
                            ($--> (seq ,$sep ,$exp ,$rep)
                                  (cons ($ 2) ($ 3)))))
       
       (define $tail (if sep-tail?
                         `((opt ,$sep))
                         `()))
        
       `(or ($--> (seq)            '())
            ($--> (seq ,$exp)       (list ($ 1)))
            ($--> (seq ,$exp ,$rep . ,$tail) (cons ($ 1) ($ 2))))]
            
            
      
      [`(opt ,exp ,value)
       ; =>
       `(opt ,(desugar-exp exp) ,value)]
      
      [`(opt ,exp)
       ; =>
       `(opt ,(desugar-exp exp))]
      
      
      [else 
       ; =>
       (error (format "could not desugar: ~a" exp))]))
  
  
  (define (desugar-rule rule)
    (match rule
      [`[,non-term ,rhs]
       `[,non-term ,(desugar-exp rhs)]]))
  
  (define transformed-rules (map desugar-rule grammar))
  
  (append transformed-rules added-rules))
  
  
;; Transform a derp-style grammar into a racket-style yacc grammar:
(define (compile-derp-rules rules)

  (define extra-rules '())
  
  (define (add-rule non-term exp)
    (set! extra-rules (cons `[,non-term ,exp] extra-rules)))
  
  ;; derp->yacc translation:
  (define (wrap-$ size body)
    (define $$ (for/list ([i (in-range 1 (+ 1 size))]) `($ ,i)))
    `(let-syntax
         [($ (λ (stx) (syntax-case stx ()
                        [(_ n)  (datum->syntax #'n (string->symbol (string-append "$" (number->string (syntax->datum #'n)))))])))]
       (let-syntax 
           ([$$ (λ (_) #'(list ,@$$))])
         . ,body)))

  (define (translate-terminal str)
    (string->symbol str))
  
  (define (translate-abstract-terminal sym)
    sym)
  
  (define (translate-exp exp)
    (match exp
      [(? string?)   (translate-terminal exp)]
      
      [(? symbol?)   exp]
      
      [`(quote ,(and aterm (? symbol?)))
       ;=>
       (translate-abstract-terminal exp)]))
  
  (define (translate-reduction reduction)
    (match reduction
      [`($--> (seq . ,exps) . ,body)
       (list (map translate-exp exps)
             (wrap-$ (length exps) body))]))
  
  (define (translate-rhs rhs)
    (match rhs
      [`(or . ,reductions) 
       (map translate-reduction reductions)]))
  
  (define (translate-rule rule)
    (match rule
      [`(,nonterm ,rhs)
       `(,nonterm ,@(translate-rhs rhs))]))
       
  
  ;; normalize
  (define (normalize-rule rule)
    (match rule
      [`(,nonterm ,exp)
       ;=>
       `[,nonterm ,(normalize-rhs exp)]]))
  
  (define (normalize-rhs exp)
    (match (force-or exp)
      [`(or . ,exps)   `(or ,@(map (compose flatten-$* force-$*) exps))]))
    
      
  
  ;; flattening
  (define (flatten-$* exp)
    (match exp
      [`($--> (seq . , exps) . ,body)
       ;=>
       `($--> (seq ,@(map flatten-exp exps)) . ,body)]))
       
  ; flatten things that can't be a *sub*-expression:
  ; opt
  ; rep
  ; rep+ 
  ; seq
  ; $-->
  (define (flatten-exp exp)
    (match exp
      [`(seq . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]
      
      [`($--> . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]
      
      
      [`(or . ,_)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm (normalize-rhs exp))
       $nonterm]

      
      [`(opt ,exp ,default-value)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm `(or ($--> (seq ,(flatten-exp exp)) ($ 1))
                               ($--> (seq) ,default-value)))
       $nonterm]
      
      [`(opt ,exp)
       ;=>
       (flatten-exp `(opt ,exp #f))]
      
      [`(rep ,exp)
       ;=>
       (define $nonterm (gensym '$nt))
       (add-rule $nonterm `(or ($--> (seq  ,(flatten-exp exp) ,$nonterm) (cons ($ 1) ($ 2)))
                               ($--> (seq) '())))
       $nonterm]

      [`(rep+ ,exp)
       ;=>
       (define $nonterm (gensym '$nt))
       (define flattened-exp (flatten-exp exp))
       (add-rule $nonterm `(or ($--> (seq ,flattened-exp ,$nonterm) (cons ($ 1) ($ 2)))
                               ($--> (seq ,flattened-exp) (list ($ 1)))))
       $nonterm]


      
      [`(quote ,(and aterm (? symbol?)))
       ;=>
       aterm]
      
      [(? symbol?)
       ;=>
       exp]
      
      [(? string?)
       ;=>
       (define $nonterm (gensym (string->symbol (string-append "$" exp))))
       (add-rule $nonterm `(or ($--> (seq ,exp) ,exp)))
       $nonterm]))
       

       
  (define old-rules (map normalize-rule rules))
  
  (define new-rules (append old-rules extra-rules))
  
  (map translate-rule new-rules))
  

(define grammar-input-file #f)
(define grammar-input-port #f)

(match (current-command-line-arguments)
  
  [(vector "--drracket")
   (set! grammar-input-file "my-python.grammar.sx")
   (set! grammar-input-port (open-input-file grammar-input-file))]
  
  [(vector file-name)
   (set! grammar-input-file file-name)
   (set! grammar-input-port (open-input-file grammar-input-file))]
  
  [(vector)
   (set! grammar-input-port (current-input-port))])



(define python-grammar (read grammar-input-port))


(define desugared-python-grammar (desugar python-grammar))

;(pretty-write desugared-python-grammar)

(define python-yacc-rules (compile-derp-rules desugared-python-grammar))


(define yacc-output-port (current-output-port))

(when grammar-input-file
  (define yacc-output-file (regexp-replace #rx"[.]grammar" grammar-input-file ".yacc"))
  (set! yacc-output-port (open-output-file yacc-output-file #:exists 'replace)))
  
  
(for ([rule python-yacc-rules])
  (pretty-write rule yacc-output-port)
  (newline yacc-output-port))

  


