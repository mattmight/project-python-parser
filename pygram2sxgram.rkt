#lang racket

; pygram2sxgram:
; 
; Takes a grammar in the format used in the python docs
; for the grammar of python and converts it into a derp-
; compatible grammar in s-expressions:

;  https://docs.python.org/3/reference/grammar.html


(require parser-tools/lex)

(require (prefix-in : parser-tools/lex-sre))

(require parser-tools/yacc)

(define-lex-abbrev hash-comment (:: #\# (:* (char-complement #\newline))))

(define-lex-abbrev nt-start (union (char-range #\a #\z)                                  
                                   #\_))

(define-lex-abbrev nt-continue (union (char-range #\a #\z)
                                      (char-range #\0 #\9)
                                      #\_))

(define-lex-abbrev non-terminal (:: nt-start (:* nt-continue)))





(define-lex-abbrev t-start (union (char-range #\A #\Z) 
                                  #\_))

(define-lex-abbrev t-continue (union (char-range #\A #\Z)
                                     (char-range #\0 #\9)
                                     #\_))

(define-lex-abbrev terminal-name (:: t-start (:* t-continue)))

(define-lex-abbrev terminal-quote (:: "'" (:* (char-complement #\')) "'"))



(define-tokens symbols (NONTERM TERM ATERM))

(define-empty-tokens operators (* + /))

(define-empty-tokens virtuals (START-RULE END))

(define-empty-tokens delimiters 
  (|(| 
   |)|
   |[|
   |]|
   :))

(define py-grammar-lexer
  (lexer
   [(eof)             '(END)]
   
   [hash-comment      (py-grammar-lexer input-port)]
   
   [(:: #\newline non-terminal) 
    (cons (token-START-RULE)
          (cons (token-NONTERM (substring lexeme 1))
                (py-grammar-lexer input-port)))]
   
   [(union #\newline #\space)
    ; =>
    (py-grammar-lexer input-port)]
   
   [#\:               (cons (token-:)
                            (py-grammar-lexer input-port))]
   
   [#\|               (cons (token-/)
                            (py-grammar-lexer input-port))]
   
   [#\(               (cons (|token-(|)
                            (py-grammar-lexer input-port))]

   [#\)               (cons (|token-)|)
                            (py-grammar-lexer input-port))]
   
   [#\[               (cons (|token-[|)
                            (py-grammar-lexer input-port))]

   [#\]               (cons (|token-]|)
                            (py-grammar-lexer input-port))]
   
   [#\*               (cons (|token-*|)
                            (py-grammar-lexer input-port))]
   
   [#\+               (cons (|token-+|)
                            (py-grammar-lexer input-port))]
   
   [terminal-name     (cons (token-ATERM lexeme)
                            (py-grammar-lexer input-port))]
   
   [terminal-quote    (cons (token-TERM (substring lexeme 1 (- (string-length lexeme) 1)))
                            (py-grammar-lexer input-port))]
   
   [non-terminal      (cons (token-NONTERM lexeme)
                            (py-grammar-lexer input-port))]))


(define grammar-input-port #f)

(define grammar-input-file #f)

(match (current-command-line-arguments)
  [(vector "--drracket")
   (set! grammar-input-file "python.grammar")
   (set! grammar-input-port (open-input-file "python.grammar"))]
  
  [(vector file-name)
   (set! grammar-input-file file-name)
   (set! grammar-input-port (open-input-file file-name))]
  
  [(vector)
   (set! grammar-input-file #f)
   (set! grammar-input-port (current-input-port))])

    
   
(define token-list
  (py-grammar-lexer grammar-input-port))


(define ((token-generator list))
  (if (null? list)
      '()
      (let ([top (car list)])
        (set! list (cdr list))
        top)))

(define gen (token-generator token-list))

(define (seq-trimmed term)
  (match term
    [`(seq ,factor) #;=> factor]
    [else            term]))

(define (or-exps exp)
  (match exp
    [`(or . ,exps) exps]
    [else          (list exp)]))

(define py-grammar-parser
  (parser 
   
   (tokens symbols operators virtuals delimiters)
   
   (grammar 
    [rule-list {() '()}
               {(rule rule-list) (cons $1 $2)}]
    
    [rule      {(START-RULE NONTERM : alternatives)
                ;=> 
                (match $4
                  [(list exp)  #;=> `(,(string->symbol $2) ,exp)]
                  [else #;=> `(,(string->symbol $2) (or ,@$4))])}]
    
    [alternatives {()              '()}
                  {(term alt-tail)  (append (or-exps (seq-trimmed $1)) $2)}]
    
    [alt-tail {()                  '()}
              {(/ term alt-tail)    (append (or-exps (seq-trimmed $2)) $3)}]
    
    [exp {(term)        (seq-trimmed $1)}
         {(term / exp)  
          ; =>
          (let ([$$1 (seq-trimmed $1)])
            `(or ,@(append (or-exps $$1) (or-exps $3))))}]
    
    [term {(factor)       `(seq ,$1)}
          {(factor term)  (match $2
                            [`(seq . ,factors) #;=> `(seq ,$1 . ,factors)]
                            #;[else                    `(seq ,$1 ,$2)])}]

    [factor {(base)       $1}
            {(base *)    `(rep  ,$1)}
            {(base +)    `(rep+ ,$1)}]
    
    [base  {(atom)          $1}
           {(|(| exp |)|)   $2}
           {(|[| exp |]|)   `(opt ,$2)}]
    
    [atom {(ATERM)         `(quote ,(string->symbol $1))}
          {(TERM)          $1}
          {(NONTERM)       (string->symbol $1)}])
   
   (end END)
   
   (error (λ (tok-ok? tok-name tok-value)
            (if tok-ok?
                (error (format "Unexpected token: ~a ~a" tok-name tok-value))
                (error (format "Invalid token: " ~a)))))
  
   (start rule-list)))
   

(define python-grammar (py-grammar-parser gen))

(define grammar-output-port (current-output-port))

(when grammar-input-file
  (set! grammar-output-port (open-output-file (string-append grammar-input-file ".sx") #:exists 'replace)))


(pretty-write python-grammar grammar-output-port)

