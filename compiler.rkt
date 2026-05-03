#lang pl

;;; ==================================================================
;;; Syntax

#| The BNF:
   <FC> ::= <num>
           | <id>
           | { set! <id> <FC> }
           | { bind {{ <id> <FC> } ... } <FC> <FC> ... }
           | { bindrec {{ <id> <FC> } ... } <FC> <FC> ... }
           | { fun { <id> ... } <FC> <FC> ... }
           | { rfun { <id> ... } <FC> <FC> ... }
           | { if <FC> <FC> <FC> }
           | { <FC> <FC> ... }
|#

;; A matching abstract syntax tree datatype:
(define-type FC
  [Num  Number]
  [Id   Symbol]
  [Set  Symbol FC]
  [Bind    (Listof Symbol) (Listof FC) (Listof FC)]
  [BindRec (Listof Symbol) (Listof FC) (Listof FC)]
  [Fun  (Listof Symbol) (Listof FC)]
  [RFun (Listof Symbol) (Listof FC)]
  [Call FC (Listof FC)]
  [If   FC FC FC])

(: unique-list? : (Listof Any) -> Boolean)
;; Tests whether a list is unique, guards Bind and Fun values.
(define (unique-list? xs)
  (or (null? xs)
      (and (not (member (first xs) (rest xs)))
           (unique-list? (rest xs)))))

(: parse-sexpr : Sexpr -> FC)
;; parses s-expressions into FCs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'set! more)
     (match sexpr
       [(list 'set! (symbol: name) new) (Set name (parse-sexpr new))]
       [else (error 'parse-sexpr "bad `set!' syntax in ~s" sexpr)])]
    [(cons (and binder (or 'bind 'bindrec)) more)
     (match sexpr
       [(list _ (list (list (symbol: names) (sexpr: nameds)) ...)
              body0 body ...)
        (if (unique-list? names)
            ((if (eq? 'bind binder) Bind BindRec)
             names
             (map parse-sexpr nameds)
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "duplicate `~s' names: ~s" binder names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    binder sexpr)])]
    [(cons (and funner (or 'fun 'rfun)) more)
     (match sexpr
       [(list _ (list (symbol: names) ...)
              body0 body ...)
        (if (unique-list? names)
            ((if (eq? 'fun funner) Fun RFun)
             names
             (map parse-sexpr (cons body0 body)))
            (error 'parse-sexpr "duplicate `~s' names: ~s" funner names))]
       [else (error 'parse-sexpr "bad `~s' syntax in ~s"
                    funner sexpr)])]
    [(cons 'if more)
     (match sexpr
       [(list 'if cond then else)
        (If (parse-sexpr cond) (parse-sexpr then) (parse-sexpr else))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list fun args ...) ; other lists are applications
     (Call (parse-sexpr fun)
           (map parse-sexpr args))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FC)
;; Parses a string containing an FC expression to a FC AST.
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;;; ==================================================================
;;; Values and environments

(define-type ENV = (Listof (Listof (Boxof VAL))))

(define-type VAL
  [BogusV]
  [RktV  Any]
  [FunV  Natural (ENV -> VAL) ENV Boolean] ; byref? flag
  [PrimV ((Listof VAL) -> VAL)])

;; a single bogus value to use wherever needed
(define the-bogus-value (BogusV))

(: extend-rec : (Listof (ENV -> VAL)) ENV -> ENV)
;; extends an environment with more lists (given compiled expressions)
(define (extend-rec compiled-exprs env)
  (define new-frame
    (map (lambda (_)
           (box the-bogus-value))
         compiled-exprs))
  (define new-env
    (cons new-frame env))
  ;; note: no need to check the lengths here, since this is only
  ;; called for `bindrec`, and the syntax make it impossible to have
  ;; different lengths
  (for-each (lambda ([idx : Natural] [compiled : (ENV -> VAL)])
              (set-box! (list-ref (list-ref new-env 0) idx)
                        (compiled new-env)))
            (build-list (length compiled-exprs) (lambda ([i : Index]) i))
            compiled-exprs)
  new-env)

(: global-lookup : Symbol -> (Boxof VAL))
;; looks for a name in the global environment
(define (global-lookup name)
  (let ([cell (assq name global-environment)])
    (if cell
        (box (second cell))
        (error 'global-lookup "no binding for ~s" name))))

(: unwrap-rktv : VAL -> Any)
;; helper for racket-func->prim-val: unwrap a RktV wrapper in
;; preparation to be sent to the primitive function
(define (unwrap-rktv x)
  (cases x
    [(RktV v) v]
    [else (error 'racket-func "bad input: ~s" x)]))

(: racket-func->prim-val : Function -> VAL)
;; converts a racket function to a primitive evaluator function which
;; is a PrimV holding a ((Listof VAL) -> VAL) function.  (the
;; resulting function will use the list function as is, and it is the
;; list function's responsibility to throw an error if it's given a
;; bad number of arguments or bad input types.)
(define (racket-func->prim-val racket-func)
  (define list-func (make-untyped-list-function racket-func))
  (PrimV (lambda (args)
           (RktV (list-func (map unwrap-rktv args))))))

;; The global environment has a few primitives:
(: global-environment : (Listof (List Symbol VAL)))
(define global-environment
  (list (list '+ (racket-func->prim-val +))
        (list '- (racket-func->prim-val -))
        (list '* (racket-func->prim-val *))
        (list '/ (racket-func->prim-val /))
        (list '< (racket-func->prim-val <))
        (list '> (racket-func->prim-val >))
        (list '= (racket-func->prim-val =))
        ;; values
        (list 'true  (RktV #t))
        (list 'false (RktV #f))))

;;; ==================================================================
;;; Compilation

(define-type BINDINGS = (Listof (Listof Symbol)))

(: find-index : Symbol BINDINGS -> (U (List Natural Natural) #f))
;; find the first index of a symbol in a bindings or #f if not found
(define (find-index symbol bindings)
  (: find-index-acc : Symbol BINDINGS Natural -> (U (List Natural Natural) #f))
  ;; convenient helper for tracking outer index
  (define (find-index-acc symbol bindings outer-idx)
    (cond
      [(null? bindings) #f]
      [else (let ([inner-idx (index-of (first bindings) symbol)])
              (if (boolean? inner-idx)
                  (find-index-acc symbol (rest bindings) (add1 outer-idx))
                  (list outer-idx inner-idx)))]))
  (find-index-acc symbol bindings 0))

(: compiler-enabled? : (Boxof Boolean))
;; a global flag that can disable the compiler
(define compiler-enabled? (box #f))

(: compile-body : (Listof FC) BINDINGS -> (ENV -> VAL))
;; compiles a list of expressions to a single Racket function.
(define (compile-body exprs bindings)
  (unless (unbox compiler-enabled?)
    (error 'compile-body "compiler disabled"))
  ;; compile the list of expressions into a single racket function.  
  ;; (Note: relies on the fact that the body is never empty.)
  (define compiled-1st (compile (first exprs) bindings))
  (define rest-exprs   (rest exprs))
  (if (null? rest-exprs)
      compiled-1st
      (let ([compiled-rest (compile-body rest-exprs bindings)])
        (lambda (env)
          (define ignored (compiled-1st env))
          ;; alternatively: (void (compiled-1st env))
          (compiled-rest env))))
  )

(: compile-get-boxes : (Listof FC) BINDINGS -> (ENV -> (Listof (Boxof VAL))))
;; utility for applying rfun
(define (compile-get-boxes exprs bindings)
  (: compile-getter : FC -> (ENV -> (Boxof VAL)))
  (define (compile-getter expr)
    (cases expr
      [(Id name)
       (define indices (find-index name bindings))
       (match indices
         [#f
          (lambda ([env : ENV])
            (let ([global (global-lookup name)])
              (error 'compile "cannot mutate a global variable ~s" name)))]
         [(list outer inner)
          (lambda ([env : ENV])
            (list-ref (list-ref env outer) inner))])]
      [else
       (lambda ([env : ENV])
         (error 'call "rfun application with a non-identifier ~s"
                expr))]))
  (unless (unbox compiler-enabled?)
    (error 'compile-get-boxes "compiler disabled"))
  (let ([getters (map compile-getter exprs)])
    (lambda (env)
      (map (lambda ([get-box : (ENV -> (Boxof VAL))]) (get-box env))
           getters))))

(: compile : FC BINDINGS -> (ENV -> VAL))
;; compiles FC expressions to Racket functions.
(define (compile expr bindings)
  ;; convenient helper for mapping
  (: compile* : FC -> (ENV -> VAL))
  (define (compile* e)
    (compile e bindings))
  ;; convenient helper for running compiled code
  (: boxed-caller : ENV -> ((ENV -> VAL) -> (Boxof VAL)))
  (define (boxed-caller env)
    (lambda (compiled) (box (compiled env))))
  (unless (unbox compiler-enabled?)
    (error 'compile "compiler disabled"))
  (cases expr
    [(Num n)   (lambda ([env : ENV]) (RktV n))]
    [(Id name)
     (define indices (find-index name bindings))
     (match indices
       [#f
        (let ([global (global-lookup name)])
          (lambda ([env : ENV]) (unbox global)))]
       [(list outer inner)
        (lambda ([env : ENV])
          (unbox (list-ref (list-ref env outer) inner)))])]
    [(Set name new)
     (define compiled-new (compile new bindings))
     (define indices (find-index name bindings))
     (match indices
       [#f
        (let ([global (global-lookup name)])
          (error 'compile "cannot mutate a global variable ~s" name))]
       [(list outer inner)
        (lambda ([env : ENV])
          (set-box! (list-ref (list-ref env outer) inner)
                    (compiled-new env))
          the-bogus-value)])]
    [(Bind names exprs bound-body)
     (define compiled-exprs (map compile* exprs))
     (define compiled-body  (compile-body bound-body (cons names bindings)))
     (lambda ([env : ENV])
       (compiled-body
        (cons (map (boxed-caller env) compiled-exprs) env)))]
    [(BindRec names exprs bound-body)
     (define updated-bindings (cons names bindings))
     (define compiled-body  (compile-body bound-body updated-bindings))
     (define compiled-exprs
       (map (lambda ([e : FC]) (compile e updated-bindings)) exprs))
     (lambda ([env : ENV])
       (compiled-body (extend-rec compiled-exprs env)))]
    [(Fun names bound-body)
     (define compiled-body (compile-body bound-body (cons names bindings)))
     (define fun-arity (length names))
     (lambda ([env : ENV]) (FunV fun-arity compiled-body env #f))]
    [(RFun names bound-body)
     (define compiled-body (compile-body bound-body (cons names bindings)))
     (define fun-arity (length names))
     (lambda ([env : ENV]) (FunV fun-arity compiled-body env #t))]
    [(Call fun-expr arg-exprs)
     (define compiled-fun  (compile fun-expr bindings))
     (define compiled-args (map compile* arg-exprs))
     (define compiled-boxes-getter (compile-get-boxes arg-exprs bindings))
     (define call-arity (length arg-exprs))
     (lambda ([env : ENV])
       (define fval (compiled-fun env))
       ;; delay evaluating the arguments
       (define arg-vals
         (lambda () (map (boxed-caller env) compiled-args)))
       (define prim-vals
         (lambda ()
           (map (lambda ([compiled : (ENV -> VAL)]) (compiled env))
                compiled-args)))
       (cases fval
         [(PrimV proc) (proc (prim-vals))]
         [(FunV num-args compiled-body fun-env byref?)
          (if (= num-args call-arity)
              (compiled-body
               (if byref?
                   (cons (compiled-boxes-getter env) fun-env)
                   (cons (arg-vals) fun-env)))
              (error 'call "function call arity mismatch"))]
         [else (error 'call "function call with a non-function: ~s"
                      fval)]))]
    [(If cond-expr then-expr else-expr)
     (define compiled-cond (compile cond-expr bindings))
     (define compiled-then (compile then-expr bindings))
     (define compiled-else (compile else-expr bindings))
     (lambda ([env : ENV])
       ((if (cases (compiled-cond env)
              [(RktV v) v] ; Racket value => use as boolean
              [else #t])   ; other values are always true
            compiled-then
            compiled-else)
        env))]))

(: run : String -> Any)
;; compiles and runs a FC program contained in a string
(define (run str)
  (set-box! compiler-enabled? #t)
  (define compiled (compile (parse str) '()))
  (set-box! compiler-enabled? #f)
  (let ([result (compiled '())])
    (cases result
      [(RktV v) v]
      [else (error 'run "the program returned a bad value: ~s"
                   result)])))

;;; ==================================================================
;;; Tests

(test (run "{{fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}} {add3 1}}")
      => 4)
(test (run "{bind {{add3 {fun {x} {+ x 3}}}
                   {add1 {fun {x} {+ x 1}}}}
              {bind {{x 3}} {add1 {add3 x}}}}")
      => 7)
(test (run "{bind {{identity {fun {x} x}}
                   {foo {fun {x} {+ x 1}}}}
              {{identity foo} 123}}")
      => 124)
(test (run "{bind {{x 3}}
              {bind {{f {fun {y} {+ x y}}}}
                {bind {{x 5}}
                  {f 4}}}}")
      => 7)
(test (run "{{{fun {x} {x 1}}
              {fun {x} {fun {y} {+ x y}}}}
             123}")
      => 124)

;; More tests for complete coverage
(test (run "{bind x 5 x}")      =error> "bad `bind' syntax")
(test (run "{fun x x}")         =error> "bad `fun' syntax")
(test (run "{if x}")            =error> "bad `if' syntax")
(test (run "{}")                =error> "bad syntax")
(test (run "{bind {{x 5} {x 5}} x}") =error> "duplicate*bind*names")
(test (run "{fun {x x} x}")     =error> "duplicate*fun*names")
(test (run "{+ x 1}")           =error> "no binding for")
(test (run "{+ 1 {fun {x} x}}") =error> "bad input")
(test (run "{1 2}")             =error> "with a non-function")
(test (run "{{fun {x} x}}")     =error> "arity mismatch")
(test (run "{if {< 4 5} 6 7}")  => 6)
(test (run "{if {< 5 4} 6 7}")  => 7)
(test (run "{if + 6 7}")        => 6)
(test (run "{fun {x} x}")       =error> "returned a bad value")

;; variable assignment tests
(test (run "{set! {+ x 1} x}")  =error> "bad `set!' syntax")
(test (run "{bind {{x 1}} {set! x {+ x 1}} x}") => 2)

;; bindrec tests
(test (run "{bindrec {x 6} x}") =error> "bad `bindrec' syntax")
(test (run "{bindrec {{fact {fun {n}
                              {if {= 0 n}
                                1
                                {* n {fact {- n 1}}}}}}}
              {fact 5}}")
      => 120)

;; tests for multiple expressions and variable assignment
(test (run "{bind {{make-counter
                     {fun {}
                       {bind {{c 0}}
                         {fun {}
                           {set! c {+ 1 c}}
                           c}}}}}
              {bind {{c1 {make-counter}}
                     {c2 {make-counter}}}
                {* {c1} {c1} {c2} {c1}}}}")
      => 6)
(test (run "{bindrec {{foo {fun {}
                             {set! foo {fun {} 2}}
                             1}}}
              {+ {foo} {* 10 {foo}}}}")
      => 21)

;; rfun tests
(test (run "{{rfun {x} x} 4}") =error> "non-identifier")
(test (run "{bind {{swap! {rfun {x y}
                            {bind {{tmp x}}
                              {set! x y}
                              {set! y tmp}}}}
                   {a 1}
                   {b 2}}
              {swap! a b}
              {+ a {* 10 b}}}")
      => 12)

;; test that argument are not evaluated redundantly
(test (run "{{rfun {x} x} {/ 4 0}}") =error> "non-identifier")
(test (run "{5 {/ 6 0}}") =error> "non-function")

;; test compiler-disabled flag, for complete coverage
;; (these tests must use the functions instead of the toplevel `run`,
;; since there is no way to get this error otherwise, this indicates
;; that this error should not occur outside of our code -- it is an
;; internal error check)
(test (compile (Num 1) '()) =error> "compiler disabled")
(test (compile-body (list (Num 1)) '()) =error> "compiler disabled")
(test (compile-get-boxes (list (Num 1)) '()) =error> "compiler disabled")

;; test find index
(test (find-index 'a '((a b c) () (c d e))) => '(0 0))
(test (find-index 'e '((a b c) () (c d e))) => '(2 2))
(test (find-index 'c '((a b c) () (c d e))) => '(0 2))
(test (find-index 'x '((a b c) () (c d e))) => #f)

;; tests for mutation of globals
(test (run "{bind {{+ 1}} +}") => 1)
(test (run "{bind {{+ 1}} {+ 2 2}}")
      =error> "call with a non-function")
(test (run "{set! + 1}") =error> "cannot mutate")
(test (run "{set! - 1}") =error> "cannot mutate")
(test (run "{set! * 1}") =error> "cannot mutate")
(test (run "{set! / 1}") =error> "cannot mutate")
(test (run "{set! + -}") =error> "cannot mutate")
(test (run "{{fun {+} +} 1}") => 1)
(test (run "{{fun {+} {+ 2 3}} 1}")
      =error> "call with a non-function")

;; test compile-get-boxes new error case
(test (run "{bind {{a 3}} {{rfun {x} {set! a x} 1} +}}")
      =error> "compile: cannot mutate a global variable +")
(test (run "{bind {{a 3}} {{rfun {x} {set! x a} 1} +}}")
      =error> "compile: cannot mutate a global variable +")

;; more custom tests
(test (run "{bind {{x 6}} {bindrec {{y x}} y}}") => 6)
(test (run "{bind {{x 6}} {bindrec {{x x}} x}}")
      =error> "the program returned a bad value: (BogusV)")
(test (run "{bindrec {{my-even? {fun {n}
                                  {if {= 0 n}
                                    true
                                    {my-odd? {- n 1}}}}}
                      {my-odd?  {fun {n}
                                  {if {= 0 n}
                                    false
                                    {my-even? {- n 1}}}}}}
              {my-even? 0}}"))
(test (not (run "{bindrec {{my-even? {fun {n}
                                       {if {= 0 n}
                                         true
                                         {my-odd? {- n 1}}}}}
                           {my-odd?  {fun {n}
                                       {if {= 0 n}
                                         false
                                         {my-even? {- n 1}}}}}}
                   {my-even? 1}}")))
(test (run "{bindrec {{my-even? {fun {n}
                                  {if {= 0 n}
                                    true
                                    {my-odd? {- n 1}}}}}
                      {my-odd?  {fun {n}
                                  {if {= 0 n}
                                    false
                                    {my-even? {- n 1}}}}}}
              {my-even? 8}}"))
(test (not (run "{bindrec {{my-even? {fun {n}
                                       {if {= 0 n}
                                         true
                                         {my-odd? {- n 1}}}}}
                           {my-odd?  {fun {n}
                                       {if {= 0 n}
                                         false
                                         {my-even? {- n 1}}}}}}
                   {my-even? 9}}")))
(test (run "{bindrec {{summation-acc {fun {x acc}
                                       {if {= x 0}
                                         acc
                                         {summation-acc {- x 1} {+ x acc}}}}}}
              {summation-acc 0 10}}")
      => 10)
(test (run "{bindrec {{summation-acc {fun {x acc}
                                       {if {= x 0}
                                         acc
                                         {summation-acc {- x 1} {+ x acc}}}}}}
              {summation-acc 5 0}}")
      => 15)
(test (run "{bind {{x 3}} {bind {{y x}} {set! y {+ 1 y}} y}}") => 4)
(test (run "{bindrec {{x 6}} {set! x 3} {* x 3} {- x 1} x}") => 3)
(test (run "{bindrec {{x 6}} {set! x 3} {+ {* x 3} {- x 1} x}}") => 14)
(test (run "{bind {{x 6}} {bindrec {{y {+ 1 x}}} {set! y 2} y}}") => 2)
(test (run "{{fun {y} {set! y {+ 1 y}} y} 3}") => 4)
(test (run "{bind {{g {rfun {x} {set! x 10}}} {z 5}}
              {{rfun {f} {set! f {rfun {y} {set! y 20}}}} g} {g z} z}")
      => 20)
(test (run "{bindrec {{summation-acc-mut {rfun {x acc}
                                           {if {= x 0}
                                             acc
                                             {bind {{old-x x}}
                                               {set! acc {+ old-x acc}}
                                               {set! x {- x 1}}
                                               {summation-acc-mut x acc}}}}}
                      {n 0}
                      {so-far 2}}
              {summation-acc-mut n so-far}}")
      => 2)
(test (run "{bindrec {{summation-acc-mut {rfun {x acc}
                                           {if {= x 0}
                                             acc
                                             {bind {{old-x x}}
                                               {set! acc {+ old-x acc}}
                                               {set! x {- x 1}}
                                               {summation-acc-mut x acc}}}}}
                      {n 3}
                      {so-far 0}}
              {summation-acc-mut n so-far}}")
      => 6)

;;; ==================================================================