#lang plai
; HW4 #1
; Solved by myself: Y
; Time Taken: 24 hours
; [contract] BFAE : BFAE BFAE DefrdSub store --> store-value
; [purpose] create BFAE. Apply interp-two for setbox, seqn, add, sub

(define-type BFAE
  [num     (n number?)]
  [add     (lhs BFAE?) (rhs BFAE?)]
  [sub     (lhs BFAE?) (rhs BFAE?)]
  [id      (name symbol?)]
  [fun     (param symbol?) (body BFAE?)]
  [newbox  (v BFAE?)]
  [setbox  (bn BFAE?) (v BFAE?)]
  [openbox (v BFAE?)]
  [seqn    (ex1 BFAE?) (ex2 BFAE?)]
  [app     (ftn BFAE?) (arg BFAE?)]
)

(define (parse sexp)
  (match sexp
    [(? number?)               (num sexp)]
    [(list '+ l r)             (add (parse l) (parse r))]
    [(list '- l r)             (sub (parse l) (parse r))]
    [(? symbol?)               (id sexp)]
    [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
    [(list 'fun (list p) b)    (fun p (parse b))]     
    [(list 'newbox v)          (newbox (parse v))]
    [(list 'setbox i v)        (setbox (parse i) (parse v))]
    [(list 'openbox i)         (openbox (parse i))]
    [(list 'seqn ex1 ex2)      (seqn (parse ex1) (parse ex2))]
    [(list f a)                (app (parse f) (parse a))]
    [else                      (error 'parse "bad syntax: ~a" sexp)])
)

(define num+ (num-op +))
(define num- (num-op -))
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y))))
)

(define-type BFAE-Value
  [numV     (n number?)]
  [closureV (param symbol?) (body BFAE?) (ds DefrdSub?)]
  [boxV     (address integer?)]
  [exprV    (expr BFAE?) (ds DefrdSub?) (value (box/c (or/c false BFAE-Value?)))]
)

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BFAE-Value?) (rest Store?)]
) 

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (address integer?) (ds DefrdSub?)]
)

(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub  (i adr saved) (if(symbol=? i name)
                            adr (lookup name saved))])
)

(define (store-lookup address sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "No value at address")]
    [aSto  (location value rest-store)
             (if(= location address)
                value
                  (store-lookup address rest-store))])
)

(define (malloc st)
  (+ 1 (max-address st))
)

(define-type store-value
  [v*s (value BFAE-Value?) (store Store?)]
)

(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st) (max n (max-address st))])
)


;Interp-two
(define (interp-two expr1 expr2 ds st handle)
  (type-case store-value (interp expr1 ds st)
    [v*s (val1 st2)
           (type-case store-value (interp expr2 ds st2)
             [v*s (val2 st3) (handle val1 val2 st3)])])
)

(define (interp bfae ds st)
  (type-case BFAE bfae
     [num (n)   (v*s(numV n) st)]
     [add (l r) (interp-two l r ds st
                            (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
     [sub (l r) (interp-two l r ds st
                            (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
     [id  (s)   (v*s (store-lookup (lookup s ds) st) st)]
     [fun (p b) (v*s (closureV p b ds) st)]
     [app (f a) (type-case store-value (interp f ds st)
                  [v*s (f-value f-store)
                       (type-case store-value (interp a ds f-store)
                         [v*s (a-value a-store)
                                (local ([define new-address (malloc a-store)])
                                  (interp (closureV-body f-value)
                                            (aSub (closureV-param f-value)
                                                    new-address
                                                      (closureV-ds f-value))
                                            (aSto new-address a-value a-store)))])])]
     [newbox (val)
               (type-case store-value (interp val ds st)
                 [v*s (vl st1)
                        (local [(define a (malloc st1))]
                          (v*s (boxV a)
                                 (aSto a vl st1)))])]
     [setbox (bx-expr val-expr)
               (interp-two bx-expr val-expr ds st
                             (lambda (bx-val val st1)
                               (v*s val
                                      (aSto (boxV-address bx-val) val st1))))]
     [openbox (bx-expr)
                (type-case store-value (interp bx-expr ds st)
                  [v*s (bx-val st1)
                         (v*s (store-lookup (boxV-address bx-val)
                                              st1)
                                st1)])]
     [seqn (a b)
             (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]
    )
)

(define (run sexp ds st)
     (interp (parse sexp) ds st))