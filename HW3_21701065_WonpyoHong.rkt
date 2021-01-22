#lang plai

;Problem 1
;Solved by myself: Yes
;Time taken : 1 hour
;[contract] : parse --> SDFAE
;[purpose]  : SDFAE parser

(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id      (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

(define (parse string)
  (match string
    [(? number?) (num string)]
    [(? symbol?) (id string)] 
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r ) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (app (fun 's i (parse e)) (parse v))]
    [(list l r) (app (parse l) (parse r))]
    [(list 's 'fun (list p) b) (fun 's p (parse b))]
    [(list 'd 'fun (list p) b) (fun 'd p (parse b))]))

;Problem 2
;Solved by myself: Yes
;Time taken: 1 hour
;[contract]: interpreter for SDFAE
;[purpose]: interpreter for SDFAE

(define-type DefSub
  [mtSub]
  [aSub (name symbol?) (value SDFAE-Value?) (ds DefSub?)])

(define (lookup name ds)
  (type-case DefSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

(define-type SDFAE-Value
  [numV (n number?)]
  [closureV (sd symbol?) (param symbol?) (body SDFAE?) (ds DefSub?)])

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds ) (interp r ds ))]
    [sub (l r) (num- (interp l ds ) (interp r ds ))]
    [id (s) (lookup s ds)]
    [fun (sd p b) (if (equal? 's sd) (closureV sd p b ds) (closureV sd p b ds))]
    [app (f a) (local[(define ftn (interp f ds)) (define atn (interp a ds ))]
               (if (equal? 'd (closureV-sd ftn))
                   (interp (closureV-body ftn)
                           (aSub (closureV-param ftn) atn ds))
                   (interp (closureV-body ftn)
                           (aSub (closureV-param ftn) atn (closureV-ds ftn)))))]))