#lang plai
;Wonpyo Hong
;2020-10-10, 19:25
;
; Problem 1.a:
; Solved by myself: Y
; Time Taken: 2 hours
; Contract: define PWAE
; Purpose: define type PWAE
; BNF for PWAE
;<PWAE> ::= <num>
;    | <op>
;    | <id>
;    | <keyword>
;    | { <PWAE> <PWAE> <op> }
;    | { { <id> <PWAE> } <PWAE> <keyword> }
(define-type PWAE
  [num     (n number?)]
  [op      (s symbol?)]
  [keyword (s symbol?)]
  [id      (s symbol?)]
  [postfix    (lhs PWAE?) (rhs PWAE?) (operator op?)]
  [substitute (name symbol?) (named-expr PWAE?) (body PWAE?) (key keyword?)])

; Problem 1.b:
; Solved by myself: Y
; Time Taken: 3 hours
; Contract: parse. sexp --> pwae
; Purpose: Implement Parser for PWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list l r '+) (postfix (parse l) (parse r) (op 'add))]
    [(list l r '-) (postfix (parse l) (parse r) (op 'sub))]
    [(list (list i v) e 'with) (substitute i (parse v) (parse e) (keyword 'with))]))

(parse '{{3 4 -} 7 + })
(parse '{{x 5} {x x +} with})
(parse '{{x {5 4 +}} {x x +} with})

; Problem 2:
; Solved by myself: Y
; Time Taken: 2 hours
; Contract: free-id, pwae to list of symbols
; Purpose: Implement free-ids of PWAE
(define (subst pwae identif val)
  (type-case PWAE pwae
    [num     (n) pwae]
    [op      (s) pwae]
    [id      (s) (if (symbol=? s identif) val pwae)]
    [keyword (s) pwae]
    [postfix    (l r o)   (postfix (subst l identif val) (subst r identif val) (subst o identif val))]
    [substitute (i v e k) (substitute i (subst v identif val) (if (symbol=? i identif) e (subst e identif val)) (subst k identif val))]))

(define (free-ids pwae)
  (type-case PWAE pwae
    [num     (n) empty]
    [op      (s) empty]
    [id      (s) (list s)]
    [keyword (s) empty]
    [postfix    (l r o)   (remove-duplicates (sort (append (free-ids l) (free-ids r)) symbol<?))]
    [substitute (i v e k) (remove-duplicates (sort (append (free-ids v) (free-ids (subst e i v))) symbol<?))]))  

;; free-ids
(test (free-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (num 3) (id 'x) (op 'sub)) (op 'add)) (keyword 'with))) '())
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (num 4) (id 'x) (op 'add)) (op 'sub)) (keyword 'with))) '(a))
(test (free-ids (substitute 'x (num 3) (postfix (id 'b) (postfix (id 'a) (id 'x) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (id 'b) (postfix (id 'x) (id 'b) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b y))
(test (free-ids (substitute 'x (id 't) (postfix (id 'x) (substitute 'y (id 'y) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b t y))
(test (free-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with))) '(x y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'a) (id 'a) (keyword 'with)) (op 'add))) '(a b c y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(b c d y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(b c d y z))

; Problem 3:
; Solved by myself: Y
; Time Taken: 1 hour
; Contract: binding-ids, pwae to list of symbols
; Purpose: Implement binding-ids of PWAE
(define (binding-ids pwae)
  (type-case PWAE pwae
    [num     (n) empty]
    [op      (s) empty]
    [id      (s) empty]
    [keyword (s) empty]
    [postfix    (l r o)   (remove-duplicates (sort (append (binding-ids l) (binding-ids r)) symbol<?))]
    [substitute (i v e k) (remove-duplicates (sort (append (list i) (binding-ids v) (binding-ids e)) symbol<?))]))

;; binding-ids
(test (binding-ids (postfix (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add))) '())
(test (binding-ids (substitute 'y (num 3) (substitute 'x (id 'x) (id 'y) (keyword 'with)) (keyword 'with))) '(x y))
(test (binding-ids (substitute 'y (num 3) (substitute 'y (id 'x) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(y))
(test (binding-ids (substitute 'y (num 3) (substitute 'y (substitute 'x (postfix (num 3) (id 'y) (op 'sub)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(x y))
(test (binding-ids (substitute 'z (num 3) (substitute 'w (substitute 'z (postfix (num 3) (id 'y) (op 'add)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (substitute 'w (id 'y) (postfix (num 7) (id 'w) (op 'add)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(w z))

; Problem 4:
; Solved by myself: Y
; Time Taken: 3 hours
; Contract:bound-ids, pwae to list of symbols
; Purpose: Implement bound-ids for PWAE
(define (bound-ids pwae)
  (type-case PWAE pwae
    [num (n) empty]
    [op      (s) empty]
    [id      (s) empty]
    [keyword (s) empty]
    [postfix    (l r o)   (remove-duplicates (sort (append (bound-ids l) (bound-ids r)) symbol<?))]
    [substitute (i v e k) (remove-duplicates (sort (cond [(member i (free-ids e)) (append (list i) (bound-ids v)(bound-ids e))]
                                                         [else (append (bound-ids v) (bound-ids e))]) symbol<?))]))

;; bound-ids
(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (num 3) (op 'add)) (keyword 'with))) '())
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (substitute 'y (num 7) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (substitute 'y (id 'x) (postfix (num 3) (id 'y) (op 'sub)) (keyword 'with)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (id 'x) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (id 'x) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (substitute 'z (num 7) (postfix (id 'z) (id 'x) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x z))
(test (bound-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(y))
(test (bound-ids (substitute 'x (id 'a) (substitute 'y (id 'b) (substitute 'z (id 'c) (postfix (id 'd) (postfix (id 'x) (postfix (id 'y) (id 'z) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(x y z))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(a x))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(x))

