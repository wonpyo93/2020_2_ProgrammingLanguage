#lang plai

;Wonpyo Hong
;2020-09-01, 20:45
;
;Problem 1:
; Solved by myself: Y (But I did look up some basic grammar of Racket)
; Time taken: 10 minutes
; [contract] get-average: number number -> number
; [purpose] to define a function that adds two numbers and divide by 2.
; [tests] (test (get-average 3 8) 5.5)
;         (test (get-average 2 8) 5)
(define (get-average a b)
  (/ (+ a b) 2))
(test (get-average 3 8) 5.5)
(test (get-average 2 8) 5)

;Problem 2:
; Solved by myself: Y
; Time Taken: 2 minutes
; [contract] inchworm-travel: number -> number
; [purpose] define inch and multiply inch with given user input.
; [tests] (test (inchworm-travel 3.2) 8.128)
;         (test (inchworm-travel 10) 25.4)
(define inch 2.54)
(define (inchworm-travel a)
  (* inch a))
(test (inchworm-travel 3.2) 8.128)
(test (inchworm-travel 10) 25.4)

;Problem 3:
; Solved by myself: Y
; Time Taken: 2 minutes
; [contract] volume-cube: number -> number
; [purpose] define volume-cube and multiply the given integer by itself 3 times.
; [tests] (test (volume-cube 3.2) 32.76800000000001)
;         (test (volume-cube 5) 125)
(define (volume-cube a)
  (* a a a))
(test (volume-cube 3.2) 32.76800000000001)
(test (volume-cube 5) 125)

;Problem 4:
; Solved by myself: Y
; Time Taken: 3 minutes
; [contract] my-BMI: number number -> number
; [purpose] define my-BMI and multiply height by itself one time and divide the result from weight.
; [tests] (test (round (my-BMI 60 1.7)) 21.0)
;         (test (round (my-BMI 82 1.79)) 26.0)
(define (my-BMI a b)
  (/ a (* b b)))
(test (round (my-BMI 60 1.7)) 21.0)
(test (round (my-BMI 82 1.79)) 26.0)

;Problem 5:
; Solved by myself: Y (I needed to find reference of how to use foldl and foldr, so I've checked documents)
;                     (https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._foldr%29%29)
; Time Taken: 20 minutes
; [contract] fib: number -> number
; [purpose] define fib as the following
;           if fib list num is 0 or 1 or 2, preset the values,
;           if fib list num is over 2, make a loop until the num - 2 (ex. if num is 3, do loop for 1 time)
;           and add the first and second number of the initial list, (1 1),
;           then add the sum to the initial list to the left and update the list.
;           when this process is over, flip the list.
;            ex) fib 5
;            (1+1) and '(1 1)       -> '(2 1 1)
;            (2+1) and '(2 1 1)     -> '(3 2 1 1)
;            (3+2) and '(3 2 1 1)   -> '(5 3 2 1 1)
;            flip -> '(1 1 2 3 5)
; [tests] (test (fib 5) '(1 1 2 3 5))
;         (test (fib -1) '())
;         (test (fib 0) '())
;         (test (fib 10) '(1 1 2 3 5 8 13 21 34 55))
(define (fib a)
  (define tempList '(1 1))
  (cond
       [(<= a 0) '()]
       [(= a 1) '(1)]
       [(= a 2) '(1 1)]
       [(> a 2)
        (for ([i (- a 2)])
          (set! tempList (cons (+ (first tempList) (second tempList)) tempList)))
  (foldl cons '() tempList)]
       ))
(test (fib 5) '(1 1 2 3 5))
(test (fib -1) '())
(test (fib 0) '())
(test (fib 10) '(1 1 2 3 5 8 13 21 34 55))

;Problem 6:
; Solved by myself: Y
; Time Taken : 30 minutes
; [contract] : vehicle-tax: Type number number number -> number
;              is-vehicle-safe: Type -> string
; [purpose] : create a type 'Vehicle' that has 3 children: Bicycle, Car and Airplane.
;             then create vehicle-tax which gives certain tax amount of wheels, windows and engines
;              which will be multiplied to the the number of ones from the Vehicle selected, respectively.
;              then, add them all and return the value.
;             then create is-vehicle-safe which checks each wheels, windows, and engines of the selected Vehicle Type
;              and see if all conditions meet.
; [tests]: (test (vehicle-tax vOne 10 20 30) 20)
;          (test (vehicle-tax vTwo 11 12 16) 92)
;          (test (vehicle-tax vThree 104 390 130) 31642)
;          (test (is-vehicle-safe vOne) "safe")
;          (test (is-vehicle-safe vTwo) "safe")
;          (test (is-vehicle-safe vThree) "unsafe")

(define-type Vehicle
  (Bicycle [wheels number?])
 (Car [wheels number?]
       [windows number?])
  (Airplane [wheels number?]
            [windows number?]
            [engines number?]))

(define vOne (Bicycle 2))
(define vTwo (Car 4 4))
(define vThree (Airplane 3 80 1))

(define (vehicle-tax a b c d)
  (cond
    [(Bicycle? a)
       (+ (* b (Bicycle-wheels a)))]
    [(Car? a)
       (+ (* b (Car-wheels a)) (* c (Car-windows a)))]
    [(Airplane? a)
       (+ (* b (Airplane-wheels a)) (* c (Airplane-windows a)) (* d (Airplane-engines a)))]
  ))

(define (is-vehicle-safe a)
  (define answer "unsafe")
  (cond
    [(Bicycle? a)
       (cond
         [(< (Bicycle-wheels a) 4)
          (set! answer "safe")])]
    [(Car? a)
     (cond
         [(and (> (Car-wheels a) 3)(> (Car-windows a) 2))
          (set! answer "safe")])]
    [(Airplane? a)
     (cond
         [(and (> (Airplane-wheels a) 2)(> (Airplane-windows a) 10) (> (Airplane-engines a) 1))
          (set! answer "safe")])])
  answer)

(test (vehicle-tax vOne 10 20 30) 20)
(test (vehicle-tax vTwo 11 12 16) 92)
(test (vehicle-tax vThree 104 390 130) 31642)
(test (is-vehicle-safe vOne) "safe")
(test (is-vehicle-safe vTwo) "safe")
(test (is-vehicle-safe vThree) "unsafe")

;Problem 7:
; Solved by myself: Y (I did have to look through the grammar again on docs.racket-lang.org,
;                     https://docs.racket-lang.org/reference/strings.html)
; Time Taken: 15 minutes
; [contract] : update-name: string string list -> list
; [purpose] : go through the input list one by one and check whether it is equal to the given string.
;             if this is true, update the list element by adding the other given string to it.
; [tests] : (test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
;           (test (update-name "wonpyo" " eats donuts" '("jc" "claire" "kate" "WonPyO" "wonpyo")) '("jc" "claire" "kate" "WonPyO" "wonpyo eats donuts")) 
;           (test (update-name "WonPyO" " plays GAMES" '("jc" "claire" "kate" "WonPyO" "wonpyo")) '("jc" "claire" "kate" "WonPyO plays GAMES" "wonpyo"))
(define (update-name a b c)
  (for ([i (length c)])
    (cond
      [(string=? a (list-ref c i))
       (set! c (list-set c i (string-append a b)))]
    ))
  c
  )
(test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
(test (update-name "wonpyo" " eats donuts" '("jc" "claire" "kate" "WonPyO" "wonpyo")) '("jc" "claire" "kate" "WonPyO" "wonpyo eats donuts"))
(test (update-name "WonPyO" " plays GAMES" '("jc" "claire" "kate" "WonPyO" "wonpyo")) '("jc" "claire" "kate" "WonPyO plays GAMES" "wonpyo"))

;Problem 8:
; Solved by myself: Y
; Time Taken : 30 minutes
; [contract] : binary-search: list number -> list
; [purpose] : to search through the given list and find the given number, and record all the history in order to find the number using recursion.
; [tests] : (test (binary-search '(1 2 3) 3) '(2 3))
;           (test (binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
;           (test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
;           (test (binary-search '(1 2 3 4 5 6 7 8) 7) '(4 6 7))
(define (binary-search a b)
  (define record '())
  (cond
    [(= b (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)))
     (set! record (append record (list b)))
     ]
    [(> b (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)))
     (set! record (append record (list (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)))))
     (set! record (append record (binary-search (member (list-ref a (exact-ceiling (/ (length a) 2))) a) b)))
     ]
    [(< b (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)))
     (set! record (append record (list (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)))))
     (set! record (append record (binary-search (foldl cons '() (member (list-ref a (- (exact-ceiling (/ (length a) 2)) 1)) (foldl cons '() a))) b)))
     ]
    )
  record
  )
(test (binary-search '(1 2 3) 3) '(2 3))
(test (binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
(test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
(test (binary-search '(1 2 3 4 5 6 7 8) 7) '(4 6 7))