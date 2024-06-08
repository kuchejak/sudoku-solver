#lang racket

; ================================================================================================
; SAMPLE SUDOKU
; ================================================================================================

; easy
(define sudoku1
  '((0 0 0 2 6 0 7 0 1)
    (6 8 0 0 7 0 0 9 0)
    (1 9 0 0 0 4 5 0 0)
    (8 2 0 1 0 0 0 4 0)
    (0 0 4 6 0 2 9 0 0)
    (0 5 0 0 0 3 0 2 8)
    (0 0 9 3 0 0 0 7 4)
    (0 4 0 0 5 0 0 3 6)
    (7 0 3 0 1 8 0 0 0)))

; harder
(define sudoku2
  '((2 0 0 3 0 0 0 0 0)
    (8 0 4 0 6 2 0 0 3)
    (0 1 3 8 0 0 2 0 0)
    (0 0 0 0 2 0 3 9 0)
    (5 0 7 0 0 0 6 2 1)
    (0 3 2 0 0 6 0 0 0)
    (0 2 0 0 0 9 1 4 0)
    (6 0 1 2 5 0 8 0 9)
    (0 0 0 0 0 1 0 0 2)))

; extra hard
(define sudoku3
  '((0 2 0 0 0 0 0 0 0)
    (0 0 0 6 0 0 0 0 3)
    (0 7 4 0 8 0 0 0 0)
    (0 0 0 0 0 3 0 0 2)
    (0 8 0 0 4 0 0 1 0)
    (6 0 0 5 0 0 0 0 0)
    (0 0 0 0 1 0 7 8 0)
    (5 0 0 0 0 9 0 0 0)
    (0 0 0 0 0 0 0 4 0)))

(define sudoku4
  '(0))

(define sudoku5
  '((0 2 4 0)
    (1 0 0 3)
    (4 0 0 2)
    (0 1 3 0)))

(define sudoku6
  '((0 6 0 0 0 0 0 8 11 0 0 15 14 0 0 16)
    (15 11 0 0 0 16 14 0 0 0 12 0 0 6 0 0)
    (13 0 9 12 0 0 0 0 3 16 14 0 15 11 10 0)
    (2 0 16 0 11 0 15 10 1 0 0 0 0 0 0 0)
    (0 15 11 10 0 0 16 2 13 8 9 12 0 0 0 0)
    (12 13 0 0 4 1 5 6 2 3 0 0 0 0 11 10)
    (5 0 6 1 12 0 9 0 15 11 10 7 16 0 0 3)
    (0 2 0 0 0 10 0 11 6 0 5 0 0 13 0 9)
    (10 7 15 11 16 0 0 0 12 13 0 0 0 0 0 6)
    (9 0 0 0 0 0 1 0 0 2 0 16 10 0 0 11)
    (1 0 4 6 9 13 0 0 7 0 11 0 3 16 0 0)
    (16 14 0 0 7 0 10 15 4 6 1 0 0 0 13 8)
    (11 10 0 15 0 0 0 16 9 12 13 0 0 1 5 4)
    (0 0 12 0 1 4 6 0 16 0 0 0 11 10 0 0)
    (0 0 5 0 8 12 13 0 10 0 0 11 2 0 0 14)
    (3 16 0 0 10 0 0 7 0 0 6 0 0 0 12 0)))

; ================================================================================================
; PRINT
; ================================================================================================

(define (print-spaces n)
  (cond
    [(= n 0) (void)]
    [#t (printf " ") (print-spaces (- n 1))]))
      

(define (nice-print-list lst max)
  (cond
   [(null? lst) (void)]
   [(null? (cdr lst)) (printf "~s" (car lst)) (print-spaces (- (count-digits max) (count-digits (car lst)))) (printf "\n")]
   [#t (printf "~s" (car lst))
       (print-spaces (+ 1 (- (count-digits max) (count-digits (car lst)))))
       (nice-print-list (cdr lst) max)]))

(define (sud-print sudoku max)
  (cond
    [(null? sudoku) (void)]
    [#t (nice-print-list (car sudoku) max)
        (sud-print (cdr sudoku) max)]))

(define (count-digits-aux n acc)
  (if (< n 10)
    (+ acc 1)
    (count-digits-aux (/ n 10) (+ acc 1))))

(define (count-digits n)
  (count-digits-aux n 0))

; ================================================================================================
; HELPER FUNCTIONS
; ================================================================================================

; returns the number of rows in the sudoku
(define (my-len lst)
  (if (null? lst)
      0
      (+ 1 (my-len (cdr lst)))))

; returns true if the list does not contain n
(define (check-list lst n)
  (cond
    [(null? lst) #t]
    [(= (car lst) n) #f]
    [#t (check-list (cdr lst) n)]))

  
; checks if the number n is in the given row
(define (check-row sudoku row n)
  (cond
    [(null? sudoku) false] 
    [(> row 0) (check-row (cdr sudoku) (- row 1) n)]
    [#t (check-list (car sudoku) n)]))

;(check-row sudoku1 2 3)
;(check-row sudoku1 2 4)
;(check-row sudoku1 8 7)

(define (get-sudoku-without-nulth-column sudoku)
  (if (null? sudoku)
      null
      (cons (cdr (car sudoku)) (get-sudoku-without-nulth-column (cdr sudoku)))))

(define (get-nulth-column sudoku)
  (if (null? sudoku)
      null
      (cons (caar sudoku) (get-nulth-column (cdr sudoku)))))

(define (get-column-n sudoku n)
  (cond
    [(null? sudoku) null]
    [(= n 0) (get-nulth-column sudoku)]
    [#t (get-column-n (get-sudoku-without-nulth-column sudoku) (- n 1))]))

(define (check-clm sudoku clm n)
  (cond
    [(null? sudoku) false]
    [#t (check-list (get-column-n sudoku clm) n)]))

;(check-clm sudoku1 3 2) ;#f
;(check-clm sudoku1 3 5) ;#t
;(check-clm sudoku1 7 9) ;#f
;(check-clm sudoku1 5 1) ;#t

; helper function for get-smaller-exp
(define (get-smaller-exp-aux base n acc)
  (cond
    [(> base n) acc]
    [#t (get-smaller-exp-aux base (- n base) (+ acc 1))]))

; returns the largest multiple of base that is less than n (used for square check)
(define (get-smaller-exp base n)
  (get-smaller-exp-aux base n 0))

; strips sudoku of the given number of rows (from the top)
(define (stripe-rows sudoku rows)
  (cond
    [(null? sudoku) null]
    [(= rows 0) sudoku]
    [#t (stripe-rows (cdr sudoku) (- rows 1))]))

(define (stripe-columns sudoku columns)
  (cond
    [(null? sudoku) null]
    [(= columns 0) sudoku]
    [#t (stripe-columns (get-sudoku-without-nulth-column sudoku) (- columns 1))]))
  
; strips sudoku of the given number of rows (from the top) and the given number of columns (from the left)
(define (stripe-rows-columns sudoku rows columns)
  (stripe-rows (stripe-columns sudoku columns) rows))

; returns the first n numbers from the list
(define (get-first-n lst n)
  (cond
    [(null? lst) null]
    [(= n 0) null]
    [#t (cons (car lst) (get-first-n (cdr lst) (- n 1)))]))

(define (convert-sqr-to-list-aux sudoku sqrdim n)
  (cond
    [(null? sudoku) null]
    [(= n 0) null]
    [#t (append (get-first-n (car sudoku) sqrdim) (convert-sqr-to-list-aux (cdr sudoku) sqrdim (- n 1)))]))
  
; transforms the top left square of the given dimension into a list
(define (convert-sqr-to-list sudoku sqrdim)
  (convert-sqr-to-list-aux sudoku sqrdim sqrdim))
  
; strips sudoku so that a square is in the top left corner, converts this square to a list, and checks if the given number is in it
(define (check-sqr sudoku clm rw n sqrdim)
  (letrec
      ([row (get-smaller-exp sqrdim rw)]
       [column (get-smaller-exp sqrdim clm)])
    (check-list (convert-sqr-to-list (stripe-rows-columns sudoku (* sqrdim row) (* sqrdim column)) sqrdim) n)))

;(check-sqr sudoku1 8 7 1 3) ; #t    
;(check-sqr sudoku1 8 7 2 3) ; #t
;(check-sqr sudoku1 8 7 3 3) ; #f
;(check-sqr sudoku1 8 7 4 3) ; #f
;(check-sqr sudoku1 2 7 4 3) ; #f
;(check-sqr sudoku1 2 7 1 3) ; #t
 

; function that determines if a given number can be placed at a given cell (true/false)
(define (can-place sudoku column row n sqrdim)
  (cond
    [(= n 0) #t] ; this condition simplifies input validation
    [#t (and (check-row sudoku row n) (check-clm sudoku column n) (check-sqr sudoku column row n sqrdim))]))

;(can-place sudoku1 4 4 4 3) ; #f - in row
;(can-place sudoku1 4 4 7 3) ; #f - in column
;(can-place sudoku1 4 4 3 3) ; #f - in square
;(can-place sudoku1 4 4 8 3) ; #t
;(can-place sudoku1 8 8 8 3) ; #f - in row
;(can-place sudoku1 8 8 4 3) ; #f - in square and column

; checks if the number at the given position is fixed (i.e., != 0)
(define (is-bound-list lst n)
  (cond
    [(null? lst) false]
    [(> n 0) (is-bound-list (cdr lst) (- n 1))]
    [#t (not (= (car lst) 0))]))
  
; checks if the number at the given coordinates is fixed (i.e., != 0)
(define (is-bound sudoku column row)
  (cond
    [(null? sudoku) false]
    [(> row 0) (is-bound (cdr sudoku) column (- row 1))]
    [#t (is-bound-list (car sudoku) column)]))

; returns the nth number
(define (get-nth-list lst n)
  (cond
    [(null? lst) '()]
    [(> n 0) (get-nth-list (cdr lst) (- n 1))]
    [#t (car lst)]))
  
; returns the number at the given coordinates
(define (get-number-from-pos sudoku column row)
  (cond
    [(null? sudoku) '()]
    [(> row 0) (get-number-from-pos (cdr sudoku) column (- row 1))]
    [#t (get-nth-list (car sudoku) column)]))


(define (change-nth lst n new)
  (cond
    [(null? lst) null]
    [(> n 0) (cons (car lst) (change-nth (cdr lst) (- n 1) new))]
    [#t (cons new (cdr lst))]))
  
; changes the number at the given coordinates
(define (change-pos sudoku col row n)
  (cond
    [(null? sudoku) null]
    [(> row 0) (cons (car sudoku) (change-pos (cdr sudoku) col (- row 1) n))]
    [#t (cons (change-nth (car sudoku) col n) (cdr sudoku))]))


; returns a list with two elements - the next coordinates for the algorithm in the format (column, row)
; returns null if no more coordinates exist
(define (get-next-crd cl rw max)
  (cond
    [(and (= cl max) (= rw max)) null]
    [(< cl max) (cons (+ cl 1) rw)]
    [(= cl max) (cons 0 (+ rw 1))]))

; ================================================================================================
; INPUT VALIDATION
; ================================================================================================
(define (validate-dim dim)
  (integer? (sqrt dim)))

(define (validate-square-form sudoku size)
  (cond
    [(null? sudoku) #f]
    [(null? (cdr sudoku)) (= (my-len (car sudoku)) size)]
    [#t (and (= (my-len (car sudoku)) size) (validate-square-form (cdr sudoku) size))]))

(define (validate-sudoku-numbers sudoku max col row sqrdim)
   (let ([coord (get-next-crd col row max)])
   (cond
    [(null? coord) (and (< (get-number-from-pos sudoku col row) (+ max 2)) (can-place (change-pos sudoku col row 0) col row (get-number-from-pos sudoku col row) sqrdim))]
    [#t (and (< (get-number-from-pos sudoku col row) (+ max 2)) (can-place (change-pos sudoku col row 0) col row (get-number-from-pos sudoku col row) sqrdim) (validate-sudoku-numbers sudoku max (car coord) (cdr coord) sqrdim))]))) 
      
(define (validate-sudoku sudoku)
  (let ([dim (my-len sudoku)])
    (and (validate-dim dim) (validate-square-form sudoku dim) (validate-sudoku-numbers sudoku (- dim 1) 0 0 (sqrt dim)))))

; ================================================================================================
; SUDOKU SOLVER
; ================================================================================================
(define (brute-last sudoku col row cur max sqrdim)
  (cond
    [(= (- cur 1) max) (cons #f sudoku)]
    [(is-bound sudoku col row) (cons #t sudoku)]
    [(not (can-place sudoku col row (+ cur 1) sqrdim)) (brute-last sudoku col row (+ cur 1) max sqrdim)]
    [#t (cons #t (change-pos sudoku col row (+ cur 1)))]))

(define (brute sudoku col row cur max sqrdim)
  (let ([coord (get-next-crd col row max)])
  (cond
    [(= (- cur 1) max) (cons #f sudoku)]
    [(null? coord) (brute-last sudoku col row cur max sqrdim)]
    [(is-bound sudoku col row) (brute sudoku (car coord) (cdr coord) 0 max sqrdim)] 
    [(not (can-place sudoku col row (+ cur 1) sqrdim)) (brute sudoku col row (+ cur 1) max sqrdim)]
    [#t (let ([res (brute (change-pos sudoku col row (+ cur 1)) (car coord) (cdr coord) 0 max sqrdim)])
              (if (car res)
                  res
                  (brute sudoku col row (+ cur 1) max sqrdim)))])))

(define (one-by-one sudoku)
  (and (= 1(my-len sudoku)) (number? (car sudoku))))
 
(define (solve-sudoku-aux sudoku)      
  (displayln "Calculating...")
  (letrec
      ([max (my-len sudoku)]
       [res (brute sudoku 0 0 0 (- max 1) (sqrt max))])
    (if (car res)
        (sud-print (cdr res) max)
        (println "No solution exists"))))

(define (solve-sudoku sudoku)
  (cond
    [(one-by-one sudoku) (if (or (= (car sudoku) 0) (= (car sudoku) 1))
                           '(1)
                           (println "Invalid input"))]
    [(not (validate-sudoku sudoku)) (println "Invalid input") (void)]
    [#t (solve-sudoku-aux sudoku)]))

