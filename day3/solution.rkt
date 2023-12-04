#lang racket

; part 1
(define (ref 2dvec r c)
  (if (or (< r 0) (>= r (vector-length 2dvec)) (< c 0) (>= c (vector-length (vector-ref 2dvec r)))) 
    #\.
    (vector-ref (vector-ref 2dvec r) c)))

(define (number grid r c)
    (define (number-acc grid r c dir acc)  ; gets digits to the left or right of grid[r][c]
      (if (char-numeric? (ref grid r (+ c dir)))
        (number-acc grid r (+ c dir) dir (cons (ref grid r (+ c dir)) acc))
        acc))
    (string->number (list->string (append (number-acc grid r c -1 '()) (cons (ref grid r c) (reverse (number-acc grid r c +1 '())))))))  ; TODO: refactor to pass a single acc to both number-acc

(define (adjacent-numbers grid r c) 
  (set->list 
    (foldl 
        (λ (x acc) (if (char-numeric? (ref grid (car x) (cdr x))) (set-add acc (number grid (car x) (cdr x))) acc))  ; get adjacent numbers of all adjacent digits
        (set) 
        (map (λ (d) (cons (+ r (first d)) (+ c (second d)))) ; adjacent indices
         (remove '(0 0) (cartesian-product '(-1 0 1) '(-1 0 1)))))))

(define (part-numbers grid)
  (define grid-vec (list->vector (map (λ (line) (list->vector line)) grid)))  ; convert grid to 2d vector for constant time access
  (first (foldl 
    (λ (row acc)
      (let* 
        ([nums-row-col 
          (foldl 
              (λ (x acc)
                (if (not (or (char-alphabetic? x) (char-numeric? x) (equal? x #\.)))  ; if x is a symbol
                  ; can make this line run faster
                  (list (append (first acc) (adjacent-numbers grid-vec (second acc) (third acc))) (second acc) (add1 (third acc)))  ; accumulate adjacent numbers
                  (list (first acc) (second acc) (add1 (third acc)))))  ; otherwise, go to the next element
              acc
              row)]
          [nums (first nums-row-col)]
          [r (second nums-row-col)])
          (list nums (add1 r) 0)))
    (list '() 0 0) 
    grid)))

(define (sum lst)
  (foldl + 0 lst))

(define grid (map (λ (line) (string->list line)) (file->lines "./day3/input.txt")))

(sum (part-numbers grid))  ; final answer: 539590

; part 2
(define (gear-ratios grid)
  (define grid-vec (list->vector (map (λ (line) (list->vector line)) grid)))  ; convert grid to 2d vector for constant time access
  (first (foldl 
    (λ (row acc)
      (let* 
        ([nums-row-col 
          (foldl 
              (λ (x acc)
                (let* ([adj-nums (adjacent-numbers grid-vec (second acc) (third acc))])
                  (if (and (equal? x #\*) (= (length adj-nums) 2))
                    (list (cons (apply * adj-nums) (first acc)) (second acc) (add1 (third acc)))
                    (list (first acc) (second acc) (add1 (third acc))))))
              acc
              row)]
          [nums (first nums-row-col)]
          [r (second nums-row-col)])
          (list nums (add1 r) 0)))
    (list '() 0 0) 
    grid)))

(sum (gear-ratios grid))  ; final answer: 80703636
