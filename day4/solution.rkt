#lang racket

; part 1
(define (matches winning mine)
  (define winning-card? (curry set-member? winning))
  (length (filter winning-card? mine)))

(define games 
  (map 
    (λ (line)
       (let* ([by-colon (string-split line ":")]
              [id (string->number (car (cdr (string-split (car by-colon)))))]
              [both (string-split (car (cdr by-colon)) "|")]
              [winning (list->set (map string->number (string-split (car both))))]
              [mine (map string->number (string-split (car (cdr both))))]) 
              (cons id (matches winning mine))))
    (file->lines "./day4/input.txt")))

(define score (λ (x) (expt 2 (sub1 x))))

(apply + (map score (filter positive? (map cdr games)))) ; Final answer: 28538

; part 2
(define (scratchcards games)  ; scratchcards
  (hash-values 
    (foldl 
      (λ (x acc) ; x = (cons id num-matches)
        (define add-scratchcards (curry + (hash-ref acc (car x))))
        (foldl 
          (λ (card-to-update acc)
            (hash-update acc card-to-update add-scratchcards))  ; increment by num-scratchcards-for-this-card
          acc ; pass in same accumulator
          (inclusive-range (+ (car x) 1) (+ (car x) (cdr x)))))  ; list of scratchcards earned by card id. 
      (make-immutable-hash ; accumulate a hashtable of scratchcard counts, starting each count with 0
        (map cons (inclusive-range 1 (length games)) (make-list (length games) 1)))
      games)))

(apply + (scratchcards games))  ; Final answer: 9425061