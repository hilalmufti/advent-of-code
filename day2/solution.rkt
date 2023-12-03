#lang racket

; part 1

(define (clean round-str)
  (string-replace (string-replace (string-replace (string-replace (string-replace round-str "," "") " " "")"red" "r") "green" "g") "blue" "b"))

(define (count-cubes str)
  (define (helper so-far counts chars)
    (match chars 
      ['() counts]
      [(cons #\r rest) (helper "" (list (string->number so-far) (second counts) (third counts)) rest)]
      [(cons #\g rest) (helper "" (list (first counts) (string->number so-far) (third counts)) rest)]
      [(cons #\b rest) (helper "" (list (first counts) (second counts) (string->number so-far)) rest)]
      [(cons char rest) (helper (string-append so-far (string char)) counts rest)]))
  (helper "" (list 0 0 0) (string->list (clean str))))

(define (process-line line)
  (let* ([str-list (string-split line ": ")]
         [id (string->number (substring (first str-list) 5))]
         [counts (map count-cubes (string-split (second str-list) ";"))])
    (cons id counts)))

(define (round-possible? round)
  (let ([bag '(12 13 14)])
    (andmap <= round bag)))

(define (game-possible? game)
  (andmap round-possible? (cdr game))) 

(define (possible-ids games)
  (map car (filter game-possible? games)))

(define (sum lst)
  (foldl + 0 lst))

(define game-strs (file->lines "input.txt"))
(define games (map process-line game-strs))
(sum (possible-ids games))  ; Final answer: 1853

; part 2

(define (power rgbs)
  (define (helper r-max g-max b-max rgbs)
    (match rgbs
      ['() (list r-max g-max b-max)]
      [(cons (list r g b) xs) (helper (max r-max r) (max g-max g) (max b-max b) xs)]))
  (foldl * 1 (helper 0 0 0 rgbs)))

(define rgbs (map cdr games))
(define powers (map power rgbs))
(sum powers)  ; Final answer: 72706