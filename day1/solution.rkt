#lang racket

; part 1
(define (only-digits str) 
  (list->string (filter char-numeric? (string->list str))))

(define (reverse-string str)
  (list->string (reverse (string->list str))))

(define (first-digit str)
  (substring (only-digits str) 0 1))

(define (last-digit str)
  (first-digit (reverse-string str)))

(define (first-last-digits num)
  (string->number (string-append (first-digit num) (last-digit num))))
  
(define (sum lst)
  (foldl + 0 lst))

(define unfiltered (file->lines "input.txt"))
(define filtered-pt1 (map first-last-digits unfiltered))

(sum filtered-pt1)  ; final answer: 54304

; part 2

(define (replace-first-number-word str)
  (regexp-replace #rx"one|two|three|four|five|six|seven|eight|nine" str
                  (lambda (match)
                    (case match
                      [("one") "1"]
                      [("two") "2"]
                      [("three") "3"]
                      [("four") "4"]
                      [("five") "5"]
                      [("six") "6"]
                      [("seven") "7"]
                      [("eight") "8"]
                      [("nine") "9"]))))

(define (replace-last-number-word str)
  (let* ((reversed-str (reverse-string str))
         (replaced (regexp-replace #rx"eno|owt|eerht|ruof|evif|xis|neves|thgie|enin" reversed-str
                                   (lambda (match)
                                      (case match
                                        [("eno") "1"]
                                        [("owt") "2"]
                                        [("eerht") "3"]
                                        [("ruof") "4"]
                                        [("evif") "5"]
                                        [("xis") "6"]
                                        [("neves") "7"]
                                        [("thgie") "8"]
                                        [("enin") "9"])))))
    (reverse-string replaced)))

(define (first-digit-including-words str)
  (first-digit (replace-first-number-word str)))

(define (last-digit-including-words str)
  (last-digit (replace-last-number-word str)))

(define first-digits (map first-digit-including-words unfiltered))
(define last-digits (map last-digit-including-words unfiltered))
(define filtered-pt2 (map string-append first-digits last-digits))

(sum (map string->number filtered-pt2))  ; final answer: 54418
