;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Recursive Bee House
;;;
;;; Description:
;;;   To know recursion,
;;;   you should already know it.
;;;   Explain it again?

(define (draw)
  (begin 
    (speed 0)
    (penup)
    (goto -150 -173.21)
    (pendown)
    (repeat_hexagon 300 3)
    (hideturtle)
    (exitonclick)))

(define (repeat k fn)
  (if (> k 0)
    (begin (fn) (repeat (- k 1) fn))
    nil
  ))

(define (repeat_hexagon d cycle)
  (if (> cycle 1)
    (repeat 6  
      (lambda () 
        (begin 
          (fd d)
          (lt 60)
          (repeat_hexagon (/ d 2) (- cycle 1))
        )))
    (begin
      (repeat 6 
        (lambda () 
          (begin 
            (fd d) 
            (lt 60)))))
  ))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)