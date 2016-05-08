;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Snow
;;;
;;; Description:
;;;   I once wrote haiku
;;;   Until the day I took an
;;;   Arrow to the knee.

(define (draw)
    (speed 100)
    (bigtree 200 6)
    (exitonclick))

(define (tree d k)
    (if (> k 0)
        (begin
            (fd d)
            (lt 60)
            (tree (/ d 2) (- k 1))
            (tree (/ d 2) (- k 1))
            (lt 120)
            (fd d)
        )
        (begin
            (fd d)
            (rt 180)
            (fd d)
        )
    )
)

(define (bigtree d k)
    (begin 
        (repeat 2
            (lambda ()
                (tree d (- k 1))
                (tree d (- k 1))
                (lt 60)
            )
        )
        (tree d (- k 1))
        (tree d (- k 1))
    )
)

(define (repeat k fn)
    (if (> k 0)
        (begin (fn) (repeat (- k 1) fn))
        nil
    )
)
; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)