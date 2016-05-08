;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Recursive Spiral Suns
;;;
;;; Description:
;;;   To know recursion,
;;;   you should already know it.
;;;   Explain it again?

(define (draw)
  (begin (background)
	(speed 0)
	(place_spiral (- 270) (- 205) '(.1 0 1) 4)
	(place_spiral (- 180) (- 30) '(.5 0 1) 6)
	(place_spiral 80 70 '(.7 0 1) 10)
	(hideturtle)
	(exitonclick)))

(define (place_spiral x y color scale)
	(begin (penup)
		(goto x y)
		(pendown)
		(color_rgb color))
		(setheading 0)
		(spiral 0 scale 7)))

(define (background)
	(begin
		(penup) 
		(goto (- 350) 350)
		(pendown)
		(color '"black")
		(begin_fill)
		(goto (- 350) (- 350))
		(goto 350 (- 350))
		(goto 350 350)
		(goto (- 350) 350)
		(end_fill) ))

(define (spiral angle step repetitions)
	(if (= repetitions 0)
		(penup)
		(if (> angle 720)
			(begin (pendown) (forward step) (right angle) (spiral (- angle 690) step (- repetitions 1)))
			(begin (pendown) (forward step) (right angle) (spiral (+ angle 1) step repetitions))
		)))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)