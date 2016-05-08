;;; Title: Untitled 4
;;;
;;; Description:
;;; Python and Scheme in-
;;; tertwine to make a dragon
;;; in recursive theme

(define (dragon n d turn)
	(cond ((zero? n) (fd d))
		(else
			(rt turn)
			(dragon (- n 1) (/ d 1.4142) 45)
			(lt (* turn 2))
			(dragon (- n 1) (/ d 1.4142) -45)
			(rt turn)
		)
	)
)

(hideturtle)

(define (draw1 x)
	(speed 0)
	(penup)
	(goto 0 0)
	(rt 120)
	(pendown)
	(dragon x 200 45)
)

(define (draw x)
	(color '"red")
	(draw1 x)
	(color '"yellow")
	(draw1 x)
	(color '"blue")
	(draw1 x)
)
