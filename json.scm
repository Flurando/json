(define-module (json)
  #:export '(decode
	     encode))

(define decode
  (lambda (str)
    (eval-string (dealer str))))

(define encode
  (lambda (sth)
    ()))

;;;object -> list or below
;;;array -> vector
;;;string -> string
;;;number -> number
;;;true -> #t
;;;false -> #f
;;;null -> '()

(define true #t)
(define false #f)
(define null '())

(define dealer
  (lambda (str)
    (let ((len (string-length str))
	  (op (open-output-string))
	  (flag #t)
	  (flag-for-paren 0)
	  (last #\.))
      (do ((i 0 (1+ i)))
	  ((>= i len))
	(let ((now (string-ref str i)))
	  (when (char=? now #\")
	    (if flag
		(begin (display "(quote " op)
		       (display #\" op)
		       (set! flag (not flag)))
		(begin
		  (display #\" op)
		  (unless (char=? last #\\)
		    (display ")" op)
		    (set! flag (not flag))))))
	  (unless (char=? #\" now)
	    (if flag
		(case now
		  [(#\[) (begin (display "(vector " op)(set! flag-for-paren (1+ flag-for-paren)))]
		  [(#\]) (begin (display ")" op)(set! flag-for-paren (1- flag-for-paren)))]
		  [(#\{) (display "(list (cons " op)]
		  [(#\}) (display "))" op)]
		  [(#\,) (if (zero? flag-for-paren) (display ")(cons " op) (display " " op))]
		  [(#\:) (display " " op)]
		  [else (display now op)])
	        (display now op)))
	  (set! last now)))
      (get-output-string op))))
