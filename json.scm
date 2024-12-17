(define-module (json)
  #:export '(decode
	     encode))

(define decode
  (lambda (str)
    (eval-string (dealer str))))

(define encode
  (lambda (sth)
    (cond
     [(eq? sth #t) "true"]
     [(eq? sth #f) "false"]
     [(eq? sth '()) "nil"]
     [(number? sth) (number->string sth)];this is not perfect because 3e5 would be translated to 300000.0 instead of 3e5
     [(string? sth) (string-append "\"" sth "\"")]
     [(alist? sth) (string-append
		    "{"
		    (let ((len (length sth)))
		      (let loop ((i 0))
			(if (>= i len)
			    ""
			    (string-append
			     (encode (car (list-ref sth i)))
			     ":"
			     (encode (cdr (list-ref sth i)))
			     (if (<= i (- len 2))
				 ","
				 "")
			     (loop (1+ i))))))
		    "}")]
     [(or (list? sth) (vector? sth))
      (let ((len (if (list? sth)
		     (length sth)
		     (vector-length sth))))
	(if (zero? len)
	    "[]"
	    (string-append
	     "["
	     (let loop ((i 0))
	       (if (>= i len)
		   ""
		   (string-append
		    (encode (if (list? sth)
				(list-ref sth i)
				(vector-ref sth i)))
		    (if (<= i (- len 2))
			","
			"")
		    (loop (1+ i)))))
	     "]")))])))

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

(define (alist? sth)
  (and (list? sth)
       (not (null? sth))
       (let ((len (length sth)))
	 (let loop ((i 0))
	   (if (>= i len)
	       #t
	       (if (and (pair? (list-ref sth i))
			(not (list? (list-ref sth i))))
		   (loop (1+ i))
		   #f))))))
