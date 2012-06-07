

(define (fact n)
  (if (> 2 n)
      n
      (* n (fact (- n 1)))))


(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (exponent? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base s) (car s))

(define (exponent s) (caddr s))

(define (multiplier s) (car s))

(define (multiplicand s) (caddr s))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-sum a1 a2) (+ a1 a2))

(define (make-prod a1 a2) (* a1 a2))

(define (make-expo a1 a2) (expt a1 a2))

(define (calc expr)
  (cond ((number? expr) expr)
	((sum? expr)
	 (make-sum (calc (addend expr))
		   (calc (augend expr))))
	((product? expr)
	 (make-prod (calc (multiplier expr))
		    (calc (multiplicand expr))))
	((exponent? expr)
	 (make-expo (calc (base expr))
		    (calc (exponent expr))))))


