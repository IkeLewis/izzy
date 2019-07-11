(define-module (izzy timeval)
  #:export (<timeval> timeval->int int->timeval seconds microseconds)
  #:use-module (oop goops)
  #:use-module (izzy log)
  #:use-module (il compare))

(define-class <timeval> ())

;; (define-method (set-seconds! (tv <timeval>) (secs <number>))
;;   (hash-set! tvht (pointer-address (scm->pointer tv)) secs))

;; (define-method (get-seconds (tv <timeval>))
;;   (or (hash-ref tvht (pointer-address (scm->pointer tv))) 0))

(define-class <timeval> ()
  (seconds #:init-value 0 #:accessor seconds)
  (microseconds #:init-value 0 #:accessor microseconds))

(define-method (initialize (obj <timeval>) initargs)
  (let ((time (if (null? initargs)
		  (gettimeofday)
		  (cons (car initargs) (cadr initargs)))))      
    (slot-set! obj 'seconds (car time))
    (slot-set! obj 'microseconds (cdr time))))

(define-method (timeval->int (tv <timeval>))
  (+ (* 1000000 (seconds tv)) (microseconds tv) ))

(define-method (int->timeval (i <number>))
  (if (>= i 0)
      (make <timeval> (quotient i 1000000) (remainder i 1000000))
      (error "i must be a non-negative number")))

(define-cops ((t1 <timeval>) (t2 <timeval>))
  (:< (<= (timeval->int t1) (timeval->int t2)))
  (:= (= (timeval->int t1) (timeval->int t2))))

(define-method (- (t1 <timeval>) (t2 <timeval>))
  (if (<= t2 t1)
      (int->timeval (- (timeval->int t1)
		       (timeval->int t2)))
      (error "first timeval must not be less than the second")))



(define-method (+ (t1 <timeval>) (t2 <timeval>))
  (int->timeval (+ (timeval->int t1)
		   (timeval->int t2))))

(define-method (* (n <number>) (tv <timeval>))
  (if (<= 0 n)
      (int->timeval (* n (timeval->int tv)))
      (error "n must be a non-negative integer")))

(define-method (* (tv <timeval>) (n <number>))
  (* n tv))


;; (define-method (* (n <number>) (tv <timeval>))
;;   (if (<= 0 n)
;;       (int->timeval (* n (timeval->int tv)))
;;       (error "n must be a non-negative integer")))


(define-method (display (tv <timeval>) . args)
  (logln (or (car args) #t) "(seconds: ~a microseconds: ~a)"
	 (seconds tv)
	 (microseconds tv)))
