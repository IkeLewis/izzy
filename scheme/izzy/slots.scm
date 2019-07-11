(define-module (izzy slots)
  #:export (slot-append! slot-remove!)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1))

(define-syntax slot-append!
  (syntax-rules ()
    ((slot-append! obj slot-name new-value)
     (slot-set! obj slot-name (append (slot-ref obj slot-name)
				      (if (list? new-value)
					  new-value
					  (list new-value)))))))

(define-syntax slot-remove!
  (syntax-rules ()
    ((slot-remove! obj slot-name slot-value)
     (slot-set! obj slot-name (remove
			       (lambda (x)
				 (equal? x slot-value))
			       (slot-ref obj slot-name))))
    ((slot-remove! obj slot-name slot-value equal-fn)
     (slot-set! obj slot-name (remove
			       (lambda (x) (equal-fn x slot-value))
			       (slot-ref obj slot-name))))))
