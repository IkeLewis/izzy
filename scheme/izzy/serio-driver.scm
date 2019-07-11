(define-module (izzy serio-driver)
  #:export (<serio-driver> serio-code->kernel-input-event)
  #:use-module (srfi srfi-1)
  #:use-module (izzy driver)
  #:use-module (izzy serio-constants)
  #:use-module ((izzy hid-constants) #:prefix hid-)
  #:use-module (izzy input-event)
  #:use-module (izzy timeval)
  #:use-module (izzy kernel-input-event)
  #:use-module (oop goops)
  #:use-module (izzy misc) ;; for errorf
  #:use-module (system foreign))

(define-class <serio-driver> (<driver>))

(define-method (serio-code->kernel-input-event (psc <integer>) (sc <integer>))
  (let ((tr (find (lambda (tr) (member sc tr)) serio-triples)))
    (if (not tr)
	(errorf "Invalid/unrecognized serio code ~a" sc)
	(make <kernel-input-event>
	  (make <timeval>)
	  hid-ev-key
	  (eval (symbol-append 'hid- (car tr))
		(resolve-module '(izzy serio-driver)))
	  (cond ((equal? sc (third tr)) hid-key-release)
		((equal? psc sc) hid-key-repeat)
		(else hid-key-press))))))

(define-method (initialize (obj <serio-driver>) initargs)
  ;; TODO: fix the order of initialized args
  (set! initargs 
    (append! (list-head initargs 1) (list ignored-serial-codes serial-codes serio-code->kernel-input-event) (cdr initargs)))
  (next-method))
