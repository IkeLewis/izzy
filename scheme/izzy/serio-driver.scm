(define-module (izzy serio-driver)
  #:export (<serio-driver> read-kernel-input-event)
  #:use-module (srfi srfi-1)
  #:use-module (izzy driver)
  #:use-module (izzy serio-constants)
  #:use-module ((izzy hid-constants) #:prefix hid-)
  #:use-module (izzy input-event)
  #:use-module (izzy timeval)
  #:use-module (izzy kernel-input-event)
  #:use-module (oop goops)
  ;; for errorf
  #:use-module (izzy misc)
  #:use-module (izzy log)
  #:use-module (rnrs io ports)
  #:use-module (system foreign))

(define-class <serio-driver> (<driver>)
  (ignored-input-codes #:init-value '() #:accessor ignored-input-codes)
  (input-codes #:init-value '() #:accessor input-codes))

(define read-kernel-input-event
  ;; TODO: ignore scrollock during initialization.
  (let ((prev-code 0))
    (lambda (dr)
      "Attempts to read a complete input event from a serial port.  This
code assumes the port is using the linux raw driver for serial ports."
      (letrec ((port2 (input-port dr))
	       (helper
		(lambda (cur-code depth)
		  (cond ((>= depth 10)
			 (logdln #t "-- serio-driver: warning: unknown code ~a\n" cur-code)
			 (logdln #t "-- serio-driver: discarding input: ~a\n" (drain-input port2))
			 (helper (get-u8 port2) 0))
			((member cur-code (ignored-input-codes dr))
			 (logdln #t "-- serio-driver: ignoring: code ~a\n" cur-code)
			 (helper (get-u8 port2) 0))
			((member cur-code (input-codes dr))
			 (let ((kie (serio-code->kernel-input-event prev-code cur-code)))
			   (set! prev-code cur-code)
			   kie))
			(else
			 (helper (+ (* 1000 cur-code) (get-u8 port2)) (+ 1 depth)))))))
	(helper (get-u8 port2) 0)))))

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
  (set! initargs
	(append!
	 ;; input-port
	 (list (car initargs))
	 ;; ignored-input-codes
	 (list ignored-serial-codes
	       ;; input-codes
	       serial-codes)
	 ;; optional notify pipe and event queue
	 (cdr initargs)))
  (next-method))
