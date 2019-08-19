(define-module (izzy driver)
  #:export (<driver> start stop)
  #:use-module (oop goops)
  #:use-module (ice-9 q)
  ;; Required by guile-2.2
  #:use-module (ice-9 threads)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (izzy serio-constants)
  #:use-module (izzy hid-constants)
  #:use-module (izzy kernel-input-event)
  #:use-module (izzy log))

(define-once izzy-debug #f)

(define-class <driver> ()
  (main-thread #:init-value #f #:accessor main-thread)
  (input-port #:init-value #f #:accessor input-port)
  (prev-code #:init-value 0 #:accessor prev-code)
  (ignored-input-codes #:init-value '() #:accessor ignored-input-codes)
  (input-codes #:init-value '() #:accessor input-codes)
  (conversion-fn #:init-value #f #:accessor conversion-fn)
  (notify-pipe #:init-value #f #:accessor notify-pipe)
  (event-queue #:init-value #f #:accessor event-queue))

(define-method (set-input-port! (dr <driver>) ip-or-filename)
  (cond ((and (is-a? ip-or-filename <input-port>)
	      (binary-port? ip-or-filename))
	 (slot-set! dr 'input-port ip-or-filename))
	((is-a? ip-or-filename <string>)
	 (slot-set! dr 'input-port (open-input-file ip-or-filename #:binary #t)))
	(else
	 (error "ip-or-filename must be a binary-input-port or a string"))))

(define-method (initialize (obj <driver>) initargs)
  ;; TODO: add type checking
  (slot-set! obj 'main-thread #f)
  (set-input-port! obj (first initargs))
  (slot-set! obj 'ignored-input-codes (second initargs))
  (slot-set! obj 'input-codes (third initargs))
  (slot-set! obj 'conversion-fn (fourth initargs))
  (if (> (length initargs) 4)
      (slot-set! obj 'notify-pipe (list-ref initargs 4))
      (slot-set! obj 'notify-pipe #f))
  (if (> (length initargs) 5)
      (slot-set! obj 'event-queue (list-ref initargs 5))
      (slot-set! obj 'event-queue #f)))

(define-method (read-kernel-input-event (dr <driver>))
  "Attempts to read a complete input event from the given driver."
  (letrec ((port2 (input-port dr))
	   (helper
	    (lambda (cur-code depth)
	      (cond ((>= depth 10)
		     (logln #t "Warning: unknown code ~a\n" cur-code)
		     (logln #t "Discarding input: ~a\n" (drain-input port2))
		     (helper (get-u8 port2) 0))
		    ((member cur-code (ignored-input-codes dr))
		     (logln #t "Ignoring: code ~a\n" cur-code)
		     (helper (get-u8 port2) 0))
		    ((member cur-code (input-codes dr))
		     ;;(logln #t "raw code:~a\n" cur-code)
		     cur-code)
		    (else
		     (helper (+ (* 1000 cur-code) (get-u8 port2)) (+ 1 depth)))))))
    (helper (get-u8 port2) 0)))




;;; Public methods

(define-method (start (dr <driver>))
  (slot-set! dr 'main-thread
	     (call-with-new-thread
	      (lambda ()
		(let ((prev-code 0))
		  (while #t
			 (let* ((cur-code (read-kernel-input-event dr))
				(kie ((conversion-fn dr) prev-code cur-code)))
			   (when (and (notify-pipe dr)
				      (event-queue dr))
			     (enq! (event-queue dr) kie)
			     (write-char #\r (cdr (notify-pipe dr)))
			     (force-output (cdr (notify-pipe dr)))
			     ;; (logln #t "0) prev-code: ~a kie: ~a"
			     ;; 	       prev-code
			     ;; 	       kie)
			     (set! prev-code cur-code)))))))))

(define-method (stop (dr <driver>))
  (when (thread? (slot-ref dr 'main-thread))
    (cancel-thread (slot-ref dr 'main-thread))))
