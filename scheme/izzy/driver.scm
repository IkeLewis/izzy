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
  (driver-thread #:init-value #f #:accessor driver-thread)
  (input-port #:init-value #f #:accessor input-port)
  (read-kie-func #:init-value (lambda (x) (error "this must be set")) #:accessor read-kie-func)
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
  (slot-set! obj 'driver-thread #f)
  (set-input-port! obj (first initargs))
  (slot-set! obj 'read-kie-func (second initargs))
  ;; list-ref regards the first element as being at position 0
  (if (> (length initargs) 2)
      (slot-set! obj 'notify-pipe (list-ref initargs 2))
      (slot-set! obj 'notify-pipe #f))
  (if (> (length initargs) 3)
      (slot-set! obj 'event-queue (list-ref initargs 3))
      (slot-set! obj 'event-queue #f)))

;;; Public methods

(define-method (start (dr <driver>))
  (slot-set! dr 'driver-thread
	     (call-with-new-thread
	      (lambda ()
		  (while #t
			 (let* ((kie ((read-kie-func dr) dr)))
			   (when (and (notify-pipe dr)
				      (event-queue dr))
			     (enq! (event-queue dr) kie)
			     (write-char #\r (cdr (notify-pipe dr)))
			     (force-output (cdr (notify-pipe dr))))))))))

(define-method (stop (dr <driver>))
  (when (thread? (slot-ref dr 'driver-thread))
    (cancel-thread (slot-ref dr 'driver-thread))))
