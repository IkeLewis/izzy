(define-module (izzy key)
  #:export (<key> code modifier-flag)
  #:use-module (izzy hid-constants)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 receive)
  #:use-module (izzy timeval)
  #:use-module (izzy log)
  #:use-module (izzy kernel-input-event)
  #:use-module (oop goops))

(define-class <key> ()
  ;;  "A single key consists of a code together with a modifier flag."
  (code #:init-value #f #:accessor code)
  (modifier-flag #:init-value #f #:accessor modifier-flag)
  (time #:init-value 0 #:accessor time))

(define-method (initialize (obj <key>) initargs)
  (for-each (lambda (var val)
	      (slot-set! obj var val))
	    (list 'code 'modifier-flag)
	    (if (null? initargs)
		(make-list 2 #f)
		(append initargs (make-list 2 #f))))
  (if (modifier-flag obj)
      (slot-set! obj 'modifier-flag #t)))

(define-method (display (k <key>) . args)
  (logln (or (car args) #t) "code: ~a\nmodifer-flag: ~a\n"
		 (code-name (code k))
		 (modifier-flag k)))

(define-method (to-key-input-events (keys <list>))
  (if (every (lambda (x) (is-a? x <key>)) keys)
      (receive (kie rest) (parse-key-input-event keys)
	;;	  (logln #t "rest:~a\n" rest)
	(if (null? rest)
	    (list kie)
	    (append (list kie) (to-key-input-events rest))))
      (error "keys must be a list of keys")))  

(define-method (keys->list-of-ie-wrapped (keys <list>))
  "Converts a list of keys to a list of wrapped input events."
  (letrec* ( ;; The time b/w key presses/releases in microseconds
	    (tbwk (make <timeval> 0 30000))
	    ;; The end and start times
	    (make-press-release-input-events
	     (lambda (keys2)
	       (let* ((k (car keys2))
		      (kp (make <kernel-input-event> (make <timeval> 0 0)
				ev-key (code k) key-press))
		      (kr (make <kernel-input-event> (make <timeval> 0 0)
				ev-key (code k) key-release)))
		 ;; TODO: Expand this into an example:
		 ;; pm1-0, pm2-1, pn-2, rn-3, rm2-4, rm1-5
		 (cond ((modifier-flag k)
			(append (list kp)
				(make-press-release-input-events (cdr keys2))
				(list kr)))
		       (else
			(list kp kr))))))
	    (set-key-time! (let ((kt (- (make <timeval>)
					(* (- (* 2 (length keys)) 1) tbwk))))
			     (lambda (key)
			       (set! (time key) kt)
			       (set! kt (+ kt tbwk))
			       key)))
	    )
    (if (every (lambda (x) (is-a? x <key>)) keys)
	(map set-key-time!
	     (apply append
		    (map make-press-release-input-events
			 (to-key-input-events keys))))	  
	(error "keys must be a list of keys"))))


(define (parse-key-input-event keys)
  "Parses a key-input event from the list of keys."
  (letrec ((helper (lambda (keys2 kie)      
		     (if (every (lambda (x) (is-a? x <key>)) keys2)
			 (let ((k (if (null? keys2) #f (car keys2))))
			   (cond ((not k) (error "keys must contain at least one non-modifier"))
				 ((not (modifier-flag k))
				  (values (append kie (list k)) (cdr keys2)))
				 (else	      
				  (helper (cdr keys2) (append kie (list k))))))
			 (error "keys must be a list of keys")))))
    (helper keys '())))
