(define-module (izzy kernel-input-event)
  #:export (<kernel-input-event> time type code value equal-codes? release? press? repeat? modifier? <kernel-input-event>->symbol key-event?)
  #:use-module (izzy timeval)
  #:use-module ((izzy hid-constants) #:prefix hid-con-)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (izzy log)
  #:use-module (system foreign))

(define-class <kernel-input-event> ()
  ;;  "A wrapper class for input events (c-structs)."
  (time #:init-value #f #:accessor time)
  (type #:init-value 0 #:accessor type) ;; ev-key, ...
  (code #:init-value 0 #:accessor code)   ;; e.g. key-a
  (value #:init-value 0 #:accessor value) ;; key-press, ...
  (modifier? #:init-value #f #:accessor modifier?))

(define-method (equal-codes? (obj <kernel-input-event>) (obj2 <kernel-input-event>))
  (= (code obj) (code obj2)))

(define-method (initialize (obj <kernel-input-event>) initargs)
  ;; The args are initialized in the following order: time, type,
  ;; code, value, repeat-count, modifier?.

  (set! initargs (append (cond ((null? initargs)
				(cons (make <timeval>) initargs))
			       ((is-a? (car initargs) <list>)
				(cons (apply make <timeval> (list-ref initargs 0)) initargs))
			       ((is-a? (car initargs) <timeval>)
				initargs)
			       (else
				(cons (make <timeval>) initargs)))
			 (list 0 0 0 #f)))

  (for-each (lambda (var val)
	      (slot-set! obj var val))
	    (list 'time 'type 'code 'value 'modifier?)
	    initargs)

  (unless (boolean? (modifier? obj))
    (slot-set! obj 'modifier? #f)))

(define-method (display (kie <kernel-input-event>) . args)
  (logln (or (car args) #t) "(time: ~a code: ~a code-name: ~a value: ~a type: ~a modifier?: ~a)\n"
	 (time kie)
	 (code kie)
	 (hid-con-code-name (code kie))
	 (value kie)
	 (type kie)
	 (modifier? kie)))

(define-method (write (kie <kernel-input-event>) . args)
  (apply write
	 (simple-format #f
			"(time: ~s code: ~s code-name: ~s value: ~s
value-name: ~s type: ~s type-name: ~s modifier?: ~s)"
			(time kie)
			(code kie)
			(hid-con-code-name (code kie))
			(value kie)
			(hid-con-value-name (value kie))
			(type kie)
			(hid-con-type-name (type kie))
			(modifier? kie))
	 args))

(define-method (release? (kie <kernel-input-event>))
   (= (value kie) hid-con-key-release))

(define-method (press? (kie <kernel-input-event>))
  (= (value kie) hid-con-key-press))

(define-method (repeat? (kie <kernel-input-event>))
  (= (value kie) hid-con-key-repeat))

(define-method (key-event? (kie <kernel-input-event>))
  (= (type kie) hid-con-ev-key))

(define-method (<kernel-input-event>->symbol (kie <kernel-input-event>))
  (string->symbol
   (string-append "<"
		  (substring (symbol->string (hid-con-code-name (code kie))) 4)
		  ">"
		  (if (modifier? kie) "'" "")
		  (case (hid-con-value-name (value kie))
		    ((key-release) "^")
		    ((key-press) "_")
		    (else "r")))))
