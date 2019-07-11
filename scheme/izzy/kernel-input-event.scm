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
	   (modifier? kie))
  ;;
  ;; (let ((code-name-ie (hid-con-code-name (code ie)))
  ;; 	  (type-name-ie (hid-con-type-name (type ie)))
  ;; 	  (value-name-ie (hid-con-value-name (value ie)))
  ;; 	  (time0 (time ie))
  ;; 	  (tn (type-name ie)))
  ;;   (apply logln (or (car args) #t) "~a\ntype: ~a\ncode: ~a\nvalue: ~a\n"
  ;; 	     (case tn
  ;; 	       ((ev-key) (list time0
  ;; 			       ;; Interpret the type as a type name
  ;; 			       tn
  ;; 			       ;; Interpret the code as a code name
  ;; 			       code-name-ie
  ;; 			       ;; Interpret the value as a press/release/repeat
  ;; 			       value-name-ie))
  ;; 	       ((ev-msc) (list time0
  ;; 			       ;; Interpret both the type and the code as type names
  ;; 			       tn
  ;; 			       (hid-con-type-name (code ie))
  ;; 			       ;; Interpret the value as a code name
  ;; 			       (hid-con-code-name (value ie))))
  ;; 	       ((ev-syn) (list time0
  ;; 			       ;; Interpret the type, code, and value as type names
  ;; 			       tn
  ;; 			       (hid-con-type-name (code ie))
  ;; 			       (hid-con-type-name (value ie))))
  ;; 	       ;; Otherwise don't interpret anything
  ;; 	       (else (list (time ie)
  ;; 			   (code ie)
  ;; 			   (value ie)
  ;; 			   (type ie))))))
  )

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
