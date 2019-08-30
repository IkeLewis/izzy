(define-module (izzy usb-generic-driver)
  #:export (<usb-generic-driver>)
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
  #:use-module (rnrs io ports)
  ;; for parse-c-struct
  #:use-module (system foreign))

(define-class <usb-generic-driver> (<driver>))

(define (read-kernel-input-event usb-dr)
  "Attempts to read a complete input event from a usb human input
device."
  (let* ((kbd-ip (slot-ref usb-dr 'input-port))
	 (bv (get-bytevector-n kbd-ip 24)))
    (if (eof-object? bv)
	bv
	(let* (
	       ;; We don't have to worry about heap allocation here
	       ;; because the memory allocated by get-bytevector-n will be
	       ;; managed by scheme and is subject to garbage collection;
	       ;; get-bytevector-n creates a scheme bytevector that holds
	       ;; 24 bytes.  We then obtain a pointer to the scheme
	       ;; bytevector's data and parse it as a C structure of the
	       ;; indicated form.
	       (kie-as-list (parse-c-struct (bytevector->pointer bv)
					    (list (list long long) uint16 uint16 int32)))
	       (timeval-as-list (first kie-as-list))
	       (event-type (second kie-as-list))
	       (event-code (third kie-as-list))
	       (event-value (fourth kie-as-list)))
	  (cond ((equal? event-type hid-ev-key)
		 (make <kernel-input-event>
		   (apply make <timeval> timeval-as-list)
		   ;; common event types:
		   ;; hid-ev-key = 1
		   ;; hid-ev-syn = 0
		   ;; hid-ev-msc = 4
		   event-type
		   ;; common event types:
		   ;; hid-key-esc = 1
		   ;; hid-key-1 = 1
		   ;; ...
		   ;; hid-key-a = 30
		   ;; ...
		   ;; hid-key-z = 44
		   ;; ...
		   event-code
		   ;; common event values:
		   ;; hid-key-press = 1
		   ;; hid-key-release = 0
		   ;; hid-key-repeat = 2
		   event-value
		   ;; TODO?: handle modifiers here; alternatively, we could
		   ;; simply defer this responsibility to the individual
		   ;; handlers.
		   ;; (modifier? event-code)
		   )
		 )
		;; Ignore all events other than key events
		(#t
		 (read-kernel-input-event usb-dr)))))))

(define-method (initialize (obj <usb-generic-driver>) initargs)
  (set! initargs
	(append!
	 (list (car initargs) read-kernel-input-event)
	 (cdr initargs)))
  (next-method))
