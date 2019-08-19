(define-module (izzy input-event-handler)
  #:export (<input-event-handler> start-ieh stop-ieh reset)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 q)
  ;; required by guile-2.2
  #:use-module (ice-9 threads)
  #:use-module (izzy log)
  #:use-module (izzy hid-constants)
  #:use-module (izzy kernel-input-event)
  #:use-module (izzy input-event)
  #:use-module (izzy slots))

(define-class <input-event-handler> ()
  (main-thread #:init-value #f #:accessor main-thread)
  (repeat-count #:init-value 0 #:accessor repeat-count)
  ;; list of kernel input events
  (events #:init-value '() #:accessor events)
  ;; the list of bi-key kernel input events
  (bis #:init-value '() #:accessor bis)
  ;; list of modifier-key kernel input events
  (ms #:init-value '() #:accessor ms)
  ;; list of non-modifier-key kernel input events
  (nm #:init-value '() #:accessor nm)
  (sent-keys #:init-value '() #:accessor sent-keys)
  (callback #:accessor callback)
  (notify-pipe #:init-value #f #:accessor notify-pipe)
  (event-queue #:init-value #f #:accessor event-queue)

  (only-modifiers #:accessor only-modifiers)
  (bi-keys #:accessor bi-keys)
  ;; TODO: remove the following line entirely
  (bi-keys-enable-repeat #:init-value #f #:accessor bi-keys-enable-repeat)
  (bi-keys-mod-map #:accessor bi-keys-mod-map))

(define-method (sent-key? (kie <kernel-input-event>) (ieh <input-event-handler>))
  (member kie (sent-keys ieh) (lambda (x y) (= (code x)
					       (code y)))))

;; To the best of my knowledge, this was a remnant of an older
;; notation and is no longer needed; down-mod-keys are now referred to
;; as bi-keys.
;;
;; (define-method (down-mod-key?
;; 		(kie <kernel-input-event>) (ieh <input-event-handler>)) (member kie
;; 		(down-mod-keys ieh) (lambda (x y) (= (code x) (code y)))))

(define-method (initialize (obj <input-event-handler>) initargs)
  ;; TODO: add type checking
  (slot-set! obj 'main-thread #f)
  (slot-set! obj 'ms '())
  (slot-set! obj 'bis '())
  (slot-set! obj 'nm '())
  (slot-set! obj 'events '())
  ;;(slot-set! obj 'down-mod-keys '())
  (slot-set! obj 'sent-keys '())
  (slot-set! obj 'repeat-count 0)
  (slot-set! obj 'callback (first initargs))
  (slot-set! obj 'notify-pipe (second initargs))
  (slot-set! obj 'event-queue (third initargs))

  (set! initargs (append! initargs
			  (list only-modifiers-dflt
				bi-keys-dflt
				;; In early development, the
				;; possibility of allowing repetitions
				;; for bi keys was considered, but
				;; after further experimentation, this
				;; approach was not pursued because
				;; more elegant ways of handling
				;; repetitions exist, namely
				;; implementing a simple repetition
				;; command.
				;; bi-keys-enable-repeat-dflt
				bi-keys-mod-map-dflt)))

  (slot-set! obj 'only-modifiers (fourth initargs))
  (slot-set! obj 'bi-keys (fifth initargs))
  ;;TODO: remove this comment block after successful testing.
  ;;(slot-set! obj 'bi-keys-enable-repeat (sixth initargs))
  ;;(slot-set! obj 'bi-keys-mod-map (seventh initargs))
  (slot-set! obj 'bi-keys-mod-map (sixth initargs)))

;;; Defaults

(define ctrl-modifiers-dflt (list key-a key-semicolon))

(define ctrl-c-modifiers-dflt (list key-w key-o key-s key-l))

;; These keys can only be used as modifiers.  Note: the following
;; four emacs keys are not included in the USB HID for some reason:
;; key-leftsuper, key-rightsuper, key-lefthyper, key-righthyper.

(define only-modifiers-dflt (list key-leftctrl
				  key-rightctrl
				  key-leftalt
				  key-rightalt
				  key-leftmeta
				  key-rightmeta
				  key-leftshift
				  key-rightshift))

;; These keys may be used as either modifiers or non-modifiers.

(define bi-keys-dflt (append ctrl-modifiers-dflt ctrl-c-modifiers-dflt))

;; Most applications don't allow ordinary keys to be treated as
;; modifier keys.  Thus for those applications we provide a map from
;; ordinary keys to the classic modifier keys: ctrl, alt, meta, and
;; shift.

;;(cons key-s key-leftmeta)
(define bi-keys-mod-map-dflt (list (cons key-a key-leftctrl)
				   (cons key-semicolon key-rightctrl)))

;; (define (callback-dflt (obj))
;;   )

(define-method (allowed-modifiers (ieh <input-event-handler>))
  "These are the keys that may be used as modifiers."
  (append (only-modifiers ieh) (bi-keys ieh)))

(define-macro (with-convenience-funcs ieh . body)
  ;; TODO: Fix type checking (so that errors are produced when kie is
  ;; not a <kernel-input-event>)
  `(let* ((remove-kie! (lambda* (kie #:optional (pred-fn (lambda (kie1) #t)))
				(let ((remove-pred (lambda (kie1 kie2)
						     (and (equal-codes? kie1 kie2)
							  (pred-fn kie1)
							  (pred-fn kie2)))))
				  (slot-remove! ieh 'ms kie remove-pred)
				  (slot-remove! ieh 'bis kie remove-pred)
				  (slot-remove! ieh 'nm kie remove-pred)
				  (slot-remove! ieh 'events kie remove-pred))))
	  (events (lambda () (events ieh)))
	  (le (lambda () (last (events))))
	  (bis (lambda () (bis ieh)))
	  (ms (lambda () (ms ieh)))
	  (nm (lambda () (nm ieh)))
	  (bi-key? (lambda (obj)
		     (and (is-a? obj <kernel-input-event>)
			  (member (code obj) (bi-keys ,ieh)))))
	  (modifier-only? (lambda (obj)
			    ;; returns true iff obj is a modifier only kie
			    (and (is-a? obj <kernel-input-event>)
				 (member (code obj) (only-modifiers ,ieh)))))
	  (mod-ie? (lambda (kie)
		     ;; returns true iff obj is a modifier kie
		     (member kie (ms) equal-codes?)))

	  (non-modifier? (lambda (obj)
			   ;; returns true iff obj is a non-modifier kie
			   (and (is-a? obj <kernel-input-event>)
				(not (or (bi-key? obj) (modifier-only? obj))))))


	  (stray-release? (lambda (kie)
			    ;; a stray release is a release without a
			    ;; corresponding key press; return true
			    ;; iff obj is stray release.
			    (not (and
				  (release? kie)
				  (any (lambda (kie2)
					 (and (equal-codes? kie kie2)
					      (press? kie2)))
				       (events))))))
	  (take-ie! (lambda ()

		      (let ((cloned-evs (map deep-clone (events))))
			(cond ((release? (le))
			       (remove-kie! (le))
			       (slot-append! ieh 'ms (bis))
			       (slot-set! ieh 'bis '())
			       (slot-set! ieh 'nm '())
			       (slot-set! ieh 'events (ms))
			       cloned-evs)
			      ((repeat? (le))
			       (let ((le-pr (deep-clone (le))))
				 ;; events: <m1> ... <mn> <bi1>_ ... <bin2>_ <bin2>t
				 ;; events: <m1> ... <mn> <bi1>_ ... <bin2>_ <nm>_ <nm>t
				 (remove-kie! (le))
				 (slot-append! ieh 'ms (bis))
				 (slot-set! le-pr 'value key-press)
				 (slot-set! ieh 'bis (if (bi-key? le-pr)
							 (list le-pr)
							 '()))
				 (slot-set! ieh 'nm (if (non-modifier? le-pr)
							(list le-pr)
							'()))
				 (slot-set! ieh 'events (append (ms) (bis) (nm)))
				 (slot-set! (last cloned-evs) 'value key-release)
				 cloned-evs))
			      (else
			       (error "An input event must end with a release or a repeat.")))))))
     ,@body))

(define-method (reset (ieh <input-event-handler>))
  (logln #t "resetting ieh\n")
  (slot-set! ieh 'ms '())
  (slot-set! ieh 'bis '())
  (slot-set! ieh 'nm '())
  (slot-set! ieh 'events '()))

;; This is the default method for reading input events
(define-method (read-input-event (ieh <input-event-handler>))
  ;; Reads and collects key-down events until a complete input event
  ;; is obtained.  Then the input event is returned.  All
  ;; non-key-input events are ignored.
  (with-convenience-funcs
   ieh
   (read-char (car (notify-pipe ieh)))
   (let* ((kie (deq! (event-queue ieh))))
     (when (key-event? kie)
       (slot-append! ieh 'events kie)

       (when (or (modifier-only? (le))
		 (mod-ie? (le)))
	 (slot-append! ieh 'ms (le)))

       (when (and (bi-key? (le))
		  (not (mod-ie? (le))))
	 (slot-append! ieh 'bis (le)))

       (when (non-modifier? (le))
	 (slot-append! ieh 'nm (le)))

       (logln #t "~a\n"
       	      (<kernel-input-event>->symbol kie))

       (cond
	;; ((press? (le))
	;;  (read-input-event ieh))

	((repeat? (le))
	 (cond ((mod-ie? (le))
		(logln #t "case: repeat, mod")
		(remove-kie! (le) repeat?)
		(read-input-event ieh))
	       ((bi-key? (le))
		(logln #t "case: repeat, bi")
		(cond ((= (count bi-key? (events)) 2)
		       ;; TODO: Revist this later.
		       ;; (cond ((prefix-key? (le))
		       ;; 	      (take-ie!)))
		       (remove-kie! (le) repeat?)
		       (if (mod-ie? (le))
			   (logln #t "case: repeat, bi, na")
			   (slot-append! ieh 'ms (bis)))
		       (slot-set! ieh 'bis '())
		       (read-input-event ieh))
		      ((= (count bi-key? (events)) 3)
		       (take-ie!))
		      (else
		       (logln #t "case: repeat,error"))))
	       ((non-modifier? (le))
		(logln #t "case: repeat,nm")
		(take-ie!))
	       (else
		(logln #t "case: repeat, o")
		(remove-kie! (le) repeat?)
		(read-input-event ieh))))

	((release? (le))
	 (cond
	  ((stray-release? (le))
	   (logln #t "case: stray release~a\n" (le))
	   (remove-kie! (le))
	   (read-input-event ieh))
	  ((lset= equal? (ms) (events))
	   (logln #t "case: <m1>_ <m2>_ ... <mn>_ <m-prev>^\n")
	   (remove-kie! (le))
	   (read-input-event ieh))
	  ((lset= equal? (append (ms) (bis)) (events))
	   (logln #t "case: <m1>_ <m2>_ ... <mn>_ <bi1>_ ... <bin2>_ <prev>^\n")
	   (take-ie!))
	  ((lset= equal? (append (ms) (bis) (nm)) (events))
	   (logln #t "case: <m1>_ <m2>_ ... <mn>_ <bi1>_ ... <bin2>_ <nm1>_ <prev>^\n")
	   (take-ie!))
	  (else
	   (logln #t "case: error\nms: ~a\nbis: ~a\nnm: ~a\nevents: ~a\n" (ms) (bis) (nm) (events)))))
	(else
	 (read-input-event ieh)))))))

;; First attempt to interpret (P, r) using

;; If the last member of P has the same code as r, then
;; interpret all but the last member of P as modifiers. Otherwise,
;; interptet P as an ordinary sequence.

;;; Public methods

(define-method (start-ieh (ieh <input-event-handler>))
  (slot-set! ieh
	     'main-thread
	     (call-with-new-thread
	      (lambda ()
		(while #t
		       ((callback ieh) (read-input-event ieh)))))))

(define-method (stop-ieh (ieh <input-event-handler>))
  (when (thread? (slot-ref ieh 'main-thread))
    (cancel-thread (slot-ref ieh 'main-thread))))
