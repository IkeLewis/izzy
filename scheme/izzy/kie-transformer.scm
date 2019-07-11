(define-module (izzy kie-transformer)
  #:export (<bi-kie-transformer> <super-kie-transformer> bi-keys state bi s1 events try-transform! test-bi-tr)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (izzy customize)
  #:use-module (izzy hid-constants)
  #:use-module (izzy slots)
  #:use-module (izzy kernel-input-event)
  #:use-module ((rnrs base) #:select (assert))
  #:use-module (izzy log))

(define-class <kie-transformer> ())

(define-method (try-transform! (kt <kie-transformer>)
			       (kie <kernel-input-event>))
  (values (list kie) #t))

(define-class <bi-kie-transformer> (<kie-transformer>)
  (bi-keys #:accessor bi-keys)
  (state #:init-value 1 #:accessor state)
  (bi #:accessor bi)
  (s1 #:accessor s1)
  (events #:accessor events))

(define-method (initialize (obj <bi-kie-transformer>) initargs)
  ;; TODO: add type-checking for bi-keys, and use the standard error
  ;; signal for too many args.
  (unless (<= (length initargs) 1)
    (error "Too many arguments"))
  (for-each (lambda (var val)
	      (slot-set! obj var val))
	    (list 'bi-keys)
	    (append initargs (list bi-keys-dflt)))
  
  (slot-set! obj 'state 0)
  (slot-set! obj 'bi #f)
  (slot-set! obj 's1 #f)
  (slot-set! obj 'events '()))

(define-macro (with-bi-convenience-funcs bi-tr . body)

  `(let* ((bi-keys (lambda () (bi-keys ,bi-tr)))
	  (state (lambda () (state ,bi-tr)))
	  (bi (lambda () (bi ,bi-tr)))
	  (s1 (lambda () (s1 ,bi-tr)))
	  (events (lambda () (events ,bi-tr)))
	  (take-events! (lambda () (let ((ev (events)))
				     (slot-set! ,bi-tr 'events '())
				     ev)))
	  (append-to-events! (lambda (kie) (slot-append! ,bi-tr 'events kie)))
	  (reset! (lambda ()
		    (let ((ev (events)))
		      (slot-set! ,bi-tr 'state 0)
		      (slot-set! ,bi-tr 'bi #f)
		      (slot-set! ,bi-tr 's1 #f)
		      (slot-set! ,bi-tr 'events '())
		      ev))))
     ;; Unfortunately, this macro does not handle 
     ,@body))

(define-method (next-state (bi-tr <bi-kie-transformer>)
			   (kie <kernel-input-event>))
  (with-bi-convenience-funcs bi-tr
			     (case (state)
			       ((0)
				;; w_
				(if (and (press? kie)
					 (member (code kie) (bi-keys)))
				    1
				    0))
			       ((1)
				;; w_ wt
				(cond ((repeat? kie) 4)
				      ;; w_ <mod>_
				      ((and (modifier? kie) (press? kie)) 4)
				      ;; w_ j_
				      ((press? kie) 2)
				      ;; w_ w^
				      ((and (release? kie)
					    (equal-codes? kie (bi))) 0)
				      ;; w_ (~w)^
				      ((release? kie) 1)))
			       ((2)
				;; w_ j_ jt
				(cond ((repeat? kie) 4)
				      ;; w_ j_ k_
				      ((press? kie) 0)
				      ;; w_ j_ j^
				      ((and (equal-codes? kie (s1))
					    (release? kie)) 4)
				      ;; w_ j_ w^
				      ((and (equal-codes? kie (bi))
					    (release? kie)) 0)
				      ;; w_ j_ (~[jw])^
				      ((release? kie) 2)))
			       ((4)
				(logln #t "bi: ~a\n" (<kernel-input-event>->symbol (bi)))
				(cond ((and (equal-codes? kie (bi))
					    (release? kie)) 0)
				      (else 4))))))

(define-method (try-transform! (bi-tr <bi-kie-transformer>)
			       (kie <kernel-input-event>))
  (let* ((bi-keys (lambda () (bi-keys bi-tr)))
	 (state (lambda () (state bi-tr)))
	 (bi (lambda () (bi bi-tr)))
	 (s1 (lambda () (s1 bi-tr)))
	 (events (lambda () (events bi-tr)))
	 (take-events! (lambda () (let ((ev (events)))
				    (slot-set! bi-tr 'events '())
				    ev)))
	 (append-to-events! (lambda (kie) (slot-append! bi-tr 'events kie)))
	 (reset! (lambda ()
		   (let ((ev (events)))
		     (slot-set! bi-tr 'state 0)
		     (slot-set! bi-tr 'bi #f)
		     (slot-set! bi-tr 's1 #f)
		     (slot-set! bi-tr 'events '())
		     ev))))
    (let* ((f (state))
	   (t (next-state bi-tr kie))
	   (evs (case f
		  ((0)
		   (append-to-events! kie)
		   (case t
		     ((0) (reset!))
		     ((1)
		      (slot-set! bi-tr 'bi kie) 	  
		      '())))
		  ((1)
		   (append-to-events! kie)
		   (case t
		     ((0)
		      (reset!))
		     ((1) '())
		     ((2)
		      (slot-set! bi-tr 's1 kie)
		      '())
		     ((4)
		      (slot-set! (bi) 'modifier? #t)
		      (slot-set! kie 'modifier? #t)
		      (take-events!))))
		  ((2)
		   (case t
		     ;; stray release
		     ((2)
		      (append-to-events! kie)
		      '())
		     ((4)
		      (append-to-events! kie)
		      (slot-set! (bi) 'modifier? #t)
		      (take-events!))
		     ((0)
		      ;; w_ j_ k_ Note that
		      ;; k_ is not taken
		      ;; because it was not
		      ;; added to the list of
		      ;; events.  TODO: add
		      ;; code to push k_ back
		      ;; into the q
		      ;; w_ mo1_ -> 4
		      (unless (press? kie)
			(append-to-events! kie))
		      (reset!))))
		  ((4)
		   (append-to-events! kie)
		   (case t
		     ((0)
		      (slot-set! kie 'modifier? #t)
		      (reset!))
		     ((4)
		      (when (and (repeat? kie) (equal-codes? kie (bi)))
			(slot-set! kie 'modifier? #t))
		      (take-events!)))))))
      (logln #t "moving from ~a to ~a\n" f t)
      (slot-set! bi-tr 'state t)
      (values evs (not (= f t 0))))))

(define-class <mo-kie-transformer> (<kie-transformer>)
  (only-modifiers #:accessor only-modifiers))

(define-method (initialize (obj <mo-kie-transformer>) initargs)
  (slot-set! obj 'only-modifiers only-modifiers-dflt))

(define-method (try-transform! (mo-tr <mo-kie-transformer>)
			       (kie <kernel-input-event>))
  (cond ((member (code kie) (only-modifiers mo-tr))
	 (slot-set! kie 'modifier? #t)
	 (values kie #t))
	(else
	 (values kie #f))))

(define-class <super-kie-transformer> (<kie-transformer>)
  (sub-transformers #:accessor sub-transformers))

(define-method (initialize (obj <super-kie-transformer>) initargs)
  (slot-set! obj 'sub-transformers (list (make <mo-kie-transformer>)
					 (make <bi-kie-transformer>)
					 (make <kie-transformer>))))

;; (transform-st! 
;;			   



(define-method (try-transform! (st <super-kie-transformer>)
			       (kie <kernel-input-event>))

  (let ((mo-tr (first (sub-transformers st)))
	(bi-tr (second (sub-transformers st))))
    (map deep-clone (try-transform! bi-tr (try-transform! mo-tr kie)))))

;; (define-method (try-transform! (st <super-kie-transformer>)
;; 			       (kie <kernel-input-event>))
;;   ;; This is a temporary hack
;;   (apply values
;; 	 (call-with-prompt 'done
;; 			   (lambda ()
;; 			     (for-each (lambda (tr)
;; 					 (call-with-values
;; 					     (lambda ()
;; 					       (try-transform! tr kie))
;; 					   (lambda (transformed-input evs)
;; 					     (when transformed-input
;; 					       (abort-to-prompt 'done
;; 								transformed-input
;; 								evs)))))
;; 				       (sub-transformers st)))
;; 			   (lambda (before ti evs2)
;; 			     (logln #t "ti: ~a, evs2: ~a\n" ti evs2)
;; 			     (list ti evs2)))))


;; TODO: make a bug report
;;
;; (define-method (try-transform! (st <super-kie-transformer>)
;; 			   (kie <kernel-input-event>))
;;   (call-with-prompt 'done
;; 		    (lambda ()
;; 		      (for-each (lambda (tr)
;; 				  (call-with-values
;; 				      (lambda ()
;; 					(try-transform! tr kie))
;; 				    (lambda (transformed-input evs)
;; 				      (when transformed-input
;; 					(abort-to-prompt 'done
;; 							 transformed-input
;; 							 evs)))))
;; 				(sub-transformers st)))
;; 		    (lambda (before ti evs2)
;; 		      (logln #t "ti: ~a, evs2: ~a\n" ti evs2)
;; 		      ;; TODO: figure out why this doesn't work as expected.
;; 		      (values ti evs2))))

;; (define (transform-st! st kie)
;;   (call-with-prompt 'done
;; 		    (lambda ()
;; 		      (for-each (lambda (tr)
;; 				  (call-with-values
;; 				      (lambda ()
;; 					(try-transform! tr kie))
;; 				    (lambda (transformed-input evs)
;; 				      (when transformed-input
;; 					(abort-to-prompt 'done
;; 							 transformed-input
;; 							 evs)))))
;; 				(sub-transformers st)))
;; 		    (lambda (before ti evs2)
;; 		      (logln #t "ti: ~a, evs2: ~a\n" ti evs2)
;; 		      (values ti evs2))))

(define-method (test-bi-tr)
  (let ((bi-tr (make <bi-kie-transformer>))
	(b_ (make <kernel-input-event> ev-key key-b key-press))
	(b^ (make <kernel-input-event> ev-key key-b key-release))
	(a_ (make <kernel-input-event> ev-key key-a key-press))
	(a^ (make <kernel-input-event> ev-key key-a key-release)))
    (assert (not (try-transform! bi-tr b_)))
    (assert (not (try-transform! bi-tr b^)))
    (assert (try-transform! bi-tr a_))
    (assert (try-transform! bi-tr a^))))

(define-method (test-su-tr)
  (let ((su-tr (make <super-kie-transformer>))
	(b_ (make <kernel-input-event> ev-key key-b key-press))
	(b^ (make <kernel-input-event> ev-key key-b key-release))
	(a_ (make <kernel-input-event> ev-key key-a key-press))
	(a^ (make <kernel-input-event> ev-key key-a key-release)))
    (assert (try-transform! su-tr b_))
    (assert (try-transform! su-tr b^))
    (assert (try-transform! su-tr a_))
    (assert (try-transform! su-tr a^))))

;; (begin
;;   (use-modules (izzy kie-transformer))
;;   (use-modules (izzy hid-constants))
;;   (use-modules (oop goops))
;;   (use-modules (izzy kernel-input-event))

;;   (reload-module (resolve-module '(izzy hid-constants)))
;;   (reload-module (resolve-module '(izzy kernel-input-event)))
;;   (reload-module (resolve-module '(izzy kie-transformer)))

;;   (define su-tr (make <super-kie-transformer>))
;;   (define a_ (make <kernel-input-event> ev-key key-a key-press))
;;   (define a^ (make <kernel-input-event> ev-key key-a key-release)))

;; (define-method (tt (n1 <number>)
;; 		   (n2 <number>))
;;   (call-with-prompt 'done
;; 		    (lambda ()
;; 		      (for-each (lambda (tr)
;; 				  (call-with-values
;; 				      (lambda ()
;; 					(values #t '()))
;; 				    (lambda (transformed-input evs)
;; 				      (when transformed-input
;; 					(abort-to-prompt 'done
;; 							 transformed-input
;; 							 evs)))))
;; 				(list 1 2 3)))
;; 		    (lambda (before ti evs2)
;; 		      (logln #t "ti: ~a, evs: ~a\n" ti evs2)
;; 		      (values ti evs2))))

;; (begin
;;   (use-modules (izzy kie-transformer))
;;   (use-modules (izzy hid-constants))
;;   (use-modules (oop goops))
;;   (use-modules (izzy kernel-input-event))

;;   (reload-module (resolve-module '(izzy hid-constants)))
;;   (reload-module (resolve-module '(izzy kernel-input-event)))
;;   (reload-module (resolve-module '(izzy kie-transformer)))

;;   (define su-tr (make <super-kie-transformer>))
;;   (define bi-tr (make <bi-kie-transformer>))
;;   (define a_ (make <kernel-input-event> ev-key key-a key-press))
;;   (define a^ (make <kernel-input-event> ev-key key-a key-release)))
