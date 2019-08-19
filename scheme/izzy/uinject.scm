(define-module (izzy uinject)
  #:export (uinject)
  #:use-module (izzy timeval)
  #:use-module (izzy kernel-input-event)
  #:use-module (system foreign)
  #:use-module (oop goops)
  #:use-module (izzy input-event)
  #:use-module (izzy log))

;; (add-to-load-path (dirname (current-filename)))
;; A wrapper for the foreign function uinject defined in the C library
;; libizzy.  Attempts to write an input event to the given port.
;; Returns 0 on success or throws an error.

(let ((time-of-last-inj (make <timeval>))
      (cuinject-wrapper-fn (pointer->procedure
			    int
			    (dynamic-func "uinject" (dynamic-link (string-append (dirname (current-filename)) "/../../lib/libuinject")))
			    (list '*))))
  (define-method (uinject (kie <kernel-input-event>))
    ;; TODO: use aif instead
    (logln #t "4) uinject kie:~a\n time: ~a\n" (<kernel-input-event>->symbol kie) (time kie))
    (let ((ret (cuinject-wrapper-fn (kernel-input-event->c-struct kie))))
      (if (zero? ret)
	  ret
	  ;; TODO: retrieve the error message
	  (error "uinject returned" ret)))))


;; Basic conversion to/from c-structs
(let ((type-desc (list (list long long) uint16 uint16 int32)))
  (define-method (kernel-input-event->c-struct (ie <kernel-input-event>))
    (make-c-struct type-desc
		   ;; You can't reuse the time from the input event!
		   ;; (list (seconds (time ie)) (microseconds (time ie)))
		   ;; instead you MUST generate a new time.
		   (let ((tv (make <timeval>)))
		     (list (list (seconds tv) (microseconds tv))
			   (type ie)
			   (code ie)
			   (value ie)))))

  (define-method (c-struct->kernel-input-event (c-struct <foreign>))
    (apply make <kernel-input-event> (parse-c-struct c-struct type-desc))))

;; Convert integers to symbols
