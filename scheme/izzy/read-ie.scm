(define-module (izzy read-ie)
  #:export (readIE readIE1 readIE2)
  #:use-module (izzy kernel-input-event)
  #:use-module (oop goops)
  #:use-module (system foreign))

;;; This module is now obsolete and may be removed in future versions.

;; A wrapper for the foreign function readIE defined in the C
;; library libizzy.  Attempts to read an input event from the given
;; port.  Returns either a list representing the input event or
;; throws an error.

(let ((f (pointer->procedure
	  int
	  (dynamic-func "readIE" (dynamic-link (string-append (dirname (current-filename)) "/../../lib/libread_ie.so")))
	  (list int '*))))
  (define-method (readIE1 (port <input-port>))
    (let* ((ie-c-struct (input-event->c-struct (make <kernel-input-event>)))
	   (ret (f (fileno port) ie-c-struct)))
      (if (zero? ret)
	  (let ((iew (c-struct->input-event ie-c-struct)))
	    (when (and debug (= (type iew) ev-key))
	      (display iew)
	      (newline))
	    iew)
	  ;; TODO: retrieve the error message
	  (error "readIE returned" ret)))))

(define-method (readIE2 (kbdp <input-port>))
  (parse-c-struct (bytevector->pointer (get-bytevector-n kbdp 24))
		  (list (list long long) uint16 uint16 int32)))

(define readIE readIE1)
