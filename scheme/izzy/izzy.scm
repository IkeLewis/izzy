#!/usr/bin/guile \
-L . --fresh-auto-compile -e main -s
!#

;; -L <path-to-site-folder>
;;
;; Don't rely on GUILE_LOAD_PATH b/c you are using different versions
;; of GUILE.
;; 
;; guile --fresh-auto-compile -L ../ -L <site-folder> -e main -s
;; izzy.scm /dev/serio_raw10

(define usage
  "--------------------------------------------------------------------------------
Usage: izzy <device-path>

Starts the izzy input handler on the specified device.\n")

;; How it works (right now):
;;
;; <input-device> 
;; V
;; <transformers>
;; V
;; <uinject>
;; 

;; TODO: izzy <input-device-path> <server-socket-path>
;;
;; Start an izzy server for the specified input device listening for
;; connections on `server-socket-path'.  If none of the clients,
;; handles the input event then the default root handler is used.
;;
;; A client may respond to an input event with `ignored', `observed',
;; or `handled'.  Also a client may issue the `quit' command at any
;; time.  All other responses cause an error to be thrown.

(define (main args)
  (if (not (= (length args) 2))
      (display usage)	     
      (izzy (cadr args))))

(define (izzy device-path)
  (let ()
    ;; (load "/root/.guile")
    ;; (add-sub-dirs-to-load-path
    ;;  "<scheme-dir>")

    (use-modules (oop goops))
    (use-modules (ice-9 q))
    (use-modules (srfi srfi-1))

    (use-modules (izzy customize))
    (use-modules (izzy driver))
    (use-modules (izzy hid-constants))
    (use-modules (izzy kernel-input-event))
    (use-modules (izzy kie-transformer))
    (use-modules (izzy log))
    (use-modules (izzy serio-driver))
    (use-modules (izzy timeval))
    (use-modules (izzy uinject))
    
    (let* ((np3 (pipe))
	   (eq3 (make-q))
	   (sd3 (make <serio-driver> device-path np3 eq3))
	   (st-tr #f)
	   (mt #f)
	   (quit-izzy (lambda ()
			(when (thread? mt)
			      (cancel-thread mt))))

	   (cus-key-prefix (lambda (key-code) 
			     ;;todo: make sure key-code is an integer
			     (list (make <kernel-input-event>
				     ev-key
				     key-leftmeta
				     key-press #t)
				   ;; 
				   (make <kernel-input-event>
				     ev-key
				     key-c
				     key-press #f)
				   (make <kernel-input-event>
				     ev-key
				     key-c
				     key-release #f)
				   (make <kernel-input-event>
				     ev-key
				     key-leftmeta
				     key-release #t)
				   (make <kernel-input-event>
				     ev-key
				     key-code
				     key-press #f)
				   (make <kernel-input-event>
				     ev-key
				     key-code
				     key-release #f))))
	   (rel-key-prefix (lambda (key-code) 
			     ;;todo: make sure key-code is an integer
			     (list (make <kernel-input-event>
				     ev-key
				     key-leftmeta
				     key-press #t)
				   ;; 
				   (make <kernel-input-event>
				     ev-key
				     key-r
				     key-press #f)
				   (make <kernel-input-event>
				     ev-key
				     key-r
				     key-release #f)
				   (make <kernel-input-event>
				     ev-key
				     key-leftmeta
				     key-release #t)
				   (make <kernel-input-event>
				     ev-key
				     key-code
				     key-press #f)
				   (make <kernel-input-event>
				     ev-key
				     key-code
				     key-release #f))))
	 
	   (transform-input! (let ((cc-mode #f)
				   (cc-code #f)
				   (prev-press #f))
			       (lambda (kie)
				 ;;todo: check that kie is a kernel-input-event
				 (let ((result
					(cond ((and (member (code kie) bi-keys-dflt)
						    (modifier? kie))
					       (cond ((= (code kie) key-semicolon)
						      (slot-set! kie 'code key-rightctrl)
						      (list kie))
						     ((= (code kie) key-a)
						      (slot-set! kie 'code key-leftctrl)
						      (list kie))
						     (else
						      (cond ((press? kie)
							     (set! cc-code (code kie))
							     (set! cc-mode #t)
							     (cus-key-prefix cc-code))
							    ((repeat? kie)
							     '())
							    ((release? kie)
							     (set! cc-code #f)
							     (set! cc-mode #f)
							     (rel-key-prefix (code kie)))))))
					      (else
					       (cond ((and cc-mode (press? kie)
							   (not (and prev-press
								     (modifier? prev-press))))
						      (append (cus-key-prefix cc-code) (list kie)))
						     ((and cc-mode (repeat? kie)
							   (not (modifier? kie)))
						      (append (cus-key-prefix cc-code)
							      (list (make <kernel-input-event>
								      ev-key
								      (code kie)
								      key-release #f)
								    (make <kernel-input-event>
								      ev-key
								      (code kie)
								      key-press #f))))
						     (else
						      (list kie)))))))
				   (when (press? kie)
					 (set! prev-press kie))
				   result)))))
      ;; Load the driver
      (start sd3)    

      (set! st-tr (make <super-kie-transformer>))
    
      ;; Start the main thread
      (set! mt
	    (call-with-new-thread
	     (lambda ()
	       (let ((start (make <timeval>)))
		 (logln #t "start time: ~a" start)
		 (while #t
			(read-char (car np3))
			(let* ((kie (deq! eq3)))
			  (when (key-event? kie)
				(logln #t "1) raw kie: ~a time: ~a"
				       (<kernel-input-event>->symbol kie)
				       (time kie))
				(unless (or (= (code kie) key-scrolllock)
					    (< (time kie) start))		 
					(for-each
					 (lambda (kie2)
					   (logln #t "2) st kie: ~a\n"
						  (<kernel-input-event>->symbol kie2))
					   (for-each
					    (lambda (kie3)
					      (logln #t "3) tr kie: ~a\n"
						     (<kernel-input-event>->symbol kie3))
					      (uinject kie3))
					    (transform-input! kie2)))
					 (try-transform! st-tr kie)))
				)))))))

      (join-thread mt))))
