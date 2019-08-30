#!/usr/bin/guile \
-L . --fresh-auto-compile -e main -s
!#

(define usage
  "--------------------------------------------------------------------------------
Usage: izzy [-allow-stale-input] <input-device-path> <unix-socket-path>

Start an izzy server for the specified input device that listens for
connections on the specified unix socket.

For example,

$ cd /root/izzy-test/scheme
$ izzy/izzy.scm /root/izzy-test/kie-pipe /root/izzy-test/unix-server-socket

")

(define (main args)
  (cond ((= (length args) 3)
	 (apply izzy (cdr args)))
	((= (length args) 4)
	 (unless (equal? (second args) "-allow-stale-input")
	   (error "Invalid argument ~A" (second args)))
	 ;; move '-allow-stale-input' to the end of the argument list
	 (apply izzy `(,@(cddr args) ,(second args))))
	(#t
	 (display usage))))

;; Previously, this list of use-modules expressions was placed within
;; the function izzy shown below; however, numerous compile warnings
;; resulted.  So they were pulled out and placed here, instead.

(use-modules (oop goops))
;; for 'make-q'
(use-modules (ice-9 q))
;; required by guile-2.2
(use-modules (ice-9 threads))
(use-modules (rnrs bytevectors))
(use-modules (srfi srfi-4))
(use-modules (srfi srfi-1))

(use-modules (izzy customize))
(use-modules (izzy driver))
(use-modules (izzy hid-constants))
(use-modules (izzy kernel-input-event))
(use-modules (izzy kie-transformer))
(use-modules (izzy log))
(use-modules (izzy serio-driver))
(use-modules (izzy usb-generic-driver))
(use-modules (izzy timeval))
(use-modules (izzy uinject))

(define* (izzy input-device-path unix-server-sock-path #:optional (allow-stale-input #f))

  ;; Start an izzy server for the specified input device listening for
  ;; connections on the unixs-server-socket-path.  If none of the
  ;; clients, handles the input event then the default root handler is
  ;; used.

  ;; TODO: check that the paths are valid

  (let* (
	 ;; Mutex for the list of clients
	 (client-list-mutex (make-mutex))
	 ;; Shared list of clients
	 (client-list '())

	 ;; Server Thread
	 ;;
	 ;; Listen for new client connections and add them to the list of
	 ;; clients.

	 (server-thread
	  (call-with-new-thread
	   (lambda ()
	     (let (
		   ;; Log function for this thread
		   (slogdln (lambda (port fmt-str . rest)
			      "Like logdln but prepends \"-- server thread: \"."
			      (apply logdln
				     port
				     (string-append "-- server thread: " fmt-str) rest))))

	       (if (file-exists? unix-server-sock-path)
		   (delete-file unix-server-sock-path))

	       (let (
		     ;; Create a new server socket
		     (ser (socket PF_UNIX SOCK_STREAM 0)))
		 ;; For a detailed description of socket options see the
		 ;; Linux manpage for SOCKET(7).

		 ;; Allow the socket to bind except when there is an
		 ;; active listening socket bound to the address
		 (setsockopt ser SOL_SOCKET SO_REUSEADDR 1)
		 ;; Activate socket lingering and linger for one second
		 (setsockopt ser SOL_SOCKET SO_LINGER (cons 1 1))
		 (bind ser AF_UNIX unix-server-sock-path)
		 ;; Listen for client connections with a backlog of 25
		 (listen ser 25)
		 (slogdln #t "ready to rock!")
		 (while #t
			(let* (
			       ;; Block until a new client connects
			       (p (accept ser))
			       (client (car p))
			       (addr (cdr p)))
			  (with-mutex client-list-mutex
				      ;; Prepend the client to the list
				      (set! client-list (cons client client-list))
				      (slogdln #t
					       "prepended client ~a to the list"
					       client)
				      (slogdln #t
					       "number of clients: ~a"
					       (length client-list))))))))))

	 ;; Input Thread
	 ;;
	 ;; Wait for input and dispatch it to clients.

	 (input-thread
	  (call-with-new-thread
	   (lambda ()

	     (let (
		   ;; Log function for this thread
		   (ilogdln (lambda (port fmt-str . rest)
			      "Like logdln but prepends \"-- input thread: \"."
			      (apply logdln
				     port
				     (string-append "-- input thread: " fmt-str) rest))))

	       (let* ((start-time (make <timeval>))
		      ;; n-p -- notification pipe
		      ;; after a new input event is placed in the event queue, a
		      ;; character (actually a carriage return) is written to the
		      ;; notification pipe.
		      (n-p (pipe))
		      ;; e-q -- the (input) event queue
		      (e-q (make-q))
		      ;; s-d -- serio driver
		      ;; this is the scheme serio driver that may be thought of
		      ;; as wrapping the raw kernel driver
		      (s-d (make <usb-generic-driver> input-device-path n-p e-q)))

		 ;; Load the scheme serio driver
		 (start s-d)

		 (ilogdln #t "started")
		 (while #t
			;; Wait for notification that an input event is
			;; waiting in the queue
			(read-char (car n-p))
			;; Remove the input event or eof-object from
			;; the queue
			(let* ((kie (deq! e-q)))
			  (if (eof-object? kie)
			      (begin
				(ilogdln #t "done")
				(break kie))
			      ;; when stale input is allowed or the
			      ;; kie is fresh
			      (when (or allow-stale-input
					(> (time kie) start-time))
				(ilogdln #t
					 "received raw kie: ~a"
					 (<kernel-input-event>->symbol kie))
				(with-mutex client-list-mutex
					    (for-each
					     (lambda (client)
					       ;; Catch all exceptions
					       (catch #t
						      ;; thunk
						      (lambda ()
							;; handle signals
							(for-each (lambda (sig)
								    (sigaction sig (lambda (sig2)
										     (error "unexpected signal: ~a" sig2))))
								  (lset-difference equal?
										   (iota 64 1)
										   ;; handle all signals except these
										   (list SIGINT SIGKILL SIGSTOP 32 33)))
							(send client (string->utf8
								      (string-append
								       (object->string kie)
								       "\n")))
							(force-output client)
							(ilogdln
							 #t
							 "dispatched input event to client: ~a"
							 client))
						      ;; exception handler
						      (lambda (key . args)
							(ilogdln #t
								 "~a ~a"
								 key
								 args)
							(shutdown client 2)
							(close-port client)
							(close client)
							(ilogdln #t
								 "removing client: ~a"
								 client)
							(set! client-list
						     	      (delete! client client-list))
							(ilogdln #t
								 "number of clients: ~a"
								 (length client-list)))))
					     client-list))))))))))))

    ;; It doesn't really matter which thread we join here.
    (join-thread input-thread)))
