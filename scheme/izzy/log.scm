(define-module (izzy log)
  #:export (log logln logd logdln tsdisplay tsdisplayln tswrite tswriteln)
  ;; For 'with-mutex'
  #:use-module (ice-9 threads)
  ;; For 'define*'
  #:use-module (ice-9 optargs))

;; Warning: guile's implementation of 'display' does not appear to be
;; thread safe!
;;
;; Example:
;;
;; Evaluating the sexp
;;
;; (let ((t1 (call-with-new-thread (lambda () (display "1\n"))))
;;       (t2 (call-with-new-thread (lambda () (display "2\n")))))
;;   (join-thread t1)
;;   (join-thread t2))
;;
;; will yield 12 sometimes and 121 other times, etc ... not good.
;;
;; 1
;; 2
;; 1

(define display-mutex (make-mutex))
(define write-mutex (make-mutex))

(define* (tsdisplay obj #:optional port)
  "Partially thread safe version of display."
  (with-mutex display-mutex
	      (display obj port)))

(define* (tsdisplayln obj #:optional port)
  "Partially thread safe version of display. A newline is written
afterwards."
  (with-mutex display-mutex
	      (display obj port)
	      (newline port)))

(define* (tswrite obj #:optional port)
  "Partially thread safe version of write."
  (with-mutex write-mutex
	      (write obj port)))

(define* (tswriteln obj #:optional port)
  "Partially thread safe version of write. A newline is written
afterwards."
  (with-mutex write-mutex
	      (write obj port)
	      (newline port)))

(define (log port fmt-str . rest)
  "Unfortunately, 'simple-format' has a bug so that it does not work well
in terminals; partially thread safe."
  (let* (
	 (out-str (apply simple-format #f fmt-str rest)))
    (if (not port)
	out-str
	(tsdisplay out-str (if (eq? port #t)
			       (current-output-port)
			       port)))))

(define (logln port fmt-str . rest)
  "Like log but inserts a newline after; partially thread safe."
  (apply log port (string-append fmt-str "\n") rest))

(define (logd port fmt-str . rest)
  "Like log but prepends the date; partially thread safe."
  (let ((date (strftime "%a %b %d %Y %H:%M:%S" (localtime (current-time)))))
    (apply log port (string-append date " " fmt-str) rest)))

(define (logdln port fmt-str . rest)
  "Like logln but prepends the date; partially thread safe."
  (apply logd port (string-append fmt-str "\n") rest))
