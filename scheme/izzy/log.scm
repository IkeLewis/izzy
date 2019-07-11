(define-module (izzy log)
  #:export (log logln))

(define (log port fmt-str . rest)
  "Note that 'simple-format' has a bug so that it does not work well
in terminals.  Otherwise, you could just use that function."
  (let ((out-str (apply simple-format #f fmt-str rest)))
    (if (not port)
	out-str
	(display out-str (if (eq? port #t)
			     (current-output-port)
			     port)))))

(define (logln port fmt-str . rest)
  "Like log but inserts a newline after."
  (apply log port (string-append fmt-str "\n") rest))
