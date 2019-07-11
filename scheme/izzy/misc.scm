(define-module (izzy misc)
  #:export (errorf)
  #:use-module (oop goops))

(define-method (errorf (format-str <string>) . args)
  (error (apply simple-format #f format-str args)))
