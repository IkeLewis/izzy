(define-module (il compare)
  #:export (define-cops)
  #:use-module (oop goops))

(define-syntax define-cops
  ;; cops -- compare sexp's
  (syntax-rules (:< :=)
    ;; TODO: make 'type1' and 'type2' optional
    ((_ ((arg1 type1) (arg2 type2)) (:< exp ...) (:= exp2 ...))
     (begin
       (define-method (< (arg1 type1) (arg2 type2))
	 exp ...)
       (define-method (= (arg1 type1) (arg2 type2))
	 exp2 ...)
       (define-method (!= (arg1 type1) (arg2 type2))
	 (not (= arg1 arg2)))
       (define-method (<= (arg1 type1) (arg2 type2))
	 (or (< arg1 arg2) (= arg1 arg2)))
       (define-method (>= (arg1 type1) (arg2 type2))
	 (not (< arg1 arg2)))
       (define-method (> (arg1 type1) (arg2 type2))
	 (not (<= arg1 arg2)))))))
