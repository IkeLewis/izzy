(define-module (izzy customize)
  #:export (bi-keys-dflt only-modifiers-dflt)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (izzy hid-constants))

;;; Defaults

;; TODO: fill in the remaining pairs.

;; The locations of these keys have a vertical line of symmetry on
;; most qwerty keyboards.
(define qwerty-symmetric-pairs (list
				;; Left hand key is 1st
				(cons key-q key-p)
				(cons key-w key-o)
				(cons key-e key-i)
				(cons key-r key-u)
				(cons key-t key-y)
				;; Line 2
				(cons key-a key-semicolon)
				(cons key-s key-l)
				(cons key-d key-k)
				(cons key-f key-j)
				(cons key-g key-h)
				;; Line 3
				(cons key-z key-slash)
				(cons key-x key-dot)
				(cons key-c key-comma)
				(cons key-v key-m)
				(cons key-b key-n)
				;; Right hand key is 1st
				(cons key-p key-q)
				(cons key-o key-w)
				(cons key-i key-e)
				(cons key-u key-r)
				(cons key-y key-t)
				;; Line 2
				(cons key-semicolon key-a)
				(cons key-l key-s)
				(cons key-k key-d)
				(cons key-j key-f)
				(cons key-h key-g)
				;; Line 3
				(cons key-slash key-z)
				(cons key-dot key-x)
				(cons key-comma key-c)
				(cons key-m key-v)
				(cons key-n key-b)))

;; Returns a list of the distinct elements of elems
(define (distinct = elems)
  (apply lset-adjoin = '() (reverse elems)))

(define (symmetric-closure keys)
  (distinct equal?
	    (append keys (map (lambda (k) (assoc-ref qwerty-symmetric-pairs k))
			      keys))))

;; Defaults

(define ctrl-modifiers-dflt (symmetric-closure (list key-a)))

(define ctrl-c-modifiers-dflt (symmetric-closure (list key-b key-m key-s key-w)))

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

;; (define bi-keys-mod-map-dflt (list (cons key-a key-leftctrl)
;; 				   (cons key-semicolon key-rightctrl)))


;; (define-method (allowed-modifiers (ieh <input-event-handler>))
;;   "These are the keys that may be used as modifiers."
;;   (append (only-modifiers ieh) (bi-keys ieh)))
