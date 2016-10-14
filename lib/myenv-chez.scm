
(define pp pretty-print)


(define-syntax declare	; Gambit-specific compiler-decl
  (syntax-rules () ((declare . x) (begin #f))))

; A few convenient functions that are not Chez
(define (call-with-input-string str proc)
    (proc (open-input-string str)))
(define (call-with-output-string proc)
  (let ((port (open-output-string)))
    (proc port)
    (get-output-string port)))


; Frequently-occurring syntax-rule macros

; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier, which occurs
; in the list of identifiers supplied by the second argument.
; All the identifiers in that list must be unique.
; Otherwise, id-memv?? expands to KF.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Commonly-used CPS macros
; The following macros follow the convention that a continuation argument
; has the form (k-head ! args ...)
; where ! is a dedicated symbol (placeholder).
; When a CPS macro invokes its continuation, it expands into
; (k-head value args ...)
; To distinguish such calling conventions, we prefix the names of
; such macros with k!

(define-syntax k!id			; Just the identity. Useful in CPS
  (syntax-rules ()
    ((k!id x) x)))

; k!reverse ACC (FORM ...) K
; reverses the second argument, appends it to the first and passes
; the result to K

(define-syntax k!reverse
  (syntax-rules (!)
    ((k!reverse acc () (k-head ! . k-args))
      (k-head acc . k-args))
    ((k!reverse acc (x . rest) k)
      (k!reverse (x . acc) rest k))))

(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg) (assert exp report: error-msg))))

(define (identify-error msg args . disposition-msgs)
  (let ((port (console-output-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (console-output-port))
		(display x (console-output-port))))
            args))

(define nl (string #\newline))

; Some useful increment/decrement operators

(define-syntax inc!		; Mutable increment
  (syntax-rules ()
    ((inc! x) (set! x (+ 1 x)))))
(define-syntax inc               ; Read-only increment
  (syntax-rules ()
    ((inc x) (+ 1 x))))

(define-syntax dec!		; Mutable decrement
  (syntax-rules ()
    ((dec! x) (set! x (- x 1)))))
(define-syntax dec		; Read-only decrement
  (syntax-rules ()
    ((dec x) (- x 1))))

; Some useful control operators

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; This primitive is often called 'unless'
(define-syntax whennot
  (syntax-rules ()
    ((whennot condition . stmts)
      (or condition (begin . stmts)))))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-syntax push!
  (syntax-rules ()
    ((push! item ls)
      (set! ls (cons item ls)))))

			; assoc-primitives with a default clause
			; If the search in the assoc list fails, the
			; default action argument is returned. If this
			; default action turns out to be a thunk,
			; the result of its evaluation is returned.
			; If the default action is not given, an error
			; is signaled

(define-syntax assq-def
  (syntax-rules ()
    ((assq-def key alist)
      (or (assq key alist)
	(error "failed to assq key '" key "' in a list " alist)))
    ((assq-def key alist #f)
      (assq key alist))
    ((assq-def key alist default)
      (or (assq key alist) (if (procedure? default) (default) default)))))

(define-syntax assv-def
  (syntax-rules ()
    ((assv-def key alist)
      (or (assv key alist)
	(error "failed to assv key '" key "' in a list " alist)))
    ((assv-def key alist #f)
      (assv key alist))
    ((assv-def key alist default)
      (or (assv key alist) (if (procedure? default) (default) default)))))

(define-syntax assoc-def
  (syntax-rules ()
    ((assoc-def key alist)
      (or (assoc key alist)
	(error "failed to assoc key '" key "' in a list " alist)))
    ((assoc-def key alist #f)
      (assoc key alist))
    ((assoc-def key alist default)
      (or (assoc key alist) (if (procedure? default) (default) default)))))

			; Convenience macros to avoid quoting of symbols
			; being deposited/looked up in the environment
(define-syntax env.find
  (syntax-rules () ((env.find key) (%%env.find 'key))))
(define-syntax env.demand
  (syntax-rules () ((env.demand key) (%%env.demand 'key))))
(define-syntax env.bind
  (syntax-rules () ((env.bind key value) (%%env.bind 'key value))))
