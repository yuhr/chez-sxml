
(library (ssax)
  (export ssax:xml->sxml)
  (import (except (scheme)
                  string-copy string-for-each string->list string-upcase
                  string-downcase string-titlecase string-hash string-copy! string-fill!
                  fold-right error)
          (prefix (only (scheme) error) scheme:)
          (srfi :0 cond-expand)
          (srfi :13 strings))


  
  (define chez-error scheme:error)
  (define error
    (lambda (msg . args)
      (chez-error 'runtime-error "~a~%" (cons msg args))))
  
  (include "lib/myenv-chez.scm")
  
  (define-syntax define-opt
    (syntax-rules (optional)
      ((define-opt (name . bindings) . bodies)
       (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

      ((define-opt "seek-optional" ((optional . _opt-bindings))
         (reqd ...) ((name . _bindings) . _bodies))
       (define (name reqd ... . _rest)
         (letrec-syntax
             ((handle-opts
               (syntax-rules ()
                 ((_ rest bodies (var init))
                  (let ((var (if (null? rest) init
                                 (if (null? (cdr rest)) (car rest)
                                     (error "extra rest" rest)))))
                    . bodies))
                 ((_ rest bodies var) (handle-opts rest bodies (var #f)))
                 ((_ rest bodies (var init) . other-vars)
                  (let ((var (if (null? rest) init (car rest)))
                        (new-rest (if (null? rest) '() (cdr rest))))
                    (handle-opts new-rest bodies . other-vars)))
                 ((_ rest bodies var . other-vars)
                  (handle-opts rest bodies (var #f) . other-vars))
                 ((_ rest bodies)		; no optional args, unlikely
                  (let ((_ (or (null? rest) (error "extra rest" rest))))
                    . bodies)))))
           (handle-opts _rest _bodies . _opt-bindings))))

      ((define-opt "seek-optional" (x . rest) (reqd ...) form)
       (define-opt "seek-optional" rest (reqd ... x) form))

      ((define-opt "seek-optional" not-a-pair reqd form)
       (define . form))			; No optional found, regular define

      ((define-opt name body)		; Just the definition for 'name',
       (define name body))		; for compatibilibility with define
      ))

  (cond-expand
   (scheme48 #f)
   (scsh #f)
   (else (define ascii->char integer->char)))
  (define (ucscode->char code)
    (cond-expand
     (bigloo
      (ucs2->char (integer->ucs2 code)))
     ((or scheme48 scsh)
      (ascii->char code))
     (else
      (integer->char code))))
  
  (define char-return (ascii->char 13))
  (define char-tab    (ascii->char 9))
  (define char-newline (ascii->char 10))
  
  (define (parser-error port message . rest)
    (apply error message rest))

  (define (ssax:warn port msg . other-msg)
    (apply cerr (cons (string-append (string #\newline) "Warning: ")
                      (cons msg
                            other-msg))))
  
  (define-opt (peek-next-char (optional (port (current-input-port))))
    (read-char port) 
    (peek-char port)) 
  
  (define-opt (assert-curr-char expected-chars comment
                                (optional (port (current-input-port))))
    (let ((c (read-char port)))
      (if (memv c expected-chars) c
          (parser-error port "Wrong character " c
                        " (0x" (if (eof-object? c) "*eof*"
                                   (number->string (char->integer c) 16)) ") "
                                   comment ". " expected-chars " expected"))))

  (define-opt (skip-until arg (optional (port (current-input-port))) )
    (cond
     ((number? arg)		; skip 'arg' characters
      (do ((i arg (dec i)))
      	  ((not (positive? i)) #f)
        (if (eof-object? (read-char port))
      	    (parser-error port "Unexpected EOF while skipping "
                          arg " characters"))))
     (else			; skip until break-chars (=arg)
      (let loop ((c (read-char port)))
        (cond
         ((memv c arg) c)
         ((eof-object? c)
          (if (memq '*eof* arg) c
              (parser-error port "Unexpected EOF while skipping until " arg)))
         (else (loop (read-char port))))))))

  (define-opt (skip-while skip-chars (optional (port (current-input-port))) )
    (do ((c (peek-char port) (peek-char port)))
        ((not (memv c skip-chars)) c)
      (read-char port)))
  
  (define input-parse:init-buffer
    (let ((buffer (make-string 512)))
      (lambda () buffer)))
  
  (define-opt (next-token-old prefix-skipped-chars break-chars
                              (optional (comment "") (port (current-input-port))) )
    (let* ((buffer (input-parse:init-buffer))
           (curr-buf-len (string-length buffer))
           (quantum curr-buf-len))
      (let loop ((i 0) (c (skip-while prefix-skipped-chars port)))
        (cond
         ((memv c break-chars) (substring buffer 0 i))
         ((eof-object? c)
          (if (memq '*eof* break-chars)
              (substring buffer 0 i)
              (parser-error port "EOF while reading a token " comment)))
         (else
          (if (>= i curr-buf-len)
              (begin
                (set! buffer (string-append buffer (make-string quantum)))
                (set! quantum curr-buf-len)
                (set! curr-buf-len (string-length buffer))))
          (string-set! buffer i c)
          (read-char port)
          (loop (inc i) (peek-char port))
          )))))

  (define-opt (next-token prefix-skipped-chars break-chars
                          (optional (comment "") (port (current-input-port))) )
    (let outer ((buffer (input-parse:init-buffer)) (filled-buffer-l '())
                (c (skip-while prefix-skipped-chars port)))
      (let ((curr-buf-len (string-length buffer)))
        (let loop ((i 0) (c c))
          (cond
           ((memv c break-chars)
            (if (null? filled-buffer-l) (substring buffer 0 i)
                (string-concatenate-reverse filled-buffer-l buffer i)))
           ((eof-object? c)
            (if (memq '*eof* break-chars)	; was EOF expected?
                (if (null? filled-buffer-l) (substring buffer 0 i)
                    (string-concatenate-reverse filled-buffer-l buffer i))
                (parser-error port "EOF while reading a token " comment)))
           ((>= i curr-buf-len)
            (outer (make-string curr-buf-len)
                   (cons buffer filled-buffer-l) c))
           (else
            (string-set! buffer i c)
            (read-char port)
            (loop (inc i) (peek-char port))))))))

  (define-opt (next-token-of incl-list/pred
                             (optional (port (current-input-port))) )
    (let* ((buffer (input-parse:init-buffer))
           (curr-buf-len (string-length buffer)))
      (if (procedure? incl-list/pred)
          (let outer ((buffer buffer) (filled-buffer-l '()))
            (let loop ((i 0))
              (if (>= i curr-buf-len)		; make sure we have space
                  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
                  (let ((c (incl-list/pred (peek-char port))))
                    (if c
                        (begin
                          (string-set! buffer i c)
                          (read-char port)			; move to the next char
                          (loop (inc i)))
                                        ; incl-list/pred decided it had had enough
                        (if (null? filled-buffer-l) (substring buffer 0 i)
                            (string-concatenate-reverse filled-buffer-l buffer i)))))))

                                        ; incl-list/pred is a list of allowed characters
          (let outer ((buffer buffer) (filled-buffer-l '()))
            (let loop ((i 0))
              (if (>= i curr-buf-len)		; make sure we have space
                  (outer (make-string curr-buf-len) (cons buffer filled-buffer-l))
                  (let ((c (peek-char port)))
                    (cond
                     ((not (memv c incl-list/pred))
                      (if (null? filled-buffer-l) (substring buffer 0 i)
                          (string-concatenate-reverse filled-buffer-l buffer i)))
                     (else
                      (string-set! buffer i c)
                      (read-char port)
                      (loop (inc i)))))))))))

  (define *read-line-breaks* (list char-newline char-return '*eof*))

  (define-opt (read-text-line (optional (port (current-input-port))) )
    (if (eof-object? (peek-char port)) (peek-char port)
        (let* ((line
                (next-token '() *read-line-breaks*
                            "reading a line" port))
               (c (read-char port)))
          (and (eqv? c char-return) (eqv? (peek-char port) #\newline)
               (read-char port))
          line)))

  (define-opt (read-string n (optional (port (current-input-port))) )
    (if (not (positive? n)) ""
        (let ((buffer (make-string n)))
          (let loop ((i 0) (c (read-char port)))
            (if (eof-object? c) (substring buffer 0 i)
                (let ((i1 (inc i)))
                  (string-set! buffer i c)
                  (if (= i1 n) buffer
                      (loop i1 (read-char port)))))))))

  (define (miscio:find-string-from-port? str <input-port> . max-no-char)
    (set! max-no-char (if (null? max-no-char) #f (car max-no-char)))
    (letrec
        ((no-chars-read 0)
         (my-peek-char
          (lambda () (and (or (not max-no-char) (< no-chars-read max-no-char))
                          (let ((c (peek-char <input-port>)))
                            (if (eof-object? c) #f c)))))
         (next-char (lambda () (read-char <input-port>)
                            (set! no-chars-read  (inc no-chars-read))))
         (match-1st-char
          (lambda ()
            (let ((c (my-peek-char)))
              (if (not c) #f
                  (begin (next-char)
                         (if (char=? c (string-ref str 0))
                             (match-other-chars 1)
                             (match-1st-char)))))))
         (match-other-chars
          (lambda (pos-to-match)
            (if (>= pos-to-match (string-length str))
                no-chars-read
                (let ((c (my-peek-char)))
                  (and c
                       (if (not (char=? c (string-ref str pos-to-match)))
                           (backtrack 1 pos-to-match)
                           (begin (next-char)
                                  (match-other-chars (inc pos-to-match)))))))))
         (backtrack
          (lambda (i matched-substr-len)
            (let ((j (- matched-substr-len i)))
              (if (<= j 0)
                  (match-1st-char)
                  (let loop ((k 0))
                    (if (>= k j)
                        (match-other-chars j)
                        (if (char=? (string-ref str k)
                                    (string-ref str (+ i k)))
                            (loop (inc k))
                            (backtrack (inc i) matched-substr-len)))))))))
      (match-1st-char)))

  (define find-string-from-port? miscio:find-string-from-port?)
  
  (include "lib/SSAX-code.scm")
  
  )
