#lang racket

;; Project 1 - Hannah Ali - CS4337

;; ---------------------------------------------------------------------------------------------
;; looking up value from history - parameters (index, hist (array of history) )
;;	validate if index is valid
;;		return "invalid expression" if not
;;	else
;;		return value at index in history
;; ---------------------------------------------------------------------------------------------

(define (lookup index hist)
  (if (or (< index 1) (> index (length hist))) ;; validating index
      (error "Error: Invalid expression, please try again.")
      (list-ref (reverse hist) (- index 1)))) ;; making sure we reverse order first

;; ---------------------------------------------------------------------------------------------
;; evaluating expression - parameters (list, hist (array of history) )
;; ---------------------------------------------------------------------------------------------

(define (evaluatingExpression tokens history)
  (if (null? tokens)
      (values "Invalid Expression" '())
      (let ([t (car tokens)]
            [rest (cdr tokens)])
        (cond
          [(equal? t "+")
           (define-values (a r1) (evaluatingExpression rest history))
           (define-values (b r2) (evaluatingExpression r1 history))
           (if (or (string? a) (string? b))
               (values "Invalid Expression" '())
               (values (+ a b) r2))]

          [(equal? t "*")
           (define-values (a r1) (evaluatingExpression rest history))
           (define-values (b r2) (evaluatingExpression r1 history))
           (if (or (string? a) (string? b))
               (values "Invalid Expression" '())
               (values (* a b) r2))]

          [(equal? t "/")
           (define-values (a r1) (evaluatingExpression rest history))
           (define-values (b r2) (evaluatingExpression r1 history))
           (cond
             [(or (string? a) (string? b)) (values "Invalid Expression" '())]
             [(zero? b) (values "Invalid Expression" '())]
             [else (values (/ a b) r2)])]

          [(equal? t "-")
           (define-values (a r1) (evaluatingExpression rest history))
           (if (string? a)
               (values "Invalid Expression" '())
               (values (- a) r1))]

          [(regexp-match #px"^\\$\\d+$" t)
           (define id (string->number (substring t 1)))
           (if (or (not id) (< id 1) (> id (length history)))
               (values "Invalid Expression" '())
               (values (list-ref (reverse history) (- id 1)) rest))]

          [(string->number t)
           => (lambda (n) (values n rest))]

          [else (values "Invalid Expression" '())]))))



;; ---------------------------------------------------------------------------------------------
;; helper to split string
;; ---------------------------------------------------------------------------------------------

(define (split str)
  (filter (lambda (s) (not (string=? s "")))
          (regexp-split #px"\\s+" str)))

;; ---------------------------------------------------------------------------------------------
;; helper to see if the program is running in interactive or batch
;; ---------------------------------------------------------------------------------------------

(define prompt?
  (let ([args (current-command-line-arguments)])
    (cond
      [(= (vector-length args) 0) #t]
      [(string=? (vector-ref args 0) "-b") #f]
      [(string=? (vector-ref args 0) "--batch") #f]
      [else #t])))

;; ---------------------------------------------------------------------------------------------
;; creating main looping through string
;; ---------------------------------------------------------------------------------------------

(define (main hist)
  (when prompt? (display "Enter expression: "))
  (define input (read-line))
  (cond
    [(eof-object? input) (void)]
    [(string=? input "quit") (void)]
    [else
     (define tokens (split input))
     (define-values (result remaining) (evaluatingExpression tokens hist))
     (if (string? result)
         (begin
           (displayln (string-append "Error: " result))
           (main hist))
         (if (null? remaining)
             (let* ([new-hist (cons result hist)]
                    [index (length new-hist)])
               (display index)
               (display ": ")
               (display (real->double-flonum result))
               (newline)
               (main new-hist))
             (begin
               (displayln "Error: Invalid expression, please try again.")
               (main hist))))]))

;; ---------------------------------------------------------------------------------------------
;; starting program
;; ---------------------------------------------------------------------------------------------

(displayln "Prefix Calculator")
(main '())