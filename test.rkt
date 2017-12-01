#lang racket

(require "expr_parser.rkt")

;; ----------------------------------------------------------------------------
;;                                         TESTS
;; ----------------------------------------------------------------------------

;; Parsing an expression from a file an getting the variables used in the
;; expression
(define expr-syntax (file->syntax "./example.expr"))
(get-ids expr-syntax)

;; Other examples/tests with string inputs converted to syntax objects
(get-ids (syntax a)) ;; --> (a, void?)
(get-ids #'(+ a b));; --> '((a . integer?) (b . integer?))
(get-ids #'(if (== x y) #t #f)) ;; --> '((x . integer?) (y . integer?))
(get-ids #'(if (or x y) #t #f)) ;; --> '((x . boolean?) (y . boolean?))

;; From the project inputs
(get-ids #'(+ a b))
(get-ids #'(if #t (+a 0) (- b 9)))
(get-ids #'(+ (+ 1 a) (+ -1 b)))
(get-ids #'(or (and a b) (and a b)))
(get-ids #'(min (max (+ xm (min 0 0)) lm) (min xm2 lm2)))
(get-ids #'(if (> (+ (- xmts lmts) xpos) (+ xaux_1 xpos)) xpos lpos))


;; Creates a syntax objects representing the declarations to make to
;; make sure all identifiers are defined in the expression.
(define declarations
  (make-all-rosette-decl (get-ids #'(+ a b (- x y)))))

;; Pretty print the racket declarations.
;; Using pretty for a nicer output. In this case, it will automatically
;; jump lines between the declarations.
(require racket/pretty)
(println "Variable declarations")
(pretty-print (syntax->datum declarations))


;; An example that runs a rosette sketch and gets the solution back
(define file_contents
"#lang rosette
(require rosette/lib/synthax)
(define-symbolic bogus integer?)
(define (to_synth a) (= 0 (??)))
(define odot
  (synthesize
   #:forall (list bogus)
   #:guarantee (assert (to_synth bogus))))
(if (sat? odot)
(display (syntax->datum (car (generate-forms odot))))
(print \"unsat\"))
"
)

(define (output-to-file f contents)
  (call-with-output-file
    f
    #:exists 'replace
    (lambda (out) (display contents out))))

(define out_filename "__out__.rkt")
(define sol_filename "__sol__.rkt")
;; OUtput the file with the problem to out
(output-to-file out_filename file_contents)
;; execute the command and redirect the output to sol filename
(system (string-join (list "racket " out_filename " > " sol_filename)))
;; read the solution
(define solution (file->syntax sol_filename))
