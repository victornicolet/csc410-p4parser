#lang rosette
(require racket/match)


;; Returns a syntax object from reading the contents of a file.
(define (file->syntax file)
  (define (read-syntax/count-lines)
    (port-count-lines! (current-input-port))
    (read-syntax))
  (define-values (base _ __) (split-path file))
  (parameterize ([current-namespace (make-base-namespace)])
    (define stx (with-handlers ([exn:fail? (const #f)])
                    (with-input-from-file file read-syntax/count-lines)))
    stx))


(define keywords (list '+ '-           ;Arithmetic
                       'sub1 'add1     ; (sub1 a) is (- a 1) and (add1 a) (+ a 1)
                       'or 'and '! '=  ;Boolean
                       '#t '#f         ;Boolean true/false
                       'max 'min       ;Conditional arithmetic
                       ;; '& '\| '^           ;Bitwise operator
                       '== '>= '<= '> '<   ;Integer comparison
                       'integer? 'boolean? ;Types
                       'void?              ;Void type
                       'if                 ;Conditionals
                       ))

;; Type of the operands of an operator.
;; The void? type represents either integer or boolean.
;; The %top operator is the default operator used as the top
;; operator when starting to parse an expression.
(define (optype? op)
  (match op
    [(or '+ '- 'min 'max '>= '> '< '<= '==) 'integer?]
    [(or 'or 'and '! '=) 'boolean?]
    [(or 'if '%top) 'void?]))

;; Returns true is the syntax objects represents an id
;; that is not a keyword of the language.
(define (is-syntax-of-id stx)
  (if (identifier? stx)
      (let ([x (syntax-e stx)])
      (<= (count (lambda (k) (equal? k x)) keywords) 0))
      false))

;; Builds a list of pairs (identifier, indentfier-type)
;; The type is inferred from the operator above the identifier occurrence
;; so this is not a correct type inference, just a hint.
;; (add-ids top_op stx l) adds the ids encountered in the syntax-object
;; stx  with top-operator top_op into list l.
(struct typed-var (id type) #:transparent #:mutable)

(define (add-ids top_op stx l)
  (if (is-syntax-of-id stx)
      (append l (list (typed-var (syntax->datum stx) (optype? top_op))))
      (let ([expanded-stx (syntax-e stx)])
        (if (list? expanded-stx)
            (foldl (lambda (maybe-id id-list)
                     (add-ids (syntax-e (car expanded-stx))
                              maybe-id id-list))
                   l
                   (cdr expanded-stx))
            l))))

;; Wrapper for add-ids starting from an empty list and
;; a dummy operator that types void

(define (get-ids stx) (add-ids '%top stx (list )))

;; Create Rosette symbolic definition from identifier + type
(define (make-rosette-decl v)
  (with-syntax
    ([vname (datum->syntax #f (typed-var-id v))]
     [vtype (datum->syntax #f (typed-var-type v))])
    #'(define-symbolic vname vtype)))

(define (make-all-rosette-decl lv)
  (datum->syntax #f (map make-rosette-decl lv)))


(provide get-ids make-all-rosette-decl make-rosette-decl file->syntax)
