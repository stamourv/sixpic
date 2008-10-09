;;; definition of ast types

(define-type ast
  extender: define-type-of-ast
  (parent unprintable:)
  subasts)

(define (link-parent! subast parent)
  (ast-parent-set! subast parent)
  parent)

(define (multi-link-parent! subasts parent)
  (for-each (lambda (subast) (link-parent! subast parent))
            subasts)
  parent)

(define (unlink-parent! subast)
  (let ((parent (ast-parent subast)))
    (if (and (def-variable? subast) (def-procedure? parent))
        (def-procedure-params-set!
          parent
          (remove subast (def-procedure-params parent)))
        (ast-subasts-set!
         parent
         (remove subast (ast-subasts parent))))
    (ast-parent-set! subast #f)
    subast))

(define (subast1 ast) (car (ast-subasts ast)))
(define (subast2 ast) (cadr (ast-subasts ast)))
(define (subast3 ast) (caddr (ast-subasts ast)))
(define (subast4 ast) (cadddr (ast-subasts ast)))

(define-type-of-ast def
  extender: define-type-of-def
  id
  unprintable:
  refs)

(define-type value
  bytes)
(define (new-value bytes)
  (make-value bytes))

(define-type byte-cell
  adr
  (interferes-with unprintable:)
  (coalesceable-with unprintable:))
(define (new-byte-cell)
  (make-byte-cell #f '() '()))

(define-type byte-lit
  val)
(define (new-byte-lit x)
  (make-byte-lit x))

(define (nb-bytes type)
  (case type
    ((void) 0)
    ((int) 1) ;; TODO have more types
    (else (error "wrong number of bytes ?"))))

(define (int->value n type)
  (let ((len (nb-bytes type)))
    (let loop ((len len) (n n) (rev-bytes '()))
      (if (= len 0)
          (new-value (reverse rev-bytes))
          (loop (- len 1)
                (arithmetic-shift n -8)
                (cons (new-byte-lit (modulo n 256))
                      rev-bytes))))))

(define (extend value type)
  value);;;;;;;;;;;;;;;;;;;;;

(define (alloc-value type)
  (let ((len (nb-bytes type)))
    (let loop ((len len) (rev-bytes '()))
      (if (= len 0)
          (new-value (reverse rev-bytes))
          (loop (- len 1)
                (cons (new-byte-cell)
                      rev-bytes))))))

(define-type-of-def def-variable
  type
  value
  unprintable:
  sets)
(define (new-def-variable subasts id refs type value sets)
  (multi-link-parent!
   subasts
   (make-def-variable #f subasts id refs type value sets)))

(define-type-of-def def-procedure
  type
  value
  params
  entry
  live-after-calls)
(define (new-def-procedure subasts id refs type value params)
  (multi-link-parent!
   subasts
   (make-def-procedure #f subasts id refs type value params #f '())))


(define-type-of-ast expr
  extender: define-type-of-expr
  type)

(define-type-of-expr literal
  val)
(define (new-literal type val)
  (make-literal #f '() type val))

(define-type-of-expr ref
  def-var)
(define (new-ref type def)
  (make-ref #f '() type def))

(define-type-of-expr oper
  op)
(define (new-oper subasts type op)
  (multi-link-parent!
   subasts
   (make-oper #f subasts type op)))

(define-type-of-expr call
  def-proc)
(define (new-call subasts type proc-def)
  (multi-link-parent!
   subasts
   (make-call #f subasts type proc-def)))

(define-type-of-ast block
  name) ; blocks that begin with a label have a name, the other have #f
(define (new-block subasts)
  (multi-link-parent!
   subasts
   (make-block #f subasts #f)))
(define (new-named-block name subasts)
  (multi-link-parent!
   subasts
   (make-block #f subasts name)))

(define-type-of-ast if)
(define (new-if subasts)
  (multi-link-parent!
   subasts
   (make-if #f subasts)))

(define-type-of-ast switch)
(define (new-switch subasts)
  (multi-link-parent!
   subasts
   (make-switch #f subasts)))

(define-type-of-ast while)
(define (new-while subasts)
  (multi-link-parent!
   subasts
   (make-while #f subasts)))

(define-type-of-ast do-while)
(define (new-do-while subasts)
  (multi-link-parent!
   subasts
   (make-do-while #f subasts)))

(define-type-of-ast for)
(define (new-for subasts)
  (multi-link-parent!
   subasts
   (make-for #f subasts)))

(define-type-of-ast return)
(define (new-return subasts)
  (multi-link-parent!
   subasts
   (make-return #f subasts)))

(define-type-of-ast break)
(define (new-break)
  (make-break #f '()))

(define-type-of-ast continue)
(define (new-continue)
  (make-continue #f '()))

(define-type-of-ast goto)
(define (new-goto label)
  (make-goto #f (list label)))

(define-type-of-ast program)
(define (new-program subasts) ;; TODO add suport for main
  (multi-link-parent!
   subasts
   (make-program #f subasts)))

(define-type op
  extender: define-type-of-op
  (six-id unprintable:)
  id
  unprintable:
  type-rule
  constant-fold
  code-gen)

(define-type-of-op op1)
(define-type-of-op op2)
