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

(define byte-cell-counter 0)
(define (byte-cell-next-id) (let ((id byte-cell-counter))
			      (set! byte-cell-counter (+ id 1))
			      id))
(define-type byte-cell
  id
  adr
  name ; to display in the listing
  (interferes-with unprintable:) ; these 2 are stored as sets
  (coalesceable-with unprintable:))
(define (new-byte-cell #!optional (name #f))
  (let ((id (byte-cell-next-id)))
    (make-byte-cell id (if allocate-registers? #f id)
		    (if name (string-append name "$" (number->string id)) "__tmp")
		    (new-empty-set) (new-empty-set))))
(define (get-register n)
  (make-byte-cell (byte-cell-next-id) n
		  (symbol->string (cdr (assv n file-reg-names)))
		  (new-empty-set) (new-empty-set)))

(define-type byte-lit
  val)
(define (new-byte-lit x)
  (make-byte-lit x))

(define types-bytes
  '((void  . 0)
    (bool  . 1)
    (int   . 1)
    (byte  . 1)
    (int8  . 1)
    (int16 . 2)
    (int24 . 3)
    (int32 . 4)))

(define (type->bytes type)
  (cond ((assq type types-bytes)
	 => (lambda (x) (cdr x)))
	(else (error "wrong type?"))))

(define (bytes->type n)
  (let loop ((l types-bytes))
    (cond ((null? l)     (error (string-append "no type contains "
					       (number->string n)
					       " bytes")))
	  ((= n (cdar l)) (caar l))
	  (else (loop (cdr l))))))

(define (int->value n type)
  (let ((len (type->bytes type)))
    (let loop ((len len) (n n) (rev-bytes '()))
      (if (= len 0)
          (new-value (reverse rev-bytes))
          (loop (- len 1)
                (arithmetic-shift n -8)
                (cons (new-byte-lit (modulo n 256))
                      rev-bytes))))))
(define (value->int val)
  (let loop ((bytes (reverse (value-bytes val)))
	     (n     0))
    (if (null? bytes)
	n
	(loop (cdr bytes)
	      (+ (* 256 n) (byte-lit-val (car bytes)))))))

;; TODO instead of carrying types around, use the length instead
(define (extend value type)
  ;; literals must be extended with literal 0s, while variables must be
  ;; extended with byte cells
  (let* ((bytes (value-bytes value))
	 (lit?  (byte-lit? (car bytes))))
    (let loop ((rev-bytes (reverse bytes))
	       (n         (max 0 (- (type->bytes type) (length bytes)))))
      (if (= n 0)
	  (new-value (reverse rev-bytes))
	  (loop (cons (if lit? (new-byte-lit 0) (new-byte-cell))
		      rev-bytes)
		(- n 1))))))

(define (alloc-value type #!optional (name #f))
  (let ((len (type->bytes type)))
    (let loop ((len len) (rev-bytes '()))
      (if (= len 0)
          (new-value rev-bytes)
          (loop (- len 1)
                (cons (new-byte-cell
		       (if name
			   ;; the lsb is 0, and so on
			   (string-append (symbol->string name)
					  (number->string (- len 1)))
			   #f))
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
  live-after-calls) ; stored as a set
(define (new-def-procedure subasts id refs type value params)
  (multi-link-parent!
   subasts
   (make-def-procedure #f subasts id refs type value params #f (new-empty-set))))


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
  (def-proc unprintable:))
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
(define (new-program subasts) ;; TODO add support for main
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
(define-type-of-op op3)
