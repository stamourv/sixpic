;;; operators

(define operators '())

(define (define-op1 six-id id type-rule constant-fold code-gen)
  (set! operators
        (cons (make-op1 six-id id type-rule constant-fold code-gen)
              operators)))

(define (define-op2 six-id id type-rule constant-fold code-gen)
  (set! operators
        (cons (make-op2 six-id id type-rule constant-fold code-gen)
              operators)))

;; no need for type checks, every type sixpic supports can be casted to / from
;; ints (except void, but this is a non-issue) and promotion (by padding) and
;; truncation is done at the cfg level
;; TODO really ignore the void issue ? assigning the "result" of a void function to an int variable should be an error
(define (type-rule-int-op1 ast)
  (expr-type (subast1 ast)))

(define (largest t1 t2)
  (if (> (type->bytes t1) (type->bytes t2))
      t1
      t2))

(define (type-rule-int-op2 ast)
  ;; used for any binary operation involving two integers where the result is
  ;; of the size of the biggest operand (subtraction, bitwise operations, ...)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (largest t1 t2)))

(define (type-rule-assign ast)
  (let ((t1 (expr-type (subast1 ast))))
    ;; the type of the rhs is irrelevant, since it will be promoted
    ;; or truncated at the cfg level
    t1))

(define (type-rule-int-comp-op2 ast)
  'bool) ;; TODO why even bother ? anything can be casted to int to be used as argument here, old version is in garbage (and in version control) if needed

(define (type-rule-bool-op2 ast)
  'bool) ;; TODO same here

(define-op1 'six.!x '!x
  type-rule-int-op1
  (lambda (ast) ;; TODO implement these ?
    ast)
  (lambda (ast)
    ...))


(define-op1 'six.++x '++x
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.x++ 'x++
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.--x '--x
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.x-- 'x--
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.~x '~x
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x%y 'x%y
  (lambda (ast)
    ;; if we know the second operand, we can have an upper bound on the size
    ;; of the result
    (if (literal? (subast1 ast))
	;; the number of bits needed by the result is lg(y)
	(bytes->type (ceiling (/ (log (literal-val (subast1 ast))) (log 2) 8)))
	;; fall back to the general case
	(type-rule-int-op2 ast))) ;; TODO is this optimization worth it, or does it break the samentics of C ?
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x*y 'x*y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.*x '*x
  (lambda (ast)
    'byte) ; we only have byte arrays
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.index 'index
  (lambda (ast)
    'byte) ; we only have byte arrays
  (lambda (ast)
    ast)
  (lambda (asr)
    ...))

(define-op2 'six.x/y 'x/y
  (lambda (ast)
    ;; if we know the second operand, we can have an upper bound on the size
    ;; of the result
    (if (literal? (subast1 ast))
	;; for every byte over 1 in the length of y, we can remove a byte from
	;; the result
	;; ex : the smallest value which needs 2 bytes to encode is 256, and
	;; dividing by 256 is equivalent to truncating the 8 lowest bits, and
	;; so on
	(let ((l1 (type->bytes (expr-type (subast1 ast))))
	      (l2 (ceiling (/ (log y) (log 2) 8))))
	  (bytes->type (- (max l1 l2) (- l2 1))))
	;; fall back to the general case
	(type-rule-int-op2 ast))) ;; TODO as for modulo, is this optimisation worth it ? if so, & could have a similar or, by being the size of the smaller operand
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x+y 'x+y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.+x '+x
  type-rule-int-op1
  (lambda (ast)
    (subast1 ast))
  (lambda (ast)
    ...))

(define-op2 'six.x-y 'x-y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.-x '-x
  type-rule-int-op1
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x<<y 'x<<y ;; TODO for the general case, would give scary results (a single byte for y can still mean a shift by 255)
  (lambda (ast)
    (if (not (literal? (subast2 ast)))
	(error "only shifting by literals is supported"))
    (let ((l1 (type->bytes (expr-type (subast1 ast))))
	  (v2 (literal-val (subast2 ast))))
      ;; we might have to add some bytes to the result
      (bytes->type (+ l1 (ceiling (/ v2 8))))))
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>>y 'x>>y
  (lambda (ast)
    (if (not (literal? (subast2 ast)))
	(error "only shifting by literals is supported"))
    (let ((l1 (type->bytes (expr-type (subast1 ast))))
	  (v2 (literal-val (subast2 ast))))
      ;; we might be able to shave some bytes off
      (bytes->type (- l1 (floor (/ v2 8))))))
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x<y 'x<y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x<=y 'x<=y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>y 'x>y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>=y 'x>=y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x!=y 'x!=y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x==y 'x==y
  type-rule-int-comp-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x&y 'x&y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op1 'six.&x '&x
  (lambda (ast)
    ...)
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x^y 'x^y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 '|six.x\|y| '|x\|y|
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x&&y 'x&&y
  type-rule-bool-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 '|six.x\|\|y| '|x\|\|y|
  type-rule-bool-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x?y:z 'x?y:z
  (lambda (ast)
    ...)
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x:y 'x:y
  (lambda (ast)
    ...)
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x%=y 'x%=y ;; TODO these don't work
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x&=y 'x&=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x*=y 'x*=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x+=y 'x+=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x-=y 'x-=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x/=y 'x/=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x<<=y 'x<<=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x=y 'x=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>>=y 'x>>=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x^=y 'x^=y
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 '|six.x\|=y| '|x\|=y|
  type-rule-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x:=y 'x:=y
  (lambda (ast)
    ...)
  (lambda (ast)
    ...)
  (lambda (ast)
    ...))

(define-op2 '|six.x,y| '|x,y|
  (lambda (ast)
    ...)
  (lambda (ast)
    ...)
  (lambda (ast)
    ...))

(define-op2 'six.x:-y 'x:-y
  (lambda (ast)
    ...)
  (lambda (ast)
    ...)
  (lambda (ast)
    ...))

(define (operation? source)
  (and (pair? source)
       (let ((x (car source)))
         (let loop ((lst operators))
           (cond ((null? lst)
                  #f)
                 ((eq? (op-six-id (car lst)) x)
                  (car lst))
                 (else
                  (loop (cdr lst))))))))
