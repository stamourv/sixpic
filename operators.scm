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

(define (type-rule-int-op2 ast) ;; TODO since we only consider the arguments (and not the size of the variable which will ultimately hold the result), we might end up with some early truncation : ex : 200 + 200, largest is 1 byte, but the result is 2, if the result goes in a int16 ,it should be 400, not 400 - 256, maybe always have some slack ? would be wasteful
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (largest t1 t2)))

(define (type-rule-int-assign ast) ;; TODO why the int in the name ?
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
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x*y 'x*y
  ;; products can be as wide as the sum of the widths of the operands
  (lambda (ast)
    (let ((l1 (type->bytes (expr-type (subast1 ast))))
	  (l2 (type->bytes (expr-type (subast2 ast)))))
      (bytes->type (+ l1 l2))))
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
  type-rule-int-op2 ;; TODO really ?
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

(define-op2 'six.x<<y 'x<<y ;; TODO would really need to check the length of the destination, what we have here is just a hack
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

(define-op2 'six.x%=y 'x%=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x&=y 'x&=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x*=y 'x*=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x+=y 'x+=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x-=y 'x-=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x/=y 'x/=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x<<=y 'x<<=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x=y 'x=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>>=y 'x>>=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x^=y 'x^=y
  type-rule-int-assign
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 '|six.x\|=y| '|x\|=y|
  type-rule-int-assign
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
