;;; operators

(define operators '())

(define (define-op constructor six-id id type-rule constant-fold code-gen)
  (set! operators
        (cons (constructor six-id id type-rule constant-fold code-gen)
              operators)))
(define (define-op1 six-id id type-rule constant-fold code-gen)
  (define-op make-op1 six-id id type-rule constant-fold code-gen))
(define (define-op2 six-id id type-rule constant-fold code-gen)
  (define-op make-op2 six-id id type-rule constant-fold code-gen))
(define (define-op3 six-id id type-rule constant-fold code-gen)
  (define-op make-op3 six-id id type-rule constant-fold code-gen))

;; no need for type checks, every type sixpic supports can be casted to / from
;; ints (except void, but this is a non-issue) and promotion (by padding) and
;; truncation is done at the cfg level
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
    ;; TODO not sure it's true anymore
    t1))

(define (type-rule-int-comp-op2 ast)
  'bool)

(define (type-rule-bool-op2 ast)
  'bool)

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
  type-rule-int-op2
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

;; TODO check with the C standard for the next 2
(define-op2 'six.x<<y 'x<<y
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 'six.x>>y 'x>>y
  type-rule-int-op2
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

(define-op3 'six.x?y:z 'x?y:z
  (lambda (ast)
    ;; largest of the 2 branches
    (let ((t1 (expr-type (subast2 ast)))
	  (t2 (expr-type (subast3 ast))))
    (largest t1 t2)))
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
