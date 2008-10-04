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

(define (type-rule-int-op1 ast)
  (let ((t1 (expr-type (subast1 ast))))
    (cond ((eq? t1 'int)
           'int) ; TODO add support for other types
          (else
           (error "type error" ast)))))

(define (type-rule-int-op2 ast)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (cond ((and (eq? t1 'int) (eq? t2 'int)) ; TODO are there any operations that do otherwise ? add cast support also
           'int)
          (else
           (error "type error" ast)))))

(define (type-rule-int-assign ast) ;; TODO add cast support, and why the int in the name ?
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (if (not (eq? t1 t2))
        (error "type error" ast))
    t1))

(define (type-rule-int-comp-op2 ast)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (cond ((and (eq? t1 'int) (eq? t2 'int))
           'bool)
          (else
           (error "type error" ast)))))

(define-op1 'six.!x '!x
  type-rule-int-op1
  (lambda (ast) ;; TODO implement these
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
    ...)
  (lambda (ast)
    ast)
  (lambda (ast)
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
    ...)
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
  type-rule-int-op2
  (lambda (ast)
    ast)
  (lambda (ast)
    ...))

(define-op2 '|six.x\|\|y| '|x\|\|y|
  type-rule-int-op2
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
