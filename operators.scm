;;; operators

(define (castable? from to)
  (if (eq? from to)
      #t ; base case
      (case to
	((int)
	 (foldl (lambda (x y) (or x (castable? from y)))
		#f
		'(byte int8 int16 int32)))
	((bool)
	 (eq? from 'int))
	;; TODO ajouter casts vers byte, int16, etc, probably not needed since operations are done on ints, and useless operations (on bytes that would not exist) are optimized away
	(else #f))))

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
    (cond ((castable? t1 'int)
           'int)
          (else
           (error "int-op1: type error" ast)))))

(define (largest t1 t2)
  (let loop ((l '(int int32 int16 int8 byte)))
    (if (null? l)
	(error "largest: unknown type")
	(let ((head (car l)))
	  (if (or (eq? head t1)
		  (eq? head t2))
	      head
	      (loop (cdr l)))))))

(define (type-rule-int-op2 ast)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (cond ((and (castable? t1 'int) (castable? t2 'int))
           (largest t1 t2))
          (else
           (error "int-op2: type error" ast)))))

(define (type-rule-int-assign ast) ;; TODO why the int in the name ?
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (if (not (castable? t2 t1)) ; the rhs must fit in the lhs
        (error "int-assign: type error" ast))
    t1))

(define (type-rule-int-comp-op2 ast)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (cond ((and (castable? t1 'int) (castable? t2 'int))
           'bool)
          (else
           (error "int-comp-op2: type error" ast)))))

(define (type-rule-bool-op2 ast)
  (let ((t1 (expr-type (subast1 ast)))
        (t2 (expr-type (subast2 ast))))
    (cond ((and (castable? t1 'bool) (castable? t2 'bool))
           'bool)
          (else
           (error "bool-op2: type error" ast)))))

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
