#! /home/vincent/sixpic/gambc-v4_2_9/bin/gsi-script -:dar

(include "pic18-sim.scm")

;------------------------------------------------------------------------------

; utilities

(define (interval n m) ; returns the list (n n+1 n+2 ... m)
  (if (<= n m) (cons n (interval (+ n 1) m)) '()))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

(define (set-equal? s1 s2)
  (and (eq? (diff s1 s2) '())
       (eq? (diff s2 s1) '())))

(define (diff s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (diff (cdr s1) s2))
        (else               (cons (car s1) (diff (cdr s1) s2)))))

(define (intersection s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (cons (car s1) (intersection (cdr s1) s2)))
        (else               (intersection (cdr s1) s2))))

(define (union s1 s2)
  (cond ((null? s1)         s2)
        ((memq (car s1) s2) (union (cdr s1) s2))
        (else               (cons (car s1) (union (cdr s1) s2)))))

(define (union-multi sets) (foldl union '() sets))

(define (foldl f base lst)
  (if (null? lst)
      base
      (foldl f (f base (car lst)) (cdr lst))))

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else              (loop (cdr lst) (+ i 1))))))

;------------------------------------------------------------------------------

(define-type ast
  extender: define-type-of-ast
  (parent unprintable:)
  subasts
)

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

(define (remove x lst)
  (cond ((null? lst)
         '())
        ((eq? x (car lst))
         (cdr lst))
        (else
         (cons (car lst)
               (remove x (cdr lst))))))

(define (subast1 ast) (car (ast-subasts ast)))
(define (subast2 ast) (cadr (ast-subasts ast)))
(define (subast3 ast) (caddr (ast-subasts ast)))
(define (subast4 ast) (cadddr (ast-subasts ast)))

(define-type-of-ast def
  extender: define-type-of-def
  id
  unprintable:
  refs
)

(define-type value
  bytes
)

(define (new-value bytes)
  (make-value bytes))

(define-type byte-cell
  adr
  (interferes-with unprintable:)
  (coalesceable-with unprintable:)
)

(define (new-byte-cell)
  (make-byte-cell #f '() '()))

(define-type byte-lit
  val
)

(define (new-byte-lit x)
  (make-byte-lit x))

(define (nb-bytes type)
  (case type
    ((void) 0)
    ((int) 1) ;; TODO have more types
    (else (error "???"))))

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
  sets
)

(define (new-def-variable subasts id refs type value sets)
  (multi-link-parent!
   subasts
   (make-def-variable #f subasts id refs type value sets)))

(define-type-of-def def-procedure
  type
  value
  params
  entry
  live-after-calls
)

(define (new-def-procedure subasts id refs type value params)
  (multi-link-parent!
   subasts
   (make-def-procedure #f subasts id refs type value params #f '())))

(define-type-of-ast expr
  extender: define-type-of-expr
  type
)

(define-type-of-expr literal
  val
)

(define (new-literal type val)
  (make-literal #f '() type val))

(define-type-of-expr ref
  def-var
)

(define (new-ref type def)
  (make-ref #f '() type def))

(define-type-of-expr oper
  op
)

(define (new-oper subasts type op)
  (multi-link-parent!
   subasts
   (make-oper #f subasts type op)))

(define-type-of-expr call
  def-proc
)

(define (new-call subasts type proc-def)
  (multi-link-parent!
   subasts
   (make-call #f subasts type proc-def)))

(define-type-of-ast block
)

(define (new-block subasts)
  (multi-link-parent!
   subasts
   (make-block #f subasts)))

(define-type-of-ast if
)

(define (new-if subasts)
  (multi-link-parent!
   subasts
   (make-if #f subasts)))

(define-type-of-ast switch
)

(define (new-switch subasts)
  (multi-link-parent!
   subasts
   (make-switch #f subasts)))

(define-type-of-ast case
)

(define (new-case subasts)
  (multi-link-parent!
   subasts
   (make-case #f subasts)))

(define-type-of-ast while
)

(define (new-while subasts)
  (multi-link-parent!
   subasts
   (make-while #f subasts)))

(define-type-of-ast do-while
)

(define (new-do-while subasts)
  (multi-link-parent!
   subasts
   (make-do-while #f subasts)))

(define-type-of-ast for
)

(define (new-for subasts)
  (multi-link-parent!
   subasts
   (make-for #f subasts)))

(define-type-of-ast return
)

(define (new-return subasts)
  (multi-link-parent!
   subasts
   (make-return #f subasts)))

(define-type-of-ast program
)

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
  code-gen
)

(define-type-of-op op1
)

(define-type-of-op op2
)

;; TODO have a table that says what types can cast to what other, and what can happen implicitly
;; TODO what casts are going to happen, only between different integer sizes ?
;; TODO have this as an a-list. a type as car, and the list of types it can cast to as cdr
(define casts '())

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

(define (predefine-var id type adr)
  (let* ((value
          (new-value (list (make-byte-cell adr '() '()))))
         (ast
          (new-def-variable '() id '() type value '())))
    ast))

(define (predefine-fun id type param-defs adr)
  (let* ((value
          (cond ((eq? type 'int)
                 (new-value (list (make-byte-cell WREG '() '()))))
                ((eq? type 'void)
                 (new-value '()))
                (else
                 (error "unknown return type"))))
         (params
          (map (lambda (x)
                 (predefine-var 'foo (car x) (cdr x)))
               param-defs))
         (ast
          (new-def-procedure '() id '() type value params))
         (entry
          (asm-make-label id adr)))
    (multi-link-parent! params ast)
    (def-procedure-entry-set! ast entry)
    ast))

(define (initial-cte) ;; TODO see what really has to be predefined
  (list (predefine-var 'X 'int 5)
        (predefine-var 'Y 'int 6)
        (predefine-var 'Z 'int 7)
        (predefine-fun 'FLASH_execute_erase
                       'void
                       '()
                       #x1EE)
        (predefine-fun 'FLASH_execute_write
                       'void
                       '()
                       #x1F0)
        (predefine-fun 'led_set
                       'void
                       (list (cons 'int WREG))
                       #x1F2)
        (predefine-fun 'irda_tx_wake_up
                       'void
                       '()
                       #x1F4)
        (predefine-fun 'irda_tx_raw
                       'void
                       (list (cons 'int WREG))
                       #x1F6)
        (predefine-fun 'irda_rx_raw
                       'int
                       '()
                       #x1F8)
        (predefine-fun 'sleep_mode
                       'void
                       '()
                       #x1FA)
        (predefine-fun 'exec_client
                       'void
                       '()
                       #x1FC)))

(define (cte-extend cte bindings)
  (append bindings cte))

(define (cte-lookup cte id)
  (cond ((null? cte)
         (error "undefined identifier" id))
        ((eq? (def-id (car cte)) id)
         (car cte))
        (else
         (cte-lookup (cdr cte) id))))

(define (parse source)

  (define (form? keyword source)
    (and (pair? source)
         (eq? (car source) keyword)))

  (define (expect-form keyword source)
    (if (not (form? keyword source))
        (error "expected" keyword source)))

  (define (get-id source)
    (expect-form 'six.identifier source)
    (cadr source))

  (define (define-variable source cte cont)
    (let* ((id (get-id (cadr source)))
           (type (caddr source))
           (dims (cadddr source))
           (val (car (cddddr source))))

      (define (def asts cte)
        (let* ((value
                (alloc-value type))
               (ast
                (new-def-variable asts id '() type value '()))
               (cte
                (cte-extend cte (list ast))))
          (cont ast
                cte)))

      (if val
          (expression val cte (lambda (ast cte) (def (list ast) cte)))
          (def '() cte))))

  (define (define-procedure source cte cont)
    (let* ((id (get-id (cadr source)))
           (proc (caddr source)))
      (expect-form 'six.procedure proc)
      (let* ((type
              (cadr proc))
             (params
              (map (lambda (x)
                     (let* ((type
                             (cadr x))
                            (value
                             (alloc-value type)))
                       (new-def-variable '() (get-id (car x)) '() type value '())))
                   (caddr proc)))
             (body
              (cadddr proc)))
        (expect-form 'six.procedure-body body)
        (let* ((value
                (alloc-value type))
               (ast
                (new-def-procedure '() id '() type value params))
               (cte
                (cte-extend cte (list ast))))
          (multi-link-parent! params ast)
          (block body
                 (cte-extend cte params)
                 (lambda (body-ast body-cte)
                   (ast-subasts-set! ast (list body-ast))
                   (link-parent! body-ast ast)
                   (cont ast
                         cte)))))))

  (define (block source cte cont)

    (define (b source cte cont)
      (cond ((null? source)
             (cont '()
                   cte))
            (else
             (let ((head (car source))
                   (tail (cdr source)))
               (statement head
                          cte
                          (lambda (ast cte)
                            (b tail
                               cte
                               (lambda (asts cte)
                                 (cont (cons ast asts)
                                       cte)))))))))

    (b (cdr source)
       cte
       (lambda (asts cte)
         (cont (new-block asts)
               cte))))

  (define (statement source cte cont)
    (cond ((form? 'six.define-variable source)
           (define-variable source cte cont))
          ((form? 'six.if source)
           (if (null? (cdddr source))
               (if1 source cte cont)
               (if2 source cte cont)))
	  ((form? 'six.switch source)
	   (switch source cte cont))
          ((form? 'six.while source)
           (while source cte cont))
          ((form? 'six.do-while source)
           (do-while source cte cont))
          ((form? 'six.for source)
           (for source cte cont))
          ((form? 'six.return source)
           (return source cte cont))
          ((form? 'six.compound source)
           (block source cte cont))
          (else
           (expression  source cte cont))))

  (define (return source cte cont)

    (define (ret asts cte)
      (cont (new-return asts)
            cte))

    (if (null? (cdr source))
        (ret '() cte)
        (expression (cadr source)
                    cte
                    (lambda (ast cte)
                      (ret (list ast) cte)))))

  (define (if1 source cte cont)
    (expression (cadr source)
                cte
                (lambda (ast1 cte)
                  (statement (caddr source)
                             cte
                             (lambda (ast2 cte)
                               (cont (new-if (list ast1 ast2))
                                     cte))))))

  (define (if2 source cte cont)
    (expression (cadr source)
                cte
                (lambda (ast1 cte)
                  (statement (caddr source)
                             cte
                             (lambda (ast2 cte)
                               (statement (cadddr source)
                                          cte
                                          (lambda (ast3 cte)
                                            (cont (new-if (list ast1 ast2 ast3))
                                                  cte))))))))

  ;; TODO should default be a six.case or a six.label ? now it's a label, should we consider the case where we have a default label outside a switch ?
  (define (switch source cte cont)
    (expression (cadr source)
		cte
		(lambda (ast1 cte) ; we matched the paren expr
		  (expect-form 'six.compound (caddr source))
		  (cases (cdaddr source) cte cont ast1 '() '()))))

  ;; TODO fuse case-list and seen ? also, this is absolutely disgusting
  (define (cases source cte cont id case-list seen) ; source is a list of cases
    (if (null? source) ; we've seen everything
	;; 1st subast is the id, all the others are cases
	(cont (new-switch (cons id case-list))
	      cte)
	(let* ((curr (car source))
	      (after-label
	       (lambda (label first-statement cte)
		 (fill-case (cdr source)
			    cte
			    ;; list with the id of the case and first statement of the body
			    (list label
				  first-statement)
			    (lambda (ast2 case cte)
			      (display "RES\n")
			      (pp case)
			      (cases ast2
				     cte
				     cont
				     id
				     (append case-list (list case))
				     (cons (cadr curr) seen)))))))
	  (display "FOO\n")
	  (pp curr)
	  (if (memq (cadr curr) seen)
	      (error "duplicate cases" (cadr curr))
	      (if (form? 'six.case (car curr))
		  (literal (cadr curr) ; the id of the label
			   cte
			   (lambda (label cte)
			     (statement (caddr source) ; first statement of the case
					cte
					(lambda (first-statement cte)
					  (after-label label first-statement cte)))))
		  (statement (caddr curr)
			     cte
			     (lambda (first-statement cte)
			       (after-label 'default first-statement cte)))))))) ; default
  ;; TODO match up to the next case or to the end of the switch, the use cont to match the rest, with seen augmented with what we just matched
  (define (fill-case source cte case cont)
    (display "CASE\n")
    (pp case)
    (if (or (null? source) ; we reached the end of the switch
	    (form? 'six.case (car source)) ; we reached another case
	    (and (form? 'six.label (car source)) ; we reached the default
		 (eq? (cadar source) 'default)))
	(cont source
	      (new-case case) ; case is a list with the id and the list of statements of the "body"
	      cte)
	(statement (car source)
		   cte
		   (lambda (ast1 cte)
		     (fill-case (cdr source) cte cont (append case (list ast1)))))))
  
  (define (while source cte cont)
    (expression (cadr source)
                cte
                (lambda (ast1 cte)
                  (statement (caddr source)
                             cte
                             (lambda (ast2 cte)
                               (cont (new-while (list ast1 ast2))
                                     cte))))))

  (define (do-while source cte cont)
    (statement (cadr source)
               cte
               (lambda (ast1 cte)
                 (expression (caddr source)
                             cte
                             (lambda (ast2 cte)
                               (cont (new-do-while (list ast1 ast2))
                                     cte))))))

  (define (for source cte cont)

    (define (opt-expr source cte cont)
      (if source
          (expression source cte cont)
          (cont #f cte)))

    (statement (cadr source)
               cte
               (lambda (ast1 cte)
                 (opt-expr (caddr source)
                           cte
                           (lambda (ast2 cte)
                             (opt-expr (cadddr source)
                                       cte
                                       (lambda (ast3 cte)
                                         (statement (car (cddddr source))
                                                    cte
                                                    (lambda (ast4 cte)
                                                      (cont (new-for
                                                             (list ast1
                                                                   (or ast2
                                                                       (new-literal 'int 1))
                                                                   (or ast3
                                                                       (new-block '()))
                                                                   ast4))
                                                            cte))))))))))

  (define (expression source cte cont)
    (cond ((form? 'six.literal source)
           (literal source cte cont))
          ((form? 'six.identifier source)
           (ref source cte cont))
          ((form? 'six.call source)
           (call source cte cont))
          ((operation? source)
           =>
           (lambda (op)
             (operation op source cte cont)))
	  ((form? 'six.label source) ; for now, we ignore the labels
	   (expression (caddr source) cte cont)) ; the body is the 3rd element
          (else
           (error "expected ???" source))))

  (define (operation op source cte cont)
    (if (op1? op)
        (expression (cadr source)
                    cte
                    (lambda (ast1 cte)
                      (let ((ast
                             (new-oper (list ast1) #f op)))
                        (expr-type-set! ast ((op-type-rule op) ast))
                        (cont ((op-constant-fold op) ast)
                              cte))))
        (expression (cadr source)
                    cte
                    (lambda (ast1 cte)
                      (expression (caddr source)
                                  cte
                                  (lambda (ast2 cte)
                                    (let ((ast
                                           (new-oper (list ast1 ast2) #f op)))
                                      (expr-type-set! ast ((op-type-rule op) ast))
                                      (cont ((op-constant-fold op) ast)
                                            cte))))))))

  (define (call source cte cont)
    (let* ((id (get-id (cadr source)))
           (binding (cte-lookup cte id)))
      (if (def-procedure? binding)
          (expressions (cddr source)
                       cte
                       (lambda (args cte)
                         (cont (new-call args (def-procedure-type binding) binding)
                               cte)))
          (error "expected procedure" source))))

  (define (expressions source cte cont)
    (cond ((null? source)
           (cont '()
                 cte))
          (else
           (let ((head (car source))
                 (tail (cdr source)))
             (expression head
                         cte
                         (lambda (ast cte)
                           (expressions tail
                                        cte
                                        (lambda (asts cte)
                                          (cont (cons ast asts)
                                                cte)))))))))

  (define (literal source cte cont)
    (cont (new-literal 'int (cadr source))
          cte))

  (define (ref source cte cont)
    (let* ((id (cadr source))
           (binding (cte-lookup cte id)))
      (if (def-variable? binding)
          (cont (new-ref (def-variable-type binding) binding)
                cte)
          (error "expected variable" source))))

  (define (toplevel source cte cont) ;; TODO have an implicit main
    (cond ((form? 'six.define-variable source)
           (define-variable source cte cont))
          ((form? 'six.define-procedure source)
           (define-procedure source cte cont))
          (else
           (statement source cte cont))))

  (define (program source cte cont)

    (define (p source cte cont)
      (cond ((null? source)
             (cont '()
                   cte))
            (else
             (let ((head (car source))
                   (tail (cdr source)))
               (toplevel head
                         cte
                         (lambda (ast cte)
                           (p tail
                              cte
                              (lambda (asts cte)
                                (cont (cons ast asts)
                                      cte)))))))))

    (p source
       cte
       (lambda (asts cte)
         (cont (new-program asts)
               cte))))

  (program source
           (initial-cte)
           (lambda (ast cte)
             ast)))

;------------------------------------------------------------------------------

; generation of control flow graph

(define-type cfg
  bbs
  next-label-num
)

(define (new-cfg)
  (make-cfg '() 0))

(define-type bb
  label-num
  label
  rev-instrs
  unprintable:
  preds
  succs
  live-before
)

(define-type instr
  extender: define-type-of-instr
  (live-before #;unprintable:)
  (live-after #;unprintable:)
  (hash unprintable:)
  id
  src1
  src2
  dst
)

(define-type-of-instr call-instr
  unprintable:
  def-proc
)

(define-type-of-instr return-instr
  unprintable:
  def-proc
)

(define (new-instr id src1 src2 dst)
  (make-instr '() '() #f id src1 src2 dst))

(define (new-call-instr def-proc)
  (make-call-instr '() '() #f 'call #f #f #f def-proc))

(define (new-return-instr def-proc)
  (make-return-instr '() '() #f 'return #f #f #f def-proc))

(define (add-bb cfg)
  (let* ((label-num (cfg-next-label-num cfg))
         (bb (make-bb label-num #f '() '() '() '())))
    (bb-label-set!
     bb
     (asm-make-label
      (string->symbol
       (string-append "$"
                      (number->string label-num)))))
    (cfg-bbs-set! cfg (cons bb (cfg-bbs cfg)))
    (cfg-next-label-num-set! cfg (+ 1 (cfg-next-label-num cfg)))
    bb))

(define (add-instr bb instr)
  (let ((rev-instrs (bb-rev-instrs bb)))
    (bb-rev-instrs-set! bb (cons instr rev-instrs))))

(define (add-succ bb succ)
  (bb-succs-set! bb (cons succ (bb-succs bb)))
  (bb-preds-set! succ (cons bb (bb-preds succ))))

(define (generate-cfg ast)

  (define cfg (new-cfg))

  (define bb #f) ; current bb

  (define (in x)
    (set! bb x))

  (define (new-bb)
    (add-bb cfg))

  (define (emit instr)
    (add-instr bb instr))

  (define current-def-proc #f)
  (define break-stack '())
  (define continue-stack '())
  (define delayed-post-incdec '())

  (define (push-break x)
    (set! break-stack (cons x break-stack)))

  (define (pop-break)
    (set! break-stack (cdr break-stack)))

  (define (push-continue x)
    (set! continue-stack (cons x continue-stack)))

  (define (pop-continue)
    (set! continue-stack (cdr continue-stack)))

  (define (push-delayed-post-incdec x)
    (set! delayed-post-incdec (cons x delayed-post-incdec)))

  (define (program ast)
    (let loop ((asts (ast-subasts ast)))
      (if (not (null? asts))
          (let ((ast (car asts)))
            (if (null? (cdr asts))
                (let ((value (expression ast)))
                  (return-with-no-new-bb value))
                (begin
                  (toplevel ast)
                  (loop (cdr asts))))))))

  (define (toplevel ast)
    (cond ((def-variable? ast)
           (def-variable ast))
          ((def-procedure? ast)
           (def-procedure ast))
          (else
           (statement ast))))

  (define (def-variable ast)
    (let ((subasts (ast-subasts ast)))
      (if (not (null? subasts))
          (let ((value (expression (subast1 ast))))
            (let ((ext-value (extend value (def-variable-type ast))))
              (move-value value (def-variable-value ast)))))))

  (define (def-procedure ast)
    (let ((old-bb bb)
          (entry (new-bb)))
      (def-procedure-entry-set! ast entry)
      (set! current-def-proc ast)
      (in entry)
      (for-each statement (ast-subasts ast))
      (return-with-no-new-bb ast)
      (set! current-def-proc #f)
      (in old-bb)))

  (define (statement ast) ;; TODO should labels go into statements or expressions ?
    (cond ((def-variable? ast)
           (def-variable ast))
          ((block? ast)
           (block ast))
          ((return? ast)
           (return ast))
          ((if? ast)
           (if (null? (cddr (ast-subasts ast)))
               (if1 ast)
               (if2 ast)))
          ((while? ast)
           (while ast))
          ((do-while? ast)
           (do-while ast))
          ((for? ast)
           (for ast))
          (else
           (expression ast))))

  (define (block ast)
    (for-each statement (ast-subasts ast)))

  (define (move from to)
    (emit (new-instr 'move from #f to)))

  (define (move-value from to)
    (for-each move
              (value-bytes from)
              (value-bytes to)))
               
  (define (return-with-no-new-bb def-proc)
    (emit (new-return-instr def-proc)))

  (define (return ast)
    (if (null? (ast-subasts ast))
        (return-with-no-new-bb current-def-proc)
        (let ((value (expression (subast1 ast))))
          (let ((ext-value (extend value (def-procedure-type current-def-proc))))
            (move-value value (def-procedure-value current-def-proc))
            (return-with-no-new-bb current-def-proc))))
    (in (new-bb)))

  (define (if1 ast)
    (let* ((bb-join (new-bb))
           (bb-then (new-bb)))
      (test-expression (subast1 ast) bb-then bb-join)
      (in bb-then)
      (statement (subast2 ast))
      (in bb-join)))

  (define (if2 ast)
    (let* ((bb-join (new-bb))
           (bb-then (new-bb))
           (bb-else (new-bb)))
      (test-expression (subast1 ast) bb-then bb-else)
      (in bb-then)
      (statement (subast2 ast))
      (goto bb-join)
      (in bb-else)
      (statement (subast3 ast))
      (goto bb-join)
      (in bb-join)))

  (define (while ast)
    (let* ((bb-cont (new-bb))
           (bb-exit (new-bb))
           (bb-body (new-bb)))
      (push-continue bb-cont)
      (push-break bb-exit)
      (goto bb-cont)
      (in bb-cont)
      (test-expression (subast1 ast) bb-body bb-exit)
      (in bb-body)
      (statement (subast2 ast))
      (goto bb-cont)
      (in bb-exit)
      (pop-continue)
      (pop-break)))

  (define (do-while ast)
    (let* ((bb-body (new-bb))
           (bb-cont (new-bb))
           (bb-exit (new-bb)))
      (push-continue bb-cont)
      (push-break bb-exit)
      (in bb-body)
      (statement (subast1 ast))
      (in bb-cont)
      (test-expression (subast2 ast) bb-body bb-exit)
      (in bb-exit)
      (pop-continue)
      (pop-break)))

  (define (for ast)
    (let* ((bb-loop (new-bb))
           (bb-body (new-bb))
           (bb-cont (new-bb))
           (bb-exit (new-bb)))
      (statement (subast1 ast))
      (goto bb-loop)
      (push-continue bb-cont)
      (push-break bb-exit)
      (in bb-loop)
      (test-expression (subast2 ast) bb-body bb-exit)
      (in bb-body)
      (statement (subast4 ast))
      (goto bb-cont)
      (in bb-cont)
      (expression (subast3 ast))
      (goto bb-loop)
      (in bb-exit)
      (pop-continue)
      (pop-break)))

  (define (goto dest)
    (add-succ bb dest)
    (emit (new-instr 'goto #f #f #f)))

  (define (test-expression ast bb-true bb-false)

    (define (test-lit id x y)
      ((case id
         ((x==y) =)
         ((x<y) <)
         ((x>y) >)
         (else (error "...")))
       x
       y))

    (define (test-byte id byte1 byte2 bb-true bb-false)
      (cond ((and (byte-lit? byte1) (byte-lit? byte2))
             (if (test-lit id (byte-lit-val byte1) (byte-lit-val byte2))
                 (goto bb-true)
                 (goto bb-false)))
            ((byte-lit? byte2)
             (add-succ bb bb-true)
             (add-succ bb bb-false)
             (emit (new-instr id byte1 byte2 #f)))
            ((byte-lit? byte1)
             (let ((id
                    (case id
                      ((x==y) 'x==y)
                      ((x<y) 'x>y)
                      ((x>y) 'x<y)
                      (else (error "...")))))
               (add-succ bb bb-true)
               (add-succ bb bb-false)
               (emit (new-instr id byte2 byte1 #f))))
            (else
             (add-succ bb bb-true)
             (add-succ bb bb-false)
             (emit (new-instr id byte1 byte2 #f)))))

    (define (test-value id value1 value2 bb-true bb-false)
      ; note: for multi-byte values, only x==y works properly
      (let* ((bytes1 (value-bytes value1))
             (bytes2 (value-bytes value2)))
        (let loop ((bytes1 bytes1) (bytes2 bytes2))
          (let ((byte1 (car bytes1))
                (byte2 (car bytes2)))
            (if (null? (cdr bytes1))
                (test-byte id byte1 byte2 bb-true bb-false)
                (let ((bb-true2 (new-bb)))
                  (test-byte id byte1 byte2 bb-true2 bb-false)
                  (in bb-true2)
                  (loop (cdr bytes1) (cdr bytes2))))))))

    (define (test-relation id x y bb-true bb-false)
      (cond ((and (literal? x) (not (literal? y)))
             (compare (case id
                        ((x==y x!=y) id)
                        ((x<y) 'x>y)
                        ((x>y) 'x<y)
                        ((x<=y) 'x>=y)
                        ((x>=y) 'x<=y)
                        (else (error "???")))
                      y
                      x
                      bb-true
                      bb-false))
            ((assq id '((x!=y . x==y) (x<=y . x>y) (x>=y . x<y)))
             =>
             (lambda (z) (compare (cdr z) x y bb-false bb-true)))
            (else
'
             (case id
               ((x==y)
                (cond ((and (literal? y) (= (literal-val y) 0))
                       (test-zero x bb-true bb-false))
                      ((literal? y)
                       (test-eq-lit x (literal-val y) bb-true bb-false))
                      (else
                       (error "unhandled case"))))
               ((x<y)
                (cond ((and (literal? y) (= (literal-val y) 0))
                       (test-negative x bb-true bb-false))
                      (else
                       (error "unhandled case"))))
               ((x>y)
                (cond ((and (literal? y) (= (literal-val y) 0))
                       (test-positive x bb-true bb-false))
                      (else
                       (error "unhandled case"))))
               (else
                (error "unexpected operator")))

             (let* ((value1 (expression x))
                    (value2 (expression y)))
               (test-value id value1 value2 bb-true bb-false))
)))

    (define (test-zero ast bb-true bb-false)

      (define (default)
        (let ((type (expr-type ast))
              (value (expression ast)))
          (test-equal value (int->value 0 type) bb-true bb-false)))

      (cond ((oper? ast)
             (let* ((op (oper-op ast))
                    (id (op-id op)))
               (case id
                 ((!x)
                  (test-zero (subast1 ast) bb-false bb-true))
                 ((x&&y)
                  ...)
                 ((|x\|\|y|)
                  ...)
                 (else
                  (test-relation id
                                 (subast1 ast)
                                 (subast2 ast)
                                 bb-true
                                 bb-false)))))
            (else
             (default))))

    (test-zero ast bb-false bb-true))

  (define (expression ast)
    (let ((result
           (cond ((literal? ast)
                  (literal ast))
                 ((ref? ast)
                  (ref ast))
                 ((oper? ast)
                  (oper ast))
                 ((call? ast)
                  (call ast))
                 (else
                  (error "unexpected ast" ast)))))
      (do-delayed-post-incdec)
      result))

  (define (literal ast)
    (let ((val (literal-val ast)))
      (int->value val (expr-type ast))))

  (define (ref ast)
    (let* ((def-var (ref-def-var ast))
           (value (def-variable-value def-var)))
      value))

  (define (add-sub id value1 value2 result)
    (let loop ((bytes1 (value-bytes value1))
               (bytes2 (value-bytes value2))
               (bytes3 (value-bytes result))
               (ignore-carry-borrow? #t))
      (if (not (null? bytes1))
          (let ((byte1 (car bytes1))
                (byte2 (car bytes2))
                (byte3 (car bytes3)))
            (emit
             (new-instr (if ignore-carry-borrow?
                            (case id ((x+y) 'add) ((x-y) 'sub))
                            (case id ((x+y) 'addc) ((x-y) 'subb)))
                        byte1
                        byte2
                        byte3))
            (loop (cdr bytes1)
                  (cdr bytes2)
                  (cdr bytes3)
                  #f)))))

  (define (do-delayed-post-incdec)
    (if (not (null? delayed-post-incdec))
        (let* ((ast (car delayed-post-incdec))
               (type (expr-type ast))
               (op (oper-op ast))
               (id (op-id op)))
          (set! delayed-post-incdec (cdr delayed-post-incdec))
          (let ((x (subast1 ast)))
            (if (not (ref? x))
                (error "assignment target must be a variable"))
            (let ((result (def-variable-value (ref-def-var x))))
              (add-sub (if (eq? id 'x++) 'x+y 'x-y)
                       result
                       (int->value 1 type)
                       result)))
          (do-delayed-post-incdec))))

  (define (oper ast)
    (let* ((type (expr-type ast))
           (op (oper-op ast))
           (id (op-id op)))
      (let ((op (oper-op ast)))
        (if (op1? op)
            (begin
              (case id
                ((-x)
                 (let ((x (subast1 ast)))
                   (let ((value-x (expression x)))
                     (let ((ext-value-x (extend value-x type)))
                       (let ((result (alloc-value type)))
                         (add-sub 'x-y
                                  (int->value 0 type)
                                  ext-value-x
                                  result)
                         result)))))
                ((++x --x)
                 (let ((x (subast1 ast)))
                   (if (not (ref? x))
                       (error "assignment target must be a variable"))
                   (let ((result (def-variable-value (ref-def-var x))))
                     (add-sub (if (eq? id '++x) 'x+y 'x-y)
                              result
                              (int->value 1 type)
                              result)
                     result)))
                ((x++ x--)
                 (let ((x (subast1 ast)))
                   (if (not (ref? x))
                       (error "assignment target must be a variable"))
                   (let ((result (def-variable-value (ref-def-var x))))
                     (push-delayed-post-incdec ast)
                     result)))
                (else
                 (error "???"))))
            (begin
              (case id
                ((x+y x-y x*y x/y x%y)
                 (let* ((x (subast1 ast))
                        (y (subast2 ast)))
                   (let* ((value-x (expression x))
                          (value-y (expression y)))
                     (let* ((ext-value-x (extend value-x type))
                            (ext-value-y (extend value-y type)))
                       (let ((result (alloc-value type)))
                         (if (or (eq? id 'x+y)
                                 (eq? id 'x-y))
                             (add-sub id ext-value-x ext-value-y result)
                             (error "..."))
                         result)))))
                ((x=y)
                 (let* ((x (subast1 ast))
                        (y (subast2 ast)))
                   (if (not (ref? x))
                       (error "assignment target must be a variable"))
                   (let ((value-y (expression y)))
                     (let ((ext-value-y (extend value-y type)))
                       (let ((result (def-variable-value (ref-def-var x))))
                         (move-value value-y result)
                         result)))))
                (else
                 (error "???"))))))))

  (define (call ast)
    (let ((def-proc (call-def-proc ast)))
      (for-each (lambda (ast def-var)
                  (let ((value (expression ast)))
                    (let ((ext-value (extend value (def-variable-type def-var))))
                      (move-value value (def-variable-value def-var)))))
                (ast-subasts ast)
                (def-procedure-params def-proc))
      (emit (new-call-instr def-proc))
      (let ((value (def-procedure-value def-proc)))
        (let ((result (alloc-value (def-procedure-type def-proc))))
          (move-value value result)
          result))))

  (in (new-bb))
  (program ast)
  cfg)

;------------------------------------------------------------------------------

(define (analyze-liveness cfg)

  (define changed? #t)

  (define (instr-analyze-liveness instr live-after)
    (let ((live-before
           (cond ((call-instr? instr)
                  (let ((def-proc (call-instr-def-proc instr)))
                    (let* ((old
                            (def-procedure-live-after-calls def-proc))
                           (new
                            (union old
                                   live-after)))
                      (if (not (set-equal? old new))
                          (begin
                            (set! changed? #t)
                            (def-procedure-live-after-calls-set! def-proc new))))
                    (let ((live
                           (union
                            (union-multi
                             (map (lambda (def-var)
                                    (value-bytes (def-variable-value def-var)))
                                  (def-procedure-params def-proc)))
                            (diff live-after
                                  (value-bytes (def-procedure-value def-proc))))))
                      (if (bb? (def-procedure-entry def-proc))
                          (intersection
                           (bb-live-before (def-procedure-entry def-proc))
                           live)
                          live))))
                 ((return-instr? instr)
;(pp (list instr: instr))
                  (let ((def-proc (return-instr-def-proc instr)))
                    (let ((live
                           (if (def-procedure? def-proc)
                               (def-procedure-live-after-calls def-proc)
                               (value-bytes def-proc))))
(let ((live (keep byte-cell? live)))
;(pp (list live: live))
                      (set! live-after live)
                      live)))
)
                 (else
;(pp (list instr: instr))
                  (let* ((src1 (instr-src1 instr))
                         (src2 (instr-src2 instr))
                         (dst (instr-dst instr))
                         (use (if (byte-cell? src1)
                                  (if (byte-cell? src2)
                                      (union (list src1) (list src2))
                                      (list src1))
                                  (if (byte-cell? src2)
                                      (list src2)
                                      '())))
                         (def (if (byte-cell? dst)
                                  (list dst)
                                  '())))
                    (if #f #;(and (byte-cell? dst) ; dead instruction?
                             (not (memq dst live-after))
                             (not (and (byte-cell? dst) (byte-cell-adr dst))))
                        live-after
                        (union use (diff live-after def))))))))
      (instr-live-before-set! instr live-before)
      (instr-live-after-set! instr live-after)
      live-before))

  (define (bb-analyze-liveness bb)
    (let loop ((rev-instrs (bb-rev-instrs bb))
               (live-after (union-multi (map bb-live-before (bb-succs bb)))))
      (if (null? rev-instrs)
          (if (not (set-equal? live-after (bb-live-before bb)))
              (begin
                (set! changed? #t)
                (bb-live-before-set! bb live-after)))
          (let* ((instr (car rev-instrs))
                 (live-before (instr-analyze-liveness instr live-after)))
            (loop (cdr rev-instrs)
                  live-before)))))

  (let loop ()
    (if changed?
        (begin
          (set! changed? #f)
          (for-each bb-analyze-liveness (cfg-bbs cfg))
          (loop)))))

(define (interference-graph cfg)

  (define all-live '())

  (define (interfere x y)
    (if (not (memq x (byte-cell-interferes-with y)))
        (begin
          (byte-cell-interferes-with-set!
           x
           (cons y (byte-cell-interferes-with x)))
          (byte-cell-interferes-with-set!
           y
           (cons x (byte-cell-interferes-with y))))))

  (define (interfere-pairwise live)
    (set! all-live (union all-live live))
    (for-each (lambda (x)
                (for-each (lambda (y)
                            (if (not (eq? x y))
                                (interfere x y)))
                          live))
              live))

  (define (instr-interference-graph instr)
    (let ((dst (instr-dst instr)))
      (if (byte-cell? dst)
          (let ((src1 (instr-src1 instr))
                (src2 (instr-src2 instr)))
            (if (byte-cell? src1)
                (begin
                  (byte-cell-coalesceable-with-set!
                   dst
                   (union (byte-cell-coalesceable-with dst)
                          (list src1)))
                  (byte-cell-coalesceable-with-set!
                   src1
                   (union (byte-cell-coalesceable-with src1)
                          (list dst)))))
            (if (byte-cell? src2)
                (begin
                  (byte-cell-coalesceable-with-set!
                   dst
                   (union (byte-cell-coalesceable-with dst)
                          (list src2)))
                  (byte-cell-coalesceable-with-set!
                   src2
                   (union (byte-cell-coalesceable-with src2)
                          (list dst))))))))
    (let ((live-before (instr-live-before instr)))
      (interfere-pairwise live-before)))

  (define (bb-interference-graph bb)
    (for-each instr-interference-graph (bb-rev-instrs bb)))

  (analyze-liveness cfg)

  (for-each bb-interference-graph (cfg-bbs cfg))

  all-live)

(define (allocate-registers cfg)
  (let ((all-live (interference-graph cfg)))

    (define (color byte-cell)
      (let ((coalesce-candidates
             (keep byte-cell-adr
                   (diff (byte-cell-coalesceable-with byte-cell)
                         (byte-cell-interferes-with byte-cell)))))
        '
        (pp (list byte-cell: byte-cell;;;;;;;;;;;;;;;
                  coalesce-candidates
;                  interferes-with: (byte-cell-interferes-with byte-cell)
;                  coalesceable-with: (byte-cell-coalesceable-with byte-cell)
))

        (if #f #;(not (null? coalesce-candidates))
            (let ((adr (byte-cell-adr (car coalesce-candidates))))
              (byte-cell-adr-set! byte-cell adr))
            (let ((neighbours (byte-cell-interferes-with byte-cell)))
              (let loop1 ((adr 0))
                (let loop2 ((lst neighbours))
                  (if (null? lst)
                      (byte-cell-adr-set! byte-cell adr)
                      (let ((x (car lst)))
                        (if (= adr (byte-cell-adr x))
                            (loop1 (+ adr 1))
                            (loop2 (cdr lst)))))))))))

    (define (delete byte-cell1 neighbours)
      (for-each (lambda (byte-cell2)
                  (let ((lst (byte-cell-interferes-with byte-cell2)))
                    (byte-cell-interferes-with-set!
                     byte-cell2
                     (remove byte-cell1 lst))))
                neighbours))

    (define (undelete byte-cell1 neighbours)
      (for-each (lambda (byte-cell2)
                  (let ((lst (byte-cell-interferes-with byte-cell2)))
                    (byte-cell-interferes-with-set!
                     byte-cell2
                     (cons byte-cell1 lst))))
                neighbours))

    (define (find-min-neighbours graph)
      (let loop ((lst graph) (m #f) (byte-cell #f))
        (if (null? lst)
            byte-cell
            (let* ((x (car lst))
                   (n (length (byte-cell-interferes-with x))))
              (if (or (not m) (< n m))
                  (loop (cdr lst) n x)
                  (loop (cdr lst) m byte-cell))))))

    (define (alloc-reg graph)
      (if (not (null? graph))
          (let* ((byte-cell (find-min-neighbours graph))
                 (neighbours (byte-cell-interferes-with byte-cell)))
            (let ((new-graph (remove byte-cell graph)))
              (delete byte-cell neighbours)
              (alloc-reg new-graph)
              (undelete byte-cell neighbours))
            (if (not (byte-cell-adr byte-cell))
                (color byte-cell)))))

    (alloc-reg all-live)))

;------------------------------------------------------------------------------

(define (cfg->vector cfg)
  (let ((vect (make-vector (cfg-next-label-num cfg) #f)))
    (for-each (lambda (bb)
                (vector-set! vect (bb-label-num bb) bb))
              (cfg-bbs cfg))
    vect))

(define (remove-branch-cascades-and-dead-code cfg)
  (let ((bbs-vector (cfg->vector cfg)))

    (define (chase-branch-cascade bb seen)
      (if (memq bb seen)
          bb
          (let* ((rev-instrs (bb-rev-instrs bb))
                 (last (car rev-instrs)))
            (if (null? (cdr rev-instrs))

                (cond ((eq? (instr-id last) 'goto)
                       (let ((old-dest
                              (car (bb-succs bb))))
                         (let ((new-dest
                                (chase-branch-cascade old-dest
                                                      (cons bb seen))))
                           (if (not (eq? old-dest new-dest))
                               (begin
                                 (bb-succs-set! bb
                                                (remove old-dest (bb-succs bb)))
                                 (bb-preds-set! old-dest
                                                (remove bb (bb-preds old-dest)))))
                           new-dest)))
                      (else
                       bb))

                bb))))

    (define (bb-process bb)
      (let* ((seen
              (list bb))
             (old-succs
              (bb-succs bb))
             (new-succs
              (map (lambda (x) (chase-branch-cascade x seen)) old-succs)))
          (for-each (lambda (old-dest new-dest)
                      (if (not (eq? old-dest new-dest))
                          (begin
                            (bb-succs-set! bb
                                           (remove old-dest (bb-succs bb)))
                            (bb-preds-set! old-dest
                                           (remove bb (bb-preds old-dest))))))
                    old-succs
                    new-succs)))

    (for-each bb-process (cfg-bbs cfg))))

(define (linearize-and-cleanup cfg)

  (define bbs-vector (cfg->vector cfg))

  (define todo '())

  (define (add-todo bb)
    (set! todo (cons bb todo)))

  (define rev-code '())

  (define (emit instr)
    (set! rev-code (cons instr rev-code)))

  (define (movlw val)
    (emit (list 'movlw val)))

  (define (movwf adr)
    (emit (list 'movwf adr)))

  (define (movfw adr)
    (emit (list 'movfw adr)))

  (define (clrf adr)
    (emit (list 'clrf adr)))

  (define (setf adr)
    (emit (list 'setf adr)))

  (define (incf adr)
    (emit (list 'incf adr)))

  (define (decf adr)
    (emit (list 'decf adr)))

  (define (addwf adr)
    (emit (list 'addwf adr)))

  (define (addwfc adr)
    (emit (list 'addwfc adr)))

  (define (subwf adr)
    (emit (list 'subwf adr)))

  (define (subwfb adr)
    (emit (list 'subwfb adr)))

  (define (cpfseq adr)
    (emit (list 'cpfseq adr)))

  (define (cpfslt adr)
    (emit (list 'cpfslt adr)))

  (define (cpfsgt adr)
    (emit (list 'cpfsgt adr)))

  (define (bra label)
    (emit (list 'bra label)))

  (define (rcall label)
    (emit (list 'rcall label)))

  (define (return)
    (if (and #f (and (not (null? rev-code))
             (eq? (caar rev-code) 'rcall))
)
        (let ((label (cadar rev-code)))
          (set! rev-code (cdr rev-code))
          (bra label))
        (emit (list 'return))))

  (define (label lab)
    (if (and #f (and (not (null? rev-code))
             (eq? (caar rev-code) 'bra)
             (eq? (cadar rev-code) lab))
)
        (begin
          (set! rev-code (cdr rev-code))
          (label lab))
        (emit (list 'label lab))))

  (define (sleep)
    (emit (list 'sleep)))

  (define (move-reg src dst)
    (cond ((= src dst))
          ((= src WREG)
           (movwf dst))
          ((= dst WREG)
           (movfw src))
          (else
           (movfw src)
           (movwf dst))))

  (define (bb-linearize bb)
    (let ((label-num (bb-label-num bb)))
      (let ((bb (vector-ref bbs-vector label-num)))

        (define (move-lit n adr)
          (cond ((= n 0)
                 (clrf adr))
                ((= n #xff)
                 (setf adr))
                (else
                 (movlw n)
                 (movwf adr))))

        (define (dump-instr instr)
          (cond ((call-instr? instr)
                 (let* ((def-proc (call-instr-def-proc instr))
                        (entry (def-procedure-entry def-proc)))
                   (if (bb? entry)
                       (begin
                         (add-todo entry)
                         (let ((label (bb-label entry)))
                           (rcall label)))
                       (rcall entry))))
                ((return-instr? instr)
                 (return))
                (else
                 (let ((src1 (instr-src1 instr))
                       (src2 (instr-src2 instr))
                       (dst (instr-dst instr)))
                   (if (and (or (not (byte-cell? dst))
                                (byte-cell-adr dst))
                            (or (not (byte-cell? src1))
                                (byte-cell-adr src1))
                            (or (not (byte-cell? src2))
                                (byte-cell-adr src2)))
                       (case (instr-id instr)
                         ((move)
                          (if (byte-lit? src1)
                              (let ((n (byte-lit-val src1))
                                    (z (byte-cell-adr dst)))
                                (move-lit n z))
                              (let ((x (byte-cell-adr src1))
                                    (z (byte-cell-adr dst)))
                                (move-reg x z))))
                         ((add addc sub subb)
                          (if (byte-lit? src2)
                              (let ((n (byte-lit-val src2))
                                    (z (byte-cell-adr dst)))
                                (if (byte-lit? src1)
                                    (move-lit (byte-lit-val src1) z)
                                    (move-reg (byte-cell-adr src1) z))
                                (case (instr-id instr)
                                  ((add)
                                   (cond ((= n 1)
                                          (incf z))
                                         ((= n #xff)
                                          (decf z))
                                         (else
                                          (movlw n)
                                          (addwf z))))
                                  ((addc)
                                   (movlw n)
                                   (addwfc z))
                                  ((sub)
                                   (cond ((= n 1)
                                          (decf z))
                                         ((= n #xff)
                                          (incf z))
                                         (else
                                          (movlw n)
                                          (subwf z))))
                                  ((subb)
                                   (movlw n)
                                   (subwfb z))))
                              (let ((x (byte-cell-adr src1))
                                    (y (byte-cell-adr src2))
                                    (z (byte-cell-adr dst)))
                                (cond ((and (not (= x y)) (= y z))
                                       (move-reg x WREG)
                                       (case (instr-id instr)
                                         ((add)
                                          (addwf z))
                                         ((addc)
                                          (addwfc z))
                                         ((sub)
                                          (subwfr z))
                                         ((subb)
                                          (subwfbr z))
                                         (else (error "..."))))
                                      (else
                                       (move-reg x z)
                                       (move-reg y WREG)
                                       (case (instr-id instr)
                                         ((add)
                                          (addwf z))
                                         ((addc)
                                          (addwfc z))
                                         ((sub)
                                          (subwf z))
                                         ((subb)
                                          (subwfb z))
                                         (else (error "..."))))))))
                         ((goto)
                          (let* ((succs (bb-succs bb))
                                 (dest (car succs)))
                            (bra (bb-label dest))
                            (add-todo dest)))
                         ((x==y x<y x>y)
                          (let* ((succs (bb-succs bb))
                                 (dest-true (car succs))
                                 (dest-false (cadr succs)))

                            (define (compare flip adr)
                              (case (instr-id instr)
                                ((x<y) (if flip (cpfsgt adr) (cpfslt adr)))
                                ((x>y) (if flip (cpfslt adr) (cpfsgt adr)))
                                (else (cpfseq adr)))
                              (bra (bb-label dest-false))
                              (bra (bb-label dest-true))
                              (add-todo dest-false)
                              (add-todo dest-true))

                            (cond ((byte-lit? src1)
                                   (let ((n (byte-lit-val src1))
                                         (y (byte-cell-adr src2)))
                                     (if (and (or (= n 0) (= n 1) (= n #xff))
                                              (eq? (instr-id instr) 'x==y))
                                         (special-compare-eq-lit n x)
                                         (begin
                                           (movlw n)
                                           (compare #t y)))))
                                  ((byte-lit? src2)
                                   (let ((x (byte-cell-adr src1))
                                         (n (byte-lit-val src2)))
                                     (if (and (or (= n 0) (= n 1) (= n #xff))
                                              (eq? (instr-id instr) 'x==y))
                                         (special-compare-eq-lit n x)
                                         (begin
                                           (movlw n)
                                           (compare #f x)))))
                                  (else
                                   (let ((x (byte-cell-adr src1))
                                         (y (byte-cell-adr src2)))
                                     (move-reg y WREG)
                                     (compare #f x))))))
                         (else
                          ;...
                          (emit (list (instr-id instr))))))))))

        (if bb
            (begin
              (vector-set! bbs-vector label-num #f)
              (label (bb-label bb))
              (for-each dump-instr (reverse (bb-rev-instrs bb)))
'
              (for-each add-todo (bb-succs bb)))))))

  (let ((prog-label (asm-make-label 'PROG)))
    (rcall prog-label)
    (sleep)
    (label prog-label))

  (add-todo (vector-ref bbs-vector 0))

  (let loop ()
    (if (null? todo)
        (reverse rev-code)
        (let ((bb (car todo)))
          (set! todo (cdr todo))
          (bb-linearize bb)
          (loop)))))

(define (assembler-gen filename cfg)

  (define (gen instr)
    (case (car instr)
      ((movlw)
       (movlw (cadr instr)))
      ((movwf)
       (movwf (cadr instr)))
      ((movfw)
       (movf (cadr instr) 'w))
      ((clrf)
       (clrf (cadr instr)))
      ((setf)
       (setf (cadr instr)))
      ((incf)
       (incf (cadr instr)))
      ((decf)
       (decf (cadr instr)))
      ((addwf)
       (addwf (cadr instr)))
      ((addwfc)
       (addwfc (cadr instr)))
      ((subwf)
       (subwf (cadr instr)))
      ((subwfb)
       (subwfb (cadr instr)))
      ((cpfseq)
       (cpfseq (cadr instr)))
      ((cpfslt)
       (cpfslt (cadr instr)))
      ((cpfsgt)
       (cpfsgt (cadr instr)))
      ((bra)
       (bra (cadr instr)))
      ((rcall)
       (rcall (cadr instr)))
      ((return)
       (return))
      ((label)
       (asm-listing
        (string-append (symbol->string (asm-label-id (cadr instr))) ":"))
       (asm-label (cadr instr)))
      ((sleep)
       (sleep))
      (else
'       (error "unknown instruction" instr))))

  (asm-begin! 0 #f)

;  (pretty-print cfg)

  (let ((code (linearize-and-cleanup cfg)))
;    (pretty-print code)
    (for-each gen code))

  (asm-assemble)

  (display "------------------ GENERATED CODE\n")

  (asm-display-listing (current-output-port))

  (asm-write-hex-file (string-append filename ".hex"))

  (display "------------------ EXECUTION USING SIMULATOR\n")

  (asm-end!)

  (execute-hex-file (string-append filename ".hex")))

(define (code-gen filename cfg)
  (allocate-registers cfg)
  (assembler-gen filename cfg)
;  (pretty-print cfg)
;  (pretty-print (reverse (bb-rev-instrs bb)))
  )

;------------------------------------------------------------------------------

(define (read-source filename)
  (shell-command (string-append "cpp -P " filename " > " filename ".tmp"))
  (with-input-from-file
      (string-append filename ".tmp")
    (lambda ()
      (input-port-readtable-set!
       (current-input-port)
       (readtable-start-syntax-set
        (input-port-readtable (current-input-port))
        'six))
      (read-all))))

(define (main filename)

  (output-port-readtable-set!
   (current-output-port)
   (readtable-sharing-allowed?-set
    (output-port-readtable (current-output-port))
    #t))

  (let ((source (read-source filename)))
    (pretty-print source) ; TODO debug
    (let ((ast (parse source)))
      (pretty-print ast)
      (let ((cfg (generate-cfg ast)))
        (remove-branch-cascades-and-dead-code cfg)
        '(pretty-print cfg)
        (let ((code (code-gen filename cfg)))
          '(pretty-print code))))))
