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
      
      ;; variables which, when found in programs, have special meanings
      ;; when any of these are encountered, its associated thunk is
      ;; called
      (define special-variables
	(list ;; TODO fit with the predefined variables in cte.scm ?
	 (cons 'SIXPIC_MEMORY_DIVIDE
	       (lambda ()
		 (set! memory-divide (cadr val)) ; must be a literal
		 (expression val cte (lambda (ast cte)
				       (def (list ast) cte)))))))


      (define (def asts cte)
        (let* ((value
                (alloc-value type id))
               (ast
                (new-def-variable asts id '() type value '()))
               (cte
                (cte-extend cte (list ast))))
          (cont ast
                cte)))
      ;; if it's a special variable, call its associated thunk instead
      (let ((target (assq id special-variables)))
	(if target
	    ((cdr target))
	    (if val
		(expression val cte (lambda (ast cte) (def (list ast) cte)))
		(def '() cte))))))

  (define (define-procedure source cte cont)
    (let* ((id (get-id (cadr source)))
           (proc (caddr source)))
      (expect-form 'six.procedure proc)
      (let* ((type
              (cadr proc))
             (params
              (map (lambda (x)
                     (let* ((id    (get-id (car x)))
			    (type  (cadr x))
                            (value (alloc-value type id)))
                       (new-def-variable '() id '() type value '())))
                   (caddr proc)))
             (body
              (cadddr proc)))
        (expect-form 'six.procedure-body body)
        (let* ((value
                (alloc-value type id))
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
      (if (null? source)
	  (cont '() cte)
	  (let ((head (car source))
		(tail (cdr source)))
	    (if (or (form? 'six.label head) ; we complete the block with a list of named blocks
		    (form? 'six.case  head))
		(named-block-list source
				  cte
				  cont) ; will return a list of named blocks
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

  ;; handles named blocks (implicit blocks delimited by labels)
  ;; useful for switch and goto
  (define (named-block-list source cte cont)
    (define (b source cte cont name body-so-far)
      (if (null? source)
	  (cont (list (new-named-block name body-so-far)) ; last block
		cte)
	  (let ((curr (car source)))
	    (if (or (form? 'six.label curr) ; we reached another named block
		    (form? 'six.case  curr))
		(named-block-list source
				  cte
				  (lambda (named-blocks cte)
				    (cont (cons (new-named-block name body-so-far)
						named-blocks)
					  cte)))
		(statement curr
			   cte
			   (lambda (ast cte)
			     (b (cdr source)
				cte
				cont
				name
				(append body-so-far (list ast)))))))))
    (let ((new-cont
	   (lambda (name cte)
	     (statement (caddar source)
			cte
			(lambda (ast cte)
			  (b (cdr source)
			     cte
			     cont
			     name
			     ;; the first statement is in the case/label form
			     (list ast)))))))
      
      (if (form? 'six.case (car source)) ; the label is a case
	  (literal (cadar source)
		   cte
		   (lambda (name cte)
		     (new-cont (list 'case (literal-val name)) cte)))
	  (new-cont (cadar source) cte)))) ; ordinary label
  
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
	  ((form? 'six.break source)
	   (break source cte cont))
	  ((form? 'six.continue source)
	   (continue source cte cont))
	  ((form? 'six.goto source)
	   (goto source cte cont))
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

  (define (break source cte cont)
    (cont (new-break)
	  cte))

  (define (continue source cte cont)
    (cont (new-continue)
	  cte))

  (define (goto source cte cont)
    (cont (new-goto (cadadr source)) ; label
	  cte))

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

  (define (switch source cte cont)
    (expression (cadr source)
		cte
		(lambda (ast1 cte) ; we matched the paren expr		  
		  (expect-form 'six.compound (caddr source))
		  (block (caddr source)
			 cte
			 (lambda (ast2 cte)
			   (cont (new-switch (cons ast1 (ast-subasts ast2)))
				 cte))))))
  
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
                                                                       (new-literal 'byte 1))
                                                                   (or ast3
                                                                       (new-block '()))
                                                                   ast4))
                                                            cte))))))))))

  (define (expression source cte cont)
    (cond ((form? 'six.literal source)
           (literal source cte cont))
	  ((form? 'six.prefix source)
	   ;; this is a hack to support hexadecimal values
	   ;; SIX does not parse C-style hex values (0x...), but falls back on
	   ;; the regular parser when it encounters Scheme-style hex values
	   ;; (#x...) and wraps them in a six.prefix.
	   ;; TODO support C-style hex values
	   (literal `(six.literal ,(cadr source)) cte cont))
          ((form? 'six.identifier source)
           (ref source cte cont))
          ((form? 'six.call source)
           (call source cte cont))
          ((operation? source)
           =>
           (lambda (op)
             (operation op source cte cont)))
          (else
           (error "expected expression" source))))

  (define (operation op source cte cont)
    (define (continue ast cte)
      (expr-type-set! ast ((op-type-rule op) ast))
      (cont ((op-constant-fold op) ast)
	    cte))
    (cond ((op1? op)
	   (expression
	    (cadr source)
	    cte
	    (lambda (ast1 cte)
	      (continue (new-oper (list ast1) #f op) cte))))
	  ((op2? op)
	   (expression
	    (cadr source)
	    cte
	    (lambda (ast1 cte)
	      (expression
	       (caddr source)
	       cte
	       (lambda (ast2 cte)
		 (continue (new-oper (list ast1 ast2) #f op) cte))))))
	  (else ; ternary operator
	   (expression
	    (cadr source)
	    cte
	    (lambda (ast1 cte)
	      (expression
	       (caddr source)
	       cte
	       (lambda (ast2 cte)
		 (expression
		  (cadddr source)
		  cte
		  (lambda (ast3 cte)
		    (continue (new-oper (list ast1 ast2 ast3) #f op)
			      cte))))))))))

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
    (let ((n (cadr source)))
      (cont (new-literal (val->type n) n)
	    cte)))

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
           initial-cte
           (lambda (ast cte)
             ast)))
