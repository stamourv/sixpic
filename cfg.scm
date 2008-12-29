;;; generation of control flow graph

;; special variables whose contents are located in the FSR registers
;; TODO put here ?
(define fsr-variables '(SIXPIC_FSR0 SIXPIC_FSR1 SIXPIC_FSR2))

(define-type cfg
  bbs
  next-label-num)

(define (new-cfg)
  (make-cfg '() 0))

(define-type bb
  label-num
  label-name ; if the block had a label
  label
  rev-instrs
  unprintable:
  preds
  succs
  live-before)

(define-type instr
  extender: define-type-of-instr
  (live-before unprintable:)
  (live-after unprintable:)
  (hash unprintable:)
  id
  src1
  src2
  dst)

(define-type-of-instr call-instr
  unprintable:
  def-proc)

(define-type-of-instr return-instr
  unprintable:
  def-proc)

(define (new-instr id src1 src2 dst)
  (make-instr '() '() #f id src1 src2 dst))

;; list of all conditional branching generic instructions
(define conditional-instrs ;; TODO add as we add specialized instructions
  '(x==y x!=y x<y x>y x<=y x>=y))

(define (new-call-instr def-proc)
  (make-call-instr '() '() #f 'call #f #f #f def-proc))

(define (new-return-instr def-proc)
  (make-return-instr '() '() #f 'return #f #f #f def-proc))

(define (add-bb cfg)
  (let* ((label-num (cfg-next-label-num cfg))
         (bb (make-bb label-num #f #f '() '() '() '())))
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
      (if (not (null? subasts)) ; if needed, set the variable
          (let ((value (expression (subast1 ast))))
            (let ((ext-value (extend value (def-variable-type ast))))
              (move-value value (def-variable-value ast)))))))

  (define (def-procedure ast)

    ;; resolve the C gotos by setting the appropriate successor to their bb
    (define (resolve-all-gotos start table visited)
      (if (not (memq start visited))
	  (begin (for-each (lambda (x)
			     (if (and (eq? (instr-id x) 'goto)
				      (instr-dst x)) ; unresolved label
				 (let ((target (assoc (instr-dst x) table)))
				   (if target
				       (begin (add-succ start (cdr target))
					      (instr-dst-set! x #f))
				       (error "invalid goto target" (instr-dst x))))))
			   (bb-rev-instrs start))
		 (for-each (lambda (x)
			     (resolve-all-gotos x table (cons start visited)))
			   (bb-succs start)))))
    
    (let ((old-bb bb)
          (entry (new-bb)))
      (def-procedure-entry-set! ast entry)
      (set! current-def-proc ast)
      (in entry)
      (for-each statement (ast-subasts ast))
      (return-with-no-new-bb ast)
      (set! current-def-proc #f)
      (resolve-all-gotos entry (list-named-bbs entry '()) '())
      (in old-bb)))

  ;; returns a list of all named bbs in the successor-tree of a given bb
  (define (list-named-bbs start visited)
    (if (not (memq start visited))
	(let ((succs
	       (apply append
		      (map (lambda (bb) (list-named-bbs bb (cons start visited)))
			   (bb-succs start)))))
	  (if (bb-label-name start)
	      (cons (cons (bb-label-name start) start) succs)
	      succs))
	'()))

  (define (statement ast)
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
	  ((switch? ast)
	   (switch ast))
	  ((break? ast)
	   (break ast))
	  ((continue? ast)
	   (continue ast))
	  ((goto? ast)
	   (goto ast))
          (else
           (expression ast))))

  (define (block ast)
    (if (block-name ast) ; named block ?
	(begin (let ((old-bb bb))
		     (in (new-bb))
		     (add-succ old-bb bb))
	       (bb-label-name-set! bb (block-name ast)) ))
    (for-each statement (ast-subasts ast)))

  (define (move from to)
    (emit (new-instr 'move from #f to)))

  (define (move-value from to)
    (let loop ((from (value-bytes from))
	       (to   (value-bytes to)))
      (cond ((null? to)) ; done
	    ((null? from) ; promote the value by padding
	     (move (new-byte-lit 0) (car to))
	     (loop from (cdr to)))
	    (else
	     (move (car from) (car to))
	     (loop (cdr from) (cdr to))))))
               
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
      (test-expression (subast1 ast) bb-then bb-else) ;; TODO invert ?
      (in bb-then)
      (statement (subast2 ast))
      (gen-goto bb-join)
      (in bb-else)
      (statement (subast3 ast))
      (gen-goto bb-join)
      (in bb-join)))

  (define (while ast)
    (let* ((bb-cont (new-bb))
           (bb-exit (new-bb))
           (bb-body (new-bb)))
      (push-continue bb-cont)
      (push-break bb-exit)
      (gen-goto bb-cont)
      (in bb-cont)
      (test-expression (subast1 ast) bb-body bb-exit)
      (in bb-body)
      (statement (subast2 ast))
      (gen-goto bb-cont)
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
      (gen-goto bb-loop)
      (push-continue bb-cont)
      (push-break bb-exit)
      (in bb-loop)
      (test-expression (subast2 ast) bb-body bb-exit)
      (in bb-body)
      (statement (subast4 ast))
      (gen-goto bb-cont)
      (in bb-cont)
      (expression (subast3 ast))
      (gen-goto bb-loop)
      (in bb-exit)
      (pop-continue)
      (pop-break)))

  (define (switch ast)
    (let* ((var (subast1 ast))
	   (case-list #f)
	   (default #f)
	   (decision-bb bb)
	   (exit-bb (new-bb))
	   (prev-bb decision-bb))
      (push-break exit-bb)
      (for-each (lambda (x) ; generate each case
		  (in (new-bb)) ; this bb will be given the name of the case
		  (add-succ decision-bb bb)
		  (if (null? (bb-succs prev-bb)) ; if the previous case didn't end in a break, fall through
		      (let ((curr bb))
			(in prev-bb)
			(gen-goto curr)
			(in curr)))
		  (statement x)
		  (set! prev-bb bb))
		(cdr (ast-subasts ast)))
      (if (null? (bb-succs prev-bb)) ; if the last case didn't end in a break, fall through to the exit
	  (add-succ prev-bb exit-bb))
      (bb-succs-set! decision-bb (reverse (bb-succs decision-bb))) ; preserving the order is important in the absence of break
      (set! case-list (list-named-bbs decision-bb '()))
      (set! default (keep (lambda (x) (eq? (car x) 'default))
			  (list-named-bbs decision-bb '())))
      (set! case-list (keep (lambda (x) (and (list? (car x))
					     (eq? (caar x) 'case)))
			    case-list))
      (bb-succs-set! decision-bb '()) ; now that we have the list of cases we don't need the successors anymore
      (let loop ((case-list case-list)
		 (decision-bb decision-bb))
	(in decision-bb)
	(if (not (null? case-list))
	    (let* ((next-bb (new-bb))
		   (curr-case (car case-list))
		   (curr-case-id (cadar curr-case))
		   (curr-case-bb (cdr curr-case)))
	      (emit (new-instr 'x==y
			       (car (value-bytes (expression var)))
			       (new-byte-lit curr-case-id) #f)) ;; TODO what about work duplication ?
	      (add-succ bb next-bb) ; if false, keep looking
	      (add-succ bb curr-case-bb) ; if true, go to the case
	      (loop (cdr case-list)
		    next-bb))
	    (gen-goto (if (not (null? default))
			  (cdar default)
			  exit-bb))))
      (in exit-bb)
      (pop-break)))

  (define (break ast)
    (gen-goto (car break-stack)))

  (define (continue ast)
    (gen-goto (car continue-stack)))
  
  ;; generates a goto with a target label. once the current function definition
  ;; is over, all these labels are resolved. therefore, we don't have any gotos
  ;; that jump from a function to another
  (define (goto ast)
    (emit (new-instr 'goto #f #f (subast1 ast))))
  
  (define (gen-goto dest)
    (add-succ bb dest)
    (emit (new-instr 'goto #f #f #f)))

  (define (test-expression ast bb-true bb-false)

    (define (test-byte id byte1 byte2 bb-true bb-false)
      (define (test-lit id x y)
	((case id
	   ((x==y) =)
	   ((x<y) <)
	   ((x>y) >)
	   (else (error "invalid test")))
	 x
	 y))
      (cond ((and (byte-lit? byte1) (byte-lit? byte2))
	     (if (test-lit id (byte-lit-val byte1) (byte-lit-val byte2))
		 (gen-goto bb-true)
		 (gen-goto bb-false)))
	    ((byte-lit? byte2)
	     (add-succ bb bb-false) ; since we cons each new successor at the front, true has to be added last
	     (add-succ bb bb-true)
	     (emit (new-instr id byte1 byte2 #f)))
	    ((byte-lit? byte1)
	     (let ((id
		    (case id
		      ((x==y) 'x==y)
		      ((x<y) 'x>y)
		      ((x>y) 'x<y)
		      (else (error "invalid test")))))
	       (add-succ bb bb-false)
	       (add-succ bb bb-true)
	       (emit (new-instr id byte2 byte1 #f))))
	    (else
	     (add-succ bb bb-false)
	     (add-succ bb bb-true)
	     (emit (new-instr id byte1 byte2 #f))))) ;; TODO doesn't change from if we had literals, at least not now

    (define (test-value id value1 value2 bb-true bb-false)
      ;; note: for multi-byte values, only x==y works properly TODO fix it, will depend on byte order, is car the lsb or msb ?
      (let loop ((bytes1 (value-bytes value1))
		 (bytes2 (value-bytes value2)))
	;; TODO won't work with values of different widths
	(let ((byte1 (car bytes1))
	      (byte2 (car bytes2)))
	  (if (null? (cdr bytes1))
	      (test-byte id byte1 byte2 bb-true bb-false)
	      (let ((bb-true2 (new-bb)))
		(test-byte id byte1 byte2 bb-true2 bb-false)
		(in bb-true2)
		(loop (cdr bytes1) (cdr bytes2)))))))
    
    (define (test-relation id x y bb-true bb-false)
      (cond ((and (literal? x) (not (literal? y))) ; literals must be in the last argument for code generation
	     (test-relation (case id
			      ((x==y x!=y) id)
			      ((x<y) 'x>y)
			      ((x>y) 'x<y)
			      ((x<=y) 'x>=y)
			      ((x>=y) 'x<=y)
			      (else (error "relation error")))
			    y
			    x
			    bb-true
			    bb-false))
	    ((assq id '((x!=y . x==y) (x<=y . x>y) (x>=y . x<y))) ; flip the destination blocks to have a simpler comparison
	     =>
	     (lambda (z) (test-relation (cdr z) x y bb-false bb-true)))
	    (else
;; 	     ' ;; TODO use these special cases, but fall back on the current implementation for default
;; 	     (case id
;; 	       ((x==y)
;; 		(cond ((and (literal? y) (= (literal-val y) 0))
;; 		       (test-zero x bb-true bb-false))
;; 		      ((literal? y)
;; 		       (test-eq-lit x (literal-val y) bb-true bb-false))
;; 		      (else
;; 		       (error "unhandled case"))))
;; 	       ((x<y)
;; 		(cond ((and (literal? y) (= (literal-val y) 0))
;; 		       (test-negative x bb-true bb-false)) ;; TODO does this exist ?
;; 		      (else
;; 		       (error "unhandled case"))))
;; 	       ((x>y)
;; 		(cond ((and (literal? y) (= (literal-val y) 0))
;; 		       (test-positive x bb-true bb-false))
;; 		      (else
;; 		       (error "unhandled case"))))
;; 	       (else
;; 		(error "unexpected operator")))
	     
	     (let* ((value1 (expression x))
		    (value2 (expression y)))
	       (test-value id value1 value2 bb-true bb-false))
	     )))

    (define (test-zero ast bb-true bb-false)

      (define (default)
	(let ((type (expr-type ast))
	      (value (expression ast)))
	  (test-value 'x==y value (int->value 0 type) bb-false bb-true)))
      
      (cond ((oper? ast)
	     (let* ((op (oper-op ast))
		    (id (op-id op)))
	       (case id
		 ((!x)
		  (test-zero (subast1 ast) bb-false bb-true))
		 ((x&&y)
		  (let ((bb-true2 (new-bb)))
		    (test-zero (subast1 ast) bb-true2 bb-false)
		    (in bb-true2)
		    (test-zero (subast2 ast) bb-true bb-false)))
		 ((|x\|\|y|)
		  (let ((bb-false2 (new-bb)))
		    (test-zero (subast1 ast) bb-true bb-false2)
		    (in bb-false2)
		    (test-zero (subast2 ast) bb-true bb-false)))
		 ((x==y x!=y x<y x>y x<=y x>=y)
		  (test-relation id
				 (subast1 ast)
				 (subast2 ast)
				 bb-true
				 bb-false))
		 (else (default)))))
	    (else (default))))

    (test-zero ast bb-true bb-false))

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
      (if (not (null? bytes3))
	  (begin (emit
		  (new-instr (if ignore-carry-borrow?
				 (case id ((x+y) 'add) ((x-y) 'sub))
				 (case id ((x+y) 'addc) ((x-y) 'subb)))
			     (if (null? bytes1) (new-byte-lit 0) (car bytes1))
			     (if (null? bytes2) (new-byte-lit 0) (car bytes2))
			     (car bytes3)))
		 (loop (if (null? bytes1) bytes1 (cdr bytes1))
		       (if (null? bytes2) bytes2 (cdr bytes2))
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

  ;; calculates an address in an array by adding the base pointer and the offset
  ;; and puts the answer in FSR0 so that changes to INDF0 change the array
  ;; location
  (define (calculate-address ast)
    ;; if we have a special FSR variable, no need to calculate the address as
    ;; it is already in the register
    (let ((base-name (array-base-name ast))
	  (index? (eq? (op-id (oper-op ast)) 'index)))
      (if (not (and base-name
		    (memq base-name fsr-variables)))
	  (let ((base    (expression (subast1 ast)))
		(address (new-value (list (get-register FSR0L)
					  (get-register FSR0H))))) ;; TODO actual addresses are 12 bits, not 16
	  (if index?
	      (add-sub 'x+y base (expression (subast2 ast)) address) ;; TODO offset is not seen
	      (move-value base address)))))) ; no offset with simple dereference
  
  (define (array-base-name ast)
    ;; returns #f if the lhs is not a direct variable reference
    ;; ex : *x++ ; (x+y)* ; ...
    (let ((lhs (subast1 ast)))
      (and (ref? lhs)
	   (def-id (ref-def-var lhs)))))
  
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
                ((x++ x--) ;; TODO not sure this works properly
                 (let ((x (subast1 ast)))
                   (if (not (ref? x))
                       (error "assignment target must be a variable"))
                   (let ((result (def-variable-value (ref-def-var x))))
                     (push-delayed-post-incdec ast)
                     result)))
		((*x)
		 ;; if it's a FSR variable, no adress to set
		 (let ((base-name (array-base-name ast)))
		   (if (and (ref? (subast1 ast)) ; do we have a FSR variable ?
			    base-name
			    (memq base-name fsr-variables))
		       (if (eq? base-name 'SIXPIC_FSR1) ;; TODO ugly, fix this
			   (new-value (list (get-register INDF1)))
			   (new-value (list (get-register INDF2))))
		       (begin (calculate-address ast)
			      (new-value (list (get-register INDF0)))))))
		;; TODO merge with setter, and merge both setters
                (else
                 (error "unary operation error" ast))))
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
                         (cond ((or (eq? id 'x+y)
				    (eq? id 'x-y))
				(add-sub id ext-value-x ext-value-y result))
			       ((eq? id 'x*y)
				(error "multiplication not implemented yet")) ;; TODO maybe just implement multiplication by powers of 2
			       ((eq? id 'x/y)
				(error "division not implemented yet")) ;; TODO implement these
			       ((eq? id 'x%y)
				(error "modulo not implemented yet")))
                         result)))))
                ((x=y)
                 (let* ((x       (subast1 ast))
                        (y       (subast2 ast))
			(value-y (expression y)))
		   (cond
		    ((ref? x)
		     (let ((ext-value-y (extend value-y type))) ;; TODO useless for now, what could it have been for ?
		       (let ((result (def-variable-value (ref-def-var x))))
			 (move-value value-y result)
			 result)))
		    ((and (oper? x) (eq? (op-id (oper-op x)) '*x))
		     (let ((base-name (array-base-name x)))
		       (if (and (ref? (subast1 x))
				base-name
				(memq base-name fsr-variables))
			   (if (eq? base-name 'SIXPIC_FSR1) ;; TODO ugly, fix this
			       (move (car (value-bytes value-y)) (get-register INDF1))
			       (move (car (value-bytes value-y)) (get-register INDF2)))
			   (begin (calculate-address x)
				  (move (car (value-bytes value-y)) (get-register INDF0))))))
		    ((and (oper? x) (eq? (op-id (oper-op x)) 'index))
		     (calculate-address x) ;; TODO as with references, won't work with SIXPIC_FSR1 or SIXPIC_FSR2
		     ;; this section of memory is a byte array, only the lsb
		     ;; of y is used
		     (move (car (value-bytes value-y)) (get-register INDF0)))
		    (else (error "assignment target must be a variable or an array slot")))))
		((index) ; TODO FSR variables won't work with index syntax, the contents of FSR0 will always be fetched, even if SIXPIC_FSR1 is used, for now, this is acceptable, the other syntax should be used instead
		 (calculate-address ast)
		 (new-value (list (get-register INDF0))))
                (else
                 (error "binary operation error" ast))))))))
  
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

  ;; remplaces empty bbs by bbs with a single goto, to have a valid CFG for optimizations
  (define (fill-empty-bbs)
    (for-each (lambda (x) (if (null? (bb-rev-instrs x))
			       (begin (in x)
				      (emit (new-instr 'goto #f #f #f)))))
	      (cfg-bbs cfg)))
  
  (in (new-bb))
  (program ast)
  (fill-empty-bbs)
  cfg)

(define (print-cfg-bbs cfg)
  (for-each (lambda (bb)
	      (pp (list "BB:" (bb-label-num bb)
			"SUCCS" (map bb-label-num (bb-succs bb))
			"PREDS" (map bb-label-num (bb-preds bb))
			(cond ((null? (bb-rev-instrs bb)) "EMPTY")
			      ((and (null? (cdr (bb-rev-instrs bb)))
				     (eq? (instr-id (car (bb-rev-instrs bb))) 'goto)) "SINGLE GOTO")
			      (else #f)))))
	    (cfg-bbs cfg)))
