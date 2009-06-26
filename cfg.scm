;;; generation of control flow graph

;; special variables whose contents are located in the FSR registers
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
  live-before) ; stored as a set

(define (bb-name bb)
  (asm-label-id (bb-label bb)))

(define-type instr
  extender: define-type-of-instr
  (live-before unprintable:) ; these 2 are stored as sets
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
  (make-instr (new-empty-set) (new-empty-set) #f id src1 src2 dst))

;; list of all conditional branching generic instructions
(define conditional-instrs ;; TODO add as we add specialized instructions
  '(x==y x!=y x<y x>y x<=y x>=y))

(define (new-call-instr def-proc)
  (make-call-instr '() '() #f 'call #f #f #f def-proc))

(define (new-return-instr def-proc)
  (make-return-instr '() '() #f 'return #f #f #f def-proc))

(define (add-bb cfg proc id) ;; TODO maybe have the name in the label for named-bbs ? would help debugging
  (let* ((label-num (cfg-next-label-num cfg))
         (bb (make-bb label-num #f #f '() '() '() (new-empty-set))))
    (bb-label-set!
     bb
     (asm-make-label
      (string->symbol
       (string-append "$"
		      (number->string label-num)
		      "$"
		      (if proc (symbol->string proc) "")
		      "$"
                      (if proc (number->string id) "")))))
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

  (define (in x) (set! bb x))

  (define (new-bb)
    (let ((bb (add-bb cfg
		      (if current-def-proc (def-id current-def-proc) #f)
		      current-def-proc-bb-id)))
      (set! current-def-proc-bb-id (+ current-def-proc-bb-id 1))
      bb))

  (define (emit instr) (add-instr bb instr))

  (define current-def-proc #f)
  (define (current-def-proc-id)
    (if current-def-proc
	(def-id current-def-proc)
	#f))
  (define current-def-proc-bb-id 0)
  (define break-stack '())
  (define continue-stack '())
  (define delayed-post-incdec '())

  (define (push-break x) (set! break-stack (cons x break-stack)))
  (define (pop-break)    (set! break-stack (cdr break-stack)))

  (define (push-continue x) (set! continue-stack (cons x continue-stack)))
  (define (pop-continue)    (set! continue-stack (cdr continue-stack)))

  (define (push-delayed-post-incdec ast)
    (set! delayed-post-incdec (cons ast delayed-post-incdec))
    ;; moves the original value to a new location (so it won't be modified)
    ;; and returns that location to the original expression
    (let ((x (subast1 ast)))
      (if (not (ref? x))
	  (error "assignment target must be a variable")
	  (let* ((def-var (ref-def-var x))
		 (result  (alloc-value (def-variable-type def-var) #f (bb-name bb))))
	    (move-value (def-variable-value def-var) result)
	    result))))
  
  ;; TODO instead of carrying types around, use the length instead, or even better, just pass the value-bytes, and calculate the length as needed
  (define (extend value type)
    ;; literals must be extended with literal 0s, while variables must be
    ;; extended with byte cells
    (let* ((bytes (value-bytes value))
	   (lit?  (byte-lit? (car bytes))))
      (let loop ((rev-bytes (reverse bytes))
		 (n         (max 0 (- (type->bytes type) (length bytes)))))
	(if (= n 0)
	    (new-value (reverse rev-bytes))
	    (loop (cons (new-byte-lit 0) ;; TODO used to extend with empty byte cells when it expanded a variable. caused weird bugs.
			rev-bytes)
		  (- n 1))))))
  
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

  (define (def-variable ast) ;; TODO set the fun for each byte-cell
    (let ((subasts (ast-subasts ast)))
      (if (not (null? subasts)) ; if needed, set the variable
          (let ((value (expression (subast1 ast))))
            (let ((ext-value (extend value (def-variable-type ast))))
              (move-value value (def-variable-value ast)))))))

  ;; resolve the C gotos by setting the appropriate successor to their bb
  (define (resolve-all-gotos start table)
    (let loop ((start start)
	       (visited (new-empty-set)))
      (if (not (set-member? visited start)) ; not visited
	  (begin (for-each
		  (lambda (x)
		    (if (and (eq? (instr-id x) 'goto)
			     (instr-dst x)) ; unresolved label
			(let ((target (assoc (instr-dst x) table))) ;; TODO use a set, but not urgent, not a bottleneck
			  (if target
			      (begin (add-succ start (cdr target))
				     (instr-dst-set! x #f))
			      (error "invalid goto target" (instr-dst x))))))
		  (bb-rev-instrs start))
		 (for-each (lambda (x)
			     (set-add! visited start)
			     (loop x visited))
			   (bb-succs start))))))
  
  (define (def-procedure ast) ;; TODO set the fun for the parameters, and maybe also return value
    (set! current-def-proc-bb-id 0)
    (set! current-def-proc ast)
    (let ((old-bb bb)
          (entry (new-bb)))
      (def-procedure-entry-set! ast entry)
      (in entry)
      (for-each statement (ast-subasts ast))
      (return-with-no-new-bb ast)
      (set! current-def-proc #f)
      (resolve-all-gotos entry (list-named-bbs entry))
      (in old-bb)))

  ;; returns a list of all named bbs in the successor-tree of a given bb
  (define (list-named-bbs start)
    (let ((visited (new-empty-set)))
      (let loop ((start start) ;; TODO not really a loop, it's tree recursion
		 (named '()))
	(if (set-member? visited start)
	    named
	    (let ((succs
		   (apply append
			  (map (lambda (bb)
				 (set-add! visited start)
				 (loop bb named))
			       (bb-succs start)))))
	      (if (bb-label-name start)
		  (cons (cons (bb-label-name start) start) succs)
		  succs))))))

  (define (statement ast)
    (cond ((def-variable? ast) (def-variable ast))
          ((block? ast)        (block ast))
          ((return? ast)       (return ast))
          ((if? ast)           (if (null? (cddr (ast-subasts ast)))
				   (if1 ast)
				   (if2 ast)))
          ((while? ast)        (while ast))
          ((do-while? ast)     (do-while ast))
          ((for? ast)          (for ast))
	  ((switch? ast)       (switch ast))
	  ((break? ast)        (break ast))
	  ((continue? ast)     (continue ast))
	  ((goto? ast)         (goto ast))
          (else                (expression ast))))

  (define (block ast)
    (if (block-name ast) ; named block ?
	(begin (let ((new (new-bb)))
		 (gen-goto new)
		 (in new))
	       (bb-label-name-set! bb (block-name ast))))
    (for-each statement (ast-subasts ast)))

  (define (move from to)
    (emit (new-instr 'move from #f to)))

  (define (move-value from to)
    (let loop ((from (value-bytes from))
	       (to   (value-bytes to)))
      (cond ((null? to))  ; done, we truncate the rest
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
      (gen-goto bb-join)
      (in bb-join)))

  (define (if2 ast)
    (let* ((bb-join (new-bb))
           (bb-then (new-bb))
           (bb-else (new-bb)))
      (test-expression (subast1 ast) bb-then bb-else)
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
      (gen-goto bb-body)
      (in bb-body)
      (statement (subast1 ast))
      (gen-goto bb-cont)
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
      (statement (subast3 ast))
      (gen-goto bb-loop)
      (in bb-exit)
      (pop-continue)
      (pop-break)))

  ;; switchs with branch tables
  ;; since offsets are calculated using one byte, switches are limited to
  ;; 60 cases or so (slightly below 64)
  (define (switch ast)
    (let* ((var      (subast1 ast))
	   (entry-bb bb)
	   (exit-bb  (new-bb)))
      (push-break exit-bb)
      (let loop ((asts    (cdr (ast-subasts ast))) ; car is the tested variable
		 (bbs     '())  ; first bb of each case
		 (end-bbs '())  ; last bb of each case
		 (cases   '())) ; case labels
	(if (not (null? asts))
	    (let ((x       (car asts))
		  (case-bb (new-bb)))
	      (in case-bb)
	      (block x)
	      (if (or (null? (bb-succs case-bb))
		      (not (bb-label-name (car (bb-succs case-bb)))))
		  ;; the first block inside the body of a switch might not
		  ;; have a label, in which case it ill be skipped
		  ;; to have a valid CFG, it must still contain an instruction
		  (begin (gen-goto exit-bb)
			 (loop (cdr asts) bbs end-bbs cases))
		  (loop (cdr asts)
			(cons case-bb bbs)
			(cons bb      end-bbs)
			;; blocks create their own bb, which contains the case label
			(cons (bb-label-name (car (bb-succs case-bb)))
			      cases))))
	    (let ((bbs     (reverse bbs))
		  (end-bbs (reverse end-bbs))
		  (cases   (reverse cases))
		  (l       (length  bbs)))
	      ;; add the case names to the bb names
	      (for-each ;; TODO do it for all named bbs, not just switch (but, since the name is on the successor, might be lost)
	       (lambda (bb case)
		 (vector-set!
		  (bb-label (car (bb-succs bb))) 2
		  (string->symbol
		   (string-append (symbol->string (bb-name bb))
				  "$"
				  (if (symbol? case)
				      ;; default
				      (symbol->string case)
				      ;; (case n)
				      (string-append
				       (symbol->string (car case))
				       (number->string (cadr case))))))))
	       bbs
	       cases)
	      ;; handle fall-throughs
	      (for-each
	       (lambda (i)
		 (let ((case-bb (list-ref end-bbs i)))
		   (if (null? (bb-succs case-bb))
		       ;; fall through
		       (begin (in case-bb)
			      (gen-goto (if (= i (- l 1)) ; last bb
					    exit-bb
					    (list-ref bbs (+ i 1))))))))
	       (iota l))
	      (let* ((default   (memq 'default cases)) ;; TODO if default, since we can't know the domain of possible values (at least, not with enough precision for it to be interesting), revert to naive switch
		     ;; cases are lists (case n) and we want the numbers
		     (cases     (map cadr (keep list? cases)))
		     (case-max  (foldl max 0        cases))
		     (case-min  (foldl min case-max cases))
		     (n-entries (+ (- case-max case-min) 1)))
		(if default (error "default is not supported with switch"))
		(in entry-bb)
		(bb-succs-set! bb
			       (map (lambda (i)
				      (cond ((pos-in-list (+ i case-min) cases)
					     => (lambda (j) (list-ref bbs j)))
					    ;; no label, jump to the exit TODO would jump to default, eventually
					    (else exit-bb)))
				    (iota n-entries)))
		;; the branch-table virtual instruction takes the byte to check
		;; to choose the branch
		(emit
		 (new-instr
		  'branch-table
		  ;; the cases now start from 0, so we might have to
		  ;; adjust the checked variable
		  (car (value-bytes
			(expression
			 (if (= case-min 0)
			     var
			     (let* ((op  (operation? '(six.x-y)))
				    (ast (new-oper
					  (list var (new-literal 'int8
								 case-min))
					  #f
					  op)))
			       (expr-type-set! ast ((op-type-rule op) ast))
			       ast)))))
		  ;; working space to calculate addresses
		  (new-byte-cell #f (bb-name bb))
		  #f))))))
      (in exit-bb)
      (pop-break)))

;;   ;; naive switch with if cascade ;; TODO revert to that if there's a case we can't handle with branch tables
;;   (define (switch ast)
;;     (let* ((var (subast1 ast))
;; 	   (case-list #f)
;; 	   (default #f)
;; 	   (decision-bb bb)
;; 	   (exit-bb (new-bb))
;; 	   (prev-bb decision-bb))
;;       (push-break exit-bb)
;;       (for-each (lambda (x) ; generate each case
;; 		  (in (new-bb)) ; this bb will be given the name of the case
;; 		  (add-succ decision-bb bb)
;; 		  ;; if the previous case didn't end in a break, fall through
;; 		  (if (null? (bb-succs prev-bb))
;; 		      (let ((curr bb))
;; 			(in prev-bb)
;; 			(gen-goto curr)
;; 			(in curr)))
;; 		  (statement x)
;; 		  (set! prev-bb bb))
;; 		(cdr (ast-subasts ast)))
;;       (if (null? (bb-succs prev-bb)) ; if the last case didn't end in a break, fall through to the exit
;; 	  (gen-goto exit-bb))
;;       (bb-succs-set! decision-bb (reverse (bb-succs decision-bb))) ; preserving the order is important in the absence of break
;;       (set! case-list (list-named-bbs decision-bb))
;;       (set! default (keep (lambda (x) (eq? (car x) 'default))
;; 			  (list-named-bbs decision-bb)))
;;       (set! case-list (keep (lambda (x) (and (list? (car x))
;; 					     (eq? (caar x) 'case)))
;; 			    case-list))
;;       (bb-succs-set! decision-bb '()) ; now that we have the list of cases we don't need the successors anymore
;;       (let loop ((case-list case-list)
;; 		 (decision-bb decision-bb))
;; 	(in decision-bb)
;; 	(if (not (null? case-list))
;; 	    (let* ((next-bb (new-bb))
;; 		   (curr-case (car case-list))
;; 		   (curr-case-id (cadar curr-case))
;; 		   (curr-case-bb (cdr curr-case)))
;; 	      (emit (new-instr 'x==y
;; 			       (car (value-bytes (expression var)))
;; 			       (new-byte-lit curr-case-id) #f))
;; 	      (add-succ bb next-bb) ; if false, keep looking
;; 	      (add-succ bb curr-case-bb) ; if true, go to the case
;; 	      (loop (cdr case-list)
;; 		    next-bb))
;; 	    (gen-goto (if (not (null? default))
;; 			  (cdar default)
;; 			  exit-bb))))
;;       (in exit-bb)
;;       (pop-break)))

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
    (if (null? (bb-succs bb))
	;; since this is an unconditional goto, we want only one
	(begin (add-succ bb dest)
	       (emit (new-instr 'goto #f #f #f)))))

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
	     ;; since we cons each new successor at the front, true has to be
	     ;; added last
	     (add-succ bb bb-false)
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
	     (emit (new-instr id byte1 byte2 #f)))))

    (define (test-value id value1 value2 bb-true bb-false)
      	 (let loop ((bytes1  (value-bytes value1)) ; lsb first
		    (bytes2  (value-bytes value2))
		    (padded1 '())
		    (padded2 '()))
	   (if (not (and (null? bytes1) (null? bytes2)))
	       ;; note: won't work with signed types, as the padding is done
	       ;; with 0s only
	       (loop (if (null? bytes1) bytes1 (cdr bytes1))
		     (if (null? bytes2) bytes2 (cdr bytes2))
		     (cons (if (null? bytes1) (new-byte-lit 0) (car bytes1))
			   padded1)
		     (cons (if (null? bytes2) (new-byte-lit 0) (car bytes2))
			   padded2))
	       ;; now so the test itself, using the padded values
	       (let ((padded1 (reverse padded1))
		     (padded2 (reverse padded2)))
		 (case id
		   ((x==y) ; unlike < and >, must check all bytes, so is simpler
		    (let loop2 ((bytes1 padded1) ;; TODO ior the xors, but increases PICOBIT's size
				(bytes2 padded2))
		      (let ((byte1 (car bytes1))
			    (byte2 (car bytes2)))
			(if (null? (cdr bytes1))
			    (test-byte 'x==y byte1 byte2 bb-true bb-false)
			    (let ((bb-true2 (new-bb)))
			      (test-byte 'x==y byte1 byte2 bb-true2 bb-false)
			      (in bb-true2)
			      (loop2 (cdr bytes1) (cdr bytes2)))))))
		   
		   (else ; < and >
		    (if (= (length padded1) 1)
			(test-byte id (car padded1) (car padded2)
				   bb-true bb-false)
			;; more than one byte, we subtract, then see if we had
			;; to borrow
			(let ((scratch (new-byte-cell #f (bb-name bb))))
			  
			  ;; our values might contain literal bytes and sub and
			  ;; subb can't have literals in their first argument,
			  ;; allocate it somewhere if needed
			  (if (and (foldl (lambda (acc new)
					    (or acc (byte-lit? new)))
					  #f padded2)
				   (eq? id 'x>y))
			      (let ((tmp (alloc-value
					  (bytes->type (length padded2))
					  #f (bb-name bb))))
				(move-value (new-value padded2) tmp)
				(set! padded2 (value-bytes tmp))))
			  (if (and (foldl (lambda (acc new) ;; TODO abstract both cases
					    (or acc (byte-lit? new)))
					  #f padded1)
				   (eq? id 'x<y))
			      (let ((tmp (alloc-value
					  (bytes->type (length padded1))
					  #f (bb-name bb))))
				(move-value (new-value padded1) tmp)
				(set! padded1 (value-bytes tmp))))
			  
			  (let loop ((bytes1  padded1)
				     (bytes2  padded2)
				     (borrow? #f))
			    (if (not (null? bytes1))
				(let ((b1 (car bytes1))
				      (b2 (car bytes2)))
				  (case id
				    ((x<y)
				     (if (not (and (byte-lit? b2)
						   (= (byte-lit-val b2) 0)))
					 (emit (new-instr
						(if borrow? 'subb 'sub)
						b1 b2 scratch))))
				    ((x>y)
				     (if (not (and (byte-lit? b1)
						   (= (byte-lit-val b1) 0)))
					 (emit (new-instr
						(if borrow? 'subb 'sub)
						b2 b1 scratch)))))
				  (loop (cdr bytes1) (cdr bytes2) #t))))
			  
			  (add-succ bb bb-false)
			  (add-succ bb bb-true)
			  (emit (new-instr 'branch-if-carry scratch #f #f))))))))))
    
    (define (test-relation id x y bb-true bb-false)
      (cond ((and (literal? x) (not (literal? y)))
	     ;; literals must be in the last argument for code generation
	     ;; flip the relation if needed
	     (test-relation (case id
			      ((x==y x!=y) id) ; commutative, no change
			      ((x<y)       'x>y)
			      ((x>y)       'x<y)
			      ((x<=y)      'x>=y)
			      ((x>=y)      'x<=y)
			      (else (error "relation error")))
			    y
			    x
			    bb-true
			    bb-false))
	    ((assq id '((x!=y . x==y) (x<=y . x>y) (x>=y . x<y)))
	     ;; flip the destination blocks to have a simpler comparison
	     =>
	     (lambda (z) (test-relation (cdr z) x y bb-false bb-true)))
	    (else
	     ;; normal case
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
;; 		       (test-negative x bb-true bb-false))
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
	  ;; since nonzero is true, we must swap the destinations to use ==
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
           (cond ((literal? ast) (literal ast))
                 ((ref? ast)     (ref ast))
                 ((oper? ast)    (oper ast))
                 ((call? ast)    (call ast))
                 (else           (error "unexpected ast" ast)))))
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
    (let loop ((bytes1 (value-bytes value1)) ; car is lsb
               (bytes2 (value-bytes value2))
               (bytes3 (value-bytes result))
               (ignore-carry-borrow? #t))
      (if (not (null? bytes3))
	  ;; if we would add or subtract 0 and not use the carry, just move
	  ;; the value
	  (let ((b1 (car bytes1)) (b2 (car bytes2)) (b3 (car bytes3)))
	    (if (and (byte-lit? b2)
		     (= (byte-lit-val b2) 0)
		     (or (eq? id 'add) (eq? id 'sub)))
		(move b1 b3)
		(emit (new-instr (if ignore-carry-borrow?
				     (case id ((x+y) 'add)  ((x-y) 'sub))
				     (case id ((x+y) 'addc) ((x-y) 'subb)))
				 b1 b2 b3)))
	    (loop (cdr bytes1) (cdr bytes2) (cdr bytes3) #f))
	  result)))

  (define (mul value-x value-y type result)
    (let* ((bytes-x (value-bytes value-x))
	   (bytes-y (value-bytes value-y))
	   ;; to determine the length of the operands, we ignore the padding
	   (lx (length (keep (lambda (x) (not (and (byte-lit? x)
						   (= (byte-lit-val x) 0))))
			     bytes-x)))
	   (ly (length (keep (lambda (x) (not (and (byte-lit? x)
						   (= (byte-lit-val x) 0))))
			     bytes-y))))
      ;; if this a multiplication by 2 or 4, we use additions instead
      ;; at this point, only y (or both x and y) can contain a literal
      (if (and (= ly 1)
	       (byte-lit? (car bytes-y))
	       (let ((v (byte-lit-val (car bytes-y))))
		 (or (= v 2) (= v 4))))
	  (case (byte-lit-val (car bytes-y))
	    ((2) (add-sub 'x+y value-x value-x result)) ; simple addition
	    ((4) (let ((tmp (alloc-value (bytes->type
					  (length (value-bytes result)))
					 #f (bb-name bb))))
		   (add-sub 'x+y value-x value-x tmp)
		   (add-sub 'x+y tmp tmp result))))
	  ;; if not, we have to do it the long way
	  (begin
	    ;; finds the appropriate multiplication routine (depending on the
	    ;; length of each argument) and turns the multiplication into a
	    ;; call to the routine
	    ;; the arguments must be the asts of the 2 arguments (x and y) and
	    ;; the type of the returned value, since these are what are
	    ;; expected by the call function

	    ;; to avoid code duplication (i.e. having a routine for 8 by 16
	    ;; multplication and one for 16 by 8), the longest operand goes first
	    (if (> ly lx)
		(let ((tmp1 y)
		      (tmp2 ly))
		  (set! y x)
		  (set! x tmp1)
		  (set! ly lx)
		  (set! lx tmp2)))
	    (routine-call
	     (string->symbol ; mul8_8, mul8_16, etc
	      ;; for now, only unsigned multiplications are supported
	      (string-append "__mul"
			     (number->string (* lx 8)) "_"
			     (number->string (* ly 8))))
	     (list value-x value-y)
	     type)))))

  (define (mod x y result)
    (let* ((bytes1 (value-bytes x))
	   (bytes2 (value-bytes y))
	   (bytes3 (value-bytes result))
	   (y0     (car bytes2)))
      ;; if y is a literal and a power of 2, we can do a bitwise and
      (if (and (byte-lit? y0)
	       (let ((x (/ (log (value->int y)) (log 2))))
		 (= (floor x) x)))
	  ;; bitwise and with y - 1
	  (begin (let* ((l   (bytes->type (length bytes2)))
			(tmp (alloc-value l #f (bb-name bb))))
		   (move-value (int->value (- (value->int y) 1)
					   (bytes->type (length bytes2)))
			       tmp)
		   (bitwise 'x&y x tmp result)))
	  ;; TODO for the general case, try to optimise the case where division and modulo are used together, since they are calculated together
	  (error "modulo is only supported for powers of 2"))
      result))

  (define (shift id value-x value-y type result)
    (let ((bytes1 (value-bytes value-x))
	  (bytes2 (value-bytes value-y))
	  (bytes3 (value-bytes result)))
      ;; if the second argument is a literal and a multiple of 8, we can simply
      ;; move the bytes around
      (let ((y0 (car bytes2)))
	(if (and (byte-lit? y0) (= (modulo (byte-lit-val y0) 8) 0))
	    ;; uses only the first byte, but shifting by 255 should be enough
	    (let ((n (/ (byte-lit-val y0) 8))
		  (l (length bytes1))) ; same length for x and result
	      (let loop ((i 0)
			 (x bytes1))
		(if (< i l)
		    (case id
		      ((x<<y)
		       (move (if (< i n)
				 (new-byte-lit 0) ; padding
				 (car x))
			     (list-ref bytes3 i))
		       (loop (+ i 1) (if (< i n) x (cdr x))))
		      ((x>>y)
		       (move (if (<= l (+ i n))
				 (new-byte-lit 0)
				 (list-ref x (+ i n)))
			     (list-ref bytes3 i))
		       (loop (+ i 1) x)))
		    result)))
	    (routine-call
	     (string->symbol
	      (string-append "__sh"
			     (case id ((x<<y) "l") ((x>>y) "r"))
			     (number->string (* 8 (length bytes1)))))
	     (list value-x value-y)
	     type)))))

  ;; bitwise and, or, xor
  ;; TODO similar to add-sub and probably others, abstract multi-byte ops
  ;; TODO use bit set, clear and toggle for some shortcuts
  (define (bitwise id value1 value2 result)
    (let loop ((bytes1 (value-bytes value1))
               (bytes2 (value-bytes value2))
               (bytes3 (value-bytes result)))
      (if (not (null? bytes3)) ;; TODO check for cases like or 0, or ff, and 0, and ff, ...
	  (begin
	    (emit (new-instr (case id ((x&y) 'and) ((|x\|y|) 'ior) ((x^y) 'xor))
			     (car bytes1) (car bytes2) (car bytes3)))
	    (loop (cdr bytes1) (cdr bytes2) (cdr bytes3)))
	  result)))

  (define (bitwise-negation x result)
    (let loop ((bytes1 (value-bytes x))
	       (bytes2 (value-bytes result)))
      (if (not (null? bytes2))
	  (begin (emit (new-instr 'not (car bytes1) #f (car bytes2)))
		 (loop (cdr bytes1) (cdr bytes2))))))
  
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
	      ;; clobbers the original value, which is fine, since it
	      ;; was moved somewhere else for the expression
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
		;; NB: actual addresses are 12 bits, not 16
		(address (new-value (list (get-register FSR0L)
					  (get-register FSR0H)))))
	    (if index?
		;; we pad up to int16, since it is the size of the addresses
		(let ((value1 (extend base 'int16))
		      (value2 (extend (expression (subast2 ast)) 'int16)))
		  (add-sub 'x+y value1 value2 address))
		;; no offset with simple dereference
		(move-value base address)))
	  (error "You used the array index syntax with a FSR variable, didn't you? I told you not to."))))
  
  (define (array-base-name ast)
    ;; returns #f if the lhs is not a direct variable reference
    ;; eg : *x++ ; (x+y)* ; ...
    (let ((lhs (subast1 ast)))
      (and (ref? lhs)
	   (def-id (ref-def-var lhs)))))

  (define (get-indf base-name)
    ;; INDF0 is not here, since it's already used for regular array accesses
    (if (eq? base-name 'SIXPIC_FSR1)
	(new-value (list (get-register INDF1)))
	(new-value (list (get-register INDF2)))))
  
  (define (oper ast)
    (let* ((type (expr-type ast))
           (op (oper-op ast))
           (id (op-id op)))

      (define (arith-op id x y value-x value-y)
	;; since code generation does not accept literals as first
	;; arguments unless both arguments are, if this is the
	;; case, we either have to swap the arguments (if
	;; possible) or allocate the argument somewhere
	(if (and (literal? x) (not (literal? y)))
	    (if (memq id '(x+y x*y x&y |x\|y| x^y))
		;; the operator is commutative, we can swap the args
		(let ((tmp value-x))
		  (set! value-x value-y)
		  (set! value-y tmp))
		;; the operator is not commutative, we have to
		;; allocate the first argument somewhere
		(let ((dest (alloc-value (expr-type x) #f (bb-name bb))))
		  (move-value value-x dest)
		  (set! value-x dest))))
	(let ((result (alloc-value type #f (bb-name bb))))
	  (case id
	    ((x+y x-y)        (add-sub id value-x value-y result))
	    ((x*y)            (mul value-x value-y type result))
	    ((x/y)            (error "division not implemented yet")) ;; TODO optimize for powers of 2
	    ((x%y)            (mod value-x value-y result))
	    ((x&y |x\|y| x^y) (bitwise id value-x value-y result))
	    ((x>>y x<<y)      (shift id value-x value-y type result)))))
      
      (cond
       ((op1? op)
	(case id
	  ((-x ~x)
	   (let ((x (extend (expression (subast1 ast))
			    type))
		 (result (alloc-value type #f (bb-name bb))))
	     (case id
	       ((-x) (add-sub 'x-y
			      (int->value 0 type)
			      x
			      result))
	       ((~x) (bitwise-negation x result)))
	     result))
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
	     ;; push-delayed-post-incdec moves the original value
	     ;; somewhere else, and returns that location
	     (push-delayed-post-incdec ast)))
	  ((*x)
	   ;; if it's a FSR variable, no adress to set
	   (let ((base-name (array-base-name ast)))
	     (if (and (ref? (subast1 ast)) ; do we have a FSR variable ?
		      base-name
		      (memq base-name fsr-variables))
		 (get-indf base-name)
		 (begin (calculate-address ast)
			(new-value (list (get-register INDF0)))))))
	  (else
	   (error "unary operation error" id))))
       
       ((op2? op)
	(case id
	  ((x+y x-y x*y x/y x%y x&y |x\|y| x^y x>>y x<<y)
	   (let* ((x (subast1 ast))
		  (y (subast2 ast)))
	     (let* ((value-x (extend (expression x) type))
		    (value-y (extend (expression y) type)))
	       (arith-op id x y value-x value-y))))
	  ((x=y)
	   (let* ((x       (subast1 ast))
		  (y       (subast2 ast))
		  (value-y (expression y)))
	     (cond
	      ;; lhs is a variable
	      ((ref? x)
	       (let ((ext-value-y (extend value-y type)))
		 (let ((result (def-variable-value (ref-def-var x))))
		   (move-value value-y result)
		   result)))
	      ;; lhs is a pointer dereference
	      ((and (oper? x) (eq? (op-id (oper-op x)) '*x))
	       (let ((base-name (array-base-name x))
		     (val       (car (value-bytes value-y))))
		 (if (and (ref? (subast1 x))
			  base-name
			  (memq base-name fsr-variables))
		     (move val (car (value-bytes (get-indf base-name))))
		     (begin (calculate-address x)
			    (move val (get-register INDF0))))))
	      ;; lhs is an indexed array access
	      ((and (oper? x) (eq? (op-id (oper-op x)) 'index))
	       ;; note: this will throw an error if SIXPIC_FSR{1,2} is
	       ;; used. this is by design, as it would clobber the value
	       ;; in the FSR registers, which goes against their purpose
	       ;; of storing a user-chosen value
	       (calculate-address x)
	       ;; this section of memory is a byte array, only the lsb
	       ;; of y is used
	       (move (car (value-bytes value-y)) (get-register INDF0)))
	      (else (error "assignment target must be a variable or an array slot")))))
	  ((index)
	   ;; note: throws an error if given SIXPIC_FSR{1,2}, see above
	   (calculate-address ast)
	   (new-value (list (get-register INDF0))))
	  ((x+=y x-=y x*=y x/=y x%=y x&=y |x\|=y| x^=y x>>=y x<<=y)
	   (let* ((x (subast1 ast))
		  (y (subast2 ast))
		  (value-x (extend (expression x) type))
		  (value-y (extend (expression y) type)))
	     (move-value (arith-op (case id
				     ((x+=y)    'x+y)
				     ((x-=y)    'x-y)
				     ((x*=y)    'x*y)
				     ((x/=y)    'x/y)
				     ((x%=y)    'x%y)
				     ((x&=y)    'x&y)
				     ((|x\|=y|) '|x\|y|)
				     ((x^=y)    'x^=y)
				     ((x>>=y)   'x>>y)
				     ((x<<=y)   'x<<y))
				   x y value-x value-y)
			 value-x)
	     value-x))
	  ((x==y x!=y x>y x>=y x<y x<=y x&&y |x\|\|y|)
	   (let ((bb-start bb)
		 (bb-true  (new-bb))
		 (bb-false (new-bb))
		 (bb-join  (new-bb))
		 (result   (alloc-value type #f (bb-name bb))))
	     (in bb-true)
	     (move-value (int->value 1 type) result)
	     (gen-goto bb-join)
	     (in bb-false)
	     (move-value (int->value 0 type) result)
	     (gen-goto bb-join)
	     (in bb-start)
	     (test-expression ast bb-true bb-false)
	     (in bb-join)
	     result))
	  (else
	   (error "binary operation error" id))))
       
       ((op3? op)
	(let ((bb-start bb)
	      (bb-true  (new-bb))
	      (bb-false (new-bb))
	      (bb-join  (new-bb))
	      (result   (alloc-value type #f (bb-name bb))))
	  (in bb-true)
	  (move-value (expression (subast2 ast)) result)
	  (gen-goto bb-join)
	  (in bb-false)
	  (move-value (expression (subast3 ast)) result)
	  (gen-goto bb-join)
	  (in bb-start)
	  (test-expression (subast1 ast) bb-true bb-false)
	  (in bb-join)
	  result)))))
  
  ;; generates the cfg for a predefined routine and adds it to the current cfg
  (define (include-predefined-routine proc)
    (define (get-bytes var)
      (value-bytes (def-variable-value var)))
    (let ((old-proc current-def-proc) ; if we were already defining a procedure, save it
	  (old-bb-no current-def-proc-bb-id)
	  (id (def-id proc))
	  (params (def-procedure-params proc))
	  (value (def-procedure-value proc))
	  (old-bb bb)
          (entry (begin (set! current-def-proc proc)
			(set! current-def-proc-bb-id 0)
			(new-bb)))) ;; TODO insipired from def-procedure, abstract
      (def-procedure-entry-set! proc entry)
      (in entry)
      (case id

	((rom_get)
	 (let* ((x  (get-bytes (car params)))
		(x0 (car  x))
		(x1 (cadr x))
		(z0 (car (value-bytes value))))
	   ;; TODO use postinc/dec and co
	   (emit (new-instr 'tblrd x0 x1 #f))
	   (move (get-register TABLAT) z0)))
	
	((__mul8_8)
	 (let ((x (car params))
	       (y (cadr params))
	       (z (value-bytes value)))
	   ;; TODO implement literal multiplication in the simulator
	   (emit (new-instr 'mul (car (get-bytes x)) (car (get-bytes y)) #f))
	   (move (get-register PRODL) (car z)))) ; lsb
	
	((__mul16_8)
	 (let* ((x  (get-bytes (car params)))
		(x0 (car  x)) ; lsb
		(x1 (cadr x))
		(y  (get-bytes (cadr params)))
		(y0 (car y))
		(z  (value-bytes value))
		(z0 (car  z)) ; lsb
		(z1 (cadr z)))
	   (emit (new-instr 'mul y0 x1 #f))
	   (move (get-register PRODL) z1)

	   (emit (new-instr 'mul y0 x0 #f))
	   (move (get-register PRODL) z0)
	   (emit (new-instr 'add  (get-register PRODH) z1 z1))))

	((__mul16_16)
	 (let* ((x  (get-bytes (car params)))
		(x0 (car  x))
		(x1 (cadr x))
		(y  (get-bytes (cadr params)))
		(y0 (car  y))
		(y1 (cadr y))
		(z  (value-bytes value))
		(z0 (car  z))
		(z1 (cadr z)))

	   (emit (new-instr 'mul x0 y0 #f))
	   (move (get-register PRODH) z1)
	   (move (get-register PRODL) z0)

	   (emit (new-instr 'mul x0 y1 #f))
	   (emit (new-instr 'add  (get-register PRODL) z1 z1))

	   (emit (new-instr 'mul x1 y0 #f))
	   (emit (new-instr 'add  (get-register PRODL) z1 z1))))

	((__mul32_16)
	 (let* ((x  (get-bytes (car params)))
		(x0 (car    x))
		(x1 (cadr   x))
		(x2 (caddr  x))
		(x3 (cadddr x))
		(y  (get-bytes (cadr params)))
		(y0 (car  y))
		(y1 (cadr y))
		(z  (value-bytes value))
		(z0 (car    z))
		(z1 (cadr   z))
		(z2 (caddr  z))
		(z3 (cadddr z)))

	   (emit (new-instr 'mul x0 y0 #f))
	   (move (get-register PRODH) z1)
	   (move (get-register PRODL) z0)

	   (emit (new-instr 'mul x1 y1 #f))
	   (move (get-register PRODH) z3)
	   (move (get-register PRODL) z2)

	   (emit (new-instr 'mul x1 y0 #f))
	   (emit (new-instr 'add  (get-register PRODL) z1 z1))
	   (emit (new-instr 'addc (get-register PRODH) z2 z2))
	   (emit (new-instr 'addc z3     (new-byte-lit 0) z3))

	   (emit (new-instr 'mul x0 y1 #f))
	   (emit (new-instr 'add  (get-register PRODL) z1 z1))
	   (emit (new-instr 'addc (get-register PRODH) z2 z2))
	   (emit (new-instr 'addc z3     (new-byte-lit 0) z3))

	   (emit (new-instr 'mul x2 y0 #f))
	   (emit (new-instr 'add  (get-register PRODL) z2 z2))
	   (emit (new-instr 'addc (get-register PRODH) z3 z3))

	   (emit (new-instr 'mul x2 y1 #f))
	   (emit (new-instr 'add  (get-register PRODL) z3 z3))

	   (emit (new-instr 'mul x3 y0 #f))
	   (emit (new-instr 'add  (get-register PRODL) z3 z3))))

	((__shl8 __shr8 __shl16 __shr16 __shl32 __shr32)
	 (let* ((id (symbol->string id))
		(left-shift? (eq? (string-ref id 4) #\l))
		(x (def-variable-value (car params)))
		(y (def-variable-value (cadr params)))
		(y0 (car (value-bytes y))) ; shift by 255 is enough
		(bytes-z (value-bytes value))
		(start-bb (new-bb))
		(loop-bb  (new-bb))
		(after-bb (new-bb)))
	   (move-value x value)
	   (gen-goto start-bb) ; fall through to the loop
	   (in start-bb)
	   ;; if we'd shift of 0, we're done
	   (add-succ bb loop-bb) ; false
	   (add-succ bb after-bb) ; true
	   (emit (new-instr 'x==y y0 (new-byte-lit 0) #f))
	   (in loop-bb)
	   ;; clear the carry, to avoid reinserting it in the register
	   (emit (new-instr 'clear
			    (get-register STATUS)
			    (new-byte-lit C)
			    #f))
	   ;; shift for each byte, since it's a rotation using the carry,
	   ;; what goes out from the low bytes gets into the high bytes
	   (for-each (lambda (b)
		       (emit (new-instr (if left-shift? 'shl 'shr)
					b #f b)))
		     (if left-shift? bytes-z (reverse bytes-z)))
	   (emit (new-instr 'sub y0 (new-byte-lit 1) y0))
	   (gen-goto start-bb)
	   (in after-bb))))
      (return-with-no-new-bb proc)
      (set! current-def-proc old-proc)
      (set! current-def-proc-bb-id old-bb-no)
      (resolve-all-gotos entry (list-named-bbs entry))
      (in old-bb)))
  
  (define (call ast #!optional (evaluated-args #f))
    ;; note: we might call this with an ast here the arguments are already
    ;; evaluated (when doing a routine call)
    (let* ((def-proc   (call-def-proc ast))
	   (arguments  (ast-subasts ast))
	   (parameters (def-procedure-params def-proc)))
      (if (and (memq (def-id def-proc) predefined-routines)
	       (not (def-procedure-entry def-proc)))
	  ;; it's the first time we encounter this predefined routine, generate
	  ;; the corresponding cfg
	  (include-predefined-routine def-proc))
      ;; argument number check
      (if (not (= (length (if evaluated-args evaluated-args arguments))
		  (length parameters))) ;; TODO check at parse time ?
	  (error (string-append "wrong number of arguments given to function "
				(symbol->string (def-id def-proc)) ": "
				(number->string (length arguments)) " given, "
				(number->string (length parameters))
				" expected")))
      (for-each (lambda (ast def-var)
                  (let ((value (if evaluated-args ast (expression ast))))
                    (let ((ext-value (extend value (def-variable-type def-var))))
                      (move-value value (def-variable-value def-var)))))
                (if evaluated-args evaluated-args arguments)
                parameters)
      (emit (new-call-instr def-proc))
      (let ((value (def-procedure-value def-proc)))
        (let ((result
	       (alloc-value (def-procedure-type def-proc) #f (bb-name bb))))
          (move-value value result)
          result))))

  ;; call to a predefined routine, a simple wrapper to an ordinary call
  ;; name is a symbol, args is a list of the evaluated arguments
  (define (routine-call name args type)
    (cond ((memp (lambda (x) (eq? (def-id x) name))
		 initial-cte)
	   => (lambda (x) (call (new-call '() type (car x)) args)))
	  (else (error "unknown routine: " name))))
  
  (in (new-bb))
  (program ast)
  cfg)

(define (print-cfg-bbs cfg)
  (for-each (lambda (bb)
	      (pp (list "BB:" (bb-name bb)
			"SUCCS" (map bb-name (bb-succs bb))
			"PREDS" (map bb-name (bb-preds bb))
			(cond ((null? (bb-rev-instrs bb)) "EMPTY")
			      ((and (null? (cdr (bb-rev-instrs bb)))
				     (eq? (instr-id (car (bb-rev-instrs bb))) 'goto)) "SINGLE GOTO")
			      (else #f)))))
	    (cfg-bbs cfg)))
