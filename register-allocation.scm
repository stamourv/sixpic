;; address after which memory is allocated by the user, therefore not used for
;; register allocation
;; in programs, located in the SIXPIC_MEMORY_DIVIDE variable
(define memory-divide #f)

;; the vector equivalent to all-byte-cells
(define byte-cells #f)
(define (id->byte-cell id) (vector-ref byte-cells id))

(define (interference-graph cfg)

  (define (analyze-liveness cfg)
    (define changed? #t)
    (define (instr-analyze-liveness instr live-after)
      (let ((live-before
	     (cond

	      ((call-instr? instr)
	       (let ((def-proc (call-instr-def-proc instr)))
		 (if (and (not (bitset-empty? live-after))
			  (not (bitset-subset?
				(def-procedure-live-after-calls def-proc)
				live-after)))
		     (begin
		       (set! changed? #t)
		       (bitset-union! (def-procedure-live-after-calls def-proc)
				      live-after)))
		 (let ((live
			(bitset-union-multi
			 byte-cell-counter
			 `(,(bitset-diff
			     live-after
			     (list->bitset
			      byte-cell-counter
			      (map (lambda (x) (byte-cell-id x))
				   (value-bytes
				    (def-procedure-value def-proc)))))
			   ,(bb-live-before (def-procedure-entry def-proc))
			   ,@(map (lambda (def-var)
				    (list->bitset
				     byte-cell-counter
				     (map (lambda (x) (byte-cell-id x))
					  (value-bytes
					   (def-variable-value def-var)))))
				  (def-procedure-params def-proc))))))
		   (if (bb? (def-procedure-entry def-proc))
		       (bitset-intersection
			(bb-live-before (def-procedure-entry def-proc))
			live)
		       live))))
	      
	      ((return-instr? instr)
	       (let* ((def-proc (return-instr-def-proc instr))
		      (live
		       (if (def-procedure? def-proc)
			   (def-procedure-live-after-calls def-proc)
			   (list->bitset byte-cell-counter
					 (map (lambda (x) (byte-cell-id x))
					      (value-bytes def-proc))))))
		 (set! live-after live)
		 live))
	      
	      (else
	       (let* ((src1 (instr-src1 instr))
		      (src2 (instr-src2 instr))
		      (dst  (instr-dst instr))
		      (s    (bitset-copy live-after)))
		 (define (add-if-byte-cell c)
		   (if (byte-cell? c)
		       (bitset-add! s (byte-cell-id c))))
		 (define (remove-if-byte-cell c)
		   (if (byte-cell? c)
		       (bitset-remove! s (byte-cell-id c))))
		 (add-if-byte-cell src1)
		 (add-if-byte-cell src2)
		 (remove-if-byte-cell dst)
		 s)))))
	
	(instr-live-before-set! instr live-before)
	(instr-live-after-set!  instr live-after)
	live-before))
    (define (bb-analyze-liveness bb)
      (let loop ((rev-instrs (bb-rev-instrs bb))
		 (live-after (bitset-union-multi
			      byte-cell-counter
			      (map bb-live-before (bb-succs bb)))))
	(if (null? rev-instrs)
	    (if (not (equal? live-after (bb-live-before bb)))
		(begin (set! changed? #t)
		       (bb-live-before-set! bb live-after)))
	    (let* ((instr (car rev-instrs))
		   (live-before (instr-analyze-liveness instr live-after)))
	      (loop (cdr rev-instrs)
		    live-before)))))

    ;; build a vector with all the byte-cells and initialise the bitsets
    (set! byte-cells (make-vector byte-cell-counter #f))
    (table-for-each (lambda (id cell)
		      (byte-cell-interferes-with-set!
		       cell (make-bitset byte-cell-counter))
		      (vector-set! byte-cells id cell))
		    all-byte-cells)
    ;; create the bitsets for each bb, instr and def-procedure
    (for-each (lambda (bb)
		(bb-live-before-set! bb (make-bitset byte-cell-counter))
		(for-each (lambda (instr)
			    (instr-live-before-set!
			     instr (make-bitset byte-cell-counter))
			    (instr-live-after-set!
			     instr (make-bitset byte-cell-counter)))
			  (bb-rev-instrs bb)))
	      (cfg-bbs cfg))
    (table-for-each (lambda (key s) (def-procedure-live-after-calls-set!
				      s (make-bitset byte-cell-counter)))
		  all-def-procedures)
    
    (let loop ()
      (if changed?
	  (begin (set! changed? #f)
		 (for-each bb-analyze-liveness (cfg-bbs cfg))
		 (loop)))))

;;-----------------------------------------------------------------------------
  
  (define all-live (new-empty-set))
  
  (define (bb-interference-graph bb)
    (define (interfere x y)
      (bitset-add! (byte-cell-interferes-with x) (byte-cell-id y))
      (bitset-add! (byte-cell-interferes-with y) (byte-cell-id x)))
    (define (make-coalesceable-with x y)
      (set-add! (byte-cell-coalesceable-with x) y)
      (set-add! (byte-cell-coalesceable-with y) x))
    
    (define (interfere-pairwise live)
      (set-union! all-live (list->set live))
      (for-each
       (lambda (x)
	 (for-each
	  (lambda (y)
	    (if (not (eq? x y))
		(bitset-add! (byte-cell-interferes-with x) (byte-cell-id y))))
	  live))
       live))
    
    (define (instr-interference-graph instr)
      (define (bitset->cells bs) (map id->byte-cell (bitset->list bs)))
      (let ((dst  (instr-dst  instr))
	    (src1 (instr-src1 instr))
	    (src2 (instr-src2 instr)))
	(if (byte-cell? dst)
	    (begin
	      (if (and (byte-cell? src1) (not (eq? dst src1)))
		  (make-coalesceable-with src1 dst))
	      (if (and (byte-cell? src2) (not (eq? dst src2)))
		  (make-coalesceable-with src2 dst))))
	(if (call-instr? instr)
	    (let* ((before (instr-live-before instr))
		   (after  (instr-live-after  instr))
		   (diff   (bitset->cells (bitset-diff before after)))
		   (diff2  (bitset-diff after before)))
	      (interfere-pairwise diff)
	      (for-each
	       (lambda (x)
		 (for-each
		  (lambda (y)
		    (if (and (not (eq? x y))
			     (not (bitset-member? diff2 (byte-cell-id y))))
			(interfere x y)))
		  (bitset->cells after)))
	       diff))
	    (if (byte-cell? dst)
		(begin (set-add! all-live dst)
		       (for-each (lambda (x)
				   (set-add! all-live x)
				   (if (not (eq? dst x))
				       (interfere dst x)))
				 (bitset->cells (instr-live-after instr))))))))

    (for-each instr-interference-graph (bb-rev-instrs bb)))
    
  (pp analyse-liveness:)
  (time (analyze-liveness cfg))

  (pp interference-graph:)
  (time (for-each bb-interference-graph (cfg-bbs cfg)))

  ;; change the bitsets to sets, to speed up graph coloring
  (pp bitsets->sets:)
  (time
   (let loop ((l (- (vector-length byte-cells) 1)))
     (if (not (< l 0))
	 (let* ((cell (id->byte-cell l)))
	   (if cell
	       (byte-cell-interferes-with-set!
		cell
		(let* ((bs  (byte-cell-interferes-with cell))
		       (n   (fxarithmetic-shift-left (u8vector-length bs) 3))
		       (set (new-empty-set)))
		  (let loop ((i (- n 1)))
		    (if (>= i 0)
			(begin (if (bitset-member? bs i)
				   (set-add! set (id->byte-cell i)))
			       (loop (- i 1)))
			set)))))
	   (loop (- l 1))))))
  
  all-live)

;;-----------------------------------------------------------------------------

(define (delete byte-cell1 neighbours)
  (set-for-each (lambda (byte-cell2)
		  (set-remove! (byte-cell-interferes-with byte-cell2)
			       byte-cell1)
		  (byte-cell-nb-neighbours-set!
		   byte-cell2 (- (byte-cell-nb-neighbours byte-cell2) 1)))
 		neighbours))
(define (undelete byte-cell1 neighbours)
  (set-for-each (lambda (byte-cell2)
 		  (set-add! (byte-cell-interferes-with byte-cell2)
 			    byte-cell1)
		  (byte-cell-nb-neighbours-set!
		   byte-cell2 (+ (byte-cell-nb-neighbours byte-cell2) 1)))
 		neighbours))

(define (coalesce graph)
  (pp coalesce:)
  (time
   (if coalesce?
       (keep
	(lambda (byte-cell)
	  (let* ((coalesceable-with   (byte-cell-coalesceable-with byte-cell))
		 (neighbours      (byte-cell-interferes-with   byte-cell))
		 (coalesce-candidates (set-diff coalesceable-with
						neighbours)))
	    (if (or (byte-cell-adr byte-cell) ; in a special register
		    (set-empty? coalesce-candidates))
		#t ;; keep it
		;; coalesce byte-cell with another cell
		(let loop ((l (set->list coalesce-candidates)))
		  (if (null? l)
		      #t ; can't coalesce, keep
		      (let ((c (car l)))
			;; don't coalesce with a special register
			(if (byte-cell-adr c)
			    (loop (cdr l))
			    (let ((c-neighbours
				   (byte-cell-interferes-with c))
				  (c-coalesceable-with
				   (byte-cell-coalesceable-with c))
				  (c-coalesced
				   (byte-cell-coalesced-with  c)))
			      ;; remove all references to byte-cell and replace
			      ;; them with references to c
			      (set-union! c-neighbours neighbours)
			      (undelete   c            neighbours)
			      (delete     byte-cell    neighbours)
			      (set-union! c-coalesced
					  (byte-cell-coalesced-with byte-cell))
			      (set-for-each
			       (lambda (cell)
				 (let ((s (byte-cell-coalesceable-with cell)))
				   (set-remove! s byte-cell)
				   #;(set-add! s                   c) ;; FOO attempt (failed)
				   #;(set-add! c-coalesceable-with cell)))
			       coalesceable-with)
			      (byte-cell-coalesceable-with-set! byte-cell
								(new-empty-set))
			      (byte-cell-interferes-with-set!   byte-cell
								(new-empty-set))
			      (set-add! c-coalesced byte-cell)
			      #f))))))))
	graph)
       graph)))

;;-----------------------------------------------------------------------------

(define register-table         (make-table))
(define reverse-register-table (make-table))
(define (allocate-registers cfg)
  (let ((all-live (coalesce (set->list (interference-graph cfg))))
	(max-adr  0)) ; to know how much ram we need

    (define (color byte-cell)
      (define (set-register-table cell adr)
	(if #f (not (string=? (byte-cell-name cell) "__tmp"))
	    (let* ((adr       (if (and (> adr #x5F) (< adr #xF60)) ; not in bank 0 ;; TODO have a function for that
				  (+ adr #xa0)
				  adr))
		   (name      (byte-cell-name cell))
		   (full-name (cons (if name name "__tmp")
				    (byte-cell-id cell))))
	      (table-set! register-table
			  adr
			  (cons (cons (byte-cell-bb   cell)
				      full-name)
				(table-ref register-table adr '())))
	      (table-set! reverse-register-table name adr))))
      (let ((neighbours (set->list (byte-cell-interferes-with byte-cell))))
	(let loop1 ((adr 0))
	  (if (and memory-divide ; the user wants his own zone
		   (>= adr memory-divide)) ; and we'd use it
	      (error "register allocation would cross the memory divide") ;; TODO fallback ?
	      (let loop2 ((lst neighbours)) ;; TODO keep using sets, but not urgent, it's not a bottleneck
		(if (null? lst)
		    (begin (byte-cell-adr-set! byte-cell adr)
			   (set-register-table byte-cell adr)
			   (set-for-each
			    (lambda (cell)
			      (byte-cell-adr-set! cell adr)
			      (set-register-table cell adr))
			    (byte-cell-coalesced-with byte-cell)))
		    (if (= adr (byte-cell-adr (car lst)))
			(loop1 (+ adr 1))
			(loop2 (cdr lst))))))
	  (set! max-adr (max max-adr adr)))))
    
    (define (find-min-neighbours graph)
      (let loop ((lst graph) (m #f) (byte-cell #f))
        (if (null? lst)
            byte-cell
            (let* ((x (car lst))
                   (n (byte-cell-nb-neighbours x)))
              (if (or (not m) (< n m))
                  (loop (cdr lst) n x)
                  (loop (cdr lst) m byte-cell))))))

    (define (alloc-reg graph)
      (if (not (null? graph))
          (let* ((byte-cell  (find-min-neighbours graph))
                 (neighbours (byte-cell-interferes-with byte-cell)))
            (let ((new-graph (remove byte-cell graph)))
              (delete byte-cell neighbours)
              (alloc-reg new-graph)
              (undelete byte-cell neighbours))
            (if (not (byte-cell-adr byte-cell))
                (color byte-cell)))))

    ;; cache the number of neighbours
    (for-each (lambda (cell)
		(byte-cell-nb-neighbours-set!
		 cell (set-length (byte-cell-interferes-with cell))))
	      all-live)
    
    (pp register-allocation:)
    (time (alloc-reg all-live))
    (display (string-append (number->string (+ max-adr 1)) " RAM bytes\n"))))
