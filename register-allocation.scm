;; address after which memory is allocated by the user, therefore not used for
;; register allocation
;; in programs, located in the SIXPIC_MEMORY_DIVIDE variable
(define memory-divide #f)

(define (interference-graph cfg)

  (define (analyze-liveness cfg)
    (define changed? #t)
    (define (instr-analyze-liveness instr live-after)
      (let ((live-before
	     (cond

	      ((call-instr? instr)
	       (let ((def-proc (call-instr-def-proc instr)))
		 (if (and (not (set-empty? live-after))
			  (not (set-equal?
				(set-intersection
				 (def-procedure-live-after-calls def-proc)
				 live-after)
				live-after)))
		     (begin
		       (set! changed? #t)
		       (set-union! (def-procedure-live-after-calls def-proc)
				   live-after)))
		 (let ((live
			(set-union
			 (set-union-multi
			  (map (lambda (def-var)
				 (list->set
				  (value-bytes (def-variable-value def-var))))
			       (def-procedure-params def-proc)))
			 (set-union
			  (set-diff
			   live-after
			   (list->set
			    (value-bytes (def-procedure-value def-proc))))
			  (bb-live-before (def-procedure-entry def-proc)))))) ;; FOO now the liveness analysis takes a whole minute
		   (if (bb? (def-procedure-entry def-proc))
		       (set-intersection ;; TODO disabling this branch saves around 12 bytes
			(bb-live-before (def-procedure-entry def-proc))
			live)
		       live))))
	      
	      ((return-instr? instr)
	       (let* ((def-proc (return-instr-def-proc instr))
		      (live
		       (if (def-procedure? def-proc)
			   (def-procedure-live-after-calls def-proc)
			   (list->set (value-bytes def-proc)))))
		 (set! live-after live)
		 live))
	      
	      (else
	       (let* ((src1 (instr-src1 instr))
		      (src2 (instr-src2 instr))
		      (dst  (instr-dst instr))
		      (use  (if (byte-cell? src1)
				(if (byte-cell? src2)
				    (set-add (new-set src1) src2)
				    (new-set src1))
				(if (byte-cell? src2)
				    (new-set src2)
				    (new-empty-set))))
		      (def  (if (byte-cell? dst)
				(new-set dst)
				(new-empty-set))))
		 (if #f
		     ;; (and (byte-cell? dst) ; dead instruction?
		     ;;      (not (set-member? live-after dst))
		     ;;      (not (and (byte-cell? dst) (byte-cell-adr dst))))
		     live-after
		     (set-union use
				(set-diff live-after def))))))))
	
	(instr-live-before-set! instr live-before)
	(instr-live-after-set!  instr live-after)
	live-before))
    (define (bb-analyze-liveness bb)
      (let loop ((rev-instrs (bb-rev-instrs bb))
		 (live-after (set-union-multi
			      (map bb-live-before (bb-succs bb)))))
	(if (null? rev-instrs)
	    (if (not (set-equal? live-after (bb-live-before bb)))
		(begin (set! changed? #t)
		       (bb-live-before-set! bb live-after)))
	    (let* ((instr (car rev-instrs))
		   (live-before (instr-analyze-liveness instr live-after)))
	      (loop (cdr rev-instrs)
		    live-before)))))
    (let loop ()
      (if changed?
	  (begin (set! changed? #f)
		 (for-each bb-analyze-liveness (cfg-bbs cfg))
		 (loop)))))

;;-----------------------------------------------------------------------------
  
  (define all-live (new-empty-set))
  
  (define (bb-interference-graph bb)
    
    (define (interfere x y)
      (if (not (set-member? (byte-cell-interferes-with y) x)) ;; TODO is this check worth it ?
	  (begin (set-add!  (byte-cell-interferes-with x) y)
		 (set-add!  (byte-cell-interferes-with y) x))))
    
    (define (interfere-pairwise live) ;; FOO now takes 10 minutes, find a better way
      (set-union! all-live live)
      (set-for-each (lambda (x)
		      (set-for-each (lambda (y)
				      (if (not (eq? x y)) (interfere x y)))
				    live))
		    live))
    
    (define (instr-interference-graph instr)
      (let ((dst (instr-dst instr)))
	(if (byte-cell? dst)
	    (let ((src1 (instr-src1 instr))
		  (src2 (instr-src2 instr)))
	      (if (and (byte-cell? src1) (not (eq? dst src1)))
		  (begin (set-add! (byte-cell-coalesceable-with dst) src1)
			 (set-add! (byte-cell-coalesceable-with src1) dst)))
	      (if (and (byte-cell? src2) (not (eq? dst src2)))
		  (begin (set-add! (byte-cell-coalesceable-with dst) src2)
			 (set-add! (byte-cell-coalesceable-with src2) dst)))
	      (interfere-pairwise (set-add (instr-live-after instr) dst)))))
      (interfere-pairwise (instr-live-before instr)))
    
    (for-each instr-interference-graph (bb-rev-instrs bb)))

  (pp analyse-liveness:)
  (time (analyze-liveness cfg))

  (pp interference-graph:)
  (time (for-each bb-interference-graph (cfg-bbs cfg)))

  all-live)

;;-----------------------------------------------------------------------------

(define (delete byte-cell1 neighbours)
  (set-for-each (lambda (byte-cell2)
		  (set-remove! (byte-cell-interferes-with byte-cell2)
			       byte-cell1))
		neighbours))
(define (undelete byte-cell1 neighbours)
  (set-for-each (lambda (byte-cell2)
		  (set-add! (byte-cell-interferes-with byte-cell2)
			    byte-cell1))
		neighbours))

(define (coalesce graph)
  (if coalesce?
      (keep
       (lambda (byte-cell)
	 (let* ((coalesceable-with   (byte-cell-coalesceable-with byte-cell))
		(old-neighbours      (byte-cell-interferes-with   byte-cell))
		(coalesce-candidates (set-diff coalesceable-with
					       old-neighbours)))
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
			   (let ((c-neighbours (byte-cell-interferes-with c))
				 (c-coalesced  (byte-cell-coalesced-with  c)))
			     ;; remove all references to byte-cell and replace
			     ;; them with references to c
			     (set-union! c-neighbours old-neighbours)
			     (undelete   c            old-neighbours)
			     (delete     byte-cell    old-neighbours)
			     (set-union! c-coalesced
					 (byte-cell-coalesced-with byte-cell))
			     (set-for-each
			      (lambda (cell)
				(let ((s (byte-cell-coalesceable-with cell)))
				  (set-remove! s byte-cell)))
			      coalesceable-with)
			     (byte-cell-coalesceable-with-set! byte-cell
							       (new-empty-set))
			     (byte-cell-interferes-with-set!   byte-cell
							       (new-empty-set))
			     (set-add! c-coalesced byte-cell)
			     #f))))))))
       graph)
      graph))

;;-----------------------------------------------------------------------------

(define register-table         (make-table))
(define reverse-register-table (make-table))
(define (allocate-registers cfg)
  (let ((all-live (coalesce (set->list (interference-graph cfg))))
	(max-adr  0)) ; to know how much ram we need

    (define (color byte-cell)
      (define (set-register-table cell adr)
	(if #f (not (string=? (byte-cell-name cell) "__tmp"))
	    (let ((adr (if (and (> adr #x5F) (< adr #xF60)) ; not in bank 0 ;; TODO have a function for that
			   (+ adr #xa0)
			   adr)))
	      (table-set! register-table
			  adr
			  (cons (cons (byte-cell-bb   cell)
				      (byte-cell-name cell))
				(table-ref register-table adr '())))
	      (table-set! reverse-register-table
			  (byte-cell-name cell) ;; TODO add the bb ?
			  adr))))
      (let ((neighbours (byte-cell-interferes-with byte-cell)))
	(let loop1 ((adr 0))
	  (if (and memory-divide ; the user wants his own zone
		   (>= adr memory-divide)) ; and we'd use it
	      (error "register allocation would cross the memory divide") ;; TODO fallback ?
	      (let loop2 ((lst (set->list neighbours))) ;; TODO keep using sets, but not urgent, it's not a bottleneck
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
                   (n (set-length (byte-cell-interferes-with x))))
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

    (pp register-allocation:)
    (time (alloc-reg all-live)) ;; TODO convert find-min-neighbours and alloc-reg to use tables, not urgent since it's not a bottleneck
    (display (string-append (number->string (+ max-adr 1)) " RAM bytes\n"))))
