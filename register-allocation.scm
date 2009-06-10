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
			 (set-diff
			  live-after
			  (list->set
			   (value-bytes (def-procedure-value def-proc)))))))
		   (if (bb? (def-procedure-entry def-proc))
		       (set-intersection ;; TODO disabling this branch saves around 12 bytes
			(bb-live-before (def-procedure-entry def-proc))
			live)
		       live))))
	      
	      ((return-instr? instr)
	       (let ((def-proc (return-instr-def-proc instr)))
		 (let ((live
			(if (def-procedure? def-proc)
			    (def-procedure-live-after-calls def-proc)
			    (list->set (value-bytes def-proc)))))
		   (set! live-after live)
		   live)))
	      
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
    
    (define (interfere-pairwise live)
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
	      (if (and (byte-cell? src1) (not (eq? dst src1))) ;; FOO not sure this fixes anything, but if I remove it, loops infinitely in shl16 (which does no shifting) problem is probably elsewhere, though
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

(define (coalesce graph) ;; FOO pc gets clobbered, it seems... (everything gets clobbered)
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
	       (let ((c (let loop ((l (set->list coalesce-candidates)))
			  (if (null? l)
			      #f
			      (let ((c (car l)))
				(if (byte-cell-adr c)
				    (loop (cdr l))
				    c))))))
		 (if (not c)
		     #t
		     (let ((c-neighbours   (byte-cell-interferes-with c)))
		       ;; remove all references to byte-cell and add references
		       ;; to c instead
		       (set-union! c-neighbours old-neighbours)
		       (undelete   c            old-neighbours)
		       (delete     byte-cell    old-neighbours)
		       (set-for-each
			(lambda (cell)
			  (let ((s (byte-cell-coalesceable-with cell)))
			    (set-remove! s byte-cell)
			    ;; (if (not (eq? cell c)) (set-add! s c)) ;; FOO disabled for now
			    ))
			coalesceable-with)
		       (byte-cell-interferes-with-set!   byte-cell
							 (new-empty-set))
		       (byte-cell-coalesceable-with-set! byte-cell
							 (new-empty-set))
		       (set-add! (byte-cell-coalesced-with c) byte-cell)
		       #f))))))
       graph)
      graph))

;;-----------------------------------------------------------------------------

(define register-table (make-table))
(define (allocate-registers cfg)
  (let ((all-live (coalesce (set->list (interference-graph cfg))))
	(max-adr  0)) ; to know how much ram we need

    (define (color byte-cell)
      (define (set-register-table cell adr)
	(if #f (not (string=? (byte-cell-name cell) "__tmp")) ;; TODO DEBUG
	    (table-set! register-table
			(if (and (> adr #x5F) (< adr #xF60))
			    ;; not in bank 0
			    (+ adr #xa0)
			    adr)
			(cons (cons (byte-cell-bb   cell)
				    (byte-cell-name cell))
			      (table-ref register-table adr '())))))
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

    (let loop ((l all-live)) ;; FOO DEBUG might be nice to know which bb these temporaries come from, really have a way to know where each byte cell is used
      (if (not (null? l))
	  (let ((head (car l)))
	    (if (or (string=? (byte-cell-name head) "env0$86")
		    (string=? (byte-cell-name head) "env1$85"))
		(begin (pp (byte-cell-name head))
		       (for-each (lambda (x) (pp (list (byte-cell-name x) (byte-cell-bb x)))) ;; FOO does it report the right one ?
				 (set->list (byte-cell-coalesced-with head)))
;; 		       (let loop ((l all-live))
;; 			 (if (not (null? l))
;; 			     (let ((h (car l)))
;; 			       (if (not (set-member? (byte-cell-interferes-with head) h))
;; 				   (pp (list N-I: (byte-cell-name h) (byte-cell-bb h)))) ;; FOO commit all changes but register-allocation.scm ALSO, I don't think it reports the right conflicts, since the reportes coalesced cells do not seem to be allocated in the same place
;; 			       (loop (cdr l)))))
		       ))
;; 	    (if (not (set-empty? (set-filter (lambda (cell)
;; 					       (or (string=? (byte-cell-name cell) "env0$86")
;; 						   (string=? (byte-cell-name cell) "env1$85")))
;; 					     (byte-cell-coalesced-with head))))
;; 		(pp (list OPP: (byte-cell-name head) I: (map (lambda (x) (byte-cell-name x)) (set->list (byte-cell-coalesced-with head))))))
	    (loop (cdr l)))))

    (pp register-allocation:)
    (time (alloc-reg all-live)) ;; TODO convert find-min-neighbours and alloc-reg to use tables, not urgent since it's not a bottleneck
    (display (string-append (number->string (+ max-adr 1)) " RAM bytes\n"))))
