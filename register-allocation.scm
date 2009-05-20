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
		 (if (not (set-empty? live-after))
		     (begin
		       (set! changed #t)
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
		       (set-intersection
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
		      (dst (instr-dst instr))
		      (use (if (byte-cell? src1)
			       (if (byte-cell? src2)
				   (set-add (new-set src1) src2)
				   (new-set src1))
			       (if (byte-cell? src2)
				   (new-set src2)
				   (new-empty-set))))
		      (def (if (byte-cell? dst)
			       (new-set dst)
			       (new-empty-set))))
		 (if #f
		     ;; (and (byte-cell? dst) ; dead instruction?
		     ;;      (not (memq dst live-after))
		     ;;      (not (and (byte-cell? dst) (byte-cell-adr dst))))
		     live-after
		     (set-union use
				(set-diff live-after def))))))))
	(instr-live-before-set! instr live-before)
	(instr-live-after-set! instr live-after)
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

  (define all-live (new-empty-set))
  (define (bb-interference-graph bb)
    (define (interfere x y)
      (if (not (set-member? (byte-cell-interferes-with y) x))
	  (begin (set-add! (byte-cell-interferes-with x) y)
		 (set-add! (byte-cell-interferes-with y) x))))
    (define (interfere-pairwise live)
      (set-union! all-live live) ;; TODO build the live set only once, if not a set already
      (set-for-each ;; TODO for each cell in live, do the union of live, diff itself, if live is not a set, we win so we can iterate on something better than a hash table
       ;; TODO since all true variables will be in the low numbers and all temps in the high numbers, we can have sparse bit vectors (if all below n is 0, don't store it) and save (or even have a bit vector that can store data by chunks, leaving empty space)
       (lambda (x)
	 (set-for-each (lambda (y)
				(if (not (eq? x y)) (interfere x y)))
			      live))
       live))
    (define (instr-interference-graph instr)
      (let ((dst (instr-dst instr)))
	(if (byte-cell? dst)
	    (let ((src1 (instr-src1 instr))
		  (src2 (instr-src2 instr)))
	      (if (byte-cell? src1)
		  (begin (set-add! (byte-cell-coalesceable-with dst) src1)
			 (set-add! (byte-cell-coalesceable-with src1) dst)))
	      (if (byte-cell? src2)
		  (begin (set-add! (byte-cell-coalesceable-with dst) src2)
			 (set-add! (byte-cell-coalesceable-with src2) dst))))))
      (interfere-pairwise (instr-live-before instr)))
    (for-each instr-interference-graph (bb-rev-instrs bb)))

  (pp analyse-liveness:)
  (time (analyze-liveness cfg))

  (pp interference-graph:)
  (time (for-each bb-interference-graph (cfg-bbs cfg)))

  all-live)

;;-----------------------------------------------------------------------------

(define (allocate-registers cfg)
  (let ((all-live (interference-graph cfg))
	(max-adr  0)) ; to know how much ram we need

    (define (color byte-cell)
      (let (;; (coalesce-candidates ; TODO right now, no coalescing is done
;;              (set-filter byte-cell-adr
;;                          (set-diff (byte-cell-coalesceable-with byte-cell)
;; 				   (byte-cell-interferes-with byte-cell))))
	    )
;;         '
;;         (pp (list byte-cell: byte-cell;;;;;;;;;;;;;;;
;;                   coalesce-candidates
;;                                         ;                  interferes-with: (byte-cell-interferes-with byte-cell)
;;                                         ;                  coalesceable-with: (byte-cell-coalesceable-with byte-cell)
;;                   ))

        (if #f #;(not (null? coalesce-candidates))
            (let ((adr (byte-cell-adr (car (set->list coalesce-candidates))))) ;; TODO have as a set all along
              (byte-cell-adr-set! byte-cell adr))
            (let ((neighbours (byte-cell-interferes-with byte-cell)))
              (let loop1 ((adr 0))
                (if (and memory-divide ; the user wants his own zone
                         (>= adr memory-divide)) ; and we'd use it
                    (error "register allocation would cross the memory divide") ;; TODO fallback ?
                    (let loop2 ((lst (set->list neighbours))) ;; TODO keep using sets, but not urgent, it's not a bottleneck
		      (if (null? lst)
			  (byte-cell-adr-set! byte-cell adr)
			  (let ((x (car lst)))
			    (if (= adr (byte-cell-adr x))
				(loop1 (+ adr 1))
				(loop2 (cdr lst)))))))
		(set! max-adr (max max-adr adr)))))))
    
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
    (time (alloc-reg (set->list all-live))) ;; TODO convert find-min-neighbors and alloc-reg to use tables, not urgent since it's not a bottleneck
    (display (string-append (number->string (+ max-adr 1)) " RAM bytes\n"))))
