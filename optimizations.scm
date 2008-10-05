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
	  (let ((rev-instrs (bb-rev-instrs bb)))
	    (cond ((null? rev-instrs) ; empty bb, remove all references to it
		   (let ((dest (car (bb-succs bb)))
			 (pred (car (bb-preds bb))))
		     (bb-succs-set! pred
				    (cons dest (remove bb (bb-succs pred))))
		     (bb-preds-set! dest
				    (cons pred (remove bb (bb-preds dest))))
		     (chase-branch-cascade dest
					   (cons bb seen))))
		  ((and (null? (cdr rev-instrs)) ; only a goto
			(eq? (instr-id (car rev-instrs)) 'goto))
		   (let ((old-dest
			  (car (bb-succs bb)))) ;; TODO let* ?
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
		  (else bb)))))

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
