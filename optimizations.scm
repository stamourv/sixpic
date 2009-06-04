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
			    (bb-succs-set! bb (replace old-dest new-dest (bb-succs bb)))
                            (bb-preds-set! old-dest
                                           (remove bb (bb-preds old-dest)))
			    (bb-preds-set! new-dest
                                           (cons bb (bb-preds new-dest))))))
                    old-succs
                    new-succs)))

    (for-each bb-process (cfg-bbs cfg))))

;------------------------------------------------------------------------------

;; remove conditions whose 2 destination branches are the same, and replaces
;; them with simple jumps
(define (remove-converging-branches cfg)

  (define (bb-process bb)
    (let ((instrs (bb-rev-instrs bb))
	  (succs  (bb-succs bb)))
      (if (and (memq (instr-id (car instrs)) conditional-instrs) ; conditional
	       (>= (length succs) 2)
	       (eq? (car succs) (cadr succs))) ; both destinations are the same
	  (bb-rev-instrs-set! bb (cons (new-instr 'goto #f #f #f)
				       (cdr instrs))))))
  
  (for-each bb-process (cfg-bbs cfg)))

;------------------------------------------------------------------------------

;; removes dead instructions (instructions after a return or after all jumps)
(define (remove-dead-instructions cfg)

  (define (bb-process bb)
    ;; since instructions are in erverse order, loop until we find a jump,
    ;; and keep everything after
    (let loop ((instrs (reverse (bb-rev-instrs bb)))
	       (new-instrs '()))
      (if (null? instrs)
	  (error "no jump in the bb:" bb))
      (let* ((head (car instrs))
	     (op   (instr-id head)))
	(if (or (memq op '(return goto branch-table branch-if-carry))
		(memq op conditional-instrs))
	    (bb-rev-instrs-set! bb (cons head new-instrs))
	    (loop (cdr instrs)
		  (cons head new-instrs))))))

  (for-each bb-process (cfg-bbs cfg)))
