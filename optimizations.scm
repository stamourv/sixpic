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
		  ;;(pp (list instr: instr))
                  (let ((def-proc (return-instr-def-proc instr)))
                    (let ((live
                           (if (def-procedure? def-proc)
                               (def-procedure-live-after-calls def-proc)
                               (value-bytes def-proc))))
		      (let ((live (keep byte-cell? live)))
			;;(pp (list live: live))
			(set! live-after live)
			live)))
		  )
                 (else
		  ;;(pp (list instr: instr))
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
                           ;; (if (not (eq? old-dest new-dest)) ;; TODO this seems to be a shortcut, and it broke a few things, so removed
			   ;;      (begin
			   ;; (pp (list "CASCADE" (bb-label-num bb)))
			   ;; (bb-succs-set! bb
			   ;; (remove old-dest (bb-succs bb)))
			   ;;  (bb-preds-set! old-dest
			   ;;  (remove bb (bb-preds old-dest)))))
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
                                           (cons bb (bb-preds old-dest))))))
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
      (let* ((head (car instrs))
	     (op   (instr-id head)))
	(if (or (eq? op 'return)
		(eq? op 'goto)
		(memq op conditional-instrs))
	    (bb-rev-instrs-set! bb (cons head new-instrs))
	    (loop (cdr instrs)
		  (cons head new-instrs))))))

  (for-each bb-process (cfg-bbs cfg)))
