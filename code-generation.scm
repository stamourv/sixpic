(define (linearize-and-cleanup cfg)

  (define bbs-vector (cfg->vector cfg))

  (define todo '())

  (define (add-todo bb)
    (set! todo (cons bb todo)))

  (define rev-code '())

  (define (emit instr)
    (set! rev-code (cons instr rev-code)))

  (define (movlw val)
    (emit (list 'movlw val)))

  (define (movwf adr)
    (emit (list 'movwf adr)))

  (define (movfw adr)
    (emit (list 'movfw adr)))

  (define (clrf adr)
    (emit (list 'clrf adr)))

  (define (setf adr)
    (emit (list 'setf adr)))

  (define (incf adr)
    (emit (list 'incf adr)))

  (define (decf adr)
    (emit (list 'decf adr)))

  (define (addwf adr)
    (emit (list 'addwf adr)))

  (define (addwfc adr)
    (emit (list 'addwfc adr)))

  (define (subwf adr)
    (emit (list 'subwf adr)))

  (define (subwfb adr)
    (emit (list 'subwfb adr)))

  (define (cpfseq adr)
    (emit (list 'cpfseq adr)))

  (define (cpfslt adr)
    (emit (list 'cpfslt adr)))

  (define (cpfsgt adr)
    (emit (list 'cpfsgt adr)))

  (define (bra label)
    (emit (list 'bra label)))

  (define (rcall label)
    (emit (list 'rcall label)))

  (define (return)
    (if (and #f (and (not (null? rev-code))
             (eq? (caar rev-code) 'rcall))
)
        (let ((label (cadar rev-code)))
          (set! rev-code (cdr rev-code))
          (bra label))
        (emit (list 'return))))

  (define (label lab)
    (if (and #f (and (not (null? rev-code))
             (eq? (caar rev-code) 'bra)
             (eq? (cadar rev-code) lab))
)
        (begin
          (set! rev-code (cdr rev-code))
          (label lab))
        (emit (list 'label lab))))

  (define (sleep)
    (emit (list 'sleep)))

  (define (move-reg src dst)
    (cond ((= src dst))
          ((= src WREG)
           (movwf dst))
          ((= dst WREG)
           (movfw src))
          (else
           (movfw src)
           (movwf dst))))

  (define (bb-linearize bb)
    (let ((label-num (bb-label-num bb)))
      (let ((bb (vector-ref bbs-vector label-num)))

        (define (move-lit n adr)
          (cond ((= n 0)
                 (clrf adr))
                ((= n #xff)
                 (setf adr))
                (else
                 (movlw n)
                 (movwf adr))))

        (define (dump-instr instr)
          (cond ((call-instr? instr)
                 (let* ((def-proc (call-instr-def-proc instr))
                        (entry (def-procedure-entry def-proc)))
                   (if (bb? entry)
                       (begin
                         (add-todo entry)
                         (let ((label (bb-label entry)))
                           (rcall label)))
                       (rcall entry))))
                ((return-instr? instr)
                 (return))
                (else
                 (let ((src1 (instr-src1 instr))
                       (src2 (instr-src2 instr))
                       (dst (instr-dst instr)))
                   (if (and (or (not (byte-cell? dst))
                                (byte-cell-adr dst))
                            (or (not (byte-cell? src1))
                                (byte-cell-adr src1))
                            (or (not (byte-cell? src2))
                                (byte-cell-adr src2)))
                       (case (instr-id instr)
                         ((move)
                          (if (byte-lit? src1)
                              (let ((n (byte-lit-val src1))
                                    (z (byte-cell-adr dst)))
                                (move-lit n z))
                              (let ((x (byte-cell-adr src1))
                                    (z (byte-cell-adr dst)))
                                (move-reg x z))))
                         ((add addc sub subb)
                          (if (byte-lit? src2)
                              (let ((n (byte-lit-val src2))
                                    (z (byte-cell-adr dst)))
                                (if (byte-lit? src1)
                                    (move-lit (byte-lit-val src1) z)
                                    (move-reg (byte-cell-adr src1) z))
                                (case (instr-id instr)
                                  ((add)
                                   (cond ((= n 1)
                                          (incf z))
                                         ((= n #xff)
                                          (decf z))
                                         (else
                                          (movlw n)
                                          (addwf z))))
                                  ((addc)
                                   (movlw n)
                                   (addwfc z))
                                  ((sub)
                                   (cond ((= n 1)
                                          (decf z))
                                         ((= n #xff)
                                          (incf z))
                                         (else
                                          (movlw n)
                                          (subwf z))))
                                  ((subb)
                                   (movlw n)
                                   (subwfb z))))
                              (let ((x (byte-cell-adr src1))
                                    (y (byte-cell-adr src2))
                                    (z (byte-cell-adr dst)))
                                (cond ((and (not (= x y)) (= y z))
                                       (move-reg x WREG)
                                       (case (instr-id instr)
                                         ((add)
                                          (addwf z))
                                         ((addc)
                                          (addwfc z))
                                         ((sub)
                                          (subwfr z))
                                         ((subb)
                                          (subwfbr z))
                                         (else (error "..."))))
                                      (else
                                       (move-reg x z)
                                       (move-reg y WREG)
                                       (case (instr-id instr)
                                         ((add)
                                          (addwf z))
                                         ((addc)
                                          (addwfc z))
                                         ((sub)
                                          (subwf z))
                                         ((subb)
                                          (subwfb z))
                                         (else (error "..."))))))))
                         ((goto)
                          (let* ((succs (bb-succs bb))
                                 (dest (car succs)))
                            (bra (bb-label dest))
                            (add-todo dest)))
                         ((x==y x<y x>y)
			  (pp (list "BB compare" (bb-label-num bb)))
                          (let* ((succs (bb-succs bb))
                                 (dest-true (car succs))
                                 (dest-false (cadr succs)))

                            (define (compare flip adr)
                              (case (instr-id instr)
                                ((x<y) (if flip (cpfsgt adr) (cpfslt adr)))
                                ((x>y) (if flip (cpfslt adr) (cpfsgt adr)))
                                (else (cpfseq adr)))
                              (bra (bb-label dest-false))
                              (bra (bb-label dest-true))
                              (add-todo dest-false)
                              (add-todo dest-true))

                            (cond ((byte-lit? src1)
                                   (let ((n (byte-lit-val src1))
                                         (y (byte-cell-adr src2)))
                                     (if (and (or (= n 0) (= n 1) (= n #xff))
                                              (eq? (instr-id instr) 'x==y))
                                         (special-compare-eq-lit n x)
                                         (begin
                                           (movlw n)
                                           (compare #t y)))))
                                  ((byte-lit? src2)
                                   (let ((x (byte-cell-adr src1))
                                         (n (byte-lit-val src2)))
                                     (if (and (or (= n 0) (= n 1) (= n #xff))
                                              (eq? (instr-id instr) 'x==y))
                                         (special-compare-eq-lit n x)
                                         (begin
                                           (movlw n)
                                           (compare #f x)))))
                                  (else
                                   (let ((x (byte-cell-adr src1))
                                         (y (byte-cell-adr src2)))
                                     (move-reg y WREG)
                                     (compare #f x))))))
                         (else
                          ;...
                          (emit (list (instr-id instr))))))))))

        (if bb
            (begin
              (vector-set! bbs-vector label-num #f)
              (label (bb-label bb))
              (for-each dump-instr (reverse (bb-rev-instrs bb)))
              (for-each add-todo (bb-succs bb)))))))
  
  (let ((prog-label (asm-make-label 'PROG)))
    (rcall prog-label)
    (sleep)
    (label prog-label))

  (add-todo (vector-ref bbs-vector 0))

  (let loop ()
    (if (null? todo)
        (reverse rev-code)
        (let ((bb (car todo)))
          (set! todo (cdr todo))
          (bb-linearize bb)
          (loop)))))


(define (assembler-gen filename cfg)

  (define (gen instr)
    (case (car instr)
      ((movlw)
       (movlw (cadr instr)))
      ((movwf)
       (movwf (cadr instr)))
      ((movfw)
       (movf (cadr instr) 'w))
      ((clrf)
       (clrf (cadr instr)))
      ((setf)
       (setf (cadr instr)))
      ((incf)
       (incf (cadr instr)))
      ((decf)
       (decf (cadr instr)))
      ((addwf)
       (addwf (cadr instr)))
      ((addwfc)
       (addwfc (cadr instr)))
      ((subwf)
       (subwf (cadr instr)))
      ((subwfb)
       (subwfb (cadr instr)))
      ((cpfseq)
       (cpfseq (cadr instr)))
      ((cpfslt)
       (cpfslt (cadr instr)))
      ((cpfsgt)
       (cpfsgt (cadr instr)))
      ((bra)
       (bra (cadr instr)))
      ((rcall)
       (rcall (cadr instr)))
      ((return)
       (return))
      ((label)
       (asm-listing
        (string-append (symbol->string (asm-label-id (cadr instr))) ":"))
       (asm-label (cadr instr)))
      ((sleep)
       (sleep))
      (else
'       (error "unknown instruction" instr))))

  (asm-begin! 0 #f)

;  (pretty-print cfg) ;; TODO 6 is still here

  (let ((code (linearize-and-cleanup cfg)))
;    (pretty-print code) ;; TODO 6 is not here anymore
    (for-each gen code))

  (asm-assemble)

  '(display "------------------ GENERATED CODE\n")

  (asm-display-listing (current-output-port)) ;; TODO debug

  (asm-write-hex-file (string-append filename ".hex"))

  '(display "------------------ EXECUTION USING SIMULATOR\n")

  (asm-end!)

  '(execute-hex-file (string-append filename ".hex"))) ;; TODO debug

(define (code-gen filename cfg)
  (allocate-registers cfg)
  (assembler-gen filename cfg)
;  (pretty-print cfg)
;  (pretty-print (reverse (bb-rev-instrs bb))) ;; TODO what ? there are no bbs here...
  )
