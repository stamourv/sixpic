(define (predefine-var id type adr)
  (let* ((value
          (new-value (list (make-byte-cell adr '() '()))))
         (ast
          (new-def-variable '() id '() type value '())))
    ast))

(define (predefine-fun id type param-defs adr)
  (let* ((value
          (cond ((eq? type 'byte) ;; TODO have the others, or make this generic (this is not actually used anyway)
                 (new-value (list (make-byte-cell WREG '() '()))))
                ((eq? type 'void)
                 (new-value '()))
                (else
                 (error "unknown return type"))))
         (params
          (map (lambda (x)
		 ;; parameters don't need names here
                 (predefine-var 'foo (car x) (cdr x)))
               param-defs))
         (ast
          (new-def-procedure '() id '() type value params))
         (entry
          (asm-make-label id adr)))
    (multi-link-parent! params ast)
    (def-procedure-entry-set! ast entry)
    ast))

(define predefined-routines '())

;; as predefine-fun, but represented as bbs, not as preloaded machine code
;; the body of the procedure (as a cfg) will be generated during the generation
;; of the main cfg
(define (predefine-routine id type param-defs)
  (let ((params
	 (map (lambda (type) ; parameters are passed like this: (type type ...)
		;; parameters don't need names here
		(new-def-variable '() 'foo '() type (alloc-value type) '()))
	      param-defs)))
    (set! predefined-routines (cons id predefined-routines))
    (new-def-procedure '() id '() type (alloc-value type) params)))

(define initial-cte ;; TODO clean this up
  (list (predefine-var 'X 'byte 5)
        (predefine-var 'Y 'byte 6)
        (predefine-var 'Z 'byte 7)
        (predefine-fun 'FLASH_execute_erase
                       'void
                       '()
                       #x1EE)
        (predefine-fun 'FLASH_execute_write
                       'void
                       '()
                       #x1F0)
        (predefine-fun 'led_set
                       'void
                       (list (cons 'byte WREG))
                       #x1F2)
        (predefine-fun 'irda_tx_wake_up
                       'void
                       '()
                       #x1F4)
        (predefine-fun 'irda_tx_raw
                       'void
                       (list (cons 'byte WREG))
                       #x1F6)
        (predefine-fun 'irda_rx_raw
                       'byte
                       '()
                       #x1F8)
        (predefine-fun 'sleep_mode
                       'void
                       '()
                       #x1FA)
        (predefine-fun 'exec_client
                       'void
                       '()
                       #x1FC)

	;; TODO maybe use some for the fsr variables ? and have the address be the fsr registers
	
	;; for multiplication
	;; TODO do others
	(predefine-routine 'mul8_8 'int16 '(byte byte))
	
	;; TODO maybe use predefine fun and have jump to a function already in rom ? then have some kind of linking to see if it's used, and if so, put the code in
	))

(define (cte-extend cte bindings)
  (append bindings cte))

(define (cte-lookup cte id)
  (cond ((null? cte)
         (error "undefined identifier" id))
        ((eq? (def-id (car cte)) id)
         (car cte))
        (else
         (cte-lookup (cdr cte) id))))
