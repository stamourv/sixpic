(define (predefine-var id type addresses)
  (let* ((value
	  ;; adrs is the list of addresses this variable is stored at
          (new-value (map (lambda (x) (make-byte-cell x '() '()))
			  addresses)))
         (ast
          (new-def-variable '() id '() type value '())))
    ast))

(define (predefine-fun id type param-defs adr)
  (let* ((value
          (cond ((eq? type 'byte) ;; TODO have the other types, or make this generic (this is not actually used anyway)
                 (new-value (list (make-byte-cell WREG '() '()))))
                ((eq? type 'void)
                 (new-value '()))
                (else
                 (error "unknown return type"))))
         (params
          (map (lambda (x)
		 ;; parameters don't need names here
		 ;; TODO support other types
                 (predefine-var 'foo (car x) (list (cdr x))))
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

(define initial-cte
  (list
   (predefine-var 'X 'byte '(5))
   (predefine-var 'Y 'byte '(6))
   (predefine-var 'Z 'byte '(7))
   
   (predefine-fun 'FLASH_execute_erase 'void '() #x1EE)
   (predefine-fun 'FLASH_execute_write 'void '() #x1F0)
   (predefine-fun 'led_set 'void (list (cons 'byte WREG)) #x1F2)
   (predefine-fun 'irda_tx_wake_up 'void '() #x1F4)
   (predefine-fun 'irda_tx_raw 'void (list (cons 'byte WREG)) #x1F6)
   (predefine-fun 'irda_rx_raw 'byte '() #x1F8)
   (predefine-fun 'sleep_mode 'void '() #x1FA)
   (predefine-fun 'exec_client 'void '() #x1FC)
   
   ;; special variables
   (predefine-var 'SIXPIC_FSR0 'int16 (list FSR0L FSR0H))
   (predefine-var 'SIXPIC_FSR1 'int16 (list FSR1L FSR1H))
   (predefine-var 'SIXPIC_FSR2 'int16 (list FSR2L FSR2H))
	
   (predefine-routine 'mul8_8   'int8  '(int8  int8))
   (predefine-routine 'mul16_8  'int16 '(int16 int8)) ;; TODO since multiplication arguments are not padded, these asymetric operations are used, they are more efficient, but padding would mean fewer necessary routines
   (predefine-routine 'mul16_16 'int16 '(int16 int16))
   (predefine-routine 'mul32_16 'int32 '(int32 int16))

   (predefine-routine 'shl8  'int8  '(int8  int8))
   (predefine-routine 'shl16 'int16 '(int16 int8))
   (predefine-routine 'shl32 'int32 '(int32 int8))
   (predefine-routine 'shr8  'int8  '(int8  int8))
   (predefine-routine 'shr16 'int16 '(int16 int8))
   (predefine-routine 'shr32 'int32 '(int32 int8))   
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
