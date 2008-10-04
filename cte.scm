(define (predefine-var id type adr)
  (let* ((value
          (new-value (list (make-byte-cell adr '() '()))))
         (ast
          (new-def-variable '() id '() type value '())))
    ast))

(define (predefine-fun id type param-defs adr)
  (let* ((value
          (cond ((eq? type 'int)
                 (new-value (list (make-byte-cell WREG '() '()))))
                ((eq? type 'void)
                 (new-value '()))
                (else
                 (error "unknown return type"))))
         (params
          (map (lambda (x)
                 (predefine-var 'foo (car x) (cdr x)))
               param-defs))
         (ast
          (new-def-procedure '() id '() type value params))
         (entry
          (asm-make-label id adr)))
    (multi-link-parent! params ast)
    (def-procedure-entry-set! ast entry)
    ast))

(define (initial-cte) ;; TODO see what really has to be predefined
  (list (predefine-var 'X 'int 5)
        (predefine-var 'Y 'int 6)
        (predefine-var 'Z 'int 7)
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
                       (list (cons 'int WREG))
                       #x1F2)
        (predefine-fun 'irda_tx_wake_up
                       'void
                       '()
                       #x1F4)
        (predefine-fun 'irda_tx_raw
                       'void
                       (list (cons 'int WREG))
                       #x1F6)
        (predefine-fun 'irda_rx_raw
                       'int
                       '()
                       #x1F8)
        (predefine-fun 'sleep_mode
                       'void
                       '()
                       #x1FA)
        (predefine-fun 'exec_client
                       'void
                       '()
                       #x1FC)))

(define (cte-extend cte bindings)
  (append bindings cte))

(define (cte-lookup cte id)
  (cond ((null? cte)
         (error "undefined identifier" id))
        ((eq? (def-id (car cte)) id)
         (car cte))
        (else
         (cte-lookup (cdr cte) id))))
