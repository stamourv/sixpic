#! gsi-script -:dar

(include "pic18-sim.scm")
(include "utilities.scm")
(include "ast.scm")
(include "operators.scm")
(include "cte.scm")
(include "parser.scm")
(include "cfg.scm")
(include "optimizations.scm")
(include "code-generation.scm")


;------------------------------------------------------------------------------

(define (read-source filename)
  (shell-command (string-append "cpp -P " filename " > " filename ".tmp"))
;;   (##read-all-as-a-begin-expr-from-path ;; TODO use vectorized notation to have info on errors (where in the source)
;;    (string-append filename ".tmp")
;;    (readtable-start-syntax-set (current-readtable) 'six)
;;    ##wrap-datum
;;    ##unwrap-datum)
  (with-input-from-file
      (string-append filename ".tmp")
    (lambda ()
      (input-port-readtable-set!
       (current-input-port)
       (readtable-start-syntax-set
        (input-port-readtable (current-input-port))
        'six))
      (read-all)))
  )

(define (main filename)

  (output-port-readtable-set!
   (current-output-port)
   (readtable-sharing-allowed?-set
    (output-port-readtable (current-output-port))
    #t))

  (let ((source (read-source filename)))
    '(pretty-print source)
    (let ((ast (parse source)))
      '(pretty-print ast)
      (let ((cfg (generate-cfg ast)))
	'(print-cfg-bbs cfg)
	'(pretty-print cfg)
        (remove-branch-cascades-and-dead-code cfg)
	(remove-converging-branches cfg)
	(remove-dead-instructions cfg)
	'(pp "AFTER")
	'(print-cfg-bbs cfg)
        '(pretty-print cfg)
        (let ((code (code-gen filename cfg)))
          (pretty-print code)
	  '(display "------------------ EXECUTION USING SIMULATOR\n")
	  (execute-hex-file (string-append filename ".hex")))))))
