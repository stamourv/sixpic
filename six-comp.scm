#! /home/vincent/sixpic/gambc-v4_2_9/bin/gsi-script -:dar

(include "pic18-sim.scm")
(include "utilities.scm")
(include "ast.scm")
(include "operators.scm")
(include "cte.scm")
(include "parser.scm")
(include "cfg.scm")
(include "optimizations.scm")
(include "code-generation.scm")

;; TODO have a table that says what types can cast to what other, and what can happen implicitly
;; TODO what casts are going to happen, only between different integer sizes ?
;; TODO have this as an a-list. a type as car, and the list of types it can cast to as cdr
(define casts '())


;------------------------------------------------------------------------------

(define (read-source filename)
  (shell-command (string-append "cpp -P " filename " > " filename ".tmp"))
;;   (##read-all-as-a-begin-expr-from-path ;; TODO use vectoruzed notation to have info on errors (where in the source)
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
    (pretty-print source)
    (let ((ast (parse source)))
      '(pretty-print ast)
      (let ((cfg (generate-cfg ast)))
        (remove-branch-cascades-and-dead-code cfg) ; TODO sometimes breaks stuff
	(pp "AFTER")
	(print-cfg-bbs cfg)
        '(pretty-print cfg)
        (let ((code (code-gen filename cfg)))
          (pretty-print code))))))
