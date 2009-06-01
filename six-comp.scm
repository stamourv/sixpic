#!/usr/bin/env gsi

(declare (standard-bindings))

(define allocate-registers? #t) ; can be turned off to reduce compilation time
(define fold-constants? #t)

;; to use when interpreting
(include "asm.scm")
(include "pic18.scm")
(include "pic18-sim.scm")
(include "utilities.scm")
(include "ast.scm")
(include "operators.scm")
(include "cte.scm")
(include "parser.scm")
(include "cfg.scm")
(include "optimizations.scm")
(include "code-generation.scm")
(include "register-allocation.scm")
(include "profiler.scm")

;; ;; to use with compiled code
;; (load "asm")
;; (load "pic18")
;; (load "pic18-sim")
;; (load "utilities")
;; (load "ast")
;; (load "operators")
;; (load "cte")
;; (load "parser")
;; (load "cfg")
;; (load "optimizations")
;; (load "code-generation")
;; (load "register-allocation")
;; (load "profiler")

;------------------------------------------------------------------------------

;; temporary solution, to support more than int
(set! ##six-types ;; TODO signed types ?
  '((int    . #f)
    (byte   . #f)
    (int8   . #f)
    (int16  . #f)
    (int32  . #f)
    (char   . #f)
    (bool   . #f)
    (void   . #f)
    (float  . #f)
    (double . #f)
    (obj    . #f)))
;; TODO typedef should add to this list

'(current-exception-handler (lambda (exc) (##repl))) ; when not running in the repl

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


(define (main filename . data)

  (output-port-readtable-set!
   (current-output-port)
   (readtable-sharing-allowed?-set
    (output-port-readtable (current-output-port))
    #t))

  (let ((source (read-source filename)))
    '(pretty-print source)
    (let* ((ast (parse source)))
      '(pretty-print ast)
      (let ((cfg (generate-cfg ast)))
	'(print-cfg-bbs cfg)
	'(pretty-print cfg)
        (remove-branch-cascades-and-dead-code cfg)
	(remove-converging-branches cfg) ;; TODO maybe make it possible to disable it, and the next one ?
	(remove-dead-instructions cfg)
 	(if allocate-registers? (allocate-registers cfg))
	(assembler-gen filename cfg)
	(asm-assemble)
	'(asm-display-listing (current-output-port))
	(with-output-to-file (string-append filename ".s")
	  (lambda () (asm-display-listing (current-output-port))))
	(with-output-to-file (string-append filename ".map")
	  (lambda () (write (table->list symbol-table))))
	(with-output-to-file (string-append filename ".reg")
	  (lambda () (write (table->list register-table))))
	(asm-write-hex-file (string-append filename ".hex"))
	(asm-end!)
	;; data contains a list of additional hex files
	(apply execute-hex-files (cons (string-append filename ".hex") data))
	#t))))

(define (picobit prog #!optional (recompile? #f))
  (set! trace-instr #f)
  (if recompile?
      (main "tests/picobit/picobit-vm-sixpic.c" prog)
      (simulate (list "tests/picobit/picobit-vm-sixpic.c.hex" prog)
		"tests/picobit/picobit-vm-sixpic.c.map")))

(define (simulate hexs map-file)
  (set! symbol-table (with-input-from-file map-file
		       (lambda () (list->table (read)))))
  (apply execute-hex-files hexs))

(include "../statprof/statprof.scm")
(define (profile) ; profile using picobit
  (time (begin (with-exception-catcher
		;; to have the profiling results even it the compilation fails
		(lambda (x)
		  (profile-stop!)
		  (write-profile-report "profiling-picobit"))
		(lambda ()
		  (profile-start!)
		  (main "tests/picobit/picobit-vm-sixpic.c")
		  (profile-stop!)
		  (write-profile-report "profiling-picobit")))
	       (pp TOTAL:))))
