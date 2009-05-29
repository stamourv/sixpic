;; basic profiler for SIXPIC's pic18 simulator
;; basically, just reports the execution count for each instruction that is
;; kept by the simulator

;; this is mostly taken from Guillaume Germain's statprof statistical profiler
;; for Gambit-C, hence is distributed with the same licence as Gambit-C

;; TODO this is not yet working, it has not been completely adapted. a temporary solution is being used instead

;; ----------------------------------------------------------------------------
;; Text formatting

(define (pad-left s l c)
  (let loop ((s (string->list s)))
    (if (< (length s) l)
        (loop (cons c s))
        (list->string s))))


;; ----------------------------------------------------------------------------
;; Palette generation & color formatting

(define (gradient from to step)
  (let ((inc (map (lambda (x) (/ x step))
                  (map - to from))))
    
    (let loop ((i 0)
               (acc '()))
      (if (= i step) 
          (reverse acc)
          (loop (+ i 1)
                (cons (map 
                       (lambda (x o) 
                         (round (+ x (* i o))))
                       from
                       inc)
                      acc))))))

(define (as-rgb col)
  (apply string-append
         (map
          (lambda (x)
            (pad-left (number->string x 16) 2 #\0))
          col)))

(define palette
  (list->vector 
   (cons '(255 255 255) 
         (gradient '(127 127 255) 
                   '(255 127 127)
                   16))))


;; ----------------------------------------------------------------------------
;; Functions to generate the report

(define (write-profile-report profile-name . sources)

  (define (iota1 n)
    (let loop ((n n)
               (l '()))
      (if (>= n 1) 
          (loop (- n 1) (cons n l))
          l)))
  
  (define directory-name (string-append (current-directory)
                                        profile-name
                                        "/"))
  (with-exception-catcher
   (lambda (e)
     ;; ignore the exception, it probably means that the directory
     ;; already existed.  If there's another problem it will be
     ;; signaled later.
     #f) 
   (lambda ()
     (create-directory (list path: directory-name
                             permissions: #o755))))
  
  (let ((max-intensity 0))
    (for-each (lambda (x) (let ((new (vector-ref instrs-counts x)))
			    (if (> new max-intensity)
				(set! max-intensity new))))
	      (iota (vector-length instrs-counts)))
    (map
     (lambda (adr)
       (let ((file (car bucket)) ;; TODO know which file we're coming from
             (data (cdr bucket)))
       
         (define (get-color n)
           (let ((i (vector-ref data n)))
             (if (= i 0)
                 (as-rgb (vector-ref palette 0))
                 (let ((x (* (/ (log (+ 1. i))
                                (max (ceiling (log max-intensity)) 1))
                             (- (vector-length palette) 1))))
                   (as-rgb (vector-ref palette 
                                       (inexact->exact (ceiling x))))))))

         (with-output-to-file (string-append 
                               directory-name
                               (path-strip-directory file)
                               ".html")
           (let ((lines (call-with-input-file file 
                          (lambda (p) (read-all p read-line)))))
             (lambda ()
               (print
                (sexp->html
                 `(html 
                   (body
                    (table 
                     cellspacing: 0 
                     cellpadding: 0
                     border: 0
                     style: "font-size: 12px;"
                     ,@(map
                        (lambda (line line#)
                          `(tr 
                            (td ,(string-append 
                                  (number->string line#)
                                  ": "))
                            ;; (td 
                            ;;  align: center
                            ;;  ,(let ((n (vector-ref data line#)))
                            ;;     (if (= n 0)
                            ;;         ""
                            ;;         (string-append "[" 
                            ;;                        (number->string n)
                            ;;                        "/"
                            ;;                        (number->string *total*)
                            ;;                        "]"))))
                            
                            (td 
                             align: center
                             ,(let ((n (vector-ref data line#)))
                                (if (= n 0)
                                    ""
                                    (string-append
                                     (number->string
                                      (round% (/ n *total*)))
                                     "% "))))
                               
                            (td (pre style: ,(string-append     
                                              "background-color:#"
                                              (get-color line#))
                                     ,line))))
                        lines
                        (iota1 (length lines)))))))))))))
     
     (iota (vector-length instrs-counts))))

  (with-output-to-file (string-append directory-name "index.html")
    (lambda ()
      (print
       (sexp->html
        `(html
          (body
           ,@(map (lambda (bucket)
                    (let ((file-path (string-append 
                                      directory-name
                                      (path-strip-directory (car bucket)) 
                                      ".html")))
                      `(p (a href: ,file-path ,file-path)
                          " ["
                          ,(round%
                            (/ (apply + (vector->list (cdr bucket)))
                               *total*)) 
                          " %]")))
                  *buckets*))))))))

(define (round% n)
  (/ (round
      (* 10000 n))
     100.))


;; ----------------------------------------------------------------------------
;; Included file "html.scm"
;; ----------------------------------------------------------------------------

;; html.scm -- A simple html generator for Gambit-C 4.0

;; Written by Guillaume Germain (germaing@iro.umontreal.ca)
;; This code is released in the public domain.


(define (stringify x)
  (with-output-to-string ""
    (lambda ()
      (print x))))

(define (to-escaped-string x)
  (stringify 
   (map (lambda (c)
          (case c
            ((#\<) "&lt;")
            ((#\>) "&gt;")
            ((#\&) "&amp;")
            (else c)))
        (string->list 
         (stringify x)))))

;; Quick and dirty conversion of s-expressions to html
(define (sexp->html exp)
  
  ;; write the opening tag
  (define (open-tag exp)
    (cond
     ;; null tag isn't valid
     ((null? exp) 
      (error "null tag"))
     
     ;; a tag must be a list beginning with a symbol
     ((and (pair? exp)
           (symbol? (car exp)))
      (list "<" 
            (car exp)
            " " 
            (maybe-args (car exp) (cdr exp))))
     
     (else
      (error "invalid tag" exp))))

  ;; take care of the keywords / arguments
  (define (maybe-args tag exp)

    (cond
     ;; does the rest of the list begins with a keyword
     ((and (pair? exp)
           (keyword? (car exp)))

      ;; does the keyword has an associated value?
      (if (or (null? (cdr exp))
              (keyword? (cadr exp)))
          ;; no, we don't put an associated value
          (list (keyword->string (car exp))
                " "
                (maybe-args tag (cdr exp)))
          ;; yes, we take the next element in the list as the value
          (list (keyword->string (car exp))
                "=\""
                (cadr exp)
                "\" "
                (maybe-args tag (cddr exp)))))

     ;; must just be some content
     (else
      (content tag exp))))

  ;; handle the content of the tag and closing it
  (define (content tag exp)
    (cond
     ;; no content...
     ((null? exp)
      ;;(list "></" tag ">"))           ; close as "<br></br>"
      (list "/>"))                      ; close as "<br/>"

     ;; write the content, handle tags inside
     ((pair? exp)
      (list ">"
            (map (lambda (e)
                   (if (pair? e)
                       (open-tag e)
                       (to-escaped-string e)))
                 exp)
            "</"
            tag
            ">"))

     ;; non-null terminated list?
     (else
      (error "strange content..."))))

  (with-output-to-string ""
                         (lambda ()
                           (print (open-tag exp)))))
