;;; utilities

(define (list-set! l i x)
  (cond ((null? l) (error "list-set!: out of bounds"))
	  ((= i 0)   (set-car! l x))
	  (else      (list-set! (cdr l) (- i 1) x))))

(define (interval n m) ; returns the list (n n+1 n+2 ... m)
  (if (<= n m) (cons n (interval (+ n 1) m)) '()))

(define (iota n) (interval 0 (- n 1)))

(define (filter f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
        (else          (filter f (cdr lst)))))

(define (identity x) x)


;; sets using hash tables
(define (new-empty-set) (make-table hash: eq?-hash test: eq?))
(define (new-set x)
  (let ((s (new-empty-set)))
    (table-set! s x #t)
    s))
(define (set-member? s x) (table-ref s x #f))
(define (set-length s) (table-length s))
(define (set-equal? s1 s2) (equal? s1 s2))
(define (set-diff s1 s2)
  (let ((s (table-copy s1)))
    (table-for-each (lambda (key val) (table-set! s key))
		    s2)
    s))
(define (set-intersection s1 s2)
  (define (inters s1 s2)
    (let ((t (table-copy s1)))
      (table-for-each (lambda (k v) (if (not (table-ref s2 k #f))
					(table-set! t k)))
		      s1)
      t))
  (if (< (table-length s1) (table-length s2))
      (inters s1 s2)
      (inters s2 s1)))
(define (set-union s1 s2)
  (if (> (table-length s1) (table-length s2))
      (table-merge s1 s2)
      (table-merge s2 s1)))
(define (set-union! s1 s2) (table-merge! s1 s2)) ; side-effects s1
(define (set-union-multi sets)
  (if (null? sets)
      (new-empty-set)
      (let loop ((l (cdr sets))
		 (s (set-copy (car sets))))
	(if (null? l)
	    s
	    (let ((s2 (car l)))
	      (if (> (set-length s) (set-length s2))
		  (begin (set-union! s s2)
			 (loop (cdr l) s))
		  (let ((s2 (set-copy s2)))
		    (set-union! s2 s)
		    (loop (cdr l) s2))))))))
(define (set-add s1 x)
  (let ((s2 (table-copy s1)))
    (table-set! s2 x #t)
    s2))
(define (set-add! s x) (table-set! s x #t)) ; faster, but side-effecting
(define (set-remove! s x) (table-set! s x))
(define (set-empty? s) (= (table-length s) 0))
(define (list->set l) (list->table (map (lambda (x) (cons x #t)) l)))
(define (set->list s) (map car (table->list s)))
(define (set-filter p s1)
  (let ((s2 (new-empty-set)))
    (table-for-each (lambda (key value)
		      (if value
			  (table-set! s2 key #t)))
		    s1)
    s2))
(define (set-for-each f s) (table-for-each (lambda (x dummy) (f x)) s))
(define (set-subset? s1 s2) ; is s2 a subset of s1 ?
  (if (> (set-length s2) (set-length s1))
      #f
      (let loop ((l (set->list s2)))
	(cond ((null? l)
	       #t)
	      ((set-member? s1 (car l))
	       (loop (cdr l)))
	      (else
	       #f)))))
(define set-copy table-copy)

(define (foldl f base lst)
  (if (null? lst)
      base
      (foldl f (f base (car lst)) (cdr lst))))

(define (pos-in-list x lst)
  (let loop ((lst lst) (i 0))
    (cond ((not (pair? lst)) #f)
          ((eq? (car lst) x) i)
          (else              (loop (cdr lst) (+ i 1))))))

(define (remove x lst)
  (cond ((null? lst)       '())
        ((eq? x (car lst)) (cdr lst))
        (else              (cons (car lst)
				 (remove x (cdr lst))))))

(define (replace x y lst)
  (cond ((null? lst)       '())
	((eq? x (car lst)) (cons y (cdr lst)))
	(else              (cons (car lst)
				 (replace x y (cdr lst))))))

(define (last lst)
  (cond ((null? lst)       #f)
	((null? (cdr lst)) (car lst))
	(else              (last (cdr lst)))))

(define (all-but-last lst)
  (let loop ((lst lst)
	     (new '()))
    (cond ((null? lst)       #f)
	  ((null? (cdr lst)) (reverse new))
	  (else              (loop (cdr lst)
				   (cons (car lst) new))))))

(define (memp p l)
  (cond ((null? l)   #f)
	((p (car l)) l)
	(else        (memp p (cdr l)))))

(define (intersperse x l)
  (cond ((or (null? l) (null? (cdr l))) l)
	(else (cons (car l) (cons x (intersperse x (cdr l)))))))
(define (unique l)
  (if (null? l)
      l
      (let ((head (car l))
	    (rest (unique (cdr l))))
	(if (member head rest)
	    rest
	    (cons head rest)))))
(define (string-append-with-separator sep . strings)
  (apply string-append (intersperse sep (unique strings))))

(define (split-string s delimiter) ; delimiter is a char
  (let loop ((s   (string->list s))
	     (acc '())
	     (res '()))
    (cond ((null? s)
	   (reverse (map (lambda (x) (list->string (reverse x)))
			 (if (null? acc) res (cons acc res)))))
	  ((eq? (car s) delimiter)
	   (loop (cdr s)
		 '()
		 (cons acc res)))
	  (else
	   (loop (cdr s)
		 (cons (car s) acc)
		 res)))))


(declare
  (standard-bindings)
  (block)
  (fixnum)
;;   (not safe)
)

(define (make-bitset n)
   (let ((len (fxarithmetic-shift-right (+ n 7) 3)))
     (make-u8vector len)))

(define (bitset-add! bs i)
   (let* ((j (fxarithmetic-shift-right i 3))
          (k (fxand i 7)))
     (u8vector-set! bs
                    j
                    (fxior (u8vector-ref bs j)
                           (fxarithmetic-shift-left 1 k)))))
(define (bitset-remove! bs i)
  (let* ((j (fxarithmetic-shift-right i 3))
	 (k (fxand i 7)))
    (u8vector-set! bs
		   j
		   (fxand (u8vector-ref bs j)
			  (fxnot (fxarithmetic-shift-left 1 k))))))

(define (bitset-member? bs i)
   (let* ((j (fxarithmetic-shift-right i 3))
          (k (fxand i 7)))
     (not (fx= 0 (fxand (u8vector-ref bs j)
                        (fxarithmetic-shift-left 1 k))))))

(define (bitset-intersection b1 b2)
  (let* ((l  (u8vector-length b1)) ; both should have the same length
	 (b3 (make-u8vector l 0))) ;; TODO abstract with diff and union!
    (let loop ((l (- l 1)))
      (if (>= l 0)
	  (begin (u8vector-set! b3 l (fxand (u8vector-ref b1 l)
					    (u8vector-ref b2 l)))
		 (loop (- l 1)))
	  b3))))

(define (bitset-diff b1 b2)
  (let* ((l  (u8vector-length b1)) ; both should have the same length
	 (b3 (make-u8vector l 0)))
    (let loop ((l (- l 1)))
      (if (>= l 0)
	  (begin (u8vector-set! b3 l (fxand (u8vector-ref b1 l)
					    (fxnot (u8vector-ref b2 l))))
		 (loop (- l 1)))
	  b3))))

(define (bitset-union! b1 b2)
  (let* ((l (u8vector-length b1))) ; both should have the same length
    (let loop ((l (- l 1)))
      (if (>= l 0)
	  (begin (u8vector-set! b1 l (fxior (u8vector-ref b1 l)
					    (u8vector-ref b2 l)))
		 (loop (- l 1)))
	  b1))))

(define (bitset-empty? bs)
  (let loop ((l (- (u8vector-length bs) 1)))
    (cond ((< l 0)
	   #t)
	  ((= (u8vector-ref bs l) 0)
	   (loop (- l 1)))
	  (else #f))))

(define (bitset-length bs)
  (let loop ((l (- (u8vector-length bs) 1))
	     (n 0))
    (if (< l 0)
	n
	(loop
	 (- l 1)
	 (+ n
	    (let ((b (u8vector-ref bs l)))
	      (let loop2 ((i 0) (n 0)) ;; TODO is there a better way ?
		(if (> i 7)
		    n
		    (loop2 (+ i 1)
			   (+ n
			      (if (= (fxand (fxarithmetic-shift-left 1 i) b) 0)
				  0
				  1)))))))))))

(define (list->bitset n lst)
   (let ((bs (make-bitset n)))
     (let loop ((lst lst))
       (if (pair? lst)
           (let ((i (car lst)))
             (bitset-add! bs i)
             (loop (cdr lst)))
           bs))))

(define (bitset->list bs)
   (let ((n (fxarithmetic-shift-left (u8vector-length bs) 3)))
     (let loop ((i (- n 1)) (lst '()))
       (if (>= i 0)
           (loop (- i 1)
                 (if (bitset-member? bs i)
                     (cons i lst)
                     lst))
           lst))))

(define bitset-copy u8vector-copy)

(define (bitset-union-multi n bitsets) ; n is necessary is bitsets is null
  (let ((bs (make-bitset n)))
    (let loop ((l bitsets))
      (if (null? l)
	  bs
	  (begin (bitset-union! bs (car l))
		 (loop (cdr l)))))))

(define (bitset-subset? b1 b2) ; is b2 a subset of b1 ?
  (equal? (bitset-intersection b1 b2) b2))
