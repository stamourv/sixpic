;;; utilities

(define (list-set! l i x)
  (cond ((null? l) (error "list-set!: out of bounds"))
	  ((= i 0)   (set-car! l x))
	  (else      (list-set! (cdr l) (- i 1) x))))

(define (interval n m) ; returns the list (n n+1 n+2 ... m)
  (if (<= n m) (cons n (interval (+ n 1) m)) '()))

(define (iota n) (interval 0 (- n 1)))

(define (keep f lst) ;; TODO call filter ?
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

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
(define (set-union-multi sets) (foldl set-union (new-empty-set) sets))
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
