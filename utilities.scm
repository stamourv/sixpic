;;; utilities

(define (interval n m) ; returns the list (n n+1 n+2 ... m)
  (if (<= n m) (cons n (interval (+ n 1) m)) '()))

(define (keep f lst)
  (cond ((null? lst)   '())
        ((f (car lst)) (cons (car lst) (keep f (cdr lst))))
        (else          (keep f (cdr lst)))))

(define (set-equal? s1 s2)
  (and (eq? (diff s1 s2) '())
       (eq? (diff s2 s1) '())))

(define (diff s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (diff (cdr s1) s2))
        (else               (cons (car s1) (diff (cdr s1) s2)))))

(define (intersection s1 s2)
  (cond ((null? s1)         '())
        ((memq (car s1) s2) (cons (car s1) (intersection (cdr s1) s2)))
        (else               (intersection (cdr s1) s2))))

(define (union s1 s2)
  (cond ((null? s1)         s2)
        ((memq (car s1) s2) (union (cdr s1) s2))
        (else               (cons (car s1) (union (cdr s1) s2)))))

(define (union-multi sets) (foldl union '() sets))

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
  (cond ((null? lst)
         '())
        ((eq? x (car lst))
         (cdr lst))
        (else
         (cons (car lst)
               (remove x (cdr lst))))))

(define (replace x y lst)
  (cond ((null? lst)
	 '())
	((eq? x (car lst))
	 (cons y (cdr lst)))
	(else
	 (cons (car lst)
	       (replace x y (cdr lst))))))
