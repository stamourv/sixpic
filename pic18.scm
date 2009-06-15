;;; File: "pic18.scm"

(define-macro (bitmask encoding . field-values)
  (let loop ((i 0)
             (pos 0)
             (mask 0)
             (fields (list (list #\space 0 0))))
    (if (< i (string-length encoding))
        (let ((c (string-ref encoding i)))
          (cond ((char=? c #\0)
                 (loop (+ i 1)
                       (+ pos 1)
                       (* mask 2)
                       fields))
                ((char=? c #\1)
                 (loop (+ i 1)
                       (+ pos 1)
                       (+ 1 (* mask 2))
                       fields))
                ((char=? c #\space)
                 (loop (+ i 1)
                       pos
                       mask
                       fields))
                (else
                 (if (and (char=? c (car (car fields)))
                          (= pos (caddr (car fields))))
                     (begin
                       (set-car! (cddr (car fields)) (+ pos 1))
                       (loop (+ i 1)
                             (+ pos 1)
                             (* mask 2)
                             fields))
                     (loop (+ i 1)
                           (+ pos 1)
                           (* mask 2)
                           (cons (list c pos (+ pos 1)) fields))))))
        (begin
          (if (not (= pos 16))
              (error "invalid bitmask" encoding))
          (cons '+
                (cons mask
                      (map (lambda (f v)
                             (let* ((width (- (caddr f) (cadr f)))
                                    (shift (- pos (caddr f))))
                               (list 'bitfield
                                     encoding
                                     (string (car f))
                                     (expt 2 width)
                                     shift
                                     v)))
                           (cdr (reverse fields))
                           field-values)))))))

(define (bitfield encoding name limit shift value)
  (if (or (< value 0) (>= value limit))
      (error "value does not fit in field" name value encoding)
      (arithmetic-shift value shift)))

;------------------------------------------------------------------------------

; Byte-oriented file register operations.

(define (addwf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "addwf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0010 01da ffff ffff" (dest d) (access a) (file f))))))

(define (addwfc f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "addwfc" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0010 00da ffff ffff" (dest d) (access a) (file f))))))

(define (andwf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "andwf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0001 01da ffff ffff" (dest d) (access a) (file f))))))

(define (clrf f #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "clrf" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 101a ffff ffff" (access a) (file f))))))

(define (comf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "comf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0001 11da ffff ffff" (dest d) (access a) (file f))))))

(define (cpfseq f #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "cpfseq" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 001a ffff ffff" (access a) (file f))))))

(define (cpfsgt f #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "cpfsgt" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 010a ffff ffff" (access a) (file f))))))

(define (cpfslt f #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "cpfslt" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 000a ffff ffff" (access a) (file f))))))

(define (decf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "decf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0000 01da ffff ffff" (dest d) (access a) (file f))))))

(define (decfsz f #!optional (d 'f) (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "decfsz" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0010 11da ffff ffff" (dest d) (access a) (file f))))))

(define (dcfsnz f #!optional (d 'f) (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "dcfsnz" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0100 11da ffff ffff" (dest d) (access a) (file f))))))

(define (incf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "incf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0010 10da ffff ffff" (dest d) (access a) (file f))))))

(define (incfsz f #!optional (d 'f) (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "incfsz" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0011 11da ffff ffff" (dest d) (access a) (file f))))))

(define (infsnz f #!optional (d 'f) (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "infsnz" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0100 10da ffff ffff" (dest d) (access a) (file f))))))

(define (iorwf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "iorwf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0001 00da ffff ffff" (dest d) (access a) (file f))))))

(define (movf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "movf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0101 00da ffff ffff" (dest d) (access a) (file f))))))

(define (movff fs fd)
  (make-instruction
   2
   (lambda ()
     (make-listing "movff" (file-text fs) (file-text fd)))
   (lambda ()
     (asm-16 (bitmask "1100 ffff ffff ffff" (file-full fs)))
     (asm-16 (bitmask "1111 ffff ffff ffff" (file-full fd))))))

(define (movwf f #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "movwf" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 111a ffff ffff" (access a) (file f))))))

(define (mulwf f #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "mulwf" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0000 001a ffff ffff" (access a) (file f))))))

(define (negf f #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "negf" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 110a ffff ffff" (access a) (file f))))))

(define (rlcf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "rlcf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0011 01da ffff ffff" (dest d) (access a) (file f))))))

(define (rlncf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "rlncf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0100 01da ffff ffff" (dest d) (access a) (file f))))))

(define (rrcf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "rrcf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0011 00da ffff ffff" (dest d) (access a) (file f))))))

(define (rrncf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "rrncf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0100 00da ffff ffff" (dest d) (access a) (file f))))))

(define (setf f #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "setf" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 100a ffff ffff" (access a) (file f))))))

(define (subfwb f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "subfwb" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0101 01da ffff ffff" (dest d) (access a) (file f))))))

(define (subwf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "subwf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0101 11da ffff ffff" (dest d) (access a) (file f))))))

(define (subwfb f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "subwfb" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0101 10da ffff ffff" (dest d) (access a) (file f))))))

(define (swapf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "swapf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0011 10da ffff ffff" (dest d) (access a) (file f))))))

(define (tstfsz f #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "tstfsz" (file-text f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0110 011a ffff ffff" (access a) (file f))))))

(define (xorwf f #!optional (d 'f) (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "xorwf" (file-text f) (dest-text d 'f) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0001 10da ffff ffff" (dest d) (access a) (file f))))))

; Bit-oriented file register operations.

(define (bcf f b #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "bcf" (file-text f) (bit-text b) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "1001 bbba ffff ffff" (bit b) (access a) (file f))))))

(define (bsf f b #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "bsf" (file-text f) (bit-text b) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "1000 bbba ffff ffff" (bit b) (access a) (file f))))))

(define (btfsc f b #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "btfsc" (file-text f) (bit-text b) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "1011 bbba ffff ffff" (bit b) (access a) (file f))))))

(define (btfss f b #!optional (a 'a))
  (make-instruction
   -2
   (lambda ()
     (make-listing "btfss" (file-text f) (bit-text b) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "1010 bbba ffff ffff" (bit b) (access a) (file f))))))

(define (btg f b #!optional (a 'a))
  (make-instruction
   1
   (lambda ()
     (make-listing "btg" (file-text f) (bit-text b) (access-text a 'a)))
   (lambda ()
     (asm-16 (bitmask "0111 bbba ffff ffff" (bit b) (access a) (file f))))))

; Control operations.

(define (bc l)
  (make-short-relative-branch-instruction
   "bc"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0010 nnnn nnnn" dist-8bit)))))

(define (bn l)
  (make-short-relative-branch-instruction
   "bn"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0110 nnnn nnnn" dist-8bit)))))

(define (bnc l)
  (make-short-relative-branch-instruction
   "bnc"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0011 nnnn nnnn" dist-8bit)))))

(define (bnn l)
  (make-short-relative-branch-instruction
   "bnn"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0111 nnnn nnnn" dist-8bit)))))

(define (bnov l)
  (make-short-relative-branch-instruction
   "bnov"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0101 nnnn nnnn" dist-8bit)))))

(define (bnz l)
  (make-short-relative-branch-instruction
   "bnz"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0001 nnnn nnnn" dist-8bit)))))

(define (bov l)
  (make-short-relative-branch-instruction
   "bov"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0100 nnnn nnnn" dist-8bit)))))

;; (define (bra l)
;;   (make-long-relative-branch-instruction
;;    "bra"
;;    l
;;    (lambda (dist-11bit)
;;      (asm-16 (bitmask "1101 0nnn nnnn nnnn" dist-11bit)))))

(define (goto l)
  (make-long-absolute-branch-instruction
   "goto"
   l
   (lambda (pos-20bit)
     (asm-16 (bitmask "1110 1111 nnnn nnnn" (modulo pos-20bit (expt 2 8))))
     (asm-16 (bitmask "1111 nnnn nnnn nnnn" (quotient pos-20bit (expt 2 8)))))))

(define (bra-or-goto l)
  (make-long-relative-or-absolute-branch-instruction
   "bra"
   "goto"
   l
   (lambda (dist-11bit)
     (asm-16 (bitmask "1101 0nnn nnnn nnnn" dist-11bit)))
   (lambda (pos-20bit)
     (asm-16 (bitmask "1110 1111 nnnn nnnn" (modulo pos-20bit (expt 2 8))))
     (asm-16 (bitmask "1111 nnnn nnnn nnnn" (quotient pos-20bit (expt 2 8)))))))

(define (bz l)
  (make-short-relative-branch-instruction
   "bz"
   l
   (lambda (dist-8bit)
     (asm-16 (bitmask "1110 0000 nnnn nnnn" dist-8bit)))))

(define (call l #!optional (s 0))
  (make-instruction
   2
   (lambda ()
     (make-listing "call" (label-text l) (lit-text s)))
   (lambda ()
     (asm-at-assembly
      (lambda (self)
        4)
      (lambda (self)
        (let ((pos-div-2 (quotient (label-pos l) 2)))
          (asm-16 (bitmask "1110 110s kkkk kkkk" s (quotient pos-div-2 4096)))
          (asm-16 (bitmask "1111 kkkk kkkk kkkk" (modulo pos-div-2 4096)))))))))

(define (clrwdt)
  (make-instruction
   1
   (lambda ()
     (make-listing "clrwdt"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0100")))))

(define (daw)
  (make-instruction
   1
   (lambda ()
     (make-listing "daw"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0111")))))

(define (nop)
  (make-instruction
   1
   (lambda ()
     (make-listing "nop"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0000")))))

(define (pop)
  (make-instruction
   1
   (lambda ()
     (make-listing "pop"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0110")))))

(define (push)
  (make-instruction
   1
   (lambda ()
     (make-listing "push"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0101")))))

(define (rcall l)
  (make-long-relative-branch-instruction
   "rcall"
   l
   (lambda (dist-11bit)
     (asm-16 (bitmask "1101 1nnn nnnn nnnn" dist-11bit)))))

(define (rcall-or-call l)
  (make-long-relative-or-absolute-branch-instruction
   "rcall"
   "call"
   l
   (lambda (dist-11bit)
     (asm-16 (bitmask "1101 1nnn nnnn nnnn" dist-11bit)))
   (lambda (pos-20bit)
     (asm-16 (bitmask "1110 1100 nnnn nnnn" (modulo pos-20bit (expt 2 8))))
     (asm-16 (bitmask "1111 nnnn nnnn nnnn" (quotient pos-20bit (expt 2 8)))))))

(define (reset)
  (make-instruction
   1
   (lambda ()
     (make-listing "reset"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 1111 1111")))))

(define (retfie #!optional (s 0))
  (make-instruction
   2
   (lambda ()
     (make-listing "retfie" (lit-text s)))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0001 000s" s)))))

(define (return #!optional (s 0))
  (make-instruction
   2
   (lambda ()
     (make-listing "return" (lit-text s)))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0001 001s" s)))))

(define (sleep)
  (make-instruction
   1
   (lambda ()
     (make-listing "sleep"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 0011")))))

(define (make-short-relative-branch-instruction mnemonic l generate)
  (make-instruction
   -1
   (lambda ()
     (make-listing mnemonic (label-text l)))
   (lambda ()
     (asm-at-assembly
      (lambda (self)
	2)
      (lambda (self)
        (let ((dist (- (label-pos l) (+ self 2))))
          (if (and (>= dist -256)
                   (<= dist 255)
                   (even? dist))
              (generate (modulo (quotient dist 2) 256))
              (error "short relative branch target is too far or improperly aligned" l dist))))))))

(define (make-long-relative-branch-instruction mnemonic l generate)
  (make-instruction
   -1
   (lambda ()
     (make-listing mnemonic (label-text l)))
   (lambda ()
     (asm-at-assembly
      (lambda (self)
        2)
      (lambda (self)
        (let ((dist (- (label-pos l) (+ self 2))))
          (if (and (>= dist -2048)
                   (<= dist 2047)
                   (even? dist))
              (generate (modulo (quotient dist 2) 2048))
              (error "long relative branch target is too far or improperly aligned" l dist))))))))

(define (make-long-absolute-branch-instruction mnemonic l generate)
  (make-instruction
   -1
   (lambda ()
     (make-listing mnemonic (label-text l)))
   (lambda ()
     (asm-at-assembly
      (lambda (self)
	4)
      (lambda (self)
	(let ((pos (label-pos l)))
	  (if (and (< pos (expt 2 21))
                   (even? pos))
              (generate (quotient pos 2))
              (error "goto branch target is too far or unaligned" l pos))))))))

(define (make-long-relative-or-absolute-branch-instruction mnemonic1 mnemonic2 l generate1 generate2)
  (make-instruction
   -1
   (lambda ()
     (make-listing mnemonic1 (label-text l))) ;; TODO should show mnemonic1 when it's used, or mnemonic2
   (lambda ()
     (asm-at-assembly

      (lambda (self)
        (let ((dist (- (label-pos l) (+ self 2))))
          (if (and (>= dist -2048)
                   (<= dist 2047)
                   (even? dist))
              2
              #f)))
      (lambda (self)
	(let ((dist (- (label-pos l) (+ self 2))))
	  (generate1 (modulo (quotient dist 2) 2048))))
      
      (lambda (self)
        4)
      (lambda (self)
        (let ((pos (label-pos l)))
          (if (and (< pos (expt 2 21))
                   (even? pos))
              (generate2 (quotient pos 2))
              (error "goto branch target is too far or unaligned" l pos))))))))

; Literal operations.

(define (addlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "addlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1111 kkkk kkkk" (lit k))))))

(define (andlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "andlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1011 kkkk kkkk" (lit k))))))

(define (iorlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "iorlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1001 kkkk kkkk" (lit k))))))

(define (lfsr f k)
  (make-instruction
   2
   (lambda ()
     (make-listing "lfsr" (file-text f) (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "1110 1110 00ff kkkk" (file f) (quotient (lit k) 256)))
     (asm-16 (bitmask "1111 0000 kkkk kkkk" (modulo (lit k) 256))))))

(define (movlb k)
  (make-instruction
   1
   (lambda ()
     (make-listing "movlb" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 0001 0000 kkkk" (lit k))))))

(define (movlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "movlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1110 kkkk kkkk" (lit k))))))

(define (mullw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "mullw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1101 kkkk kkkk" (lit k))))))

(define (retlw k)
  (make-instruction
   2
   (lambda ()
     (make-listing "retlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1100 kkkk kkkk" (lit k))))))

(define (sublw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "sublw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1000 kkkk kkkk" (lit k))))))

(define (xorlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "xorlw" (lit-text k)))
   (lambda ()
     (asm-16 (bitmask "0000 1010 kkkk kkkk" (lit k))))))

; Data memory program memory operations.

(define (tblrd*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1000")))))

(define (tblrd*+)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*+"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1001")))))

(define (tblrd*-)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*-"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1010")))))

(define (tblrd+*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd+*"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1011")))))

(define (tblwt*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1100")))))

(define (tblwt*+)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*+"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1101")))))

(define (tblwt*-)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*-"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1110")))))

(define (tblwt+*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt+*"))
   (lambda ()
     (asm-16 (bitmask "0000 0000 0000 1111")))))

;------------------------------------------------------------------------------



#|

(define (andlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "andlw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000101100000000 (lit8 k))))))

(define (iorlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "iorlw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000100100000000 (lit8 k))))))

(define (lfsr f k)
  (make-instruction
   2
   (lambda ()
     (make-listing "lfsr" (lit-text f) "," (lit-text k)))
   (lambda ()
     (asm-16 (+ #b1110111000000000 (* (lit2 f) 16) (quotient (lit12 k) 256)))
     (asm-16 (+ #b1111000000000000 (modulo (lit12 k) 256))))))

(define (movlb k)
  (make-instruction
   1
   (lambda ()
     (make-listing "movlb" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000000100000000 (lit4 k))))))

(define (movlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "movlw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000111000000000 (lit8 k))))))

(define (mullw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "mullw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000110100000000 (lit8 k))))))

(define (retlw k)
  (make-instruction
   2
   (lambda ()
     (make-listing "retlw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000110000000000 (lit8 k))))))

(define (sublw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "sublw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000100000000000 (lit8 k))))))

(define (xorlw k)
  (make-instruction
   1
   (lambda ()
     (make-listing "xorlw" (lit-text k)))
   (lambda ()
     (asm-16 (+ #b0000101000000000 (lit8 k))))))

(define (tblrd*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*"))
   (lambda ()
     (asm-16 #b0000000000001000))))

(define (tblrd*+)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*+"))
   (lambda ()
     (asm-16 #b0000000000001001))))

(define (tblrd*-)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd*-"))
   (lambda ()
     (asm-16 #b0000000000001010))))

(define (tblrd+*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblrd+*"))
   (lambda ()
     (asm-16 #b0000000000001011))))

(define (tblwt*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*"))
   (lambda ()
     (asm-16 #b0000000000001100))))

(define (tblwt*+)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*+"))
   (lambda ()
     (asm-16 #b0000000000001101))))

(define (tblwt*-)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt*-"))
   (lambda ()
     (asm-16 #b0000000000001110))))

(define (tblwt+*)
  (make-instruction
   2
   (lambda ()
     (make-listing "tblwt+*"))
   (lambda ()
     (asm-16 #b0000000000001111))))

(define (lit2 n)
  (if (and (>= n 0) (<= n 3))
      n
      (error "2 bit literal expected but got" n)))

(define (lit8 n)
  (if (and (>= n 0) (<= n 255))
      n
      (error "8 bit literal expected but got" n)))

(define (lit12 n)
  (if (and (>= n 0) (<= n 2047))
      n
      (error "12 bit literal expected but got" n)))

|#

(define (make-instruction cycles listing-thunk code-thunk)
  (code-thunk)
  (listing-thunk))

(define (make-listing mnemonic . operands)

  (define (operand-list operands)
    (if (null? operands)
        ""
        (let ((rest (operand-list (cdr operands))))
          (string-append (car operands)
                         (if (string=? rest "")
                             ""
                             (string-append ", " rest))))))

  (asm-listing
   (list "    "
         mnemonic
         (make-string (- 8 (string-length mnemonic)) #\space)
         (operand-list operands))))

(define (dest d)
  (cond ((eq? d 'w) 0)
        ((eq? d 'f) 1)
        (else       (error "destination bit must be w or f"))))

(define (dest-text d default)
  (cond ((eq? d default) "")
        ((eq? d 'w) "w")
        ((eq? d 'f) "f")
        (else       (error "destination bit must be w or f"))))

(define (access a)
  (cond ((eq? a 'a) 0)
        ((eq? a 'b) 1)
        (else       (error "access bit must be a or b"))))

(define (access-text a default)
  (cond ((eq? a default) "")
        ((eq? a 'a) "a")
        ((eq? a 'b) "b")
        (else       (error "access bit must be a or b"))))

(define (lit k)
  k)

(define (lit-text k)

  (define (text k)
    (if (<= k 10)
        (number->string k)
        (string-append "0x" (number->string k 16))))

  (if (< k 0)
      (string-append "-" (text (abs k)))
      (text k)))

(define (bit b)
  b)

(define (bit-text b)
  (lit-text b))

(define (file f)
  (if (or (>= f #xf80) (< #x080))
      (modulo f #x100)
      (error "illegal file register")))

(define (file-full f)
  f)

(define (file-text f)
  (let ((x (assv f file-reg-names)))
    (if x
	(symbol->string (cdr x))
	(let ((x (table-ref register-table f #f)))
	  (if #f ;x
	      (apply string-append-with-separator (cons "/" x)) ;; TODO unreadable with picobit
	      (lit-text f))))))

(define (label-text label)
  (if (number? label)
      (string-append "0x" (number->string label 16))
      (symbol->string (asm-label-id label))))

(define (label-pos label)
  (if (number? label)
      label
      (asm-label-pos label)))

;------------------------------------------------------------------------------

(define TOSU     #xfff)
(define TOSH     #xffe)
(define TOSL     #xffd)
(define STKPTR   #xffc)
(define PCLATU   #xffb)
(define PCLATH   #xffa)
(define PCL      #xff9)
(define TBLPTRU  #xff8)
(define TBLPTRH  #xff7)
(define TBLPTRL  #xff6)
(define TABLAT   #xff5)
(define PRODH    #xff4)
(define PRODL    #xff3)
(define INDF0    #xfef)
(define POSTINC0 #xfee)
(define POSTDEC0 #xfed)
(define PREINC0  #xfec)
(define PLUSW0   #xfeb)
(define FSR0H    #xfea)
(define FSR0L    #xfe9)
(define WREG     #xfe8)
(define INDF1    #xfe7)
(define POSTINC1 #xfe6)
(define POSTDEC1 #xfe5)
(define PREINC1  #xfe4)
(define PLUSW1   #xfe3)
(define FSR1H    #xfe2)
(define FSR1L    #xfe1)
(define BSR      #xfe0)
(define INDF2    #xfdf)
(define POSTINC2 #xfde)
(define POSTDEC2 #xfdd)
(define PREINC2  #xfdc)
(define PLUSW2   #xfdb)
(define FSR2H    #xfda)
(define FSR2L    #xfd9)
(define STATUS   #xfd8)
(define TMR1H    #xfcf)
(define TMR1L    #xfce)
(define PORTE    #xf84)
(define PORTD    #xf83)
(define PORTC    #xf82)
(define PORTB    #xf81)
(define PORTA    #xf80)

(define file-reg-names '(
  (#xfff . TOSU)
  (#xffe . TOSH)
  (#xffd . TOSL)
  (#xffc . STKPTR)
  (#xffb . PCLATU)
  (#xffa . PCLATH)
  (#xff9 . PCL)
  (#xff8 . TBLPTRU)
  (#xff7 . TBLPTRH)
  (#xff6 . TBLPTRL)
  (#xff5 . TABLAT)
  (#xff4 . PRODH)
  (#xff3 . PRODL)
  (#xfef . INDF0)
  (#xfee . POSTINC0)
  (#xfed . POSTDEC0)
  (#xfec . PREINC0)
  (#xfeb . PLUSW0)
  (#xfea . FSR0H)
  (#xfe9 . FSR0L)
  (#xfe8 . WREG)
  (#xfe7 . INDF1)
  (#xfe6 . POSTINC1)
  (#xfe5 . POSTDEC1)
  (#xfe4 . PREINC1)
  (#xfe3 . PLUSW1)
  (#xfe2 . FSR1H)
  (#xfe1 . FSR1L)
  (#xfe0 . BSR)
  (#xfdf . INDF2)
  (#xfde . POSTINC2)
  (#xfdd . POSTDEC2)
  (#xfdc . PREINC2)
  (#xfdb . PLUSW2)
  (#xfda . FSR2H)
  (#xfd9 . FSR2L)
  (#xfd8 . STATUS)
  (#xfd7 . TMR0H)
  (#xfd6 . TMR0L)
  (#xfd5 . T0CON)
  (#xfd3 . OSCCON)
  (#xfcf . TMR1H)
  (#xfce . TMR1L)
  (#xfcd . T1CON)
  (#xfc1 . ADCON1)
  (#xfdb . PLUSW2)
  (#xfa1 . PIR2)
  (#xfa0 . PIE2)
  (#xf9e . PIR1)
  (#xf9d . PIE1)
  (#xf93 . TRISB)
  (#xf92 . TRISA)
  (#xf8a . LATB)
  (#xf89 . LATA)
  (#xf84 . PORTE)
  (#xf83 . PORTD)
  (#xf82 . PORTC)
  (#xf81 . PORTB)
  (#xf80 . PORTA)
  (#xf7f . UEP15)
  (#xf7e . UEP14)
  (#xf7d . UEP13)
  (#xf7c . UEP12)
  (#xf7b . UEP11)
  (#xf7a . UEP10)
  (#xf79 . UEP9)
  (#xf78 . UEP8)
  (#xf77 . UEP7)
  (#xf76 . UEP6)
  (#xf75 . UEP5)
  (#xf74 . UEP4)
  (#xf73 . UEP3)
  (#xf72 . UEP2)
  (#xf71 . UEP1)
  (#xf70 . UEP0)
  (#xf6f . UCFG)
  (#xf6e . UADDR)
  (#xf6d . UCON)
  (#xf6c . USTAT)
  (#xf6b . UEIE)
  (#xf6a . UEIR)
  (#xf69 . UIE)
  (#xf68 . UIR)
  (#xf67 . UFRMH)
  (#xf66 . UFRML)
  (#xf65 . SPPCON)
  (#xf64 . SPPEPS)
  (#xf63 . SPPCFG)
  (#xf62 . SPPDATA)
  ))

(define C  0)
(define DC 1)
(define Z  2)
(define OV 3)
(define N  4)

;------------------------------------------------------------------------------

(define (label-offset-reference label offset)
  (asm-at-assembly
    (lambda (self)
      2)
    (lambda (self)
      (asm-16 (+ (asm-label-pos label) offset)))))

(define (label-instr label opcode)
  (asm-at-assembly
    (lambda (self)
      2)
    (lambda (self)
      (let ((pos (asm-label-pos label)))
	(asm-8 (+ (quotient pos 256) opcode))
	(asm-8 (modulo pos 256))))))

;------------------------------------------------------------------------------

(define irda_send_newline               #x0078)
(define irda_send                       #x007E)
(define irda_recv_with_1_sec_timeout    #x00A2)
(define irda_recv                       #x00A4)
(define sec_sleep                       #x00B0)
(define msec_sleep                      #x00B6)
(define delay_7                         #x00D4)
(define led_set                         #x00D6)
(define bit_set                         #x00EC)
(define FLASH_execute_erase             #x0106)
(define FLASH_execute_write             #x0108)
(define parse_hex_byte                  #x0184)
(define parse_hex_digit                 #x0194)
(define irda_send_hex                   #x01AE)
(define irda_send_nibble                #x01B6)

;------------------------------------------------------------------------------
