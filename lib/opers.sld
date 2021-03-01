(define-library (opers)
  (import (chibi) (chibi match))
  (export lnot mk-& bk-& land mk-/ bk-/ lior mk-= bk-= leql
          mk-> bk-> mk-E bk-E mk-V bk-V)
  (begin
    ;; This library is rather important.  Scheme has both
    ;; short-circuiting operators and considers symbols to
    ;; to be "truthy".  This combination means that an
    ;; implementation cannot reuse builtin operators.

    (define (lnot p)
      (match p
        ('T 'B )
        ('B 'T )
        (else `(~ ,p) )) )

    (define (mk-& p q) `(& ,p ,q) )
    (define (bk-& f)
      (match f (('& p q) `(,p ,q) ) (_ (error "bk-& hates you") )) )
    (define (conjuncts x)
      (match x (('& p q) (append (conjuncts p) (conjuncts q)) ) (_ `(,x) )) )
    (define (land . l)
      (match l
        (('T 'T) 'T )
        ((or ('B a) (a 'B)) 'B )
        ((or ('T a) (a 'T) (a a)) a )
        ((a b) `(& ,a ,b) )) )

    (define (mk-/ p q) `(/ ,p ,q) )
    (define (bk-/ f)
      (match f (('/ p q) `(,p ,q) ) (_ (error "bk-/ hates you") )) )
    (define (disjuncts x)
      (match x (('/ p q) (append (disjuncts p) (disjuncts q)) ) (_ `(,x) )) )
    (define (lior . l)
      (match l
        (('B 'B) 'B )
        ((or ('T a) (a 'T)) 'T )
        ((or ('B a) (a 'B) (a a)) a )
        ((a b) `(/ ,a ,b) )) )

    (define (mk-= p q) `(= ,p ,q) )
    (define (bk-= f)
      (match f (('= p q) `(,p ,q) ) (_ (error "bk-= hates you") )) )
    (define (leql . l)
      (match l
        ((a a) 'T )
        (((? symbol?) (? symbol?)) 'B )
        ((a b) `(= ,a ,b) )) )


    (define (mk-> p q) `(> ,p ,q) )
    (define (bk-> f)
      (match f (('> p q) `(,p ,q) ) (_ (error "bk-= hates you") )) )
    (define (antecedent x) (car  (bk-> x)) )
    (define (consequent x) (cadr (bk-> x)) )

    (define (mk-E p q) `(E ,p ,q) )
    (define (bk-E f)
      (match f (('E p q) `(,p ,q) ) (_ (error "bk-= hates you") )) )

    (define (mk-V p q) `(V ,p ,q) )
    (define (bk-V f)
      (match f (('V p q) `(,p ,q) ) (_ (error "bk-= hates you") )) )

    ))

