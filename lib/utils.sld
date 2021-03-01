(define-library (utils)
  (import (chibi) (lib misc))
  (export setify)
  (begin

    (define (rember c e) (filter (λ (x) (not (eq? x e))) c) )
    (define (setify c)
      (cond
        ((null? c) c )
        (else (cons (car c) (setify (rember (cdr c) (car c)))) )) )
  ))
