(define-library (atoms)
  (import (chibi) (chibi match) (lib misc) (utils))
  (export over-atoms atom-union atoms)
  (begin
    (define (over-atoms x f b)
      (match x
        ((or 'T 'B) b )
        ((? symbol? a) (f a b) )
        (('~ p) (over-atoms p f b) )
        (((or '& '/ '> '=) p q) (over-atoms p f (over-atoms q f b)) )
        (((or 'V 'E) x p) (over-atoms p f b) )) )

    (define (atom-union x f)
      (setify (over-atoms x (λ (h t) (append (f h) t)) `())) )

    (define (atoms x) (atom-union x (λ (a) `(,a))) )
  ))
