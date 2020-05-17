#lang racket

(provide (all-defined-out))


(define lenLista
  (lambda (Lista)
    (define largo
      (lambda (Lista elementos)
        (if (null? (cdr Lista))
            elementos
            (largo (cdr Lista) (+ elementos 1))
            )
        )
      )
    (largo Lista 1)
    )
  )

(define esString?
  (lambda (Lista)
    (if (null? Lista)
        #t
        (if (string? (car Lista))
            (esString? (cdr Lista))
            #f
            )
        )
    )
  )

(define posicionEnLista
  (lambda (Lista elemento)
    (define posicionEnListaAux
      (lambda (L elem voyEn)
        (if (null? L)
            #f
            (if (equal? elem (car L))
                voyEn
                (posicionEnListaAux (cdr L) elem (+ voyEn 1))
                )
            )
        )
      )
    (posicionEnListaAux Lista elemento 1)
    )
  )

(define elementoEnLista
  (lambda (L posicion)
    (define elementoEnListaAux
      (lambda (L posicion voyEn)
        (if (null? L)
            #f
            (if (= voyEn posicion)
                (car L)
                (elementoEnListaAux (cdr L) posicion (+ voyEn 1))
                )
            )
        )
      )
    (elementoEnListaAux L posicion 1)
    )
  )









        
        
        