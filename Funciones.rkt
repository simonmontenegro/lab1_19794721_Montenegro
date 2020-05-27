#lang racket
(provide (all-defined-out))

(require "Listas.rkt")

(define esCommit?
  (lambda (commit)
    (if (null? commit)
        #t
        (if (and (string? (car (car commit))) (esString? (car(cdr (car commit)))))
            (esCommit? (cdr commit))
            #f
            )
        )
    )
  )

(define elementoCommit
  (lambda (listaCommit posicion)
    (define elementoAux
      (lambda (listaCommit posicion voyEn)
        (if (null? listaCommit)
            #f
            (if (= voyEn posicion)
                (car listaCommit)
                (elementoAux (cdr listaCommit) posicion (+ voyEn 1))
                )
            )
        )
      )
    (elementoAux listaCommit posicion 1)
    )
  )

(define posCommit
  (lambda (listaCommit elemento)
    (define posAux
      (lambda (listaCommit elemento voyEn)
        (if (null? listaCommit)
            #f
            (if (equal? elemento (car listaCommit))
                voyEn
                (posAux (cdr listaCommit) elemento (+ voyEn 1))
                )
            )
        )
      )
    (posAux listaCommit elemento 1)
    )
  )
      
;(define com (list (list "mensaje1" '("uno" "dos")) (list "mensaje2" '("tres" "cuatro"))))
;(esCommit? (list (list "mensaje1" (list "uno" "dos")) (list "mensaje2" (list "tres" "cuatro"))))