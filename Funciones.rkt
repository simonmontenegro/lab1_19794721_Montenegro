#lang racket

(require "Listas.rkt")
(provide (all-defined-out))

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

(define agregarComm
  (lambda (local commit)
    (define agregarCommAux
      (lambda (local commit nuevoLocal)
        (if (null? local)
            (cons commit nuevoLocal)
            (agregarCommAux (cdr local) commit (cons (car local) nuevoLocal))
            )
        )
      )
    (agregarCommAux local commit '())
    )
  )

(define eliminarComm
  (lambda (local elem)
    (define eliminarCommAux
      (lambda (local elem nuevoLocal)
        (if (null? local)
            nuevoLocal
            (if (equal? elem (car local))
                (eliminarCommAux (cdr local) elem nuevoLocal)
                (eliminarCommAux (cdr local) elem (cons (car local) nuevoLocal))
                )
            )
        )
      )
    (eliminarCommAux local elem '())
    )
  )

(define getStringArchivosCommit
  (lambda (listaCommits)
    (define getStringArchivosCommitAux
      (lambda (listaCommits string)
        (if (null? listaCommits)
            string
            (getStringArchivosCommitAux (cdr listaCommits) (string-append string (car listaCommits) "; "))
            )
        )
      )
    (getStringArchivosCommitAux listaCommits " ")
    )
  )

;(define com (list (list "mensaje1" '("uno" "dos")) (list "mensaje2" '("tres" "cuatro"))))
;(esCommit? (list (list "mensaje1" (list "uno" "dos")) (list "mensaje2" (list "tres" "cuatro"))))



















       