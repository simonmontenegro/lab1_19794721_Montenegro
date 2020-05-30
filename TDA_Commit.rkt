#lang racket

(require "Listas.rkt")
(provide (all-defined-out))

; |-------------------------------------------------------|
; |-------------------- ~ TDA Commit ~ -------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion constructora de un "commit"
;Dominio: String, Lista (Lista de strings)
;Recorrido: Lista
;Recursion: No aplica
(define commit
  (lambda (mensaje archivos)
    (if (and (esString? archivos) (string? mensaje))
        (list mensaje archivos)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define commit?
  (lambda (com)
    (and (list? com) (not (equal? (commit (car com) (car(cdr com))) #f)))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getMensajeCommit
  (lambda (commit)
    (car commit)
    )
  )

(define getArchivosCommit
  (lambda (commit)
    (car (cdr commit)
         )
    )
  )

; |-------------------------------------------------------|
; |                       ~ Otros ~                       |
; |-------------------------------------------------------|


;Descripcion: Funcion que verifica si los elementos de una lista son commit
;Dominio: Lista
;Recorrido: Booleano
;Recursion: natural
(define esCommit?
  (lambda (listaCommits)
    (if (null? listaCommits)
        #t
        (if (commit? (car listaCommits))
            (esCommit? (cdr listaCommits))
            #f
            )
        )
    )
  )

(define elementoCommit
  (lambda (listaCommits posicion)
    (define elementoAux
      (lambda (listaCommits posicion voyEn)
        (if (null? listaCommits)
            #f
            (if (= voyEn posicion)
                (car listaCommits)
                (elementoAux (cdr listaCommits) posicion (+ voyEn 1))
                )
            )
        )
      )
    (elementoAux listaCommits posicion 1)
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
  (lambda (repos commit)
    (if (commit? commit)
        (if (null? repos)
            (list (list commit))
            (append  (list commit) repos)
            )
        #f
        )
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



















       