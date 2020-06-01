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
(define TDAcommit
  (lambda (mensaje archivos)
    (if (and (string? mensaje) (esString? archivos) (and (not (null? archivos))))
        (list mensaje archivos)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica que un commit efectivamente sea un commit
;Dominio: commit
;Recorrido: booleano
;Recursion: No aplica
(define commit?
  (lambda (com)
    (and (list? com) (not (equal? (TDAcommit (car com) (car(cdr com))) #f)))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que retorna el mensaje asociado a un commit
;Dominio: commit
;Recorrido: String
;Recursion: No aplica
(define getMensajeCommit
  (lambda (commit)
    (if (commit? commit)
        (car commit)
        #f
        )
    )
  )

;Descripcion: Funcion que retorna la lista de archivos asociada a un commit
;Dominio: commit
;Recorrido: Lista
;Recursion: No aplica
(define getArchivosCommit
  (lambda (commit)
    (if (commit? commit)
        (car (cdr commit))
        #f
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

;Descripcion: Funcion que retorna un commit en determinada posicion de una lista de commits
;Dominio: Lista (de commits)
;Recorrido: commit
;Recursion: Cola
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
    (if (esCommit? listaCommits)
        (elementoAux listaCommits posicion 1)
        #f
        )
    )
  )

;Descripcion: Funcion que retorna la posicion de un commit en una lista de commits
;Dominio: Lista (de commits)
;Recorrido: Entero
;Recursion: Cola
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
    (if (esCommit? listaCommit)
        (posAux listaCommit elemento 1)
        #f
        )
    )
  )

;Descripcion: Funcion que agrega un commit a una lista de commits
;Dominio: Lista (de commits), commit
;Recorrido: Lista (de commits)
;Recursion: No aplica
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

;Descripcion: Funcion que elimina un commit de una lista de commits
;Dominio: Lista (de commits), commit
;Recorrido: Lista (de commits)
;Recursion: Cola
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

;Descripcion: Funcion que representa una lista de commits en formato string
;Dominio: Lista (de commits)
;Recorrido: String
;Recursion: Cola
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



















       