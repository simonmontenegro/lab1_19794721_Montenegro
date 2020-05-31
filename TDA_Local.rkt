#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "TDA_Commit.rkt")


; (list (list "mensaje" (list "archivo 1" "archivo 2")) ... )
; |-------------------------------------------------------|
; |--------------- ~ TDA LocalRepository ~ ---------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que constructora de un localRepository
;Dominio: Lista (de commits)
;Recorrido: Lista (de commits)
;Recursion: No aplica
(define localRepository
  (lambda (listaCommits)
    (if (> (lenLista listaCommits) 0)
      (if (esCommit? listaCommits)
          listaCommits
          '()
          )
      #f
      )
    )
  )

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica si un LocalRepository es efectivamente un LocalRepository
;Dominio: LocalRepository
;Recorrido: Booleano
;Recursion: No aplica
(define localRepository?
  (lambda (local)
    (and (not (null? local)) (esCommit? local))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que retorna un commit en determinada posicion en el LocalRepository
;Dominio: LocalRepository, Entero
;Recorrido: commit
;Recursion: No aplica
(define getElemCommit
  (lambda (local pos)
    (if (localRepository? local)
        (elementoCommit local pos)
        '()
        )
    )
  )

;Descripcion: Funcion que retorna la posicion de determinado commit en el LocalRepository 
;Dominio: LocalRepository, commit
;Recorrido: Entero
;Recursion: No aplica
(define getPosCommit
  (lambda (local elemento)
    (if (localRepository? local)
        (posCommit local elemento)
        '()
        )
    )
  )
        
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que añade un commit al LocalRepository
;Dominio: LocalRepository, commit
;Recorrido: LocalRepository
;Recursion: No aplica
(define addCommit
  (lambda (local nuevoCommit)
    (if (localRepository? local)
        (agregarComm local nuevoCommit)
        '()
        )
    )
  )

;Descripcion: Funcion que elimina un commit del LocalRepository
;Dominio: LocalRepository, commit
;Recorrido: LocalRepository
;Recursion: No aplica
(define delCommit
  (lambda (local delCommit)
    (if (localRepository? local)
        (eliminarComm local delCommit)
        '()
        )
    )
  )


; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|
;Descripcion: Funcion que representa el LocalRepository en formato string
;Dominio: LocalRepository
;Recorrido: String
;Recursion: Cola
(define getStringDeLocal
  (lambda (local)
    (define getStringDeLocalAux
      (lambda (local string)
        (if (null? local)
            string
            (getStringDeLocalAux (cdr local) (string-append string (getStringArchivosCommit (car (cdr (car local)))) "-> " (car (car local)) "\n\n" ))
            )
        )
      )
    (getStringDeLocalAux local "\n\n\n~Local Repository~\n")
    )
  )


  

(define local (localRepository (list (list "mensaje1" (list "archivo1" "archivo2")) (list "mensaje2" (list "archivo3" "archivo4" "archivo5"))))) 