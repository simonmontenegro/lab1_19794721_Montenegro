#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "TDA_Commit.rkt")

; (list (list "mensaje" (list "archivo 1" "archivo 2")) ... )
; |-------------------------------------------------------|
; |-------------- ~ TDA RemoteRepository ~ ---------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que constructora de un RemoteRepository
;Dominio: Lista (de commits)
;Recorrido: Lista (de commits)
;Recursion: No aplica
(define remoteRepository
  (lambda (commit)
    (if (> (lenLista commit) 0)
      (if (esCommit? commit)
          commit
          '()
          )
      #f
      )
    )
  )

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica si un RemoteRepository es efectivamente un RemoteRepository
;Dominio: RemoteRepository
;Recorrido: Booleano
;Recursion: No aplica
(define remoteRepository?
  (lambda (remote)
    (and (not (null? remote)) (esCommit? remote))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que retorna un commit en determinada posicion en el RemoteRepository
;Dominio: RemoteRepository, Entero
;Recorrido: commit
;Recursion: No aplica
(define getElemCommitRemote
  (lambda (remote pos)
    (if (remoteRepository? remote)
        (elementoCommit remote pos)
        '()
        )
    )
  )

;Descripcion: Funcion que retorna una lista de todos los archivos contenidos en el RemoteRepository
;Dominio: RemoteRepository
;Recorrido: Lista (de archivos)
;Recursion: Cola
(define getArchivosRemote
  (lambda (remote)
    (define getArchivosAux
      (lambda (remote listaArchivos)
        (if (null? remote)
            listaArchivos
            (getArchivosAux (cdr remote) (append (car(cdr (car remote))) listaArchivos))
            )
        )
      )
    (getArchivosAux remote '() )
    )
  )

;Descripcion: Funcion que retorna la posicion de determinado commit en el RemoteRepository 
;Dominio: RemoteRepository, commit
;Recorrido: Entero
;Recursion: No aplica
(define getPosCommitRemote
  (lambda (remote elemento)
    (if (remoteRepository? remote)
        (posCommit remote elemento)
        '()
        )
    )
  )
        
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que añade un commit al RemoteRepository
;Dominio: RemoteRepository, commit
;Recorrido: RemoteRepository
;Recursion: No aplica
(define pushCommit
  (lambda (remote nuevoCommit)
    (if (remoteRepository? remote)
        (agregarComm remote nuevoCommit)
        '()
        )
    )
  )


; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|
;Descripcion: Funcion que representa el RemoteRepository en formato string
;Dominio: RemoteRepository
;Recorrido: String
;Recursion: Cola
(define getStringDeRemote
  (lambda (remote)
    (define getStringDeRemoteAux
      (lambda (remote string)
        (if (null? remote)
            string
            (getStringDeRemoteAux (cdr remote) (string-append string (getStringArchivosCommit (car (cdr (car remote)))) "-> " (car (car remote)) "\n\n" ))
            )
        )
      )
    (getStringDeRemoteAux remote "\n\n\n~Remote Repository~\n")
    )
  )


  
(define rem (list (list "commit 2" (list "archivo 7" "archivo 8")) (list "commit 3" (list "archivo 9" "archivo 10"))))
(define rem2 (pushCommit rem (list "mensaje1000" (list "archivo1000" "archivo2000"))))
(define remote (remoteRepository (list (list "mensaje1" (list "archivo1" "archivo2")) (list "mensaje2" (list "archivo3" "archivo4" "archivo5"))))) 
