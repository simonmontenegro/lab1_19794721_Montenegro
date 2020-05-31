#lang racket
(require "Listas.rkt")
(require "TDA_Commit.rkt")
(require "TDA_Index.rkt")
(require "TDA_WorkSpace.rkt")
(require "TDA_Local.rkt")
(require "TDA_Remote.rkt")
(require "HistorialComandos.rkt")

(provide (all-defined-out))

; |-------------------------------------------------------|
; |------------------- ~ TDA Zonas ~ ---------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion constructora de Zonas
;Dominio: WorkSpace, Index, LocalRepository, RemoteRepository
;Recorrido: Lista
;Recursion: No aplica
(define Zonas
  (lambda (workspace index local remote)
    (if (and (workSpace? workspace) (index? index) (localRepository? local) (remoteRepository? remote))
        (list workspace index local remote (list))
        #f
    )
  )
)

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica que una Zona sea efectivamente Zonas
;Dominio: Zonas
;Recorrido: Booleano
;Recursion: No aplica
(define Zonas?
  (lambda (Zonas)
    (and (not (null? Zonas)) (workSpace? (car Zonas)) (index? (car (cdr Zonas))) (localRepository? (car (cdr (cdr Zonas))))
         (remoteRepository? (car (cdr (cdr (cdr Zonas))))))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que retorna el WorkSpace de Zonas
;Dominio: Zonas
;Recorrido: WorkSpace
;Recursion: No aplica
(define getWorkSpace
  (lambda (Zonas)
    (car Zonas)
    )
  )

;Descripcion: Funcion que retorna el Index de Zonas
;Dominio: Zonas
;Recorrido: Index
;Recursion: No Aplica
(define getIndex
  (lambda (Zonas)
    (car (cdr Zonas))
    )
  )

;Descripcion: Funcion que retorna el LocalRepository de Zonas
;Dominio: Zonas
;Recorrido: LocalRepository
;Recursion: No Aplica
(define getLocalRepository
  (lambda (Zonas)
    (car (cdr (cdr Zonas)))
    )
  )

;Descripcion: Funcion que retorna el RemoteRepository de Zonas
;Dominio: Zonas
;Recorrido: RemoteRepository
;Recursion: No Aplica
(define getRemoteRepository
  (lambda (Zonas)
    (car (cdr (cdr (cdr Zonas))))
    )
  )

;Descripcion: Funcion que retorna el Historial de Zonas
;Dominio: Zonas
;Recorrido: Lista (historial de comandos)
;Recursion: No Aplica
(define getHistorial
  (lambda (Zonas)
    (car (cdr (cdr (cdr (cdr Zonas)))))
    )
  )

; |-------------------------------------------------------|
; |                   ~ Modificador ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que añade un comando y la fecha de aplicacion de dicho comando a Zonas
;Dominio: Zonas, String (comando)
;Recorrido: Zonas
;Recursion: No aplica
(define historialZonas
  (lambda (zonas comando)
    (list (getWorkSpace zonas) (getIndex zonas) (getLocalRepository zonas) (getRemoteRepository zonas)
          (agregarComando comando (getHistorial zonas)) )
    )
  )
    

(define zon (Zonas (list "archivo 1" "archivo 2") (list "archivo 3" "archivo 4")
                         (list (list "commit 1" (list "archivo 5" "archivo 6"))) (list (list "commit 2" (list "archivo 7" "archivo 8"))) )  )


; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|
;Descripcion: Funcion que representa el Historial de Zonas en formato string
;Dominio: Zonas
;Recorrido: String
;Recursion: Cola
(define getStringDeHistorial
  (lambda (historial)
    (define getStringDeHistorialAux
      (lambda (historial string)
        (if (null? historial)
            string
            (getStringDeHistorialAux (cdr historial) (string-append string (car (car historial)) "  " (car(cdr (car historial))) "\n"))
            )
        )
      )
    (getStringDeHistorialAux historial "\n\n\n~Historial~\n")
    )
  )

(define histo (list (list "ADD" "Friday, May 29th, 2020 9:52:15pm") (list "PULL" "Friday, May 29th, 2020 9:52:09pm")))