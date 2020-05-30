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
(define Zonas
  (lambda (workspace index local remote)
    (if (and (workSpace? workspace) (index? index) (localRepository? local) (remoteRepository? remote))
        (list workspace index local remote historial)
        #f
    )
  )
)

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define Zonas?
  (lambda (Zonas)
    (and (not (null? Zonas)) (workSpace? (car Zonas)) (index? (car (cdr Zonas))) (localRepository? (car (cdr (cdr Zonas))))
         (remoteRepository? (car (cdr (cdr (cdr Zonas))))))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getWorkSpace
  (lambda (Zonas)
    (car Zonas)
    )
  )

(define getIndex
  (lambda (Zonas)
    (car (cdr Zonas))
    )
  )
  
(define getLocalRepository
  (lambda (Zonas)
    (car (cdr (cdr Zonas)))
    )
  )
(define getRemoteRepository
  (lambda (Zonas)
    (car (cdr (cdr (cdr Zonas))))
    )
  )

(define getHistorial
  (lambda (Zonas)
    (car (cdr (cdr (cdr (cdr Zonas)))))
    )
  )

; |-------------------------------------------------------|
; |                   ~ Modificador ~                     |
; |-------------------------------------------------------|
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