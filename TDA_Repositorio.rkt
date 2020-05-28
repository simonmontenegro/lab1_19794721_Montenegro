#lang racket
(require "Listas.rkt")
(require "Funciones.rkt")
(require "TDA_Index.rkt")
(require "TDA_WorkSpace.rkt")
(require "TDA_Local.rkt")
(require "TDA_Remote.rkt")

(provide (all-defined-out))

; |-------------------------------------------------------|
; |---------------- ~ TDA Repositorio ~ ------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define Repositorio
  (lambda (workspace index local remote)
    (if (and (workSpace? workspace) (index? index) (localRepository? local) (remoteRepository? remote))
        (list workspace index local remote)
        #f
    )
  )
)

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define Repositorio?
  (lambda (repositorio)
    (and (not (null? repositorio)) (workSpace? (car repositorio)) (index? (car (cdr repositorio))) (localRepository? (car (cdr (cdr repositorio))))
         (remoteRepository? (car (cdr (cdr (cdr repositorio))))))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getWorkSpace
  (lambda (repositorio)
    (car repositorio)
    )
  )

(define getIndex
  (lambda (repositorio)
    (car (cdr repositorio))
    )
  )
  
(define getLocalRepository
  (lambda (repositorio)
    (car (cdr (cdr repositorio)))
    )
  )
(define getRemoteRepository
  (lambda (repositorio)
    (car (cdr (cdr (cdr repositorio))))
    )
  )

(define rep (Repositorio (list "archivo 1" "archivo 2") (list "archivo 3" "archivo 4")
                         (list (list "commit 1" (list "archivo 5" "archivo 6"))) (list (list "commit 2" (list "archivo 7" "archivo 8"))) )  )
