#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "Funciones.rkt")

; |-------------------------------------------------------|
; |--------------- ~ TDA LocalRepository ~ ---------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define localRepository
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
(define localRepository?
  (lambda (local)
    (and (not (null? local)) (esCommit? local))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getElemCommit
  (lambda (local pos)
    (if (localRepository? local)
        (elementoCommit local pos)
        '()
        )
    )
  )

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

(define addCommit
  (lambda (local nuevoCommit)
    (if (localRepository? local)
        (agregarComm local nuevoCommit)
        '()
        )
    )
  )

(define delCommit
  (lambda (local delCommit)
    (if (localRepository? local)
        (eliminarComm local delCommit)
        '()
        )
    )
  )
