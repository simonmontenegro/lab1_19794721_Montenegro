#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "Funciones.rkt")

; |-------------------------------------------------------|
; |-------------- ~ TDA RemoteRepository ~ ---------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
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
(define remoteRepository?
  (lambda (remote)
    (and (not (null? remote)) (esCommit? remote))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getElemCommitRemote
  (lambda (remote pos)
    (if (remoteRepository? remote)
        (elementoCommit remote pos)
        '()
        )
    )
  )

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

(define pushCommit
  (lambda (remote nuevoCommit)
    (if (remoteRepository? remote)
        (agregarComm remote nuevoCommit)
        '()
        )
    )
  )