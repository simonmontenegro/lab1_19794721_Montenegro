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


; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|

; (list (list "mensaje" (list "archivo 1" "archivo 2")) ... )
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