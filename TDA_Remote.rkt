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

(define rem (list (list "commit 2" (list "archivo 7" "archivo 8")) (list "commit 3" (list "archivo 9" "archivo 10"))))
(define rem2 (pushCommit rem (list "mensaje1000" (list "archivo1000" "archivo2000"))))


; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|

; (list (list "mensaje" (list "archivo 1" "archivo 2")) ... )
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


  

(define remote (remoteRepository (list (list "mensaje1" (list "archivo1" "archivo2")) (list "mensaje2" (list "archivo3" "archivo4" "archivo5"))))) 
