#lang racket

(require "Listas.rkt")
(require "TDA_Commit.rkt")
(require "TDA_Index.rkt")
(require "TDA_WorkSpace.rkt")
(require "TDA_Local.rkt")
(require "TDA_Remote.rkt")
(require "TDA_Zonas.rkt")
(require "HistorialComandos.rkt")

(require racket/date)

(provide (all-defined-out))

(define git
  (lambda (comando)
    (if (equal? comando pull)
        pull
        (if (equal? comando add)
            add
            (if (equal? comando commit)
                commit
                (if (equal? comando push)
                    push
                    (if (equal? comando "status")
                        "status"
                        (if (equal? comando "log")
                            "log"
                            "error"
                            )
                        )
                    )
                )
            )
        )
    )
  )

(define pull
  (lambda (zonas)
    (list (append (getWorkSpace zonas) (getArchivosRemote (getRemoteRepository zonas)))
          (getIndex zonas)
          (getLocalRepository zonas)
          (getRemoteRepository zonas)
          (agregarComando "PULL" (getHistorial zonas))
          )
    )
  )
    
(define add
  (lambda (archivos)
    (lambda (zonas)
      (if (null? archivos)
          (list (car zonas)
                (append (getWorkSpace zonas) (getIndex zonas))
                (getLocalRepository zonas)
                (getRemoteRepository zonas)
                (agregarComando "ADD" (getHistorial zonas))
                )
          (if (equal? (listaArchivosInWorkSpace? (car zonas) archivos) #t)
              (list (getWorkSpace zonas)
                    (append (getIndex zonas) archivos)
                    (getLocalRepository zonas)
                    (getRemoteRepository zonas)
                    (agregarComando "ADD" (getHistorial zonas))
                    )
              #f
          
              )
          )
      )
    )
  )

(define commit
  (lambda (mensaje)
    (lambda (zonas)
      (if (string? mensaje)
          (list
           (getWorkSpace zonas)
           (getIndex zonas)
           (append (list (list mensaje (getIndex zonas))) (getLocalRepository zonas))
           (getRemoteRepository zonas)
           (agregarComando "COMMIT" (getHistorial zonas))
           )
          #f
          )
      )
    )
  )

(define push
  (lambda (zonas)
    (list (getWorkSpace zonas)
          (getIndex zonas)
          (getLocalRepository zonas)
          (append (getRemoteRepository zonas) (getLocalRepository zonas))
          (agregarComando "PUSH" (getHistorial zonas))
          )
    )
  )

(define zonas->string
  (lambda (zonas)
    (string-append (getStringDeWorkSpace (getWorkSpace zonas)) (getStringDeIndex (getIndex zonas))
                   (getStringDeLocal (getLocalRepository zonas)) (getStringDeRemote (getRemoteRepository zonas))
                   (getStringDeHistorial (getHistorial zonas))
                   )
    )
  )

(define status
  (lambda (zonas)
    (string-append (getStringDeIndex (getIndex zonas)) "\nCantidad de Commits en Repositorio Local: "
                   (number->string (lenLista (getWorkSpace zonas))) "\nRama Actual: Master")
    )
  )

(define log
  (lambda (zonas)
    (define logAux
      (lambda (localRepository voyEn string)
        (if (or (equal? voyEn 5) (null? (cdr localRepository)))
            (string-append string "\n-> " (getMensajeCommit (car localRepository)) )
            (logAux (cdr localRepository) (+ voyEn 1) (string-append string "\n-> " (getMensajeCommit (car localRepository))))
            )
        )
      )
    (logAux (getLocalRepository zonas) 1 "Ultimos commits: \n")
    )
  )



        

(define zonas1 (Zonas (WorkSpace "archivo 1" "archivo 2")
                      (index "archivo 3" "archivo 4")
                      (list (list "commit 1" (list "archivo 5" "archivo 6")))
                      (list (list "commit 2" (list "archivo 7" "archivo 8")))
                      )
  )

(define commit2(((git commit)"mi segundo commit")zonas1))
(define commit3(((git commit)"mi tercer commit")commit2))
(define commit4(((git commit)"mi cuarto commit")commit3))
(define commit5(((git commit)"mi quinto commit")commit4))
(define commit6(((git commit)"mi sexto commit")commit5))
(define commit7(((git commit)"mi septimo commit")commit6))
(define commit8(((git commit)"mi octavo commit")commit7))
(define zonas2 ((git pull)zonas1))
(define zonas3 (((git add)'())zonas2))
(define zonas4 (((git commit)"mi primer commit")zonas3))
(define zonas5 ((git push)zonas4))






         