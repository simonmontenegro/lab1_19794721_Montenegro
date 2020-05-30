#lang racket

(require "Listas.rkt")
(require "Funciones.rkt")
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
                    #f
                    )
                )
            )
        )
    )
  )

(define pull
  (lambda (zonas)
    (list (myAppend (getWorkSpace zonas) (getArchivosRemote (car (cdr (cdr (cdr zonas))))))
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
                (myAppend (getWorkSpace zonas) (getIndex zonas))
                (getLocalRepository zonas)
                (getRemoteRepository zonas)
                (agregarComando "ADD" (getHistorial zonas))
                )
          (if (equal? (listaArchivosInWorkSpace? (car zonas) archivos) #t)
              (list (getWorkSpace zonas)
                    (myAppend archivos (getIndex zonas))
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
           (myAppend (getLocalRepository zonas) (list (list mensaje (car (cdr zonas)))))
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
          (myAppend (getLocalRepository zonas) (getRemoteRepository zonas) )
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

(define zonas1 (Zonas (list "archivo 1" "archivo 2")
                      (list "archivo 3" "archivo 4")
                      (list (list "commit 1" (list "archivo 5" "archivo 6")))
                      (list (list "commit 2" (list "archivo 7" "archivo 8")))
                      )
  )

;(define zonas2 ((git pull)zonas1))
;(define zonas3 (((git add)'())zonas2))
;(define zonas4 (((git commit)"mi primer commit")zonas3))
;(define zonas5 ((git push)zonas4))

;(string-append "string 1 " "string 2")





         