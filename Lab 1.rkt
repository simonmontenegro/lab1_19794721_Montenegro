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
        (if (equal? comando "add")
            "add"
            (if (equal? comando "commit")
                "commit"
                (if (equal? comando "push")
                    "push"
                    #f
                    )
                )
            )
        )
    )
  )

(define pull
  (lambda (zonas)
    (list (myAppend (car zonas) (getArchivosRemote (car (cdr (cdr (cdr zonas)))))) (car (cdr zonas)) (car (cdr (cdr zonas))) (car (cdr (cdr (cdr zonas))))
              (agregarComando "PULL" (car (cdr (cdr (cdr (cdr zonas))))))
              )
    )
  )
    
(define add
  (lambda (archivos)
    (lambda (zonas)
      (if (null? archivos)
          (list (car zonas) (myAppend (car zonas) (car (cdr zonas))) (car (cdr (cdr zonas))) (car (cdr (cdr (cdr zonas))))
                       (agregarComando "ADD" (car (cdr (cdr (cdr (cdr zonas)))))))
          (list (car zonas) (myAppend archivos (car (cdr zonas))) (car (cdr (cdr zonas))) (car (cdr (cdr (cdr zonas))))
                       (agregarComando "ADD" (car (cdr (cdr (cdr (cdr zonas)))))))
          )
      )
    )
  )

(define zonas1 (Zonas (list "archivo 1" "archivo 2") (list "archivo 3" "archivo 4")
                         (list (list "commit 1" (list "archivo 5" "archivo 6"))) (list (list "commit 2" (list "archivo 7" "archivo 8"))) )  )






         