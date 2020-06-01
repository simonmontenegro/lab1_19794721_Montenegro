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


;Descripcion: Función de orden superior que permite aplicar los comandos a Zonas
;Dominio: Funcion
;Recorrido: Funcion
;Recursion: No aplica
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
                    (if (equal? comando status)
                        status
                        "error"
                        )
                    )
                )
            )
        )
    )
  )


;Descripcion: Funcion que crea agrega todos los archivos del remoteRepository al WorkSpace
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: Cola
(define pull
  (lambda (zonas)
    (define pullAux
      (lambda (WorkSpace ArchivosRemote)
        (if (null? ArchivosRemote)
            WorkSpace
            (pullAux (append WorkSpace (list (car ArchivosRemote))) (cdr ArchivosRemote))
            )
        )
      )
    (if (equal? (lenLista zonas) 5)
        (list (pullAux (getWorkSpace zonas) (getArchivosRemote (getRemoteRepository zonas)))
              (getIndex zonas)
              (getLocalRepository zonas)
              (getRemoteRepository zonas)
              (agregarComando "PULL" (getHistorial zonas))
              )
        #f
        )
    )
  )      
    
;Descripcion: Funcion que añade todos los archivos del WorkSpace al Index
;Dominio: Lista y Zonas ; null y Zonas
;Recorrido: Zonas
;Recursion: No aplica
(define add
  (lambda (archivos)
    (lambda (zonas)
      (if (equal? (lenLista zonas) 5)
          (if (null? archivos)
              (list (getWorkSpace zonas)
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
          #f
          )
      )
    )
  )

;Descripcion: Funcion que hace un commit con todos los archivos al local repository
;             acompañado de un mensaje descriptivo
;Dominio: String, Zonas
;Recorrido: Zonas
;Recursion: No aplica
(define commit
  (lambda (mensaje)
    (lambda (zonas)
      (if (equal? (lenLista zonas) 5)
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
          #f
          )
      )
    )
  )
;Descripcion: Funcion que envia todos los commits del localRepository al remoteRepository
;Dominio: Zonas
;Recorrido: Zonas
;Recursion: No aplica
(define push
  (lambda (zonas)
    (if (equal? (lenLista zonas) 5)
        (list (getWorkSpace zonas)
              (getIndex zonas)
              (getLocalRepository zonas)
              (append (getRemoteRepository zonas) (getLocalRepository zonas))
              (agregarComando "PUSH" (getHistorial zonas))
              )
        #f
        )
    )
  )
;Descripcion: Funcion que genera un string descriptivo del estado de Zonas
;Dominio: Zonas
;Recorrido: String
;Recursion: No aplica
(define zonas->string
  (lambda (zonas)
    (if (equal? (lenLista zonas) 5)
        (string-append (getStringDeWorkSpace (getWorkSpace zonas)) (getStringDeIndex (getIndex zonas))
                       (getStringDeLocal (getLocalRepository zonas)) (getStringDeRemote (getRemoteRepository zonas))
                       (getStringDeHistorial (getHistorial zonas))
                       )
        #f
        )
    )
  )
;Descripcion: Funcion que genera un string en el que se ven los archivos de index, cantidad
;             de commits en el localRepository y la rama actual en la que se esta trabajando.
;Dominio: Zonas
;Recorrido: String
;Recursion: No aplica
(define status
  (lambda (zonas)
    (if (equal? (lenLista zonas) 5)
        (string-append (getStringDeIndex (getIndex zonas)) "\nCantidad de Commits en Repositorio Local: "
                       (number->string (lenLista (getWorkSpace zonas))) "\nRama Actual: Master")
        #f
        )
    )
  )
;Descripcion: Funcion que genera un string con los ultimos 5 commits (o menos en el caso de no haber 5)
;             se muestra el mensaje de cada commit
;Dominio: Zonas
;Recorrido: String
;Recursion: No aplica
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
    (if (equal? (lenLista zonas) 5)
        (logAux (getLocalRepository zonas) 1 "Ultimos commits: \n")
        #f
        )
    )
  )



;-------------------------------------------------------------------------------
;EJEMPLOS DE USO PARA CADA UNO DE LOS REQUERIMIENTOS FUNCIONALES:

;TDA'S
;TDA WorkSpace
;Primer Ejemplo
(display "TDA WorkSpace, Ejemplo_1:\n")
(define ejemploWork_1 (WorkSpace "archivo 1" "archivo 2"))
ejemploWork_1

;Segundo Ejemplo
(display "TDA WorkSpace, Ejemplo_2 (input inválido):\n")
(define ejemploWork_2 (WorkSpace 2 "archivo 2"))
ejemploWork_2

;Tercer Ejemplo
(display "TDA WorkSpace, Ejemplo_3 (input inválido):\n")
(define ejemploWork_3 (WorkSpace "archivo 1" "archivo 2" (list "archivo 3" "archivo 4")))
ejemploWork_3

(display "\n\n")

;TDA Index
;Primer Ejemplo
(display "TDA Index, Ejemplo_1:\n")
(define ejemploIndex_1 (index "archivo 1" "archivo 2"))
ejemploIndex_1

;Segundo Ejemplo
(display "TDA Index, Ejemplo_2 (input inválido):\n")
(define ejemploIndex_2 (index 2 "archivo 2"))
ejemploIndex_2

;Tercer Ejemplo
(display "TDA Index, Ejemplo_3 (input inválido):\n")
(define ejemploIndex_3 (index "archivo 1" "archivo 2" (list "archivo 3" "archivo 4")))
ejemploIndex_3

(display "\n\n")

;TDA Commit
;Primer Ejemplo
(display "TDA Commit, Ejemplo_1:\n")
(define ejemploCommit__1 (TDAcommit "Mi primer commit" (list "Archivo_1" "Archivo_2")))
ejemploCommit__1

;Segundo Ejemplo
(display "TDA Commit, Ejemplo_2:\n")
(define ejemploCommit__2 (TDAcommit "Mensaje 1" (list "archivo 1" "archivo 2" "archivo 3" "archivo 4")))
ejemploCommit__2

;Tercer Ejemplo
(display "TDA Commit, Ejemplo_3 (input inválido):\n")
(define ejemploCommit__3 (TDAcommit 2 (list "archivo 3" "archivo 4")))
ejemploCommit__3

(display "\n\n")

;TDA LocalRepository
;Primer Ejemplo
(display "TDA LocalRepository, Ejemplo_1:\n")
(define ejemploLocal_1 (localRepository (list (list "Mensaje 1" (list "archivo 1" "archivo 2")) (list "Mensaje 2" (list "archivo 3" "archivo 4")))))
ejemploLocal_1

;Segundo Ejemplo
(display "TDA LocalRepository, Ejemplo_2:\n")
(define ejemploLocal_2 (localRepository (list (list "Mensaje 1" (list "archivo 1" "archivo 2")))))
ejemploLocal_2

;Tercer Ejemplo
(display "TDA LocalRepository, Ejemplo_3 (input inválido):\n")
(define ejemploLocal_3 (localRepository (list 2 (list "Mensaje 2" (list "archivo 3" "archivo 4")))))
ejemploLocal_3

(display "\n\n")

;TDA RemoteRepository
;Primer Ejemplo
(display "TDA RemoteRepository, Ejemplo_1:\n")
(define ejemploRemote_1 (remoteRepository (list (list "Mensaje 1" (list "archivo 1" "archivo 2")) (list "Mensaje 2" (list "archivo 3" "archivo 4")))))
ejemploRemote_1

;Segundo Ejemplo
(display "TDA RemoteRepository, Ejemplo_2:\n")
(define ejemploRemote_2 (remoteRepository (list (list "Mensaje 1" (list "archivo 1" "archivo 2")))))
ejemploRemote_2

;Tercer Ejemplo
(display "TDA RemoteRepository, Ejemplo_3 (input inválido):\n")
(define ejemploRemote_3 (remoteRepository (list 2 (list "Mensaje 2" (list "archivo 3" "archivo 4")))))
ejemploRemote_3

(display "\n\n")



;TDA Zonas
;Primer Ejemplo
(display "TDA Zonas, Ejemplo_1:\n")
(define ejemploZonas_1 (Zonas
                        (WorkSpace "archivo 1" "archivo 2")
                        (index "archivo 3" "archivo 4")
                        (list (list "commit 1" (list "archivo 5" "archivo 6")))
                        (list (list "commit 2" (list "archivo 7" "archivo 8")))
                        )
  )
ejemploZonas_1

;Segundo Ejemplo
(display "TDA Zonas, Ejemplo_2:\n")
(define ejemploZonas_2 (Zonas
                        (WorkSpace "archivo 1" "archivo 2" "archivo 3" "archivo 4")
                        (index "archivo 5" "archivo 6")
                        (list (list "commit 1" (list "archivo 7" "archivo 8")) (list "commit 2" (list "archivo 9" "archivo 10")))
                        (list (list "commit 3" (list "archivo 11" "archivo 12")))
                        )
  )
ejemploZonas_2

;Tercer Ejemplo
(display "TDA Zonas, Ejemplo_3 (input inválido):\n")
(define ejemploZonas_3 (Zonas
                        (WorkSpace 2)
                        (index (list "chao"))
                        (list (list "commit 1" (list "archivo 7" "archivo 8")) (list "commit 2" (list "archivo 9" "archivo 10")))
                        (list (list "commit 3" (list "archivo 11" "archivo 12")))
                        )
  )
ejemploZonas_3

(display "\n\n")
;-------------------------------------------------------------------------------
;La función git es una funcion que se debe utilizar en conjunto con alguna de las
;funciones (comandos). Por lo tanto se entiende que cada uno de los ejemplos de
;las funciones (comandos) es un ejemplo además del uso de la funcion GIT.
;-------------------------------------------------------------------------------

;TENGO QUE CREAR LOS EJEMPLOS DE CADA TDA, DEL TDA ZONAS, DE STATUS, DE LOG Y DE STRING->ZONAS

;Caso particular de la función git: cuando se le entrega un comando inexistente
(define comando "comando")
(display "Este ejemplo corresponde a la funcion git cuando se le entrega un comando inexistente\n->")
(git comando)
(display "\n\n")


;Función Pull
;Primer Ejemplo
(display "Funcion Pull, Ejemplo 1:\n")
(define ejemploPull_1 (Zonas
                       (WorkSpace "archivo 1" "archivo 2")
                       (index "archivo 3" "archivo 4")
                       (localRepository (list (list "PrimerCommit" (list "archivo5" "archivo6")) (list "SegundoCommit" (list "archivo7" "archivo8" "archivo9"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 10" "archivo 11"))))
                       ))
((git pull)ejemploPull_1)

;Segundo Ejemplo
(display "Funcion Pull, Ejemplo 2:\n")
(define ejemploPull_2 (Zonas
                       (WorkSpace "archivo 1")
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo3" "archivo4")) (list "SegundoCommit" (list "archivo5" "archivo6" "archivo7"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 8" "archivo 9")) (list "CuartoCommit" (list "archivo 10" "archivo 11"))))
                       ))
((git pull)ejemploPull_2)

;Tercer Ejemplo
(display "Funcion Pull, Ejemplo 3:\n")
(define ejemploPull_3 (Zonas
                       (WorkSpace "archivo 1" "archivo 2" "archivo 3" "archivo 4")
                       (index "archivo 5")
                       (localRepository (list (list "PrimerCommit" (list "archivo6" "archivo7")) (list "SegundoCommit" (list "archivo8" "archivo9" "archivo10"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 8"))))
                       ))
((git pull)ejemploPull_3)

(display "\n\n")

;Funcion Add
;Primer ejemplo:
(display "Funcion Add, Ejemplo 1:\n")
(define ejemploAdd_1 (Zonas
                       (WorkSpace "archivo 1" "archivo 2")
                       (index "archivo 3" "archivo 4")
                       (localRepository (list (list "PrimerCommit" (list "archivo5"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 6"))))
                       ))
(((git add)(list "archivo 1" ))ejemploAdd_1)

;Segundo ejemplo:
(display "Funcion Add, Ejemplo 2:\n")
(define ejemploAdd_2 (Zonas
                       (WorkSpace "archivo 1" "archivo 2")
                       (index "archivo 3" "archivo 4")
                       (localRepository (list (list "PrimerCommit" (list "archivo5"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 6"))))
                       ))
(((git add)null)ejemploAdd_2)

;Tercer ejemplo:
(display "Funcion Add, Ejemplo 3:\n")
(define ejemploAdd_3 (Zonas
                       (WorkSpace "archivo 1" "archivo 2" "archivo 3" "archivo 4")
                       (index "archivo 5" "archivo 6")
                       (localRepository (list (list "PrimerCommit" (list "archivo7"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 8"))))
                       ))
(((git add)(list "archivo 1" "archivo 3"))ejemploAdd_3)

;Cuarto ejemplo
(display "Funcion Add, Ejemplo 4:\n")
(define ejemploAdd_4 (Zonas
                       (WorkSpace "archivo 1" "archivo 2" "archivo 3" "archivo 4")
                       (index "archivo 5" "archivo 6" "archivo 7" "archivo 8")
                       (localRepository (list (list "PrimerCommit" (list "archivo 9"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 10"))))
                       ))
(((git add)null)ejemploAdd_4)

(display "\n\n")

;Funcion commit
;Primer ejemplo:
(display "Funcion Commit, Ejemplo 1:\n")
(define ejemploCommit_1 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2" "archivo 3" "archivo 4" "archivo 5")
                       (localRepository (list (list "PrimerCommit" (list "archivo 6"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 7"))))
                       ))
(((git commit)"Ejemplo 1 de funcion Commit")ejemploCommit_1)

;Segundo ejemplo:
(display "Funcion Commit, Ejemplo 2:\n")
(define ejemploCommit_2 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo 3"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 4"))))
                       ))
(((git commit)"Ejemplo 2 de funcion Commit")ejemploCommit_2)

;Tercer ejemplo:
(display "Funcion Commit, Ejemplo 3:\n")
(define ejemploCommit_3 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo3" "archivo4")) (list "SegundoCommit" (list "archivo5" "archivo6" "archivo7"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 8"))))
                       ))
(((git commit)"Ejemplo 3 de funcion Commit")ejemploCommit_3)

(display "\n\n")

;Funcion Push
;Primer ejemplo:
(display "Funcion Push, Ejemplo 1:\n")
(define ejemploPush_1 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo 3"))))
                       (remoteRepository (list (list "SegundoCommit" (list "archivo 4"))))
                       ))
((git push)ejemploPush_1)

;Segundo ejemplo:
(display "Funcion Push, Ejemplo 2:\n")
(define ejemploPush_2 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo 3")) (list "SegundoCommit" (list "archivo 4" "archivo 5"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 6"))))
                       ))
((git push)ejemploPush_2)

;Tercer ejemplo:
(display "Funcion Push, Ejemplo 3:\n")
(define ejemploPush_3 (Zonas
                       (WorkSpace "archivo 1" )
                       (index "archivo 2")
                       (localRepository (list (list "PrimerCommit" (list "archivo 3")) (list "SegundoCommit" (list "archivo 4" "archivo 5"))))
                       (remoteRepository (list (list "TercerCommit" (list "archivo 6")) (list "CuartoCommit" (list "archivo 7" "archivo 8"))))
                       ))
((git push)ejemploPush_3)

(display "\n\n")

;Funcion Status
;Primer ejemplo:
(display "Funcion Status, Ejemplo 1:\n")
(define ejemploStatus_1 (Zonas
                         (WorkSpace "archivo 1" )
                         (index "archivo 2")
                         (localRepository (list (list "PrimerCommit" (list "archivo 3"))))
                         (remoteRepository (list (list "SegundoCommit" (list "archivo 4"))))
                         ))
((git status)ejemploStatus_1)

;Segundo ejemplo:
(display "Funcion Status, Ejemplo 2:\n")
(define ejemploStatus_2 (Zonas
                         (WorkSpace "archivo 1" )
                         (index "archivo 2" "archivo 3 " "archivo 4")
                         (localRepository (list (list "PrimerCommit" (list "archivo 5")) (list "SegundoCommit" (list "archivo 6" "archivo 7"))))
                         (remoteRepository (list (list "TercerCommit" (list "archivo 8"))))
                         ))
((git status)ejemploStatus_2)

;Tercer ejemplo:
(display "Funcion Status, Ejemplo 3:\n")
(define ejemploStatus_3 (Zonas
                         (WorkSpace "archivo 1")
                         (index "archivo 2")
                         (localRepository (list (list "PrimerCommit" (list "archivo 5")) (list "SegundoCommit" (list "archivo 6" "archivo 7")) (list "TercerCommit" (list "archivo 7" "archivo 8"))))
                         (remoteRepository (list (list "CuartoCommit" (list "archivo 9"))))
                         ))
((git status)ejemploStatus_3)

(display "\n\n")

;Funcion Log
;Primer ejemplo:
(display "Funcion Log, Ejemplo 1:\n")
(define ejemploLog_1 (Zonas
                      (WorkSpace "archivo 1" )
                      (index "archivo 2")
                      (localRepository (list (list "PrimerCommit" (list "archivo 3"))))
                      (remoteRepository (list (list "SegundoCommit" (list "archivo 4"))))
                      ))
(log ejemploLog_1)

;Segundo ejemplo:
(display "Funcion Log, Ejemplo 2:\n")
(define ejemploLog_2 (Zonas
                      (WorkSpace "archivo 1" )
                      (index "archivo 2" "archivo 3 " "archivo 4")
                      (localRepository (list (list "PrimerCommit" (list "archivo 5")) (list "SegundoCommit" (list "archivo 6" "archivo 7"))))
                      (remoteRepository (list (list "TercerCommit" (list "archivo 8"))))
                      ))
(log ejemploLog_2)

;Tercer ejemplo:
(display "Funcion Log, Ejemplo 3:\n")
(define ejemploLog_3 (Zonas
                         (WorkSpace "archivo 1")
                         (index "archivo 2")
                         (localRepository (list (list "PrimerCommit" (list "archivo 5")) (list "SegundoCommit" (list "archivo 6" "archivo 7")) (list "TercerCommit" (list "archivo 7" "archivo 8"))))
                         (remoteRepository (list (list "CuartoCommit" (list "archivo 9"))))
                         ))
(log ejemploLog_3)

(display "\n\n")


;ADICIONAL
;Ejemplo de uso estandar del programa
(display "Uso estandar del programa\nDefinimos la siguiente Zona:")
(define ejemploZona (Zonas
                     (WorkSpace "archivo 1")
                     (index "archivo 2")
                     (localRepository (list (list "PrimerCommit" (list "archivo 3" "archivo 4"))))
                     (remoteRepository (list (list "SegundoCommit" (list "archivo 5" "archivo 6"))))
                     ))
ejemploZona

(display "Aplicamos git pull:\n")
(define ejemploZonaPull ((git pull)ejemploZona))
ejemploZonaPull
(display "Aplicamos git add del <archivo 1> y <achivo 6>:\n")
(define ejemploZonaAdd1 (((git add)(list "archivo 1" "archivo 6"))ejemploZonaPull))
ejemploZonaAdd1
(display "Aplicamos git add del <archivo 5>:\n")
(define ejemploZonaAdd2 (((git add)(list "archivo 5"))ejemploZonaAdd1))
ejemploZonaAdd2
(display "Aplicamos git commit:\n")
(define ejemploZonaCommit (((git commit)"CommitPruebaGeneral")ejemploZonaAdd2))
ejemploZonaCommit
(display "Aplicamos git push:\n")
(define ejemploZonaPush ((git push)ejemploZonaCommit))
ejemploZonaPush
(display "~Fin Prueba General~")