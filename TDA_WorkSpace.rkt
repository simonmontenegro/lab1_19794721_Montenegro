#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "TDA_Commit.rkt")


; |-------------------------------------------------------|
; |------------------ ~ TDA WorkSpace ~ ------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion constructora de un "WorkSpace"
;Dominio: String, ... , String
;Recorrido: Lista (de strings)
;Recursion: No aplica
(define (WorkSpace . archivo)
  (if (> (lenLista archivo) 0)
      (if (esString? archivo)
          archivo
          #f
          )
      #f
      )
  )
       
; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica que un WorkSpace efectivamente sea un WorkSpace
;Dominio: WorkSpace
;Recorrido: Booleano
;Recursion: No aplica
(define workSpace?
  (lambda (L)
    (if (list? L)
        (if (null? L)
            #f
            (if (esString? L)
                #t
                #f
                )
            )
        #f
        )
    )
  )
; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
;Descripcion: Funcion que obtiene la posicion de un archivo en el WorkSpace
;Dominio: WorkSpace, String (archivo)
;Recorrido: Entero
;Recursion: No Aplica
(define getPosWorkSpace
  (lambda (workspace archivo)
    (if (workSpace? workspace)
        (posicionEnLista workspace archivo)    
        #f
        )
    )
  )

;Descripcion: Funcion que obtiene un archivo en determinada posicion del WorkSpace
;Dominio: WorkSpace, Entero
;Recorrido: String (archivo)
;Recursion: No aplica
(define getElemWorkSpace
  (lambda (workspace posicion)
    (if (workSpace? workspace)
        (elementoEnLista workspace posicion)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|
;Descripcion: Funcion que verifica si un archivo esta contenido en el WorkSpace
;Dominio: WorkSpace, String (archivo)
;Recorrido: Booleano
;Recursion: Natural
(define archivoInWorkSpace?
  (lambda (workspace archivo)
      (if (null? workspace)
          #f
          (if (equal? (car workspace) archivo)
              #t
              (archivoInWorkSpace? (cdr workspace) archivo)
              )
          )
    )
  )

;Descripcion: Funcion que verifica si los archivos de una lista de archivos estan
;             contenidos en el WorkSpace
;Dominio: WorkSpace, Lista (de archivos)
;Recorrido: Booleano
;Recursion: Natural
(define listaArchivosInWorkSpace?
  (lambda (workspace lista)
    (if (null? lista)
        #t
        (if (equal? (archivoInWorkSpace? workspace (car lista)) #t)
            (listaArchivosInWorkSpace? workspace (cdr lista))
            #f
            )
        )
    )
  )

;Descripcion: Funcion que representa el WorkSpace en formato string
;Dominio: WorkSpace
;Recorrido: String
;Recursion: Cola
(define getStringDeWorkSpace
  (lambda (workspace)
    (define getStringDeWorkSpaceAux
      (lambda (workspace string)
        (if (null? workspace)
            string
            (getStringDeWorkSpaceAux (cdr workspace) (string-append string (car workspace) "\n"))
            )
        )
      )
    (getStringDeWorkSpaceAux workspace "\n\n\n~WorkSpace~\n")
    )
  )

          

