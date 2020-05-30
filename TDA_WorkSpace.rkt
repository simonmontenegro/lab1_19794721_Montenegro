#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "Funciones.rkt")


; |-------------------------------------------------------|
; |------------------ ~ TDA WorkSpace ~ ------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define (workSpace . archivo)
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
(define workSpace?
  (lambda (L)
    (if (null? L)
        #f
        (if (esString? L)
            #t
            #f
            )
        )
    )
  )
; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getPosWorkSpace
  (lambda (L elemento)
    (if (workSpace? L)
        (posicionEnLista L elemento)    
        #f
        )
    )
  )

(define getElemWorkSpace
  (lambda (L posicion)
    (if (workSpace? L)
        (elementoEnLista L posicion)
        #f
        )
    )
  )
         
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
(define deleteElemWorkSpace
  (lambda (L eliminar)
    (if (workSpace? L)
        (quitarElemento L eliminar)
        #f
        )
    )
  )

(define addElemWorkSpace
  (lambda (L agregar)
    (if (workSpace? L)
        (agregarElemento L agregar)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|

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

(define work (workSpace "archivo1" "archivo2" "archivo3" "archivo4"))
          

