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


