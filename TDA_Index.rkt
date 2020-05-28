#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "Funciones.rkt")

; |-------------------------------------------------------|
; |-------------------- ~ TDA Index ~ --------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define (index . archivo)
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
(define index?
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
(define getElemIndex
  (lambda (L elemento)
    (if (index? L)
        (posicionEnLista L elemento)    
        #f
        )
    )
  )

(define getPosIndex
  (lambda (L posicion)
    (if (index? L)
        (elementoEnLista L posicion)
        #f
        )
    )
  )
         
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
(define deleteElemIndex
  (lambda (L eliminar)
    (if (index? L)
        (quitarElemento L eliminar)
        #f
        )
    )
  )

(define addElemIndex
  (lambda (L agregar)
    (if (index? L)
        (agregarElemento L agregar)
        #f
        )
    )
  )
