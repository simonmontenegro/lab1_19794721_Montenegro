#lang racket
(provide (all-defined-out))

(require "Listas.rkt")

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
(define esIndex?
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
(define encontrarElemento
  (lambda (L elemento)
    (if (esIndex? L)
        (posicionEnLista L elemento)    
        #f
        )
    )
  )

(define encontrarElementoEnPosicion
  (lambda (L posicion)
    (if (esIndex? L)
        (elementoEnLista L posicion)
        #f
        )
    )
  )
         

; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|










         