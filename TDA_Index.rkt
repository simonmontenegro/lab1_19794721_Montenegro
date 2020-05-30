#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "TDA_Commit.rkt")

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

; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|

(define getStringDeIndex
  (lambda (index)
    (define getStringDeIndexAux
      (lambda (index string)
        (if (null? index)
            string
            (getStringDeIndexAux (cdr index) (string-append string (car index) "\n"))
            )
        )
      )
    (getStringDeIndexAux index "\n\n\n~Index~\n")
    )
  )

(define ind (index "archivo1" "archivo2" "archivo3" "archivo4"))