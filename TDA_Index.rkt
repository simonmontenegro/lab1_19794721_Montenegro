#lang racket
(provide (all-defined-out))

(require "Listas.rkt")
(require "TDA_Commit.rkt")

; |-------------------------------------------------------|
; |-------------------- ~ TDA Index ~ --------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion Constructora de un Index
;Dominio: String, ... , String
;Recorrido: Lista (de Strings)
;Recursion: No aplica
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
;Descripcion: Funcion que verifica que un index efectivamente sea un index
;Dominio: Index
;Recorrido: Booleano
;Recursion: No aplica
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
;Descripcion: Funcion que retorna la posicion de un archivo en un index
;Dominio: Index, String (archivo)
;Recorrido: Entero
;Recursion: No aplica
(define getElemIndex
  (lambda (index archivo)
    (if (index? index)
        (posicionEnLista index archivo)    
        #f
        )
    )
  )

;Descripcion: Funcion que retonra un archivo en determinada posicion de un index
;Dominio: Index, Entero
;Recorrido: String (archivo)
;Recursion: No aplica
(define getPosIndex
  (lambda (index posicion)
    (if (index? index)
        (elementoEnLista index posicion)
        #f
        )
    )
  )
         
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
;Descripcion: Funcion que elimina un archivo de un index
;Dominio: Index, String (archivo)
;Recorrido: Index
;Recursion: No aplica
(define deleteElemIndex
  (lambda (index eliminar)
    (if (index? index)
        (quitarElemento index eliminar)
        #f
        )
    )
  )

;Descripcion: Funcion que añade un archivo a un index
;Dominio: Index, String (archivo)
;Recorrido: Index
;Recursion: No aplica
(define addElemIndex
  (lambda (index agregar)
    (if (index? index)
        (agregarElemento index agregar)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                      ~ Otros ~                        |
; |-------------------------------------------------------|
;Descripcion: Funcion que representa el Index en formato string
;Dominio: Index
;Recorrido: String
;Recursion: Cola
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