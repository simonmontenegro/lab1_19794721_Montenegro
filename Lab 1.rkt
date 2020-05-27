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
; |--------------- ~ TDA LocalRepository ~ ---------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define localRepository
  (lambda (commit)
    (if (> (lenLista commit) 0)
      (if (esCommit? commit)
          commit
          '()
          )
      #f
      )
    )
  )

; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define localRepository?
  (lambda (local)
    (and (not (null? local)) (esCommit? local))
    )
  )

; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define getElemCommit
  (lambda (local pos)
    (if (localRepository? local)
        (elementoCommit local pos)
        '()
        )
    )
  )

(define getPosCommit
  (lambda (local elemento)
    (if (localRepository? local)
        (posCommit local elemento)
        '()
        )
    )
  )
        
; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|

(define addCommit
  (lambda (local nuevoCommit)
    (if (localRepository? local)
        (agregarComm local nuevoCommit)
        '()
        )
    )
  )

(define delCommit
  (lambda (local delCommit)
    (if (localRepository? local)
        (eliminarComm local delCommit)
        )
    )
  )







(define listaCom (list (list "mensaje1" (list "uno" "dos")) (list "mensaje2" (list "tres" "cuatro"))))

        




         