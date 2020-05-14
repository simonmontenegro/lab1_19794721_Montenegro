#lang racket
; |-------------------------------------------------------|
; |----------------- ~ TDA Repositorio ~ -----------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define repositorio
  (lambda(nombre_autor)
    (list nombre_autor (list ) (list ) (list ) (list ))
  )
)
; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define esRepositorio?
  (lambda(Lista)
    (if (null? Lista)
        #f
        (if (string? (car Lista))
            (if (and (list? (cadr Lista)) (list? (caddr Lista)) (list? (cadddr Lista)))
                #t
                #f
                )
            #f
            )
        )
    )
  )
            
; |-------------------------------------------------------|
; |                    ~ Selectores ~                     |
; |-------------------------------------------------------|
(define repositorio_getAutor
  (lambda (Lista)
    (if (esRepositorio? Lista)
        (car Lista)
        #f
        )
    )
  )

; |-------------------------------------------------------|
; |                  ~ Modificadores ~                    |
; |-------------------------------------------------------|
