#lang racket
; |-------------------------------------------------------|
; |------------------- ~ TDA ARCHIVO ~ -------------------|
; |-------------------------------------------------------|
; |                    ~ Constructor ~                    |
; |-------------------------------------------------------|
(define archivo
  (lambda(nombre_archivo contenido)
    (list nombre_archivo contenido)
  )
)
; |-------------------------------------------------------|
; |                    ~ Pertenencia ~                    |
; |-------------------------------------------------------|
(define es_archivo?
  (lambda(lista_aux)
    (if (null? lista_aux)
        #f
        (if (list? lista_aux)
            #t
            #f
        )
    )
  )
)