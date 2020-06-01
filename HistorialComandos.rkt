#lang racket
(require racket/date)
(require "Listas.rkt")
(provide (all-defined-out))


;Descripcion: Funcion que agrega un comando y la fecha de utilizacion de dicho comando a una lista  
;Dominio: String, Lista
;Recorrido: Lista
;Recursion: No aplica
(define agregarComando
  (lambda (comando historial)
    (if (null? historial)
        (list (list comando (date->string (current-date) second)))
        (append historial (list(list comando (date->string (current-date) second))))
        )
    
    )
  )
