#lang racket
(require racket/date)
(provide (all-defined-out))

(define historial (list))

(define agregarComando
  (lambda (comando historial)
    (define agregarComandoAux
      (lambda (comando historial nuevoHistorial)
        (if (null? historial)
            (cons (list comando (date->string (current-date) second)) nuevoHistorial)
            (agregarComandoAux comando (cdr historial) (cons (car historial) nuevoHistorial))
            )
        )
      )
    (agregarComandoAux comando historial '())
    )
  )