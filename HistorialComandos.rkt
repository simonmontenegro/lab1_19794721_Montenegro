#lang racket
(require racket/date)
(require "Listas.rkt")
(provide (all-defined-out))

(define historial (list))

(define agregarComando
  (lambda (comando historial)
    (if (null? historial)
        (list (list comando (date->string (current-date) second)))
        (append historial (list(list comando (date->string (current-date) second))))
        )
    
    )
  )

;(define historial2 (agregarComando "pull" historial))
;(define historial3 (agregarComando "add" historial2))
;(define historial4 (agregarComando "commit" historial3))
;(define historial5 (agregarComando "push" historial4))



;'(("commit" "Friday, May 29th, 2020 11:33:00pm") ("add" "Friday, May 29th, 2020 11:33:00pm") ("pull" "Friday, May 29th, 2020 11:33:00pm") )