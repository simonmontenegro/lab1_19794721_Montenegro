#lang racket

(provide (all-defined-out))


(define lenLista
  (lambda (Lista)
    (define largo
      (lambda (Lista elementos)
        (if (null? (cdr Lista))
            elementos
            (largo (cdr Lista) (+ elementos 1))
            )
        )
      )
    (largo Lista 1)
    )
  )

(define esString?
  (lambda (Lista)
    (if (null? Lista)
        #t
        (if (string? (car Lista))
            (esString? (cdr Lista))
            #f
            )
        )
    )
  )
        
        