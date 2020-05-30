#lang racket

(provide (all-defined-out))


;Descripcion: Funcion que retorna el largo de una lista de elementos
;Dominio: Lista
;Recorrido: Entero (cantidad de elementos de la lista)
;Recursion: Cola
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

;Descripcion: Funcion que verifica si cada elemento de una lista es String
;Dominio: Lista
;Recorrido: Booleano
;Recursion: Natural
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

;Descripcion: Funcion que retorna un elemento en una posicion determinada
;Dominio: Lista, Entero (posicion del elemento)
;Recorrido: Elemento
;Recursion: Cola
(define posicionEnLista
  (lambda (Lista elemento)
    (define posicionEnListaAux
      (lambda (L elem voyEn)
        (if (null? L)
            #f
            (if (equal? elem (car L))
                voyEn
                (posicionEnListaAux (cdr L) elem (+ voyEn 1))
                )
            )
        )
      )
    (posicionEnListaAux Lista elemento 1)
    )
  )

;Descripcion: Funcion que retorna la posicion de un elemento en una lista
;Dominio: Lista, Elemento
;Recorrido: Entero (posicion del elemento)
;Recursion: Cola
(define elementoEnLista
  (lambda (L posicion)
    (define elementoEnListaAux
      (lambda (L posicion voyEn)
        (if (null? L)
            #f
            (if (= voyEn posicion)
                (car L)
                (elementoEnListaAux (cdr L) posicion (+ voyEn 1))
                )
            )
        )
      )
    (elementoEnListaAux L posicion 1)
    )
  )

;Descripcion: Funcion que agrega un elemento a una lista
;Dominio: Lista, Elemento
;Recorrido: Lista
;Recursion: No aplica
(define agregarElemento
  (lambda (L agregar)
    (if (null? L)
        (list agregar)
        (append L (list agregar))
        )
    )
  )

;Descripcion: Funcion que elimina un elemento de una lista
;Dominio: Lista, Elemento
;Recorrido: Lista
;Recursion: Cola
(define quitarElemento
  (lambda (L quitar)
    (define quitarElementoAux
      (lambda (L quitar Lnueva)
        (if (null? L)
          Lnueva
          (if (equal? (car L) quitar)
              (quitarElementoAux (cdr L) quitar Lnueva)
              (quitarElementoAux (cdr L) quitar (agregarElemento Lnueva (car L)))
              ) 
          )
        )
      )
    (quitarElementoAux L quitar '( ) )
      )
    )










        
        
        