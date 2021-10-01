#lang racket

(require  "TDAworkspace.rkt")

;TDA que representa el index
;la representacion seria identica al workspace

;CONSTRUCTOR
;Funcion que construye un index
;entradas; un string que representa los cambios
;salidas: una lista que contiene los cambios
;recursion natural
(define (createIndex workspace archivos)
  ;caso base
  (if (and (workspace? workspace) (null? archivos))
       null
       (if (buscar workspace (car archivos))
           ;si el archivo esta, se agrega al index
           (cons (car archivos) (createIndex workspace (cdr archivos)))
           ;si no se hace el caso recursivo
           (createIndex workspace (cdr archivos))
       )
  )
)

;PERTENENCIA
;Funcion que verifica si es un Index 
;entradas: un index (lista)
;salida: valor booleano
;recursion: de cola(no deja estados pendientes)
(define (index? index)
  (if (list? index)
      ;caso base
      (if (null? index)   ;si llega a la lista vacia significa que reviso todos los elementos
           #t
           ;caso recursivo
           (if (string? (car index))
               ;en caso de ser string revisa el siguiente
               (index? (cdr index))
                #f
            )
       )
       #f
   )
)

;SELECTORES:
;Funcion que selecciona el "cambio" n-esimo del index
;tipo de recursion: de cola(no deja estados pendientes)
;dominio: una lista
;recorrido: strings
;entradas: un index(lista)
;salida: un cambio (string)
(define (seleccionarCambio index n)
  ;condiciones para entrar a buscar el archivo
  (if (and (index? index) (< n (length index)))
      ;caso base
      (if (= n 0)
          (car index)
          ;caso recursivo
          (seleccionarCambio (cdr index) (- n 1))
      )
      #f
   )
)


;MODIFICADORES
;Funcion que agrega un cambio al final del index
;tipo de recursion: de pila (deja estados pendientes)
;dominio: una lista
;recorrido:strings
;entradas: un index y un cambio (lista y string)
;salidas: la misma lista con un elemento extra al final
(define (agregarCambio index nuevoCambio)
  (if (and (index? index) (string? nuevoCambio))
      ;caso base
      (if (null? index)
          (cons nuevoCambio null)
          ;caso recursivo
          (cons (car index) (agregarCambio (cdr index) nuevoCambio))
      )
      #f
  )    
)


;OPERADORES
;funcion que busca si esta el elemento en una lista 
;entradas: una lista y un string
;salidas; valor booleano
;dominio: una lista
;recorrido: strings
;recursion; de cola (no deja estados pendientes)
(define (buscar lista elemento)
  ;caso base
  (if (or (null? lista) (not (string? elemento)))   ;si el elemento no es un string o si llega al final de la lista significa que elemento no está
        #f
        (if (and (string? elemento) (eq? elemento (car lista))) ;si la lista no esta vacia y el elemento coincide con la cabeza de la lista 
             #t
             ;caso recursivo
             (buscar (cdr lista) elemento)
         )
   )
)

;--------------------------------EJEMPLOS------------
(define Index1 (createIndex workspace1 (list "file1.rkt" "file3.rkt" "file5.rkt")))
(define Index2 (createIndex workspace2 (list "archivo1.rkt" "archivo3.rkt" "archivo5.rkt")))
(define Index3 (createIndex workspace3 (list "file1.rkt" "file3.rkt" "file5.rkt"))) ;->ejemplo no valido
;esos ejemplos se usaran en el localRepository

(define verificarIndex1 (index? Index1))
(define verificarIndex2 (index? Index2))
(define verificarindex3 (index? Index3)) ;-> no valido

(define cambio1 (seleccionarCambio Index1 1))
(define cambio2 (seleccionarCambio Index2 2))
(define cambio3 (seleccionarCambio Index2 4)) ;->no valido

(define modIndex1 (agregarCambio Index1 "file6"))
(define modIndex2 (agregarCambio Index2 "archivo6"))
(define modIndex3 (agregarCambio Index2 6456)) ;->no valido


(provide (all-defined-out))  