#lang racket

(require "TDAindex.rkt")

;TDA que representa al repositorio local
;REPRESENTACION
;uns lista que contiene el commit (string) y los cambios guardados en el index(lista)

;CONSTRUCTOR
;funcion que me crea el repositorio local
;entradas: un string (commit) y una lista (index)
;salida: una lista (repositorio local)
(define (createLocalRepository commit index)
  (if (and (string? commit) (index? index))
      (list (list commit index))
      '()
  )
)

;PERTENENCIA
;Funcion que verifica si el argumento es un repositorio local
;entradas: un repositorio local (lista de listas)
;salida: valor booleano
;recursion de cola (no deja estados pendientes
;dominio: lista
;recorrido, listas y strings
(define (localRepository? localRep)
  ;caso base
  (if (null? localRep)
      ;si llega al final es verdad
      #t
      ;caso recursivo
      ;se comprueba que los elementos de la lista son los correspondientes
      (if (and (list? (car localRep))
           (string? (car (car localRep)))
           ;el primer elemento de la cola del commit actual (PD: estuve peleando con racket porque hacer un debug es un suplicio)
           (index? (car(cdr (car localRep)))))
          (localRepository? (cdr localRep))
          #f
      )
   )
)

;SELECTORES
;Funcion que selecciona el commit escrito en el n-esimo elemento repositorio local 
;entradas: un repositorio local (lista)
;salidas: el commit (string)
;recursion, de cola no deja estados pendientes
(define (seleccionarCommit localRep n)
  ;caso especial
  (if (or (not (localRepository? localRep)) (> n (length localRep)))
      #f
      ;caso base
      (if (and (localRepository? localRep)
           (= n 0))
           (car (car localRep))
           ;caso recursivo
           (seleccionarCommit (cdr localRep) (- n 1))
      )
  )
)

;funcion que selecciona los cambios(index) agregados al n-esimo elemento repositorioLocal
;entradas: repositorio local(lista)
;salidas: un index (lista)
(define (seleccionarIndex localRep n)
  ;caso especial
  (if (or (not (localRepository? localRep)) (> n (length localRep)))
      #f
      ;caso base
      (if (and (localRepository? localRep)
           (= n 0))
           (cadr (car localRep))
           ;caso recursivo
           (seleccionarCommit (cdr localRep) (- n 1))
      )
  )
)

;MODIFICADORES
;funcion que agrega un commit al repositorio local (agrega el elemento al final de la lista
;entradas: un repoLocal, un commit y un index
;salida: el repo local modificado
;recursion de pila (deja estados pendientes)
;dominio: listas
;recorrido:listas
(define (agregarCommit localRep commit index)
  ;veremos el caso particular cuando el commit y el index no cumple la condificion
  (if (or (not (string? commit)) (not (index? index)) (not (localRepository? localRep)))
      ;como no es valido el commit o el index, veremos si el repositorio local es valido para retornarlo o retornar una lista vacia
      (if (localRepository? localRep)
          localRep
          null
      )
      ;si el commit cumple 
      (if (and (null? localRep)
           (string? commit)
           (index? index))
          ;si cumple la condicion
          (cons (list commit index) null)
          ;si no cumple condicion
          (cons (car localRep) (agregarCommit (cdr localRep) commit index))
      )
  )
)

;----------------------------------------------------------------EJEMPLOS--------------------
(define localRep1 (createLocalRepository "Mi commit1" Index1))
(define localRep2 (createLocalRepository "Mi commit2" Index2))
(define localRep3 (createLocalRepository "Mi commit malo porque no funca uwu" Index3)) ;.> no valido

(define verificarLocalRep1 (localRepository? localRep1))
(define verificarLocalRep2 (localRepository? localRep2))
(define verificarLocalRep3 (localRepository? localRep3)) ;-> no valido

(define commit1 (seleccionarCommit localRep1 0))
(define commit2 (seleccionarCommit localRep2 0))
(define commit3 (seleccionarCommit localRep3 0)) ;-> no valido

(define indexLocal1 (seleccionarIndex localRep1 0))
(define indexLocal2 (seleccionarIndex localRep2 0))
(define indexLocal3 (seleccionarIndex localRep3 0))

(define localRepMod1 (agregarCommit localRep1 "este es un nuevo commit" Index2))
(define localRepMod2 (agregarCommit localRep2 "este commit es nuevo" Index2))
(define localRepMod3 (agregarCommit localRep1 "este  commit no va a entrar" Index3))


(provide (all-defined-out)) 