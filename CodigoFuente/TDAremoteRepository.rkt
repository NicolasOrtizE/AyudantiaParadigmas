#lang racket

(require "TDAlocalRepository.rkt")
(require "TDAindex.rkt")

;TDA que representa al repositorio local
;REPRESENTACION
;uns lista que contiene repositorios locales, cada elemento es un repositorio local

;CONSTRUCTOR
;funcion que me crea el repositorio local
;entradas: una lista (repositorioLocal)
;salida: una lista (repositorio remoto)
;recursion natural (deja estados pendientes)
;dominio:lista
;recorrido:listas
(define (createRemoteRepository localRep)
  (if (localRepository? localRep)
      (if (null? localRep)
          null
          (cons (car localRep) (createRemoteRepository (cdr localRep)))
      )
      '()
  )
)

;PERTENENCIA
;Funcion que verifica si el argumento es un repositorio local
;entradas: un repositorio local (lista)
;salida: valor booleano
;recursion: de cola(no deja estados pendientes)
;dominio:lista
;recorridos;listas
(define (remoteRepository? remoteRep)
  ;caso base, cuando llega al final de la lista (significa que todos los elementos cumple
  (if (localRepository? remoteRep)
      #t
      #f   
   )
)

;SELECTORES
;Funcion que selecciona el n-esimo repositorio local guardado en el repositorio remoto
;entradas: un repositorio remoto (lista), un entero (n)
;salidas: el repositoriolocal n guardado
;recursion: de cola, no deja estados pendientes
;dominio:lista
;recorrido:lista
(define (seleccionarRemoteRep remoteRep n)
  (if (and (remoteRepository? remoteRep) (< n (length remoteRep)))
      ;caso base
      (if (= n 0)
          (car remoteRep)
          ;si n no es 0 caemos en el caso recursivo
          (seleccionarRemoteRep (cdr remoteRep) (- n 1))
      )   
      #f
  )
)

;MODIFICADORES
;funcion que grega un repositorio local al repo remoto
;entradas, un repoRemoto, un elemento del repositorio local
;salidas, un repo remoto modificado
(define (agregarRemoteRep remoteRep commit)
  (if (and (remoteRepository? remoteRep)
           (string? (car commit))
           (index? (cadr commit)))
      (cons commit remoteRep)
      remoteRep
  )
)

;------------------------------------------EJEMPLOS----------
(define remoteRep1 (createRemoteRepository localRep1))
(define remoteRep2 (createRemoteRepository localRep2))
(define remoteRep3 (createRemoteRepository localRep3)) ;->no valido

(define isRemoRep1? (remoteRepository? remoteRep1))
(define isRemoRep2? (remoteRepository? remoteRep2))
(define isRemoRep3? (remoteRepository? remoteRep3))

(define commitRemoto1 (seleccionarRemoteRep remoteRep1 0))
(define commitRemoto2 (seleccionarRemoteRep remoteRep2 0))
(define commitRemoto3 (seleccionarRemoteRep remoteRep3 0))

(define remoteRepMod1 (agregarRemoteRep remoteRep1 (list "este es un commit para el remoto" (list "fileUWU.rkt" "fileACAB.rkt" "file1312.rkt"))))
(define remoteRepMod2 (agregarRemoteRep remoteRep2 (list "este remoto es un commit (dislexia pls)" (list "fileUWU.rkt" "fileACAB.rkt" "file1312.rkt"))))
(define remoteRepMod3 (agregarRemoteRep remoteRep3 (list "este es un commit no surge porque no es valido" (list "fileUWU.rkt" "fileACAB.rkt" "file1312.rkt"))))
  
(provide (all-defined-out)) 