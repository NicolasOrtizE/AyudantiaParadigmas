#lang racket

(require "TDAworkspace.rkt")
(require "TDAindex.rkt")
(require "TDAlocalRepository.rkt")
(require "TDAremoteRepository.rkt")

;TDA que representan las zonas de trabajo
;REPRESENTACION
;es una lista de 4 elementos donde el primer elemento es el workspace,
;el 2do es un index, el 3ro es el repositorio local y el 4to es el repositorio remoto

;CONSTRUCTOR
;funcion que crea el TDA zonas
;entradas, un workspace, un index, repositorio loca, y un repositorio remoto
;salidas: una lista con los 4 elementos
(define (createZonas workspace index localRep remoteRep registro)
  (if (and (workspace? workspace)
           (index? index)
           (localRepository? localRep)
           (remoteRepository? remoteRep)
           (list? registro))
      (list workspace index localRep remoteRep registro)
      '()
   )
)

;PERTENENCIA
;funcion que verifica que el TDA zonas sea valido
;entradas, zonas
;salida valor verdad
(define (zonas? zonas)
  (if (and (workspace? (car zonas))
           (index? (cadr zonas))
           (localRepository? (caddr zonas))
           (remoteRepository? (cadddr zonas))
           (list? (car (cdr (cdr (cdr (cdr zonas)))))))
      #t
      #f
  )
)

;SELECTORES
;funcion que selecciona el workspace
;entrada:un TDAzonas
;salidas:un workspace
(define (getWorkspace zonas)
  (if (zonas? zonas)
      (car zonas)
      '()
  )
)

;funcion que selecciona el index
;entrada:un TDAzonas
;salidas:un index
(define (getIndex zonas)
  (if (zonas? zonas)
      (cadr zonas)
      '()
  )
)
;funcion que selecciona el repositorio local
;entrada:un TDAzonas
;salidas:un repositorio local
(define (getLocalRep zonas)
  (if (zonas? zonas)
      (caddr zonas)
      '()
  )
)
;funcion que selecciona el repositorio remoto
;entrada:un TDAzonas
;salidas:un repositorio remoto
(define (getRemoteRep zonas)
  (if (zonas? zonas)
      (cadddr zonas)
      '()
  )
)

;funcion que selecciona el registro de comandos
;entrada:un TDAzonas
;salidas:un registro de los comandos (lista)
(define (getRegistro zonas)
  (if (zonas? zonas)
      (car (cdr (cdr (cdr (cdr zonas)))))
      '()
  )
)


;MODIFICADORES
;funcion que modifica el workspace del TDA zonas
;entradas: zonas y el workspace modificado
;salida:un TDA zonas modificado
(define (modificaWorkspace zonas newWorkspace)
  (if (and (zonas? zonas)
           (workspace? newWorkspace))
      (createZonas newWorkspace (getIndex zonas) (getLocalRep zonas) (getRemoteRep zonas) (getRegistro zonas))
      #f
  )
)

;funcion que modifica el index del TDA zonas
;entradas: zonas y el index modificado
;salida:un TDA zonas modificado
(define (modificaIndex zonas newIndex)
  (if (and (zonas? zonas)
           (index? newIndex))
      (createZonas (getWorkspace zonas) newIndex (getLocalRep zonas) (getRemoteRep zonas) (getRegistro zonas))
      #f
  )
)

;funcion que modifica el repositorio local del TDA zonas
;entradas: zonas y el repositorio local modificado
;salida:un TDA zonas modificado
(define (modificaLocalRep zonas newLocalRep)
  (if (and (zonas? zonas)
           (localRepository? newLocalRep))
      (createZonas (getWorkspace zonas) (getIndex zonas) newLocalRep (getRemoteRep zonas) (getRegistro zonas))
      #f
  )
)

;funcion que modifica el repositorio remoto del TDA zonas
;entradas: zonas y el repositorio remoto modificado
;salida:un TDA zonas modificado
(define (modificaRemoteRep zonas newRemoteRep)
  (if (and (zonas? zonas)
           (remoteRepository? newRemoteRep))
      (createZonas (getWorkspace zonas) (getIndex zonas) (getLocalRep zonas) newRemoteRep (getRegistro zonas))
      #f
  )
)

;funcion que modifica el registro de comandos del TDA zonas
;entradas: zonas y el registro de comandos modificado
;salida:un TDA zonas modificado
(define (modificaRegistro zonas newComando)
  (if (and (zonas? zonas)
           (or (eq? newComando "pull")
               (eq? newComando "add")
               (eq? newComando "commit")
               (eq? newComando "push")
               (eq? newComando "status")
               (eq? newComando "log")))
      (createZonas (getWorkspace zonas) (getIndex zonas) (getLocalRep zonas) (getRemoteRep zonas) (cons newComando (getRegistro zonas)))
      #f
  )
)

;-------------------------------------------------EJEMPLOS------------------------------------------------
(define zona1 (createZonas workspace1 Index1 localRep1 remoteRep1 '()))
(define zona2 (createZonas workspace2 Index2 localRep2 remoteRep2 '()))
(define zona3 (createZonas workspace3 Index3 localRep3 remoteRep3 '())) ;-> no valido

(define iszona1 (zonas? zona1))
(define iszona2 (zonas? zona2))
(define iszona3 (zonas? zona3)) ;-> ejemplo no valido

;los modificadores se haran en el main mediante los comandos

(provide (all-defined-out)) 