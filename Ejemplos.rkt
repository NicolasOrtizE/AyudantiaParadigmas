#lang racket

; comentarios
; tipos de datos
; enteros
; flotantes
; listas
; pares
; strings

(define a 12)

(define (funcion parametro1 parametro2)
  (list parametro1 parametro2)
)

(define (suma n1 n2)
  (+ n1 n2)
)

(define b '("string1" 98 '("asldkjaslkd" 20186094 123.5) "string2") )

(define c (cons b "string3"))

(define d (cons (cons b "string3") null))


;TDA FECHA
;nivel 0: representacion
; Este TDA representa una fecha con dia mes año y se ordena en una lista con ese mismo orden

;nivel 1 constructor
;funcion que contruye el TDA fecha
;dominio: Enteros
;recorrido; lista
;recusion; no
(define (createFecha dia mes anho)
  (list dia mes anho)
 )

;nivel 2 pertencia
;funcion que verifica si el TDA fecha es correcto
;dominio es una lista
;un valor booleano
(define (isfecha? fecha)
  (if (and  (and (>= (car fecha) 1) (<= (car fecha) 31))
            (and (>= (car (cdr fecha)) 1) (<= (car (cdr fecha)) 12))
            (= (car (cdr (cdr fecha))) 2021)
       )
      #t ;True
      #f ;False
  )
)

;nivel 3 getter
(define (getDia fecha)
  (if (isfecha? fecha)
      (car fecha)
      null               ;En los getter no retornar Falso porque induce a un Break en el codigo
  )
)

(define (getMes fecha)
  (if (isfecha? fecha)
      (car (cdr fecha))
      null               ;En los getter no retornar Falso porque induce a un Break en el codigo
  )
)

(define (getAnho fecha)
  (if (isfecha? fecha)
      (car (cdr (cdr fecha)))
      null               ;En los getter no retornar Falso porque induce a un Break en el codigo
  )
)

;nivel 4 setters
(define (setDia fecha newDia)
  (if (isfecha? fecha)
      (if (and (>= newDia 1)
               (<= newDia 31)
           )
          (createFecha newDia (getMes fecha) (getAnho fecha))
          fecha)
      null
  )
)

(define (setMes fecha newMes)
  (if (isfecha? fecha)
      (if (and (>= newMes 1)
               (<= newMes 12)
           )
          (createFecha (getDia fecha) newMes (getAnho fecha))
          fecha)
      null
  )
)

;(cabeza, cola)
; ( car lista) = cabeza
; (cdr lista) =cola


;Nivel 5
(define (isBisiesto? fecha)
  (if (isfecha? fecha)
      (if (= (modulo (car (cdr (cdr fecha))) 4) 0)
          #t
          #f
      )
      #f
   )
)


(define ejemplo1 (createFecha 1 10 2021))
(define ejemplo3 (createFecha 2 11 2021))
(define ejemplo2 (createFecha 32 98 3546654))

(define pertenencia1 (isfecha? ejemplo1))
(define pertenencia2 (isfecha? ejemplo2))
(define pertenencia3 (isfecha? ejemplo3))

(define getterDia1 (getDia ejemplo1))
(define getterDia2 (getDia ejemplo2))
(define getterDia3 (getDia ejemplo3))

(provide (all-define-out))