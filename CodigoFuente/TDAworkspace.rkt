#lang racket

;TDA que representa el workspace
;El workspace es solo el directorio donde estan almacenados los archivos

;REPRESENTACION
;Sera una lista con los archivos (strings) simulando la carpeta donde se clona el repositorio

;CONSTRUCTOR
;Funcion que crea un workspace
(define (createWorkspace archivo)
  (if (string? archivo)
      (list archivo)
      '()
  )
)

;PERTENENCIA
;funcion que verifica que es un workspace
;tipo de recursion: de cola(no deja estados pendientes)
;dominio: una lista
;recorrido:strings
(define (workspace? workspace)
  ;esta condicion significa que recorrio toda la lista por tanto, todos los elementos son "archivos"
  (if (list?  workspace)
      ;caso base
      (if (null? workspace)
          ;en caso de ser nulo, significa que llega al final de la lista por tanto todos los elementos son strings
          #t
          (if (string? (car workspace))
              ;en caso de ser string se revisa el siguiente (recursion)
              (workspace? (cdr workspace))
              ;si no es string se retorna falso
              #f
          )
      )
      #f
  )
)

;SELECTORES
;Funcion que selecciona el "archivo" n-esimo de la "carpeta local"
;tipo de recursion: de cola(no deja estados pendientes)
;dominio: una lista
;recorrido: strings
(define (seleccionarArchivo workspace n)
  ;condiciones para entrar a buscar el archivo
  (if (and (workspace? workspace) (< n (length workspace)))
      ;caso base
      (if (= n 0)
          (car workspace)
          ;caso recursivo
          (seleccionarArchivo (cdr workspace) (- n 1))
      )
      #f
   )
)

;MODIFICADORES
;Funcion que agrega un elemento al final del workspace
;tipo de recursion: de pila (deja estados pendientes)
;dominio: una lista
;recorrido:strings
(define (agregarArchivo workspace nuevoArchivo)
  (if (and (workspace? workspace) (string? nuevoArchivo))
      ;caso base
      (if (null? workspace)
          (cons nuevoArchivo null)
          ;caso recursivo
          (cons (car workspace) (agregarArchivo (cdr workspace) nuevoArchivo))
      )
      #f
  )    
)

;Funcion que elimimina el elemento n-esimo del workspace
;tipo de recursion: de pila (deja estados pendientes
;dominio: una lista
;recorrido: strings
(define (eliminarArchivo workspace archivo)
  (if (and (string? archivo) (workspace? workspace))
      ;caso base
      (if (null? workspace)
          ;si es nulo, llegamos al final de la lista 
          null 
          ;caso recursivo
          (if (eq? archivo (car workspace))
              (eliminarArchivo (cdr workspace) archivo)
              (cons (car workspace) (eliminarArchivo (cdr workspace) archivo))
          )
      )
      #f
  )
)

;-------------------------------------------------------------EJEMPLOS---------------------------
;EL TERCER EJEMPLO SIEMPRE SERA ERRONEO
(define ejemplo1 (createWorkspace "file1.rkt"))
(define ejemplo2 (createWorkspace "archivo1.rkt"))
(define ejemplo3 (createWorkspace 1))

(define verficarWorkspace1 (workspace? ejemplo1))
(define verficarWorkspace2 (workspace? ejemplo2))
(define verficarWorkspace3 (workspace? ejemplo3))

(define WS1 (agregarArchivo ejemplo1 "file2.rkt"))
(define WS2 (agregarArchivo ejemplo2 "archivo2.rkt"))
(define WS3 (agregarArchivo ejemplo3 2))

(define Ws1 (eliminarArchivo WS1 "file1.rkt"))
(define Ws2 (eliminarArchivo WS2 "Archivo1.rkt"))
(define Ws3 (eliminarArchivo WS2 "ArchivoquenoexisteenelWs2.uwu"))

(define archivo2 (seleccionarArchivo (list "file1.rkt" "file2.rkt" "file3.rkt" "file4.rkt") 2))
(define archivo3 (seleccionarArchivo (list "file1.rkt" "file2.rkt" "file3.rkt" "file4.rkt") 3))
(define archivomalo (seleccionarArchivo (list "file1.rkt" "file2.rkt" 3 "file4.rkt") 4))

;----------------------------------EJEMPLOS QUE USARE EN EL INDEX----------------------------------
(define workspace1 (list "file1.rkt" "file2.rkt" "file3.rkt" "file4.rkt" "file5.rkt"))
(define workspace2 (list "archivo1.rkt" "archivo2.rkt" "archivo3.rkt" "archivo4.rkt" "archivo5.rkt"))
(define workspace3 (list "elemento1.rkt" "elemento2.rkt" 23456 "elemento4.rkt" "elemento5.rkt")) ;-> ejemplo no valido

(provide (all-defined-out))            
              
          
