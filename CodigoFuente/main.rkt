#lang racket

(require "TDAworkspace.rkt")
(require "TDAindex.rkt")
(require "TDAlocalRepository.rkt")
(require "TDAremoteRepository.rkt")
(require "TDAzonas.rkt")

;-------------------------------REQUISITOS FUNCIONALES--------------

;Función que permite aplicar los comandos
;entradas:el comando (de manera currificada entra, las zonas, los archivos, el mensaje del commit)
;salida: las zonas modificadas
;no requiere recursion
(define (git comando)(lambda (zonas)
                       (lambda (archivos)
                         (lambda (mensaje)   
  (cond
    [(eq? comando "pull")   (pull zonas)]
    [(eq? comando "add")    ((add archivos)zonas)]
    [(eq? comando "commit") ((commit mensaje)zonas)]
    [(eq? comando "push")   (push zonas)]
    [(eq? comando "status") (status (modificaRegistro zonas "status"))]
    [(eq? comando "log")    (log (modificaRegistro zonas "log"))]
    [else (zonas)]
  )
))))

;funcion que trae todos los cambios del repositorio reoto al workspace
;fusiona todos los commits del repositorio remoto en el workspace
;entradas: zonas de trabajo (lista?)
;salida:lista (un workspace)
;recursion:de cola(no deja estados pendientes)
;dominio:listas
;recorrido: listas

;NOTA el parametro zonas es una lista de 4 elementos (workspace,index,local,remoto) en ese orden
(define (pull zonas)
  ;como el pull lleva los elementos del remoto al workspace
  (if (zonas? zonas)
      ;tenemos un caso base, cuando el repositorio remoto, la lista que contie a los archivos esta vacia
      (if (null? (getRemoteRep zonas))
          (modificaRegistro zonas "pull")
          ;si el remositorio remoto no esta vacio
          ;se fusionan los elementos del repositorio remotoen el workspace
          (pull (createZonas (fusionar (getWorkspace zonas) (cdr (getRemoteRep zonas))) (getIndex zonas) (getLocalRep zonas) (cdr (getRemoteRep zonas)) (getRegistro zonas)))
      )
      #f
  )        
)

;funcion que añade los cambios registrados al index
;entradas:archivos (lista)
;salida: un index (lista)
;recursion:natural (deja estados pendientes)
;dominio:lista
;recorrido:strings
(define (add archivos) (lambda (zonas) 
  ;se ve el caso particular cuando no hay cambios (lista nula)
  (if (null? archivos)
      zonas                ;NOTA: aqui deberia ir add--all pero aun no la hago, si logro terminarla irá aqui
      ;entonces como sabemos que esamos en el final de la lista?
      (if (null? (cdr archivos))
          ;eso significa que el car es el ultimo archivo por lo tanto verificamos que el archivo esté en el workspace
          (if (esta? (getWorkspace zonas) (car archivos))
              ;si esta, como es el ultimo devolvemos el index con los cambios
              (createZonas (getWorkspace zonas) (agregarCambio (getIndex zonas) (car archivos)) (getLocalRep zonas) (getRemoteRep zonas) (cons "add" (getRegistro zonas)))
              ;si no está, retornamos las zonas
              zonas
          )
          ;si no es el ultimo, la recursion tiene 2 factores
          (if (esta? (getWorkspace zonas) (car archivos))
              ;tenemos el caso recursivo cuando está que seria asi
              ((add (cdr archivos)) (createZonas (getWorkspace zonas) (agregarCambio (getIndex zonas) (car archivos)) (getLocalRep zonas) (getRemoteRep zonas) (getRegistro zonas)))
              ;y en caso de que no esté el archivo, no se modifican las zonas, solo pasamos por alto agregar ese archivo
              ((add (cdr archivos)) zonas)
          )
      )
  )
))

;funcion que genera un commit con los cambios, y un mensaje descriptivo para dejarlos en el repositorio remoto
;entradas: el mensaje (string), las zonas de trabajo
;salida, las zonas modificadas
;no requiere recursion
(define (commit mensaje) (lambda (zonas)
  (if (and (string? mensaje) (index? (cadr zonas)))
      ;si cumple las mismas condiciones para hacer un 
      (createZonas (getWorkspace zonas) (getIndex zonas) (agregarCommit (getLocalRep zonas) mensaje (getIndex zonas)) (getRemoteRep zonas) (cons "commit" (getRegistro zonas)))
      ;en caso de no cumplir, retorno las zonas sin cambios
      zonas
  )
))

;Función que envía los commit desde el repositorio local al repositorio remoto registrado en las zonas de trabajo
;entradas:zonas de trabajo
;salida; las zonas de trabajo modificadas
;recursion natural
;dominio:una lista
;recorrido:listas
(define (push zonas) 
  ;comprobamos que las zonas son verdaderas
  (if (zonas? zonas)
      ;vamos a nuestro caso base
      ;si el repositorio local esta vacio 
      (if (null? (getLocalRep zonas))
          ;si es nulo, ya se agregaron los cambios, por tanto devolvemos las zonas
          (modificaRegistro zonas "push")
          ;sino caso recursivo
          (push (createZonas (getWorkspace zonas) (getIndex zonas) (cdr (getLocalRep zonas)) (cons (car (getLocalRep zonas)) (getRemoteRep zonas)) (getRegistro zonas)))
      )
      #f
  )
)

;funcion que tranforma todas las zonas de trabajo a string
;entradas:las zonas de trabajo
;salida un string
;recursion de cola
(define (zonas->string zonas)
  (zonaToString (string-append (zonaToString (string-append (zonaToString (string-append (zonaToString "WORKSPACE\n" (car zonas)) "INDEX\n") (cadr zonas)) "LOCAL REPOSITORY\n") (caddr zonas)) "REMOTE REPOSITORY\n") (cadddr zonas))
)

;-----------------------------------------REQUERIMIENTOS EXTRAS-----------------------------------------
;funcion que retorna un string con la informacion del ambiente de trabajo
;entrada:zonas
;salida: un string
(define (status zonas)
  (if (zonas? zonas)
      (string-append "ARCHIVOS AGREGADOS AL INDEX:\n" (zonaToString "" (getWorkspace zonas)) "\nCANTIDAD DE COMMITS EN EL LOCAL REPOSITORY: " (length (getLocalRep zonas)) "\nRAMA ACTUAL: master")
       "el TDA zonas no cumple los requisitos"
  )
)

;funcion que muestro los ultimos 5 commits de la rama actual
;entradas las zonas de trabajo
;salida: un string
;no hay recursion de la funcion, pero si de las funciones dentro del cuerpo de esta funcion
(define (log zonas)
  (if (zonas? zonas)
      ;por coincidencia, y por fortuna, la lista resultante de esta funcion, es similar a un workspace
      ;usando eso a nuestro favor
      (zonaToString "" (get5commit (getRemoteRep zonas) 0))
      "las zonas de trabajo no son validas"
  )
)

;-----------------------------------OTRAS FUNCIONES---------------------------------------------------------------
  
;funcion que verifica si un elemento está en una lista
;entradas:una lista, y un string
;salida:valor booleano
;recursion: de cola
;dominio, una lista
;recorrido; strings
(define (esta? lista palabra)
  ;caso base: cuando llegamos al final de la lista, significa que el elemento no esta
  (if (null? lista) 
      #f
      ;en caso de aun no llegar al final, preguntamos si los elementos son iguales
      (if (eq? (car lista) palabra)
          ;si son iguales, encontramos el elemento y devolvemos una valor verdadero
          #t
          ;si no hacemos el caso recursiovo
          (esta? (cdr lista) palabra)
      )
  )
)

;Funcion que agrega un elemento al final de la lista
;tipo de recursion: de pila (deja estados pendientes)
;dominio: una lista
;recorrido:elementos de la lista
(define (agregarElemento lista elemento)
  (if (list? lista) 
      ;caso base
      (if (null? lista)
          (cons elemento null)
          ;caso recursivo
          (cons (car lista) (agregarElemento (cdr lista) elemento))
      )
      #f
  )    
)

;funcion que me convierte 1  sola zona en String
;entradas: un string y una zona (lista)
;salida un string
;recursion de cola
;dominio:una lista
;recorrido: strings
(define (zonaToString cadena zona)
  ;se trabaja de 2 formas, si la zona es workspace index
  ;o si es local o remoto, dado que lo dejaremos de diferentes formas
  (if (or (workspace? zona)(index? zona))
      ;nuestro caso base si es workspace o index es
      (if (null? zona)
          (string-append cadena "\n")
          ;en caso de que la lista no este vacia
          (zonaToString (string-append cadena (car zona) " ") (cdr zona))
      )
      (if (or (localRepository? zona) (remoteRepository? zona))
          ;si ya no quedan mas repositorios por leer
          (if (null? zona)
              (string-append cadena "\n")
              (zonaToString (string-append cadena "COMMIT: " (car (car zona)) (zonaToString "" (cdr (car zona)))) (cdr zona))
          )
          #f
      )
  )
)

;funcion que toma 2 listas, los elementos de la lista 2 se van a la lista 1 siempre y cuando no esten repetidos
;entradas: 2 listas
;salida: una lista con los elementos fusionados
;recursion: de cola
(define (fusionar lista1 lista2)
  ;caso base
  (if (null? lista2)
      ;retornamos lista1
      lista1
      ;caso recursivo
      (fusionar (agregarElemento lista1 (car lista2)) (cdr lista2))
  )
)

;funcion que toma los 5 mensajes del repositorio remoto
;entradas: un repositorio remoto
;salidas una lista con los mensajes del commit
;recursion: natural (deja estados pendientes)
(define (get5commit remoteRep contador)
  (if (or (= contador 5) (null? remoteRep))
      null
      (if (remoteRepository? remoteRep)
          (cons (car (car remoteRep)) (get5commit (cdr remoteRep) (+ contador 1)))
          #f
      )
  )
)

;-----------------------------------------ejemplos---------
(define ejemplo1 (((git "add")(list "file1.rkt" "file3.rkt" "file5.rkt"))(createZonas workspace1 '() '() '() '())))
(define ejemplo2 ((git "add")(list "archivo1.rkt" "archivo3.rkt" "archivo5.rkt")(createZonas workspace2 '() '() '() '())))
(define ejemplo3 ((git "add")(list "archivo1.rkt" "archivo3.rkt" "archivo5.rkt")(createZonas workspace3 '() '() '() '()))) ;-> ejemplo no valido

(define commit1 (((git "commit")("este commit es para el ejemplo1"))ejemplo1))
(define commit2 (((git "commit")("este commit es para el ejemplo2"))ejemplo2))
(define commit3 (((git "commit")("este commit no funca porque es el ejemplo 3"))ejemplo3)) ;-> no valido

(define push1 ((git "push")commit1))
(define push2 ((git "push")commit2))
(define push3 ((git "push")commit3)); ->no valido

(define pull1 ((git "pull")commit1))
(define pull2 ((git "pull")commit2))
(define pull3 ((git "pull")commit3));- no valido

(define status1 ((git "status")pull1))
(define status2 ((git "status")pull2))
(define status3 ((git "status")pull3));-> no valido

(define log1 ((git "log")pull1))
(define log2 ((git "log")pull2))
(define log3 ((git "log")pull3));-> no valido

(define string1 (zonas->string pull1))
(define string2 (zonas->string pull2))
(define string3 (zonas->string pull3));->no valido