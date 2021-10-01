#lang racket

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