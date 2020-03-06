        program T02_1941559
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Creado por Diego  Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- 1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Fecha 23/01/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa: Programa que lee un numero real y encuentra su raiz cuadrada, el valor de e elevado a ese numero y su tangente
!Apendice de Variables
!-__- Enteros: n (Valor a leer), sq (Raiz del numero),ex(el exponencial a ese numero),tan (tangente del numero)
!-__-__- Reales:
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos:

       Implicit none

       Real:: n,sq,ex,tn

       write(*,*) "Escribe el numero"
       read (*,*) n

       sq = (n)**.5
       ex = exp(n)
       tn = tan(n)

       Write (*,*) "Presiona enter para obtener los valores"


       write (*,*) "La raiz del numero es" , sq
       write (*,*) "El valor de e elevado a ese numero es" , ex
       write (*,*) "La tangente de dicho numero es" , tn


       end program


