       program T04_1941559
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 28 /01/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!Funcion de Programa: Imprimir la serie de Fibonacci hasta cierto numero
!Apendice de Variables
!-__- Enteros: n (Numero de digitos de la serie, menores que 100 ), Sum (Suma de la serie) , k (Contador)
!-__-__- Reales:
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos: SF (Arreglo donde se guarda la serie)

       Implicit none

       integer::n,k,sum
      integer,dimension(500):: SF
      
      write (*,*) "Dame un numero para la serie que Fibonacci"
      Write(*,*) "Recuerda que si tu numero no coincide con la serie"
      write(*,*) "Se tomara el numero menor mas cercano a la serie"
       read(*,*) n
       Sum = 0
       SF(1) = 1
       SF(2) = 1

       do k=3,n

         SF(k) = SF(k-1) + SF(k-2)

         end do

         Write(*,*) "Ahora se va a imprimir la serie hasta el numero",n
         write(*,*) "O en su caso el menor mas proximo a este"
    
         
         do k=1,n
         
         if (SF(k).le.n.and.SF(k).gt.0)then
         
         write(*,*) SF(k)
         
         end if
         
         end do
         
    
         
           end program




