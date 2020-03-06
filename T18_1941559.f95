program T18

implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 26/02/20
!Descripcion de Programa:Lea n numeros y los ordene de menor a mayor 


!Apendice de Constantes
 character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables
Integer::p,k

 real,allocatable,dimension(:) :: Mn
!n (Cantidad de numeros)



write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*) "                   Ordenacion  Creciente  de Numeros "
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"


write(*,*) "La funcion del programa es"
write(*,*) "Leer n numeros y los ordene de menor a mayor "
pause 

write(*,*) "Escribe la cantidad de numeros propuestos" 
read(*,*) p

   allocate(Mn(p)) !Creacion de vector con de columna igual a los n numeros
   write(*,*) "A continuacion se van a ingresar los",p,"numeros"
   pause
   
   Mn = 0
     do k = 1,p
     print*, "Numero",k, !Ingreso de los valores al vector
     read(*,*) Mn(k)
     end do
     print*, Listo
     pause

     print*, "Lista de numeros" 
     write(*,110) Mn
   110 format (1F6.3)
   pause 

   
call fun (p,Mn)    
          
           
write(*,*) "Programa terminado con exito"    
                                                       
end program

Subroutine fun(n,arr)
integer :: n,i,j
real arr(n),a

  do j=2,n
     a = arr(j)
        do i=j-1,1,-1

         if (arr(i) .le.a)go to 10 
           
          arr (i+1) = arr(i)
      end do 
i=0

10 arr(i+1) = a 
end do 
print*, "Lista ordenada de numeros"
write(*,111) arr 
111 format (1F6.3)

return 
END 