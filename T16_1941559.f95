program T16

implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 21/02/20
!Descripcion de Programa:
!Crear un programa que realice el factorial como una subfuncion


!Apendice de Constantes
!character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables
integer:: n,Fact,F_Fact



write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*) "Funcion Factorial"

write(*,*) "La funcion del programa es"
write(*,*) "Realizar la funcion factorial como parte de una sub - funcion"

write(*,*) "Numero para obtener su factorial"
read(*,*) n


Fact = F_Fact(n)

write(*,*) "Factorial de",n
write(*,*) Fact



write(*,*) "Programa terminado con exito"                                                           
end program




integer FUNCTION F_Fact(f)
integer:: f,k

F_Fact = 1

do k=1,f

F_Fact = F_Fact*k

end do 


RETURN 
END FUNCTION 



