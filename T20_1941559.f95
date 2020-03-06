program T20

implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha ::26/02/20
!Descripcion de Programa: Encontrar el seno usando la definicion de serie utilizando una funcion para el factorial


!Apendice de Constantes
!character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables

integer:: Fact,F_Fact,n,k
real:: sen,x

write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)                       "Seno con Sumatoria"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"


write(*,*) "La funcion del programa es"
write(*,*) "Encuentrar el seno usando la definicion de serie con  una funcion para el factorial "
pause 

write(*,*) "Dame el valor de seno que deseas calcular"
read(*,*) x

write(*,*) "Dame el numero de iteraciones" 
read(*,*) n



Fact = F_Fact(n)  !Ya tenemos el valor del factorial 

sen = 0

 do k=1,n
  sen = ( (-1)**(k-1) )* ( x**( (2*k)-1 )  )/(fact) + sen
  end do 
  



print*, "Valor de Seno de",x, 
print*, Sen 






                  
   
write(*,*) "Programa terminado con exito"                                                           
end  program




integer FUNCTION F_Fact(f)
integer:: f,k,g

g = (2*f) -1 




F_Fact = 1 



do k=1,g

F_Fact = (F_Fact*k)

end do 


RETURN 
END FUNCTION 
