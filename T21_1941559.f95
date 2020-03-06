program T21 

implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 27/02/20
!Descripcion de Programa:
!Desarrolle un programa utilizando subrutinas calcule la pendiente 
! la interseccion del coeficiente de correlacion de un conjunto de puntos x y para una 
!regresion lineal 


!Apendice de Constantes
character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables

real,allocatable,dimension(:,:) :: M1
integer:: n,k,p
  real:: Sx,Sy,Sxy,Sx2,m,b,x,y

write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)".                Dendiente de una Recta con Regresion Lineal"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"


write(*,*) "La funcion del programa es"
write(*,*) "Utilizando un conjunto de puntos dados, obtener una recta , su interseccion "
print*,    "y pendiente (En en el espacio cartesiano)"
pause 

print*, "Dame la cantidad de puntos" 
read(*,*) n 

   allocate(M1(n,2)) !Creacion de la matriz de puntos 
print*, "Llenar los valores de los",n,"puntos"
pause 


do k=1,n

print*, "Punto",k
print*, "Coordenadas en x"
read(*,*) M1(k,1)
print*, "Coordenadas en y"
read(*,*) M1(k,2)

end do 
print*, Listo

print*, "Esta es la lista de puntos"

                  do k=1,n !fila
                  write(*,112)(M1(k,p),p=1,2)!columna
              112 format (2x,1F5.2,2x,1F5.2)    
                  end do
pause !Impresion de puntos lista 
!Como obtener la recta, interceccion y pendiente 

Sx = 0 
Sy = 0 
Sxy = 0 
Sx2 = 0 
do k=1,n
  Sx = M1(k,1) + Sx
  Sy = M1(k,2) + Sy
  Sxy = (M1(k,1)*M1(k,2)) + Sxy 
  Sx2 = (M1(k,1))**2 + Sx2
  
end do 
!Aplicado la definicion de  linea recta y = mx + b con los conocimientos de minimos cuadrados 

m = (  (n*Sxy)-(sx*sy)  )/ (  (n*sx2)-(sx**2)  )

b =  ( (sx*sy) - (sx*sxy) )/ (  (n*sx2)-(sx**2)  )

print*, "La ecuacion de la recta es"

print*, "y =",m,"x +",b

x = -b/m

y = b

print*, "Las intersecciones son"
print*, "Eje x",x 
print*, "Eje y",y








   
   




                  
    

    
    
    
    
    
    






write(*,*) "Programa terminado con exito"                                                           
end program