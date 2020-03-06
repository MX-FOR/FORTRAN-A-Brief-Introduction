program T15_1941559
implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 18/02/20
!================================================================================================================================
!Descripcion del programa
!Haga un programa en fortran que encuentre mediante una funcion la distancia del origen a un punto dado por sus coordenadas los puntos que considerara 
 !(1,1)(1,2) ... (1,10)
 !(2,1)(2,2).... (2,10)
  ! .               .
  ! .               . 
  ! .               .
  !(10,1)    ...   (10,10)

!Al final imprima en un archivo la coordenada con su distancia respectiva 
!=================================================================================================================================
!Apendice de constantes

!Apendice de Variables
real,dimension(10,10) :: Res = 0 
!Matriz de las distancias al punto 0,0
real:: Dis_f,z0,w0,z1,w1
integer:: k,p
!Dist :: Valor en el programa de la funcion 
!Dis:: Declaracion de funcion 

write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*) "                  Distancias del origen a puntos dados                            "

write(*,*) "La funcion del programa es"
write(*,*) "Encontrar mediante una funcion la distancia del origen a un punto dado por sus coordenadas"
pause 
!(z0,w0) (x0,y0)Coordenadas en el origen 
! (z1,w1) (x1,y1) Coordenadas variables 
z0 = 0 
w0 = 0 

z1 = 1 
w1 = 1 
                                           !Llenado de la matriz resultante con las distancias 
  do p =1,10 
  do k = 1,10
    Res(k,p) = Dis_f(z0,w0,z1,w1)
    z1 = z1 + 1 
    end do 
    z1 = 1
    w1 = w1 + 1
    end do
                                          !Impresion de archivo con distancias y su coordenadas 
                                                            
    OPEN (UNIT=10,FILE="Distancias.TXT",STATUS= "UNKNOWN")
                        
           WRITE(10,*) "Coordenadas y su distancia al origen"
           
           do k=1,10 !fila
           do  p = 1,10
             write(10,*) "Coordenada",k,p,"y distacia al origen ",Res(k,p)                
                  end do
                  end do 

      CLOSE(10) 

Write(*,*) "La lista de coordenadas y su distancia al origen ha sido escrita en"
write(*,*) "Llamado Distancias.txt"
pause 
                                         
                                             
    
    
 
write(*,*) "Programa terminado con exito"                                                           
end program
                 
REAL FUNCTION Dis_f(x0,y0,x1,y1)
real::x0,y0,x1,y1

dis_f =  SQRT(((Y1-Y0)**2 + (X1-X0)**2 )) 



RETURN 
END FUNCTION 
