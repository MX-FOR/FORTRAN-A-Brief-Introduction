program T14_1941559
implicit none 
!Funcion de Programa:
!Distribucion de temperaturas sobre una placa metalica 
!Bajo condiciones de estado estacionario la temperatura de un punto cualquiera sobre la duperficie metalica 
!se puede encontrar como el promedio de todos sus putos a su alrededor, este hecho puede ser usado en un proceso
!iterativo para calcular la distrivucion de temperaturas de todos los puntos sobre el plato 
!Si se tiene una placa metalica cuadrada 11x11 y se considera que un segmento cualquiera de temepratura  T(i,j) 
!La nueva temperatura T(i,j) seria igual al promedio de los segmentos que rodean a T(i,j)

!(1) T(i,j) = 0.25 *(Ti+1,j) + Ti-1,j + T i,j+1 + Ti,j-1 

!Escriba un programa en fortran que calcule la distribucion de temperaturas en estado estacionario en una placa metalica cuadrada de 11x11 
!asumiendo que la temperatura en todos los segmentos de la orilla se encuentra fija a 22ºC , el segmento 5,5 se encuentra fijo a 95 ºC  y el resto de los segmentos tienen 
!una temperatura inicial de 55ºC 
!El programa aplicacando la ecuacion (1) de manera iterativa hasta que el cambio maximo de temperatura para cada uno de los nodos sea menor que 0.01 ºC v
!Comparar la nueva temperatura con la anterior, con todos los cuadritos menos en la orilla 

!T0 - Tf = C 
!si C < 0.01 entonces terminar programa, hasta que todos los cuadros tengan una diferencia de temperatura menor de 0.01

!Variables y  Constantes
real,dimension(11,11) :: Placa = 55,Nplaca =  0,PPlaca = 0 
!Placa : Valores iniciales de la placa 
!PPlaca :Temperatura Promedio segun la ecuacion 1 
!NPlaca : Diferencia entre Placa y NPlaca 
integer :: k,p , it = 1!Contadores e numero de iteraciones
real::sum = 0 ,prom = 1 !Sumatoria y Promedio
character(35),parameter:: Listo = "Datos leidos de manera correcta"


write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)"-_-_-_-_-_-_-_-_- Distribucion de Temperaturas en estado -_-_-_--_-_-_-_-_-_-_"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Estacionario _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*) "La funcion del programa es"
write(*,*)"Calcular la distribucion de temperaturas en estado estacionario en una placa metalica cuadrada de 11x11"
 
                                      !Distribucion de temperaturas originales 
!Placa = 55
Placa(5,5) = 95
do k=1,11
         Placa(k,1) = 22
         Placa(k,11) = 22
         Placa (1,k) = 22
         Placa(11,k) = 22
end do 
                                     !Impresion de temeraturas iniciales
PRINT*, "Distribucion Inicial de Temperaturas en la tabla "
write(*,*) Listo 

OPEN (UNIT=10,FILE="PLACA.TXT",STATUS= "UNKNOWN")  !Escribir en archivo placa inicial 
write(10,*) "Temeratura inicial de la placa"
 do k=1,11 !fila
                  write(10,117)(PLACA(k,p),P=1,11)!columna
              117 format (3x,11F7.2,5x)    
                  end do


do k=1,11 !fila
   write(*,112)(Placa(k,p),P=1,11)!columna
  112 format (1x,11F7.2,2x)
  end do
pause


                                        !Hacer hasta parametro 0.01
do while (prom.ge.0.01)
 
!Primer promedio resultante (it = 1)     
  do k=2,10
  do p=2,10
    PPlaca(k,p) = 0.25*( Placa(k+1,p) + Placa(k-1,p) + Placa(k,p+1)  + Placa(k,p-1)  )
    NPlaca(k,p) = Placa(k,p) - PPlaca(k,p)  !Diferencia entre la placa vieja y la nueva
    end do
    end do 
    
 !Tenemos 3 Placas de 11x11 
 !Placa (Valores originales) 
 !PPlaca (Valores promedios)
 !NPlaca (Diferencia de Valores) 
                                                 !Obtencion del promedio para NPlaca     
Prom = 0 
do k=2,10
do p=2,10
Prom = Prom + NPlaca(k,p) 
end do 
end do 
Prom = Prom/81
it = it +1 

                                                    !Igualar la placa original con la nuevo promedio 
 do  k=2,10
  do p=2,10
    Placa(k,p) = PPlaca(k,p) 
      end do
      end do 
        
end do 
  
 write(*,*) "La diferencia de temperatura promedio de la placa es menor que 0.01"
 Write(*,*) "El numero de iteraciones que tardo es", It 
 !Impresion de temeraturas iniciales
 
                                           !Impresion final de la placa con su temperatura en la  n iteracion 
 PRINT*, "Distribucion Final de Temperaturas en la tabla "

                 do k=1,11 !fila
                  write(*,114)(Placa(k,p),P=1,11)!columna
           114 format (1x,11F7.2,2	x)    
                end do
                
 write(10,*) "Temperatura final de la placa"              
do k=1,11 !fila
        write(10,118)(PLACA(k,p),P=1,11)!columna
    118 format (3x,11F7.2,5x)    
  end do
  
 write(10,*) "La diferencia de temperatura promedio de la placa es menor que 0.01"
 Write(10,*) "El numero de iteraciones que tardo es", It 
                            
      CLOSE(10)

 
write(*,*) "Programa terminado con exito" 
end program 