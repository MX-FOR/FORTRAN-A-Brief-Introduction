program T12_1941559 
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 13/02/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa: Tabla de dos dimenciones que calcule el promedio de los datos leidos 
!Apendice de Variables
!-__- Enteros: k,p (Contadores)
!-__-__- Reales: Sumt, sum,sum2 (Suma total para el promedio, suma de filas y columnas)
! -__-__-__- Caracteres: fil y col (Nombres de las filas y columnas)
! -__-__-__-__- Arreglos: mat (ingreso de los datos a promediar), mat2 ( Promedio de los datos) 
  
  !Redaccion 
  Implicit none
  integer::k,p
  real:: SumT,Sum,Sum2
  real,dimension(7,7):: mat
  real,dimension(6,2)::mat2
  character(35),parameter:: Listo = "Datos leidos de manera correcta"
  character(10)::fil,col

    
 !Ingreso de valores para filas y columnas 
  print*, "Escribe el nombre de los valores de la columna"
  read(*,*) col
  print*, "Escribe el nombre de los valores de la fila"
  read(*,*) fil
  print*, "Llenar la columna de",col,"y la fila de",fil,"con sus respectivos datos"
  pause


      !Llenado de los valores constantes     
  	do k=2,7 !Llenar columnas
    Print*, "Dato",k-1,"de",col
      read(*,*) mat(1,k)
      end do 
      write(*,*) Listo
      pause
      do k=2,7 !Llenas filas
        print*,"Dato",k-1,"de",fil
       read(*,*) mat(k,1)
          end do 
          print*, Listo
          pause

          
          
          !Imprimir fila y columna
          print*, "Datos de",fil !Filas
          do k=2,7
            write(*,111) mat(1,k)
         111 format (2x,1F5.2)   
            end do           
          print*, "Datos de",col !Columnas
            do k=2,7             
              write(*,111) mat(k,1)
         ! 112 format (2x,1F5.2)     
              end do 
             pause
           

           !Ingreso de los valores variables
           print*, "Llenar los datos variables"
           print*, "Para simplificar el ingreso de valores" 
           print*, "se llenara por columnas de manera decendente"

           do k=2,7
             do p=2,7
               write(*,*) "Valor",p-1, "de",col,"con valor (",mat(1,k),")"
               read(*,*) mat(p,k)
               end do 
               write(*,*) "Cambio de columna"
               pause
               end do
               write(*,*) Listo
               pause
               
               !Impresion de valores variables
                  print*, "Estos son los valores de la tabla"             
                  do k=2,7 !columna
                  write(*,112)(mat(k,p),p=2,7)!fila 
              112 format (2x,6F5.2)    
                 !write(*,111)(mat(k,p),p=2,6)!fila imprime como el fila
                 end do
                 

                 !Calculo de promedios 
                 print*, "Ahora se van a imprimir los promedios de los valores por"
                 print*, "Filas y Columnas"
                 pause   
                 mat2 = 0 
                 sum = 0 
                 sum2 = 0  
                 sumt= 0
                  do k=1,6
                   do p =1,6 
                     Sum = Sum + mat(k+1,p+1)!Suma de columnas
                     sum2 = sum2 + mat(p+1,k+1) !suma de filas 
                     end do 
                     sumt = sumt + sum + sum2
                     mat2(k,1) = sum/6 !Promedio de columnas
                     mat2(k,2) = sum2/6 !Promedio de filas 
                     end do 
                     sumt = sumt/36 

                     !Impresion final 
                  do k=2,7
                    write(*,*) "Promedio",k-1, "de",col,"con valor (",mat(1,k),")"
                    write(*,*) mat2(k-1,1)
                    write(*,*) "Promedio",k-1, "de",fil,"con valor (",mat(k,1),")"
                    write(*,111) mat2(k-1,2)
                 end do 
                 write(*,*) "Promedio final"
                 write(*,*) sumt
                     

                                   
               
pause
end program 


