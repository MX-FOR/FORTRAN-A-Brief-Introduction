program T13_1941559
implicit none 
!Funcion de Programa: le  dos matrices 5x5 las sume e imprima en un arhivo las dos matrices y la matriz resultante 


!Apendice de Variables
!-__- Enteros: 
!-__-__- Reales: 
! -__-__-__- Caracteres: 
! -__-__-__-__- Arreglos: 
 

!Variables y  Constantes
character(35),parameter:: Listo = "Datos leidos de manera correcta"
real,dimension(5,5):: M1,M2,MR
INTEGER:: K,P


write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-"
write(*,*)"-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_ 13/02/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_" 
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_" 

write(*,*) "                      La funcion del programa es"
write(*,*) "Le  dos matrices 5x5 las sume e imprima en un arhivo las dos matrices "
write(*,*) "la matriz resultante" !Escribir funcion del programa
PAUSE

PRINT*,"Se van a ingresar los valores de las matrices"
!ingresar valores de la matriz 
write(*,*)"MATRIZ 1"
DO K=1,5
  DO P=1,5
    WRITE(*,*) "FILA",P,"COLUMNA",K
    READ(*,*) M1(P,K)   
    END DO 
    END DO
    WRITE(*,*) LISTO
    PAUSE
WRITE(*,*) "MATRIZ 2"
DO K=1,5
  DO P=1,5
    WRITE(*,*) "FILA",P,"COLUMNA",K
    READ(*,*) M2(P,K)  
    END DO 
    END DO 
    PRINT*, LISTO
    PAUSE
    !HACER SUMA DE MATRIZ RESULTANTE 
    
     DO K=1,5
         DO P=1,5
           MR(K,P) = M1(K,P)+M2(K,P)
           END DO 
           END DO 

    !IMPRESION DE MATRICES 
    PRINT*, "ESTA SON LAS MATRICES"
    PRINT*, "MATRIZ 1"

                  do k=1,5 !fila
                  write(*,112)(M1(k,p),P=1,5)!columna
              112 format (2x,5F5.2,2x)    
                  end do

     PRINT*,"MATRIZ 2"

        do k=1,5 !fila
                  write(*,113)(M2(k,p),P=1,5)!columna
              113 format (2x,5F5.2,2x)    
                  end do

      
           WRITE(*,*) "MATRIZ RESULTANTE"

           do k=1,5 !fila
                  write(*,114)(MR(k,p),P=1,5)!columna
              114 format (2x,5F5.2,2x)    
                  end do


!======================================================================================================!   
                           
             OPEN (UNIT=10,FILE="EJEMPLO.TXT",STATUS= "UNKNOWN")
            !open(unit=10,file="T13.txt",status=unknown)
             WRITE(10,*) "ESTA SON LAS MATRICES"
             WRITE(10,*) "MATRIZ 1"

                  do k=1,5 !fila
                  write(10,115)(M1(k,p),P=1,5)!columna
              115 format (2x,5F5.2,2x)    
                  end do

         WRITE(10,*)"MATRIZ 2"

        do k=1,5 !fila
                  write(10,116)(M2(k,p),P=1,5)!columna
              116 format (2x,5F5.2,2x)    
                  end do

      
           WRITE(10,*) "MATRIZ RESULTANTE"

           do k=1,5 !fila
                  write(10,117)(MR(k,p),P=1,5)!columna
              117 format (2x,5F5.2,2x)    
                  end do

      CLOSE(10)             

   
                  

         


write(*,*) "Programa terminado con exito"                                                           
end program 