program  T19

implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 26/02/20
!Descripcion de Programa: Lea una matriz nxn y calcule el valor de la matriz elevado a la k 




!Apendice de Constantes
character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables
integer:: Fm1,k,p,n!Cm1,Fm1(k,p) Contadores
real,allocatable,dimension(:,:) :: M1,Mr
       

write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*)                           "Matriz a la n potencia"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"


write(*,*) "La funcion del programa es"
write(*,*) "Lea una matriz de mxm y la eleve a la potencia n"


              
              

              print*, "Dame las dimensiones de las matriz mxm "
              read(*,*) Fm1
              print*, "Dame la potencia a elevear la matriz" 
              read(*,*) n
 
              
             allocate(M1(fm1,fm1))
             allocate(Mr(fm1,fm1))

             do k=1,fm1                             !Llenado de matriz 
               do p =1,fm1
                 print*, "Valor",k,p,"De la matriz"
                 read(*,*) M1(k,p)
                 end do 
                 end do
                 print*, Listo 

                 Mr = M1

                 do k = 1,n-1
                 
                     Mr = matmul(MR,M1)
                   
                 end do

                 
                    !Impresion de matriz
                   PRINT*, "Matriz Resultante "

                  do k=1,fm1 !fila
                  write(*,*)(Mr(p,k),P=1,fm1)!columna
                  end do     
    






write(*,*) "Programa terminado con exito"                                                           
end program