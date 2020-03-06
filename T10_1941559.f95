         program T10_1941559 
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 05/02/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa: Programa que lee dos matrices y si es posible multiplica el resultado 
!Apendice de Variables
!-__- Enteros: i,j,m,n (dimensiones de matrices)
!-__-__- Reales:
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos: M1,M2 (Matrices del usurario)

       Implicit none

                     integer:: Cm1,Fm1,Cm2,Fm2,k,p!Cm1,Fm1,Cm2,Fm2,Cmr,Fmr, (k,p) Contadores
                     real,allocatable,dimension(:,:) :: M1,M2,Mr
       
              !Trazado de dimensiones 

              write(*,*) "Este programa multiplica 2 matrices dadas" 
              

              print*, "Dame las dimensiones de las matriz 1"
              print*,"Filas"
              read(*,*) Fm1
              print*,"Columnas"
              read(*,*)Cm1
              !M1 (Fm1xCm1) 

                  
              print*, "Dame las dimensiones de las matriz 2"
              print*,"Filas"
              read(*,*) Fm2
              print*,"Columnas"
              read(*,*)Cm2

              if (Cm1.eq.Fm2) then 

              write(*,*) "Ahora vas a ingresar los valores de la matriz 1 y 2"


                

              !M2 (Fm2xCm2)
              !Mr (Cm1xFm2)

             

              allocate(M1(fm1,cm1))
              allocate(M2(fm2,cm2))
              allocate(Mr(cm1,fm2)) !Definicion de producto de matrices

              !Fin de trazado de dimensiones 

              !Ingresar valores a la matrices
              

                      

              do k=1,cm1
                do p=1,fm1

                  write(*,*) "Dame el valor (",k,".",p,")De la matriz 1"
                  read(*,*) M1(k,p) 
                  
                  end do                    
                end do
                
                write(*,*) "Valores de la matriz 2"
                
              do k=1,cm2
                do p=1,fm2

                  write(*,*) "Dame el valor (",k,".",p,")De la matriz 2"
                  read(*,*) M2(k,p) 
                  
                  end do                 

                end do

                !Fin de ingreso de valores 
                Write(*,*) "Ya has ingresado todos los valores de las matrices"

                  write(*,*) "Estas son las dos matrices"
               
                  write(*,*) "-_-_-_-_ Matriz 1 -_-_-_-_-"

               do k=1,cm1!Fila !Columna 
                 write(*,*)(M1(k,p),p=1,fm1)!fila 
                 end do 
                 !Imprimir matrices 

                 write(*,*) "-_-_-_-_- Matriz 2 -_-_-_-"

                  do k=1,cm2 !columna
                 write(*,*)(M2(k,p),p=1,fm2)!fila
                 end do

                 !Para que las matrices se puedan multilpicar cm1=fm2 para tener una matriz resultante de (fm1x fm1) 

                 !Tenemos 2 matrices de M1 (fm1xcm1) y M2 (fm2xcm2) y una matriz vacia resultante de Mr (fm1xcm2) 
                 !La definicion de multiplicacion de matrices es 
                 !C(fm1xcm2) = Sum de k =1 hasta fm1 M1(fm1,k)*M2(k,cm2)
                                  
                 write(*,*) "A continuacion se hara la multiplicacion de matrices"
                  !Algoritmo para multiplicar matrices

                 !do k=1,fm1
                  ! do p=1,cm2
                   !  do o=1,fm1
                    !   Mr(k,p) = Mr(k,p) + M1(k,o)*M2(o,p) !NO FUNCIONA!!!
                     !end do 
                   !end do
                !end do 

                 !Hay una funcion que multiplica matrices? 
                                      !Si la hay!! 

                 Mr = matmul(M1,M2)                
                 write(*,*) "-_-_-_-_-_ Matriz producto -_-_-_-_-_ " !Escrito correcto
                  do k=1,fm2 !Fila 
                 write(*,*)(Mr(k,p),p=1,cm1)!Columna 
                 end do

                 else 
                   write(*,*) "No se pueden multiplicar las matrices"
                   end if 

                 

            


      

         end program 