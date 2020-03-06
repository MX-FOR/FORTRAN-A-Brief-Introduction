                program T08_1941559
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 01/02/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa: programa en Fortran que sume dos matrices del tamaño que el usuario le indique e imprima el resultado. 
!Apendice de Variables
!-__- Enteros: 
!-__-__- Reales:
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos:

              integer:: n,i,j
              real,allocatable,dimension(:,:) :: M1,M2,M3
              real::f
              

              print*, "Dame las dimensiones de las matrices"
              print*,"Filas"
              read(*,*) n
              print*,"Columnas"
              read(*,*)m

              allocate(M1(n,m))
              allocate(M2(n,m))
              allocate(M3(n,m)) !n , filas, m,columnas

                do i=1,n !filas
                do j=1,m !columnas

                  print*, "En la matriz 1 , dame el valor"
                  print*,i,".",j
                  read(*,*)f
                  M1(i,j)=f
                                    
                 end do                 

                end do

               do i=1,n !filas
                do j=1,m !columnas

                  print*, "En la matriz 2 , dame el valor"
                  print*,i,".",j
                  read(*,*)f
                  M2(i,j)=f
                                    
                 end do                 

                end do 

              


              

              do i=1,n !filas
                do j=1,m !columnas
                  
                  M3(i,j) = M1(i,j)+M2(i,j) !Suma de matrices
                 end do                 

                end do 

               print*, "Ahora se van a sumar las matrices 1  y 2"
               pause 

               write(*,*)"La matriz suma es" 
               do i=1,n
                 write(*,*) (M3(i,j),j=1,m)
                 end do 


          end program 
        
