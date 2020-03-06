            program T05_1941559
!_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_--_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 28/01/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!Funcion de Programa: programa  que lea los coeficientes de una ecuacion cuadratica, sin calcular las raices determine la naturaleza de estas
!las raices, imprima la naturaleza y las raices. 
!Apendice de Variables
!-__- Enteros: 
!-__-__- Reales:a,b,c (Coeficientes de la ecuacion cuadratica),dis(discriminante b**2 - 4*a*c),x1,x2 (Las raices de la ecuacion), im (valor imaginario"  
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos: 


            implicit none 

            real::a,b,c,dis,x1,x2,im
  


            write(*,*) "Dame el valor de los coeficientes a,b,c"
            write (*,*) "En su respectivo orden "
            read(*,*) a,b,c 

            dis = b**2 -(4*a*c) 

              if (dis.ge.0)then

                write(*,*)"Las soluciones son reales" 

                else 

                  write (*,*) "Las soluciones son imaginarias"

              end if
              
                write(*,*) "Raices"




                   if (dis.ge.0)then
                     
                   x1 = (-b+sqrt(dis))/2*a
                   x2 = (-b-sqrt(dis))/2*a
                   write(*,*) "Las raices de la ecuacion son"
                   write(*,*) x1,x2


                else

                                      
                   x1 = (-b)/2*a 

                   im =(sqrt(abs(dis))/2*a)

                   Write(*,*) "Las raices de la ecuacion son" 
                   Write (*,*) x1,"+", "i",im 
                   write(*,*) x1 , "-","i",im 
                     
                   
 
                   

              end if


                  
                   
                   
 

                    
            end program