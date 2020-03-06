 program T17_1941559 
implicit none
!Programador:: Diego Alejandro Tellez Martinez (1941559) 
!Fecha :: 21/02/20
!Descripcion de Programa:
!Haga un  programa en Fortran que lea los coeficientes de una ecuacion cuadratica y 
! utilize una subrutina para encontrar las raices y que el programa imprima las raices resultantes 

!Apendice de Constantes
character(35),parameter:: Listo = "Datos leidos de manera correcta" !Constante para indicar la captura de datos 
!Apendice de Variables
Real:: a0,b0,c0,dis2,x1,x2,im

write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"
write(*,*) "                       Subrutina para Ecuacion cuadratica"
write(*,*)"-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_"

write(*,*) "La funcion del programa es"
write(*,*) "Obtener la naturaleza y el resultado de una ecuacion cuadratica"
pause 
! ============================================================== Llenado de coeficientes ===========================================
write(*,*) "Dame el valor de los coeficientes a,b,c"
            write (*,*) "teniendo en cuenta que a es para la ecuacion"
            write (*,*)"cuadratica, b para la lineal y c para la constante"
            write(*,*) "Valor de a"
            read(*,*) a0
            write(*,*) "Valor de b"
            read(*,*) b0
            Write(*,*) "Valor de c"
            read(*,*) c0
            write(*,*) Listo 

! ================================================= Llamado a sub_funcion ==========================================================
            
            Call Raices (a0,b0,c0,dis2,x1,x2,im)
            
             if (dis2.ge.0)then
                write(*,*)"Las soluciones son reales" 
                 write(*,*) "Las raices de la ecuacion son"
                   write(*,*) x1
                    write(*,*) x2

                else 
                  write (*,*) "Las soluciones son imaginarias"
                    Write(*,*) "Las raices de la ecuacion son" 
                     Write (*,*) x1,"+", "i",im 
                      write(*,*) x1 , "-","i",im 
              end if

! =======================================================================================================================================            




              

write(*,*) "Programa terminado con exito"                                                           
end program 

subroutine Raices (a,b,c,dis,xi,xii,ima)
 real::a,b,c,dis,xi,xii,ima
  
            dis = b**2 -(4*a*c) 
            
                   if (dis.ge.0)then
                     
                   xi = (-b+sqrt(dis))/2*a
                   xii = (-b-sqrt(dis))/2*a                
                else                                  
                   xi = (-b)/2*a 
                   ima =(sqrt(abs(dis))/2*a)
                  
              end if
              
RETURN 

END SUBROUTINE 

                  