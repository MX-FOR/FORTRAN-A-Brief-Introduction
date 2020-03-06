           program T06_1941559 

!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 23/01/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa:Programa en fortran utilizando la ley de Stefan Boltzman para radiacion de cuerpo negro leyendo la temperatura en grados centrigrados, le de la energa de radiacion a esa temperatura y pregunte al usuario si desea repetir para una temperatura diferente
!Apendice de Variables
!-__- Enteros: 
!-__-__- Reales:TC (Temperatura en grados centigrados), E_b(Energia de radiacion) 
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos:

       Implicit none


       Real:: T_C,E_b,K,Kel
       integer::op

       K = 5.670373E-08 

       write (*,*) "Dame la temperatura en grados centigrados"

       read (*,*) T_C

       Kel = T_C + 273.15 
      

       E_b = K*Kel**4


       write(*,*) "La energia radiada por el cuerpo negro es",E_b 
       write(*,*) "¿Deseas agregar otra temperatura?" 
       write(*,*) "Si asi lo deseas escribe 1, si no escribe 0" 

       read(*,*)Op

       if (Op.eq.1) then 
         write (*,*) "Dame la temperatura en grados centigrados"

           read (*,*) T_C
            Kel = T_C + 273.15 
            E_b = K*Kel**4
            write(*,*) "La energia radiada por el cuerpo negro es",E_b 
 

         end if 

       





       End program 

       
      
       


       

       

