              Program T09_1941559
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Creado por -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- Diego Alejandro Tellez Martinez -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_1941559 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_- -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_ Fecha 05/02/20 -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
!-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
!Funcion de Programa: prograam que lea 2 vectores de 3 componentes, calcule su producto punto e  imprima el resultado 
!Apendice de Variables
!-__- Enteros: i (contador)
!-__-__- Reales: Pro (Resultado del prducto punto)
! -__-__-__- Caracteres:
! -__-__-__-__- Arreglos: Vn,Vm (Vectores dados)


       Implicit none


       Real::Pro
       integer::i

       real,dimension(3)::Vn,Vm
       print*, "Dame los 3 valores del vector 1" 
       read(*,*) Vn
       print*, "Dame los 3 valores del vector 2" 
       read(*,*) Vm
       !Produto punto VnVm = Vn1*Vm1 + Vn2*Vm2 + Vn3*Vm3 
       Pro = 0 

       do i=1,3

        Pro  = Pro +  Vn(i)*Vm(i)
         
         end do 

           print*, "Ahora se va a imprimir el producto punto"

           pause 

           write (*,*) Pro

           
       

       

              

              end program 
              