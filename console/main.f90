program main
use funcOverload,  only: fug
use mod_mem
implicit none
real(8), pointer :: obj

logical :: a

character(10) :: c
class(mem),pointer :: pointermem

allocate(obj)
allocate(pointermem)

obj = 10d0;
c  = 'assman';

!obj kan v�re peker eller en annen definert datatype; klasse, type etc
obj     = fug(obj)
a       = fug(pointermem)
c       = fug(c)

!om man skal definere funksjonen som en metode tilh�rende klassen, i.e. om kalle skal se s�nn ut
! call obj.fug(), s� m� den defineres som deferred, den vil hoppe direkte dit da, uten noen select.
! men det er implementert det allerede p� �verste niv�

!ved init kan du selvsalgt tillegge pekere til predefinerte strukturer/klasser som ogs� inneholder metoder, men da f�r vi det ekstra laget som vi ikke vil ha.


end

