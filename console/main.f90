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

!obj kan være peker eller en annen definert datatype; klasse, type etc
obj     = fug(obj)
a       = fug(pointermem)
c       = fug(c)

!om man skal definere funksjonen som en metode tilhørende klassen, i.e. om kalle skal se sånn ut
! call obj.fug(), så må den defineres som deferred, den vil hoppe direkte dit da, uten noen select.
! men det er implementert det allerede på øverste nivå

!ved init kan du selvsalgt tillegge pekere til predefinerte strukturer/klasser som også inneholder metoder, men da får vi det ekstra laget som vi ikke vil ha.


end

