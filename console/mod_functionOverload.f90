module funcOverload
use mod_mem

private

public :: fug

interface fug
	module procedure pointerass
end interface

interface fug
	module procedure typeass
end interface

interface fug
	module procedure objectass
end interface

contains

function objectass(obj1) result(obj2)
implicit none
type(mem), intent(in) :: obj1
type(mem) :: obj2

obj2 = obj1		
							
end function objectass

function pointerass(pointer1) result(pointer2)
implicit none
real(8), pointer, intent(in) :: pointer1
real(8), pointer :: pointer2

allocate(pointer2)
pointer2 => pointer1;		
							
end function pointerass


function typeass(real1) result(real2)
implicit none
character*(*), intent(in) :: real1
character(len=10) :: real2

real2 = trim(real1);		
							
end function typeass


end module funcOverload