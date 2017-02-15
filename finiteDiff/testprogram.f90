program testProgram
!----------------------------------------------------------------
!Andrew Tobiesen,  Mai 2009
!----------------------------------------------------------------

IMPLICIT NONE
!----------------------------------------------------------------
INTEGER		:: dummy_neq ! = (defined globally)
REAL(8)		:: Eps_c	
INTEGER		:: i = 0, j = 0
INTEGER, parameter :: neq_g = 9
REAL(8), DIMENSION(neq_g) :: z
REAL(8)  :: x
REAL(8), DIMENSION(neq_g, neq_g)  :: df ! partial derivatives
REAL(8), DIMENSION(neq_g) :: fs  
REAL(8), DIMENSION(neq_g, neq_g) :: zb, z111, f111
REAL(4) :: s

!----------------------------------------------------------------
!df = df/dZ
!z = dZ

!----------------------------------------------------------------
s = SECNDS(0.0)
do i = 1, 100000
	z = 1d0
	call finiteDiff(x, z, df, neq_g, 'twoPointForward')
end do
s = SECNDS(s)
write(*,*) 'relative tolerance', (df(1,1) - cos(1d0)) /  df(1,1) 
write(*,*) 'elapsed time', s, 'secs'
!----------------------------------------------------------------


!----------------------------------------------------------------
s = SECNDS(0.0)
do i = 1, 100000
	z = 1d0
	call finiteDiff(x, z, df, neq_g, 'threePointCentral')
end do
s = SECNDS(s)
write(*,*) 'relative tolerance', (df(1,1) - cos(1d0)) /  df(1,1) 
write(*,*) 'elapsed time', s, 'secs'
!----------------------------------------------------------------




end 
