SUBROUTINE function1(x, y, dy, neq_g)
!----------------------------------------------------------------
!Andrew Tobiesen,  Mai 2009
!----------------------------------------------------------------	
IMPLICIT NONE
INTEGER, intent(in) :: neq_g 
REAL(8), INTENT(IN) :: x !uavhengig var
REAL(8), DIMENSION(neq_g), INTENT(OUT)  :: dy
REAL(8), DIMENSION(neq_g), INTENT(IN)  :: y

dy = sin(y)

end
