
SUBROUTINE finiteDiff(x, z, df, neq_g, method)
!----------------------------------------------------------------
!Andrew Tobiesen,  Mai 2009
!----------------------------------------------------------------	
IMPLICIT NONE
!----------------------------------------------------------------
REAL(8)		:: Eps_c	
INTEGER		:: i = 0, j = 0
INTEGER, intent(in) :: neq_g 
REAL(8), DIMENSION(neq_g), INTENT(IN) :: z
REAL(8), INTENT(IN) :: x
REAL(8), DIMENSION(neq_g, neq_g), INTENT(OUT) :: df ! partial derivatives
REAL(8), DIMENSION(neq_g) :: ui_j  
REAL(8), DIMENSION(neq_g, neq_g) :: zb, z111, z222, ui_j1, ui_j2, zc
REAL(8) :: dz = 1.0d-8
Character(*), intent(in) :: method
!----------------------------------------------------------------

select case (trim(method))

case('twoPointForward')

	! Make j-vectors equivalent to outgoing vector 
	DO j = 1, neq_g
			zb(j,1:neq_g) = z(1:neq_g)
	END DO	
	! give diagonal of vector zb
	DO i = 1, neq_g
		zb(i,i) = z(i) + dz
	END DO
		
	! calls function for start values.
	CALL function1 (x, z, ui_j, neq_g) ! ui_j = startvalues

	! stores values in z-vectors, one for each function equation
	z111 = TRANSPOSE(zb)

	! calls function for initial values for model functions 
	! forward
	DO i = 1, neq_g
		CALL function1(x,z111(:,i),ui_j1(:,i), neq_g) 
	END DO

	!forward approximation
	!and generates partial derivatives
	DO i = 1, neq_g
			df(i,1:neq_g) = (ui_j1(i,1:neq_g) - ui_j(i)) / dz
	END DO

case('threePointCentral')
	
	! Make j-vectors equivalent to outgoing vector 
	DO j = 1, neq_g
			zc(j,1:neq_g) = z(1:neq_g)
	END DO
		
	DO i = 1, neq_g
		zc(i,i) = z(i) - dz
	END DO
	z222 = TRANSPOSE(zc)
	! 1point backward
	DO i = 1, neq_g
		CALL function1(x, z222(:,i), ui_j2(:,i), neq_g) 
	END DO
	!test 3 point central approximation
	DO i = 1, neq_g
			df(i,1:neq_g) = ( ui_j1(i,1:neq_g) - ui_j2(i,1:neq_g) ) / (2d0*dz)
	END DO

end select

END SUBROUTINE 