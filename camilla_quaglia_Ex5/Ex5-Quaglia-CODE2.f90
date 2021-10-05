! To run the program 'gfortran p.f90 -o  p.x -llapack -lblas'
! To run the executable ' ./p.x'
MODULE H_MATRIX2
 
  IMPLICIT NONE
  INTEGER*8 :: N  ! dimension of the matrix (square matrix)
 
CONTAINS
  SUBROUTINE normal_rand(x, y)
    ! this subroutine genetares two random numbers, x & y
    ! sampled from the normal distribution, through the BOX-MULLER algorithm
    REAL*8 :: u1, u2, r, theta
    REAL*8, INTENT(OUT) :: x, y
    
    CALL RANDOM_NUMBER(u1) ! u1 and u2 are random numbers drawn from ~ U([0,1])
    CALL RANDOM_NUMBER(u2)
    
    r = SQRT(2*(-LOG(u1)))
    ! π = 2*arcsin(1)
    theta = 2*(2*ASIN(1.)) * u2
				
    x = r*COS(theta)
    y = r*SIN(theta)
    
  END SUBROUTINE normal_rand

  SUBROUTINE DIMENSION(N)
    ! This subroutine asks the user the dimension of the matrix
    INTEGER*8 :: N
    
    N=0      ! all the dimensions are 'inizialized' to zero
   
    DO WHILE (N <= 0)
       PRINT*, "The dimension of the matrix is:"
       READ*, N
       IF (N <= 0) PRINT*, "Try with a positive value"
    ENDDO
  END SUBROUTINE DIMENSION
 
  SUBROUTINE SORT(array)
 ! This subroutine orders an array given in input in crescent order
    REAL*8, DIMENSION(:), INTENT(INOUT) :: array
    INTEGER*8 :: i1, i2
    REAL*8 :: temp_num		
    DO i2=1,SIZE(array)
       DO i1=1,(SIZE(array)-1)
          IF (array(i1).GE.array(i1+1)) THEN
             temp_num = array(i1)
             array(i1) = array(i1+1)
             array(i1+1) = temp_num
          END IF
       END DO
    END DO
    RETURN
  END SUBROUTINE SORT
END MODULE H_MATRIX2
	

PROGRAM Ex5_REAL
	
  USE H_MATRIX2

  IMPLICIT NONE

  INTEGER*2 :: ii,jj
  REAL*8, DIMENSION(:), ALLOCATABLE :: delta_eig, s_i , diag
  !  delta_eig = λ_(i+1) - λ_i &  s_i = Δλ_i / <Δλ_i>
 
  REAL*8 :: num1, num2


  CALL DIMENSION(N)
	
  ALLOCATE(diag(N))
	
	! Allocating eig differeces and spacings array
  ALLOCATE(delta_eig(N),s_i(N))


! Case dim_ is odd
  IF (MOD(N,2).EQ.1) THEN
     DO ii=1,((N-1)/2)
        CALL normal_rand(num1,num2)
        diag(2*ii-1) = num1
        diag(2*ii) = num2
     END DO
     CALL normal_rand(num1,num2)
     diag(N) = num1
  ELSE
	! Case dim_ is even
     DO ii=1,(N/2)
        CALL normal_rand(num1,num2)
        diag(2*ii-1) = num1
        diag(2*ii) = num2
     END DO
  END IF

  CALL SORT(diag)

	
	! Evaluating differences between consecutive eigenvalues
  delta_eig(:) = diag(2:) - diag(:N-1)
	
  s_i = delta_eig/(SUM(delta_eig)/(N-1))
	
	
	! Opening and REWRITING existing file
  OPEN(11, file="s_iREAL.dat", position="APPEND", action='WRITE')
	
  DO ii=1,(N-1)
     WRITE(11,"(F13.7)")s_i(ii)
  END DO
	
  CLOSE(11)
	
  STOP
	
	
	! Deallocating 
  DEALLOCATE(diag)
  DEALLOCATE(delta_eig, s_i)
  STOP
  
END PROGRAM Ex5_REAL

