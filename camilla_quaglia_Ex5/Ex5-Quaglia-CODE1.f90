! To run the program 'gfortran Ex5-Quaglia-CODE1.f90 -o  Ex5-Quaglia-CODE1.x -llapack -lblas'
! To run the executable ' ./Ex5-Quaglia-CODE1.x'

MODULE H_MATRIX
 
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
    ! pi = 2*arcsin(1)
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
END MODULE H_MATRIX
	
PROGRAM Ex5
	
  USE H_MATRIX

  IMPLICIT NONE

  COMPLEX*16, DIMENSION(:,:),ALLOCATABLE :: A ! A is the Hermitian matrix
  INTEGER*8 ::  info  ! info is a variable required by the CHEEV subroutine
! (check LAPACK's documentation http://www.netlib.org/lapack/explore-html/d9/de3/group__complex_h_eeigen_ga003ee37091d65ee62fd72da1035f06e2.html#ga003ee37091d65ee62fd72da1035f06e2 )
   ! if info == 0 eigenvalues are stored in ascending order
  REAL*8, DIMENSION(:), ALLOCATABLE :: eig_vec ! vector in which are stored the eigeivalues of A 
  INTEGER*2 :: ii,jj
  INTEGER*8, DIMENSION(2) :: N_
  REAL*8 :: re, im ! real and imaginary part of the entries of the matrix A
  REAL*8, DIMENSION(:), ALLOCATABLE :: delta_eig, s_i 
        !  delta_eig = λ_(i+1) - λ_i &  s_i = Δλ_i / <Δλ_i>
  	
	! variables to be used in CHEEV (check the link below)
  COMPLEX*16, DIMENSION(:), ALLOCATABLE ::  WORK_
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RWORK_
	
  CALL DIMENSION(N)
	
 ! Defining N_
  N_(1) = N
  N_(2) = N

	! Allocating SQUARE matrix using file
  ALLOCATE(A(N,N))
	
	! Allocating eigenvalues array
  ALLOCATE(eig_vec(N))
	
	! Allocating eig differeces and spacings array
  ALLOCATE(delta_eig(N-1), s_i(N-1))
	

! Making the matrix hermitian
  DO ii=1,N
     DO jj=1,ii
        CALL normal_rand(re,im)
        
        IF (ii.EQ.jj) THEN
           A(ii,jj) = CMPLX(re, 0e0)
        ELSE
           A(ii,jj) = CMPLX(re, im)
           A(jj,ii) = CMPLX(re, -im)
        END IF
     END DO
  END DO

	! Allocating variables to make subroutine works with dummy variables
  ALLOCATE(WORK_(2*N-1), RWORK_(3*N-2))
	
	! CHEEV(JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, RWORK, INFO)
	! Arguments are:
	!  JOBZ	: JOBZ is CHARACTER*1. If = 'N': Compute eigenvalues only; if = 'V': Compute eigenvalues and eigenvectors.
	!  UPLO	: UPLO is CHARACTER*1. If = 'U':  Upper triangle of A is stored; if = 'L': Lower triangle of A is stored.
	!  N		: N is INTEGER. The order of the matrix A.  N >= 0.
  CALL ZHEEV('N', 'L', N, A, N, eig_vec, WORK_, 2*N-1, RWORK_, info)
 
  IF (info == 0) THEN
     print*,"Eigenvalues are stored in ascending order" ! check if info == 0
  END IF

	! Evaluating differences between consecutive eigenvalues
  delta_eig(:) = eig_vec(2:) - eig_vec(:N-1)
	
  s_i = delta_eig/(SUM(delta_eig)/(N-1))
	
	! Opening and REWRITING existing file
  OPEN(11, file="s_i.dat", position= 'append', action='WRITE') ! status='REPLACE'
	
  DO ii=1,(N-1)
     WRITE(11,"(F13.7)")s_i(ii)
  END DO
	
  CLOSE(11)
	
  STOP
	
	
  ! Deallocating 
  DEALLOCATE(A)
  DEALLOCATE(eig_vec, delta_eig, s_i)
 
  STOP
  
END PROGRAM Ex5
