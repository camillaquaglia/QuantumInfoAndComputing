!     to compile 'gfortran Ex6-Quaglia-CODEa.f90 -o Ex6-Quaglia-CODEa.x -llapack -lblas'
!     to run the executable  './Ex6-Quaglia-CODEa.x'

MODULE DEBUGMOD
  IMPLICIT NONE

  ! This module is to debug a code
  ! Each subroutine, defined in the interface "DEBUG" has 4 inputs:
  ! debug : logical flag. If TRUE the debug is activated 
  ! var : variable (that can be of any type) on which the debug is performed
  ! test : logical flag. If TRUE the check is activated 
  ! message : the warning strings that the user wants to print

  LOGICAL :: turn_debug_on

  INTERFACE DEBUG
     MODULE PROCEDURE INT4_DEBUG
     MODULE PROCEDURE INT2_DEBUG
     MODULE PROCEDURE REAL4_DEBUG
     MODULE PROCEDURE REAL8_DEBUG
     MODULE PROCEDURE CMPLX8_DEBUG
     MODULE PROCEDURE CMPLX16_DEBUG
     MODULE PROCEDURE CMPLX8MAT_DEBUG
     MODULE PROCEDURE CMPLX16MAT_DEBUG
     MODULE PROCEDURE REAL8MAT_DEBUG
     MODULE PROCEDURE REAL4MAT_DEBUG
     MODULE PROCEDURE INT2VEC_DEBUG
     MODULE PROCEDURE INT4VEC_DEBUG
  END INTERFACE DEBUG

  
CONTAINS
  
  SUBROUTINE INT4_DEBUG(debug,var,test,message)
    INTEGER*4 :: var ! 4 bytes integer variable 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE INT4_DEBUG
  
  SUBROUTINE INT2_DEBUG(debug,var,test,message)
    INTEGER*2 :: var ! 2 bytes integer variable 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE INT2_DEBUG

  SUBROUTINE CMPLX8_DEBUG(debug,var,test,message)
    COMPLEX*8 :: var ! single precision complex variable 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE CMPLX8_DEBUG

  SUBROUTINE CMPLX16_DEBUG(debug,var, test,message)
    COMPLEX*16 :: var ! double precision complex variable
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE CMPLX16_DEBUG

  SUBROUTINE CMPLX8MAT_DEBUG(debug,var,test,message)
    COMPLEX*8, DIMENSION(:,:) :: var ! single precision complex matrix
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE CMPLX8MAT_DEBUG

  SUBROUTINE CMPLX16MAT_DEBUG(debug,var,message)
    COMPLEX*16, DIMENSION(:,:) :: var ! double precision complex matrix
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE CMPLX16MAT_DEBUG

  SUBROUTINE REAL8_DEBUG(debug,var,test,message)
    REAL*8 :: var        ! double precision real variable
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE REAL8_DEBUG

  SUBROUTINE REAL4_DEBUG(debug,var, test,message)
    REAL*4 :: var          ! single precision real variable 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE REAL4_DEBUG

  SUBROUTINE REAL8MAT_DEBUG(debug,var,test,message)
    REAL*8, DIMENSION(:,:) :: var  ! a double precision real matrix 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE REAL8MAT_DEBUG

  SUBROUTINE REAL4MAT_DEBUG(debug,var, test,message)
    REAL*4, DIMENSION(:,:) :: var  ! a single precision real matrix 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE REAL4MAT_DEBUG

  SUBROUTINE INT4VEC_DEBUG(debug,var, test,message)
    INTEGER*4, DIMENSION(:) :: var   ! 4 bytes integer vector 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE INT4VEC_DEBUG

  SUBROUTINE INT2VEC_DEBUG(debug,var, test,message)
    INTEGER*2, DIMENSION(:) :: var  ! 2 bytes integer vector 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE INT2VEC_DEBUG

END MODULE DEBUGMOD


MODULE HERMITE
  ! this module is an Hermite Polynomials generator
  ! For its construction one can check https://sukhbinder.wordpress.com/hermite-polynomials-fortran-module/
  
CONTAINS

  RECURSIVE FUNCTION HermitePoly(n) RESULT(hp_coeff)
    
    INTEGER*8 :: n
    REAL*8, DIMENSION(n+1) ::  hp_coeff, coef1, coef2

    IF(n .EQ. 0) THEN
       hp_coeff(1)=1.0 ! Hermite polynomial of 0 order
       RETURN
    END IF

    IF(n .EQ. 1) THEN
       hp_coeff(1)=2.0
       hp_coeff(2)=0.0
    ELSE

       coef1(1:n+1)=0.0
       coef1(1:n)=2.0*HermitePoly(n-1)

       coef2(1:n+1)=0.0
       coef2(3:)=2.0*(n-1)*HermitePoly(n-2)

       hp_coeff = coef1 - coef2

    END IF
	
    RETURN

  END FUNCTION HermitePoly
	
  FUNCTION evalHerm_Poly(xi,n) RESULT(y)
	
    INTEGER*8 :: n, pow_i, ii, jj
    REAL*8 :: xi(:), y(size(xi)),h_coeff(n+1)

    k=size(xi)

    h_coeff=HermitePoly(n)

    y(1:k)=h_coeff(n+1)
	
    pow_i=1
	
    DO ii=n,1,-1
       DO jj=1,k
          y(jj) = y(jj)+h_coeff(ii)*xi(jj)**pow_i
       END DO
       pow_i = pow_i + 1
    END DO
	
  END FUNCTION evalHerm_Poly

	
END MODULE HERMITE


MODULE SOLVE_AO
  ! This module contains the implementation of some functions and
  ! subroutine to solve the one dimensional time-independent Schrodinger
  ! equation through the finite difference method

  USE HERMITE

  IMPLICIT NONE
  
  DOUBLE PRECISION, PARAMETER :: h_bar = 1  ! fixed to 1 for simplicity
  DOUBLE PRECISION, PARAMETER :: PI = ACOS(-1.d0)

CONTAINS
  
  SUBROUTINE SEC_DERIVATIVE(N,m)
    ! WARNING: of course this is the LAPLACIAN
    ! in greater dimensions

    ! this subroutine computes the second derivative
    ! (present in the momentum operator) on a
    ! discretized 1dimensional grid of size N
    ! input: N, the size of the grid
    ! output: the matrix m
    DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: m  ! the matrix that we want
    INTEGER :: N, ii
    
    ALLOCATE(m(N,N))  ! the matrix is tridiagonal
    DO ii = 1,N
       m(ii,ii) = -2
       m(ii,ii+1) = 1
       m(ii,ii-1) = 1
    END DO
    RETURN
  END SUBROUTINE SEC_DERIVATIVE

  SUBROUTINE POTENTIAL(N,m,OMEGA,x_small,x_high,mass)    
    ! this subroutine computes the diagonal matrix
    ! that represent the discretized harmonic potential
    ! on 1dimensional grid of size N
    ! INPUTS:
    ! the size of the grid N
    ! omega
    ! mass
    ! x_small and x_high (extremes of the grid)
    ! OUTPUT: the matrix m
    DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: m  ! the matrix that we want
    INTEGER :: N, ii
    DOUBLE PRECISION :: OMEGA, x_small, x_high, step, mass

    step=(x_high-x_small)/N

    ALLOCATE(m(N,N))
    DO ii=1,N
       m(ii,ii)=mass/2*(omega*(x_small+step*(ii-1)))**2 ! diagonal elements
    END DO
  END SUBROUTINE POTENTIAL

  SUBROUTINE EIGVALS(m,egvals,info)
! This subroutine does the diagonalization
! though the subroutine ZHEEV provided by LAPACK. 
! Input:
! matrix : the matrix to be diagonalized.
! Outputs:                
! egvals : an array, contains the eigenvalues
! info   : an INTEGER*4, communicating the status of
!              the diagonalization. If = 0, all good.
! matrix : contains the eigenvectors.

    COMPLEX*16, DIMENSION(:,:) :: m
    REAL*8, DIMENSION(SIZE(m,1)) :: egvals
    INTEGER*4 :: info
    ! utilities for zheev
    INTEGER*4 :: N,LWORK
    COMPLEX*16, DIMENSION(:), ALLOCATABLE :: WORK
    REAL*8, DIMENSION(:), ALLOCATABLE :: RWORK
    ! preparing for optimization
    LWORK=-1
    N=SIZE(m,1)
    ALLOCATE(RWORK(3*N-2))
    ALLOCATE(WORK(1))
    ! querying optimal workspace
    CALL ZHEEV('V','U',N,m,N,egvals,WORK,LWORK,RWORK,info)
    ! preparing to get the results
    LWORK=INT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))
    CALL ZHEEV('V','U',N,m,N,egvals,WORK,LWORK,RWORK,info)
    ! removing exhausted variables 
    DEALLOCATE(WORK,RWORK)
  END SUBROUTINE EIGVALS
  
  FUNCTION FACTORIAL(n)
  ! function to compute the factorial of a number
  ! Takes in input the number "n"
  ! Gives in output the factorial "factorial"
    INTEGER*8 :: n,ii,factorial
    factorial=1
    DO ii=1,n
       factorial=factorial*ii
    END DO
    RETURN
  END FUNCTION FACTORIAL


  SUBROUTINE WRITE_ON_FILEeigvals(filename, array)

    REAL*8, DIMENSION(:) :: array
    CHARACTER(LEN=*) :: filename
    INTEGER*4 :: ii
    
    OPEN(1, FILE=filename, STATUS="replace", ACTION="write")
    
    DO ii=1,SIZE(array,1)
       WRITE(1,*) array(ii)
    END DO
    CLOSE(1)
    RETURN
  END SUBROUTINE WRITE_ON_FILEeigvals

  SUBROUTINE WRITE_ON_FILEeigvec(filename, xarray,mat)
    REAL*8, DIMENSION(:) :: xarray

    REAL*8, DIMENSION(:,:) :: mat
    CHARACTER(LEN=*) :: filename
    INTEGER*4 :: ii,jj
    
    OPEN(2, FILE=filename, STATUS="replace", ACTION="write")
    
    DO ii=1,SIZE(mat,1)
       WRITE(2,*) xarray(ii), (mat(ii,jj), jj=1,SIZE(mat,2))
    END DO
    CLOSE(2) 
    RETURN
  END SUBROUTINE WRITE_ON_FILEeigvec


  
END MODULE SOLVE_AO

PROGRAM ANALITIC_SOL
  USE DEBUGMOD
  USE HERMITE
  USE SOLVE_AO

  IMPLICIT NONE

  DOUBLE PRECISION :: omega,x_small, x_high, mass,step
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: egfunc
  REAL*8, DIMENSION(:), ALLOCATABLE :: point
  INTEGER*4 :: N, ii, kk
  INTEGER*8 :: p_ord
  REAL*8 :: fact_const,func_const
  CHARACTER*10 :: tmp_arg


  LOGICAL :: debug_flag  = .TRUE.
  
  PRINT*, "The size of the grid is:"
       READ*, N
  PRINT*, "The omega parameter is:"
       READ*, omega
  PRINT*, "The lower point/extreme of the grid is:"
       READ*, x_small
  PRINT*, "The maximum point/extreme of the grid is:"
       READ*, x_high
  PRINT*, "The mass is:"
       READ*, mass
  PRINT*, "The number of theoretical eigenfunctions we want to compute is:"
       READ*, kk
 

  CALL DEBUG(debug_flag,N,N < 0,"N is negative!")
  CALL DEBUG(debug_flag,kk,kk < 0,"k is negative!")
  CALL DEBUG(debug_flag,omega,omega < 0,"omega is negative!")
  CALL DEBUG(debug_flag,x_high-x_small,(x_high-x_small)< 0,"Extremes not ordered!")  
 

  step = (x_high-x_small)/N

   
  ALLOCATE(point(N))
  DO ii=1,N
     point(ii)=x_small+step*(ii-1)
  END DO


  ALLOCATE(egfunc(N,kk))  ! compte eigenfunctions theoretically 

func_const=(mass*omega/h_bar)**0.5 
  DO p_ord=1,kk
     fact_const= FLOAT(FACTORIAL(p_ord-1))
     egfunc(:,p_ord)=(func_const/SQRT(pi))**0.5*((SQRT(fact_const*(2**(p_ord-1))))**(-1))
     egfunc(:,p_ord)=egfunc(:,p_ord)*evalHerm_Poly((func_const*point), (p_ord-1))*EXP(-0.5*(func_const*point)**2)
  END DO
  
   CALL WRITE_ON_FILEeigvec('Analitycal_eigvects.dat', point, REAL(egfunc))

END PROGRAM ANALITIC_SOL

