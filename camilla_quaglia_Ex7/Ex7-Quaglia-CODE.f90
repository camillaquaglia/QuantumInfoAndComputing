!     to compile 'gfortran Ex7-Quaglia-CODE.f90 -o Ex7-Quaglia-CODE.x -llapack -lblas --lfftw3 -lfftw3f
!     to run the executable  './Ex7-Quaglia-CODE.x'

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
     MODULE PROCEDURE CMPLX16VEC_DEBUG
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
  SUBROUTINE CMPLX16VEC_DEBUG(debug,var, test,message)
    COMPLEX*16, DIMENSION(:) :: var  ! double complex vector 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
    END SUBROUTINE CMPLX16VEC_DEBUG

END MODULE DEBUGMOD
MODULE FOURIER
 !  In any Fortran subroutine where you want to use FFTW functions, you should begin with these commands http://fftw.org/doc/Overview-of-Fortran-interface.html
    use, intrinsic :: iso_c_binding
    include '/usr/include/fftw3.f03' ! the file 'fftw3.f03' is in this path

END MODULE FOURIER

MODULE SOLVE_AO
  ! This module contains the implementation of some functions and
  ! subroutines to solve the one dimensional time-independent Schrodinger
  ! equation through the finite difference method

  IMPLICIT NONE
  
  DOUBLE PRECISION, PARAMETER :: PI = ACOS(-1.d0)

CONTAINS
  
  SUBROUTINE SEC_DERIVATIVE(N,m)
    ! WARNING: of course this is the LAPLACIAN
    ! in greater dimensions
    ! this subroutine computes the second derivative
    ! (present in the momentum operator) on a
    ! discretized 1dimensional grid of size N
    ! INPUT: N, the size of the grid
    ! OUTPUT: the matrix m
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

  SUBROUTINE EIGVALS(matrix,egvals,info)
! This subroutine does the diagonalization
! though the subroutine ZHEEV provided by LAPACK. 
! Input:
! matrix : the matrix to be diagonalized.
! Outputs:                
! egvals : an array, contains the eigenvalues
! info   : an INTEGER*4, communicating the status of
!              the diagonalization. If = 0, all good.
! matrix : contains the eigenvectors.

    COMPLEX*16, DIMENSION(:,:) :: matrix
    REAL*8, DIMENSION(SIZE(matrix,1)) :: egvals
    INTEGER*4 :: info
    ! utilities for zheev
    INTEGER*4 :: N,LWORK
    COMPLEX*16, DIMENSION(:), ALLOCATABLE :: WORK
    REAL*8, DIMENSION(:), ALLOCATABLE :: RWORK
    ! preparing for optimization
    LWORK=-1
    N=SIZE(matrix,1)
    ALLOCATE(RWORK(3*N-2))
    ALLOCATE(WORK(1))
    ! querying optimal workspace
    CALL ZHEEV('V','U',N,matrix,N,egvals,WORK,LWORK,RWORK,info) !ZHEEV
    ! preparing to get the results
    LWORK=INT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))
    CALL ZHEEV('V','U',N,matrix,N,egvals,WORK,LWORK,RWORK,info) !ZHEEV
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
  SUBROUTINE FIRST_K_EG(matr,kk,egvals,egvecs,info)
!  This subroutine does the diagonalization
! though the subroutine ZHEEVX provided by LAPACK. 
!  INPUTS:
!  matr   : a COMPLEX*16 matrix, the matrix to be diagonalized.  
!  kk     : an INTEGER*4, the number of eigvals/eigvecs to return 
!  OUTPUTS:
!  egvals : a REAL*8 array that contains the first kk eigenvalues
!  egvecs : a COMPLEX*16 matrix that contains the first kk eigvectors
!  info   : an INTEGER*4, communicating the status of
!              the diagonalization. If = 0, all good.
    COMPLEX*16, DIMENSION(:,:) :: matr
    REAL*8, DIMENSION(SIZE(matr,1)) :: egvals
    COMPLEX*16, DIMENSION(SIZE(matr,1),kk) :: egvecs
    INTEGER*4 :: info, kk
    
    INTEGER*4 :: N,LWORK,M
    COMPLEX*16, DIMENSION(:), ALLOCATABLE :: WORK
    REAL*8, DIMENSION(7*SIZE(matr,1)) :: RWORK
    INTEGER*4, DIMENSION(5*SIZE(matr,1)) :: IWORK
    INTEGER*4, DIMENSION(SIZE(matr,1)) :: IFAIL
    REAL*8 :: abstol
    
    ! preparing for optimization
    LWORK=-1
    N=SIZE(matr,1)
    ALLOCATE(WORK(1))

    abstol = 2D-323    !theoretical optimal abstol
    CALL ZHEEVX('V','I','U',N,matr,N,0.D0,0.D0,1,kk,abstol,&
         M,egvals,egvecs,N,WORK,LWORK,RWORK,IWORK,IFAIL,info)
   
    ! preparing to get the results
    LWORK=INT(WORK(1))
    DEALLOCATE(WORK)
    ALLOCATE(WORK(LWORK))

    ! getting the results
    CALL ZHEEVX('V','I','U',N,matr,N,0.D0,0.D0,1,kk,abstol,&
         M,egvals,egvecs,N,WORK,LWORK,RWORK,IWORK,IFAIL,info)
    DEALLOCATE(WORK)
  END SUBROUTINE FIRST_K_EG
  SUBROUTINE MOD_SQUARE(m,probs)
!  This subroutine computes the probabilities corresponding
!  to a given eigenvector.
!  INPUT:
!  m  : a COMPLEX*16 matrix, its columns contain
!             the eigenvectors  
!  OUTPUT:
!  probs  : a REAL*8 matrix, its columns contain the
!               probabilities corresponding to the eigenvectors
    COMPLEX*16, DIMENSION(:,:) :: m
    REAL*8, DIMENSION(SIZE(m,1),SIZE(m,2)) :: probs
    INTEGER*4 :: ii

    DO ii=1,SIZE(m,2)
       probs(:,ii)=ABS(m(:,ii))**2
    END DO
    RETURN
  END SUBROUTINE MOD_SQUARE

END MODULE SOLVE_AO

MODULE GRID1D
! This module contains tools used to create
! the grid of points

  use, intrinsic :: iso_c_binding
  USE FOURIER

  IMPLICIT NONE

  TYPE LTC1D
     INTEGER*4 :: numpoints
     REAL*8 :: stepsize, lower, upper
     REAL*8, DIMENSION(:), ALLOCATABLE :: grid
  END TYPE LTC1D

CONTAINS

  FUNCTION GRID_POINTS(npnt,lower,upper)  RESULT(ltc)
  ! this function takes as INPUTS:
  ! npnt = an INTEGER that represent the number of points
  !                          we want in our grid
  ! lower & upper = extremes points, DOUBLE PRECISION
  ! OUTPUT: ltc = an array of dimension npnt, containing all the points
    REAL*8, DIMENSION(npnt) :: ltc
    INTEGER*4 :: npnt,ii
    REAL*8 :: lower,upper,step
    step = (upper-lower)/(npnt-1) ! the step 
    DO ii=1,npnt
       ltc(ii)=lower+(ii-1)*step
    END DO
  END FUNCTION GRID_POINTS

  FUNCTION DHP(ltc,center)
    REAL*8, DIMENSION(:) :: ltc
    REAL*8 :: center
    REAL*8, DIMENSION(SIZE(ltc)) :: DHP
    DHP=(ltc-center)**2
  END FUNCTION DHP

  
  FUNCTION FT_1D(func)RESULT(res)
    COMPLEX*16, DIMENSION(:) :: func
    COMPLEX*16, DIMENSION(SIZE(func)) :: res
    INTEGER*8 :: plan,veclen

    veclen=SIZE(func)
    
    CALL dfftw_plan_dft_1d(plan,veclen,func,res,FFTW_FORWARD,FFTW_ESTIMATE)
    CALL dfftw_execute_dft(plan, func, res)
    CALL dfftw_destroy_plan(plan)
    res=res/SQRT(REAL(veclen))
    RETURN
  END FUNCTION FT_1D
  
  
  FUNCTION AFT_1D(func)RESULT(res)
    COMPLEX*16, DIMENSION(:) :: func
    COMPLEX*16, DIMENSION(SIZE(func)) :: res
    INTEGER*8 :: plan,veclen

    veclen=SIZE(func)
    
    CALL dfftw_plan_dft_1d(plan,veclen,func,res,FFTW_BACKWARD,FFTW_ESTIMATE)
    CALL dfftw_execute_dft(plan, func, res)
    CALL dfftw_destroy_plan(plan)
    res=res/SQRT(REAL(veclen))
    RETURN
  END FUNCTION AFT_1D
  
END MODULE GRID1D

PROGRAM EX7
  USE GRID1D
  USE SOLVE_AO
  USE DEBUGMOD

  USE, INTRINSIC :: iso_c_binding
  USE FOURIER
  
  IMPLICIT NONE

  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: T,V,H,egvecs
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: evol_operatorV,evol_operatorT
  REAL*8, DIMENSION(:), ALLOCATABLE :: grid, egvals, pgrid
  INTEGER*4 :: NN, kk, num_tsteps
  REAL*8 :: omega, TT, x_low, x_high, step, time_inst, center, tstep, mass
  
  COMPLEX*16, DIMENSION(:,:,:), ALLOCATABLE :: wave_evo
  REAL*8, DIMENSION(:,:,:), ALLOCATABLE :: prob_evo
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: pot_evo
  
  INTEGER*4 :: ii, jj, INFO
  CHARACTER*3 :: int_to_str

  LOGICAL :: debug_flag = .TRUE.

  PRINT*, "The size of the grid is:"
       READ*, NN
  PRINT*, "The omega parameter is:"
       READ*, omega
  PRINT*, "The lower point/extreme of the grid is:"
       READ*, x_low
  PRINT*, "The maximum point/extreme of the grid is:"
       READ*, x_high
  PRINT*, "The mass is:"
       READ*, mass
  PRINT*, "The number of eigenvalues and eigenfunctions we want to compute is:"
       READ*, kk
  PRINT*, "The numer of time steps is:"
       READ*, num_tsteps
  PRINT*, "T is:"
       READ*, TT

  ! allocating the variables
  ALLOCATE(grid(NN))
  ALLOCATE(pgrid(NN))
  ALLOCATE(H(NN,NN))
  ALLOCATE(egvals(NN))
  ALLOCATE(evol_operatorV(NN))
  ALLOCATE(evol_operatorT(NN))
  ALLOCATE(egvecs(NN,kk))
  ALLOCATE(wave_evo(NN,kk,num_tsteps+1))
  ALLOCATE(prob_evo(NN,kk,num_tsteps+1))
  ALLOCATE(pot_evo(NN,num_tsteps+1))
  
  grid = GRID_POINTS(NN,x_low,x_high) ! create a grid of points
  step = grid(3)-grid(2)
  pgrid= GRID_POINTS(NN, 0.d0, (2*pi)/step*(NN-1)/NN ) ! grid for the momentum
  pgrid(NN/2:)=pgrid(NN/2:)-pgrid(NN)-pgrid(2)
  
  CALL SEC_DERIVATIVE(NN, T)
  CALL POTENTIAL(NN, V, omega, x_low, x_high,mass)

  H=-(1/step**2)*T/2+V
  DEALLOCATE(V,T)

  CALL FIRST_K_EG(H,kk,egvals,egvecs,INFO)
  DEALLOCATE(H)
  
  !check INFO 
  IF(INFO.NE.0)THEN
     PRINT*, 'ERROR: FAILED DIAGONALIZATION!'
  ELSE     
     ! normalize the eigenvectors
     egvecs=egvecs/SQRT(step)
  END IF
 
  CALL MOD_SQUARE(egvecs,prob_evo(:,:,1))
  wave_evo(:,:,1)=egvecs
  pot_evo(:,1)=0D0

  ! check initial norm
  CALL DEBUG(debug_flag,egvecs(:,1),ABS(SUM(ABS(egvecs(:,1)*SQRT(step))**2)-1) >= (10E-4),"state not normalized!")
  

  tstep=1/TT

  !computing the kinetic evolution operator (in p representation) 
  evol_operatorT=EXP(-(tstep*0.5)*CMPLX(0.D0, (pgrid)**2))

  ! performing the evolution steps
  DO ii=0,num_tsteps-1
     time_inst=ii*tstep
     
     center=time_inst
     !computing the potential evolution operator (in x representation) 
     evol_operatorV=EXP(-(tstep*0.25)*CMPLX(0.D0,(omega**2)*DHP(grid,center)))
     
     !computing the effect of the operator on the state
     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evol_operatorV
     END DO
     
     !transforming each eigenvector
     DO jj=1,kk
        egvecs(:,jj)=FT_1D(egvecs(:,jj))
     END DO

     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evol_operatorT ! apply kinetic operator on the state
     END DO

     DO jj=1,kk
        egvecs(:,jj)=AFT_1D(egvecs(:,jj)) ! trasform back
     END DO

     DO jj=1,kk
        egvecs(:,jj)=egvecs(:,jj)*evol_operatorV ! apply potential operator on the state
     END DO


       ! check norm of the state after the evolution
     CALL DEBUG(debug_flag,egvecs(:,1),ABS(SUM(ABS(egvecs(:,1)*SQRT(step))**2)-1)>=(10E-4),"state not normalized ")
  
     pot_evo(:,ii+1) = (omega**2)*DHP(grid,center)
     wave_evo(:,:,ii+1) = egvecs
     CALL MOD_SQUARE(egvecs,prob_evo(:,:,ii+1))
     
  END DO

  DO ii=1,kk
     
     CALL WRITE_ON_FILEeigvec('prob.dat',&
          grid,prob_evo(:,ii,:))
  END DO
  DO ii=1,kk
    

     CALL WRITE_ON_FILEeigvec('realpart.dat',&
          grid,REALPART(wave_evo(:,ii,:)))

     CALL WRITE_ON_FILEeigvec('imaginarypart.dat',&
          grid,IMAGPART(wave_evo(:,ii,:)))
  END DO

  CALL WRITE_ON_FILEeigvec('potential.dat',grid,pot_evo)
  
  
END PROGRAM EX7

  


  
    


