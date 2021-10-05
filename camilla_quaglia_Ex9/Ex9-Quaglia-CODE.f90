!     to compile 'gfortran Ex9-Quaglia-CODE.f90 -o Ex9-Quaglia-CODE.x -llapck -lblas'
!     to run the executable  './Ex9-Quaglia-CODE.x'

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
     MODULE PROCEDURE INT8_DEBUG
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
  
    SUBROUTINE INT8_DEBUG(debug,var,test,message)
    INTEGER*8 :: var ! 2 bytes integer variable 
    LOGICAL :: debug, test
    CHARACTER(*) :: message
    IF (debug .and. test) THEN       
       PRINT*, message
       PRINT*, "The variable to debug is: ", var
  
    ENDIF
  END SUBROUTINE INT8_DEBUG

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

MODULE OPER
	
CONTAINS

  FUNCTION TENSOR_PROD(m_1, m_2)
    ! This function takes as input two matrices
    ! and does the tensor product 
    IMPLICIT NONE
		
    COMPLEX*16, DIMENSION(:,:) :: m_1, m_2
    COMPLEX*16, DIMENSION(SIZE(m_1,1)*SIZE(m_2,1),SIZE(m_1,2)*SIZE(m_2,2)) :: TENSOR_PROD
    INTEGER*8 :: ii, jj, left_r, left_c, right_r, right_c
		
    left_r = SIZE(m_1,1)
    left_c = SIZE(m_1,2)
		
    right_r = SIZE(m_2,1)
    right_c= SIZE(m_2,2)
		
    DO ii=0, left_r-1
       DO jj=0, left_c-1
          TENSOR_PROD(ii*right_r+1:(ii+1)*right_r,jj*right_c+1:(jj+1)*right_c) = m_1(ii+1,jj+1)*m_2(:,:)
       END DO
    END DO
    
    RETURN
  END FUNCTION TENSOR_PROD
  
END MODULE OPER

PROGRAM EX9
! program to study a system of N spin-1/2 particles on 1D lattice

USE DEBUGMOD
USE OPER

IMPLICIT NONE

INTEGER*8 :: N, d, step, ii, jj ,ww! N is the number of particles, d is the dimension of every subsystem, step is used to built the non interacting term of the Hamiltonian
INTEGER*4, DIMENSION(:), ALLOCATABLE :: diagonal ! vector of the entries of the diagonal matrix
COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: H ! matrix that represent the whole Hamiltonian
COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: H_nonint, H_int ! non-interaction and interaction parts of H
COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: container ! a 'container' matrix
DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: eig_val ! vectors of eigenvalues
DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: eig_valN
REAL*8, DIMENSION(15,6) :: eigval_res ! matrix that stores lambda and the first 5 eigenvalues for each iteraction (15 iteractions in total)
REAL*8 :: lambda ! parameter of H

COMPLEX*16, DIMENSION(2,2), PARAMETER ::&
		id_2 = RESHAPE((/ &               ! 2D identity matrix
		CMPLX(1d0,0d0), CMPLX(0d0,0d0),&
		CMPLX(0d0,0d0), CMPLX(1d0,0d0)/),&
		SHAPE(id_2)),&
		sigma_x = (RESHAPE((/ &            ! 1st Pauli matrix
		CMPLX(0d0,0d0), CMPLX(1d0,0d0),&
		CMPLX(1d0,0d0), CMPLX(0d0,0d0)/),&
		SHAPE(sigma_x))),&
		sigma_y = (RESHAPE((/ &           ! 2st Pauli matrix
		CMPLX(0d0,0d0), CMPLX(0d0,1d0),&
		CMPLX(0d0,-1d0), CMPLX(0d0,0d0)/),&
		SHAPE(sigma_y))),&
		sigma_z = (RESHAPE((/ &           ! 3rd Pauli matrix
		CMPLX(1d0,0d0), CMPLX(0d0,0d0),&
		CMPLX(0d0,0d0), CMPLX(-1d0,0d0) /),&
		SHAPE(sigma_z)))

LOGICAL :: debug_flag


! parameters to use in DYSEV subroutine
COMPLEX*16, DIMENSION(:), ALLOCATABLE ::  WORK_, wk_opt
DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: RWORK_
INTEGER*8 :: LWORK_, info_eig

PRINT*, "Insert the number of particles"
READ*, N

PRINT*, "Insert the dimension of the subsystems"
READ*, d

debug_flag = .TRUE.

CALL DEBUG(debug_flag,d,d < 0 ,'Insert a positive dimension')
CALL DEBUG(debug_flag,N,N < 0 ,'Insert a positive number of particle')


ALLOCATE(H(d**N,d**N))
ALLOCATE(H_nonint(d**N,d**N))
ALLOCATE(H_int(d**N,d**N))	
ALLOCATE(container(d**N,d**N))	
ALLOCATE(eig_val(d**N))
ALLOCATE(eig_valN(d**N))
ALLOCATE(diagonal(d**N))	
ALLOCATE(RWORK_(3*d**N-2))

! Initializing entries to zero
diagonal = 0

! Initializing hamiltonian to zero
H(:,:) = 0
H_nonint(:,:) = 0
H_int(:,:) = 0

container = 0

! Building diagonal matrix for the non interaction term
DO ii=1,N
   step = d**(N+1-ii)
   DO jj=0,d**N-1,step
      diagonal((jj+1):(jj+step/2)) = diagonal((jj+1):(jj+step/2))+1
      diagonal((jj+step/2+1):(jj+step)) = diagonal((jj+step/2+1):(jj+step))-1
   END DO
END DO

DO ii=1,d**N
   H_nonint(ii,ii) = diagonal(ii)
END DO

! interaction terms (that are N-1)
DO ii=1,N-1
	
   container = 0
	
   DO jj=1,N
		
      IF (jj.EQ.1) THEN
			
         IF (((N+1-jj).EQ.ii).OR.((N+1-jj).EQ.(ii+1))) THEN
            container(1:d**(jj),1:d**(jj)) = sigma_x(:,:)
				
         ELSE
            container(1:d**(jj),1:d**(jj)) = id_2(:,:)
         
         END IF
      ELSE
    
         IF (((N+1-jj).EQ.ii).OR.((N+1-jj).EQ.(ii+1))) THEN
            container(1:d**(jj),1:d**(jj)) = TENSOR_PROD(sigma_x, container(1:d**(jj-1),1:d**(jj-1) ))
	
         ELSE
            container(1:d**(jj),1:d**(jj)) = TENSOR_PROD(id_2, container(1:d**(jj-1),1:d**(jj-1) ))
	
         END IF
      END IF
   END DO
 
   H_int(:,:) = H_int(:,:) + container(:,:) ! Update the interaction term
   
END DO
	
! loop to evaluate lambda
DO ii=0,14

   lambda = 3.0/(14.)*ii

   WRITE(*,*) "lambda = ", lambda
   
   H = 0
   H(:,:) =  H_int(:,:) + lambda*H_nonint(:,:)

   ALLOCATE(wk_opt(1))
 
   LWORK_=-1
		
! documentation http://www.netlib.org/lapack/explore-html/df/d9a/group__complex16_h_eeigen.html 

   CALL ZHEEV('N', 'U', d**N, H, d**N, eig_val, wk_opt, LWORK_, RWORK_, info_eig)
   LWORK_ = INT(wk_opt(1))
   ALLOCATE(WORK_(LWORK_))  ! optimal dimension for WORK

   !subroutine to do the diagonalization	
   CALL ZHEEV('N', 'U', d**N, H, d**N, eig_val, WORK_, LWORK_, RWORK_, info_eig)
		
   DEALLOCATE(wk_opt)
   DEALLOCATE(WORK_)

   PRINT*,  eig_val(1:5)
   DO ww=1,d**N
        eig_val(ww) = eig_val(ww)/N
   END DO 

   PRINT*,  eig_val(1:5)
   ! results to plot	
   eigval_res(ii+1,1) = lambda      
   eigval_res(ii+1,2:6) = eig_val(1:5)! first five eigenvalues for every lambda
   PRINT*, eigval_res(ii+1,2:6)
END DO


OPEN(66, file="lambda_eigenvalues.txt", status='REPLACE', action='WRITE')
	
DO ii=1,15
   WRITE(66,"(F12.8)",advance="No") eigval_res(ii,1)

   DO jj=2,6-1
      WRITE(66,"(F15.8)",advance="No") eigval_res(ii,jj)
   END DO
		
   WRITE(66,"(F15.8)",advance="Yes") eigval_res(ii,6)
END DO
	
 CLOSE(66)

DEALLOCATE(H)
DEALLOCATE(H_nonint)
DEALLOCATE(H_int)
DEALLOCATE(container)
DEALLOCATE(eig_val)
DEALLOCATE(diagonal)
DEALLOCATE(RWORK_)

END PROGRAM EX9

