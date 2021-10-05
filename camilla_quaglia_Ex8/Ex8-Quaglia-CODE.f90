!     to compile 'gfortran Ex8-Quaglia-CODE.f90 -o Ex8-Quaglia-CODE.x '
!     to run the executable  './Ex8-Quaglia-CODE.x'

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

PROGRAM ex8
  USE DEBUGMOD
  IMPLICIT NONE
  
  !   re_part & im_part : variables to initialize the real and the imaginary parts of matrix elements
  ! N : number of subsystems
  ! D : dimension of the subsystems
  ! tr : variable to store the trace of the density matrix
  ! subsys : a number used to select the subsys to be traced out:
  !           it should be 1 ≤ subsys ≤ N

  DOUBLE PRECISION :: re_part, im_part, tr
  INTEGER*8 :: N, D, ii, jj, reduced_dim, ii_min, ii_max, res_t, rem_t, ii_red, subsys, index_i, index_j
  COMPLEX*16, DIMENSION(:), ALLOCATABLE :: psi_sep, psi_not_sep,  psi_sep_temporary
  COMPLEX*16, DIMENSION(:,:), ALLOCATABLE :: rho_sep, rho_not_sep, rho_squared, rho_reduced

  LOGICAL :: debug_flag, Herm_flag


  debug_flag = .TRUE.

  PRINT*, 'Insert the number of subsystems:'
  READ*, N
  
  PRINT*, 'Insert the dimension of the subsystems:'
  READ*, D
  

  CALL DEBUG(debug_flag,N,N .LE. 0, "Insert a positive number of subsystems!")
  CALL DEBUG(debug_flag,D,D .LE. 0, "Insert a positive dimension!")
  
  ALLOCATE(psi_sep(D**N))
  ALLOCATE(psi_sep_temporary(D**N))
  ALLOCATE(psi_not_sep(D**N))

  ALLOCATE(rho_sep(D**N,D**N))
  ALLOCATE(rho_not_sep(D**N,D**N))
  ALLOCATE(rho_squared(D**N,D**N))
  
  ! wave function vector in the not separable case (vector of D**N components)
  DO ii=1,D**N
     CALL RANDOM_NUMBER(re_part) ! re_part is a random number ~ U([0,1])
     CALL RANDOM_NUMBER(im_part) ! im_part is a random number ~ U([0,1])
     psi_not_sep(ii) = CMPLX(2*re_part-1.,2*im_part-1) ! the component are complex values in the interval [-1,1] 
  END DO

  psi_not_sep(:) = psi_not_sep(:) / SQRT(SUM(ZABS(psi_not_sep)**2)) ! normalization

  OPEN(1, file="not_separable_case.dat",status="REPLACE", action='WRITE')
  WRITE(1,*)  "Norm of the wave function" , SQRT(SUM(ZABS(psi_not_sep)**2))
  WRITE(1,*)  "Wave function of the system"
  DO ii=1,D**N   
          WRITE(1,"(F11.8,F13.8)",advance="No")    psi_not_sep(ii)   
  END DO
	
  ! wave function vector in the separable case (for each single subsystem)
  DO ii=0,N-1
     DO jj=1,D
        CALL RANDOM_NUMBER(re_part)
        CALL RANDOM_NUMBER(im_part)
        psi_sep_temporary(ii*D + jj) =  CMPLX(2*re_part-1.,2*im_part-1)
     END DO
     ii_min = ii*D + 1
     ii_max = ii*D + jj
     psi_sep_temporary(ii_min:ii_max) =  psi_sep_temporary(ii_min:ii_max) /SQRT(SUM(ZABS( psi_sep_temporary(ii_min:ii_max)**2)))
  END DO

     ! density matrix in the non separable case
     DO ii=1,D**N
        DO jj=1,D**N
           rho_not_sep(ii,jj) = psi_not_sep(ii)*CONJG(psi_not_sep(jj))
        END DO
     END DO
     WRITE(1,*) "Density matrix:" 
     DO ii=1,D**N 
       DO jj=1,D**N  
          WRITE(1,"(F15.8,F13.8)",advance="No") rho_not_sep(ii,jj)  
       END DO 
     END DO

     tr = 0
     rho_squared = MATMUL(rho_not_sep, rho_not_sep)
     DO ii=1,D**N
        tr = tr + rho_squared(ii,ii)
     END DO

     WRITE(1,*)  "Trace of density matrix^2:", tr
     Herm_flag = .TRUE.	
     DO ii=1,D**N
        DO jj=ii,D**N
           IF (rho_not_sep(ii,jj).NE.CONJG(rho_not_sep(jj,ii))) THEN
              Herm_flag = .FALSE.
           END IF
        END DO
     END DO

     WRITE(1,*)  "Is the density matrix Hermitian?", Herm_flag

     ! wave function vector (complete case!) in the separable case
     DO ii=1,D**N
        res_t = ii-1
        psi_sep(ii) = (1.,0.)	
        DO jj=1,N
           rem_t = MOD(res_t,D)
   
               psi_sep(ii) = psi_sep(ii) * psi_sep_temporary(D*(N - jj) + rem_t + 1)
			
           res_t = (res_t-rem_t)/D
        END DO
        
     END DO
     OPEN(2, file="separable_case.dat",status="REPLACE", action='WRITE')
     WRITE(2,*)  "Wave function of the system"
     DO ii=1,D**N   
        WRITE(2,"(F11.8,F13.8)",advance="No") psi_sep(ii)   
     END DO

  ! density matrix in the separable case
     DO ii=1,D**N
        DO jj=1,D**N
           rho_sep(ii,jj) = psi_sep(jj)*CONJG(psi_sep(ii))
        END DO
     END DO

     tr = 0
     rho_squared = MATMUL(rho_sep, rho_sep)
     DO ii=1,D**N
        tr = tr + rho_squared(ii,ii)
     END DO
     
     WRITE(2,*) "Density matrix:" 
     DO ii=1,D**N 
        DO jj=1,D**N  
           WRITE(2,"(F15.8,F13.8)",advance="No")  rho_sep(ii,jj)  
        END DO
     END DO
     WRITE(2,*)  "Trace of density matrix^2:", tr


    ! Checking the density matrix is hermitian
     Herm_flag = .TRUE.	
     DO ii=1,D**N
        DO jj=ii,D**N
           IF (rho_not_sep(ii,jj).NE.CONJG(rho_not_sep(jj,ii))) THEN
              Herm_flag = .FALSE.
           END IF
        END DO
     END DO
     WRITE(2,*)  "Is the density matrix Hermitian?", Herm_flag


     
     ! EVALUATION OF THE PARTIAL TRACE
   
!	For N=2 (and D >= 0)
!	Density matrices (in both separable and not sep. case) are D*D,
!       so the system density matrix is (D*D)^N
!	When we trace out a subsystem the dimension becomes (D*D)^(N-1)

     reduced_dim = D*(N-1)

     ALLOCATE(rho_reduced(reduced_dim,reduced_dim))
	
	! Initialize rho_reduced 
     DO ii=1,reduced_dim
        DO jj=1,reduced_dim
           rho_reduced(ii,jj) = CMPLX(0d0,0d0)
        END DO
     END DO

     PRINT*, "The particle/system to trace out is (type 1 or 2):"
     READ*, subsys
	
	! build the reduced matrix
     DO ii=0,D**(N-1)-1
        DO jj=0,D**(N-1)-1
           DO ii_red=0,D-1
              index_i = ii*D**(subsys-1)+ii_red*D**(N-subsys) + 1
              index_j = jj*D**(subsys-1)+ii_red*D**(N-subsys) + 1
              rho_reduced(ii+1,jj+1) = rho_reduced(ii+1,jj+1) + rho_sep(index_i,index_j)
           END DO
        END DO
     END DO

     OPEN(3,file="reduced_matrix.dat", status="REPLACE", ACTION="WRITE")
     WRITE(3,*) "Evaluated reduced matrix"
     DO ii=1,D**(N-1)
        DO jj=1,D**(N-1)
           WRITE(3,"(F15.8,F13.8)",advance="Yes") rho_reduced(ii,jj)
        END DO
     END DO

	! Writing expected rho_reduced (it corresponds to the density matrix of the other subsys (N=2))
     WRITE(3,*) "Expected reduced matrix"
     DO ii=1,D
        DO jj=1,D
           WRITE(3,"(F15.8,F13.8)",advance="yes") psi_sep_temporary((N-subsys)*D+jj)*CONJG(psi_sep_temporary((N-subsys)*D+ii))
        END DO
     END DO

     CLOSE(1)
     CLOSE(2)
     CLOSE(3)
 DEALLOCATE(psi_sep)
 DEALLOCATE(psi_not_sep)
 DEALLOCATE(psi_sep_temporary)
 DEALLOCATE(rho_not_sep)
 DEALLOCATE(rho_sep)
 DEALLOCATE(rho_squared)
	
 STOP

END PROGRAM EX8
