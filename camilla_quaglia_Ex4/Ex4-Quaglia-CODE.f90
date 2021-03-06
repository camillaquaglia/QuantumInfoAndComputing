!     to compile 'gfortran Ex4-Quaglia-CODE.f90 -o Ex4-Quaglia-CODE.x'
!     to run the executable  './Ex4-Quaglia-CODE.x'

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


MODULE matrix
  ! This module is similar to the one done in the ex3, first assigment.
  ! It contains 3 functions to do the matrix-matrix multiplication.
  IMPLICIT NONE

  REAL*8, DIMENSION(:,:), ALLOCATABLE :: matrix1,matrix2,res1,res2,res3,res4
  ! Matrices to be multiplied and the resulting ones are defined. They
  ! will contain real numbers,in double precision, and for now their size is undefined (ALLOCATABLE)

  INTEGER*4 :: r1,c1,r2,c2  ! rows&columns
  INTEGER*4 ::  ii, jj, kk  ! indexes
  INTEGER*4, DIMENSION(2) :: dims1,dims2 ! Integer arrays that contain the dimensions of the matrices.
  REAL*8 :: start,finish  ! double precision real numbers that indicate the start and the end of the duration of the process to do the matrix multiplication

CONTAINS

  SUBROUTINE DIM_FILE(file_name,sizes)
    ! This subroutine opens the file from which the dimensions
    ! of the matrices to be multiplied are read.
    !The dimensions are then stored in an array.
   
    CHARACTER(*) :: file_name     ! the name of the aforementioned file
    INTEGER*4, DIMENSION(4) :: sizes ! integer array in which the dimensions arre stored
    LOGICAL :: file_exists
    
    inquire (file = file_name, exist=file_exists)  ! test if the input file exists
    IF (.not. file_exists) THEN
       PRINT*, 'The file does not exist'
       STOP
    ELSE IF  (file_exists) THEN
       OPEN (12, file = file_name, status = 'old') ! status='old' means 'the file should exist'
       DO ii = 1,4
          READ(12,*) sizes(ii)
       ENDDO
       CLOSE(12)
    ENDIF
  END SUBROUTINE DIM_FILE
      
  SUBROUTINE DIMS(r1,c1,r2,c2) 
  ! This subroutine asks the user the dimensions of the matrices to be
  ! multiplied. It takes in input the integer values that are these inserted dimensions (rows&columns).
    INTEGER*4 :: r1,c1,r2,c2
    r1=0  
    c1=0      ! all the dimensions are 'inizialized' to zero
    r2=0
    c2=0   

    DO WHILE (r1 <= 0)
       PRINT*, "The number of rows of the first matrix is:"
       READ*, r1
       IF (r1 <= 0) PRINT*, "Try with a positive value"
    ENDDO
    DO WHILE (c1 <= 0)
       PRINT*, "The number of columns of the first matrix is:"
       READ*, c1
       IF (c1 <= 0) PRINT*, "Try with a positive value"
    ENDDO
    DO WHILE (r2 <= 0)
       PRINT*, "The number of rows of the second matrix is:"
       READ*, r2
       IF (r2 <= 0) PRINT*, "Try with a positive value"
    ENDDO
    DO WHILE (c2 <= 0)
       PRINT*, "The number of columns of the second matrix is:"
       READ*, c2
       IF (c2 <= 0) PRINT*, "Try with a positive value"
    ENDDO
  END SUBROUTINE DIMS
  
  FUNCTION test_DIMS(dims1,dims2)
    ! This function checks if the dimensions of the matrices allow the multiplication
    ! (so must be 'number of rows matrix1 = number of columns matrix2')
    ! It takes in input two vectors, containing the dimensions of the two matrices,
    ! and returns a logical value, 'test_dims'.
    INTEGER*4, DIMENSION(2) :: dims1, dims2
    LOGICAL :: test_dims
    IF ((dims1(2)) .eq. (dims2(1))) THEN
       test_dims = .TRUE.
    ELSE
       test_dims = .FALSE.
    ENDIF
    RETURN
  END FUNCTION test_DIMS
  
  FUNCTION matrix_multiplication1(m1,m2)
      ! This function takes in input two real matrices,in double precision, 
      ! does the multiplication using the intrinsic function DOT_PRODUCT(),
      ! and gives the resulting matrix with the proper dimension.
    
    REAL*8, DIMENSION(:,:) :: m1,m2
    REAL*8, DIMENSION(SIZE(m1,1),SIZE(m2,2)) :: matrix_multiplication1

    matrix_multiplication1 = 0.d0
    dims1 = SHAPE(m1)
    dims2 = SHAPE(m2)
    IF (test_DIMS(dims1,dims2)) THEN
       do ii=1,dims1(1)
          do jj=1,dims2(2)
             matrix_multiplication1(ii,jj)= DOT_PRODUCT(m1(ii,:),m2(:,jj)) ! intrinsic function DOT_PRODUCT()
          enddo
       enddo
       RETURN
    ELSE
       print*,"WRONG DIMENSIONS!"
       RETURN
    END IF
  END FUNCTION matrix_multiplication1
      
  FUNCTION matrix_multiplication2(m1,m2) 
      ! This function takes in input two real matrices,in double precision, 
      ! does the multiplication using the classical rule 'rows times columns',
      ! and give the resulting matrix with the proper dimension.
   
    REAL*8, DIMENSION(:,:) :: m1,m2
    REAL*8, DIMENSION(SIZE(m1,1),SIZE(m2,2)) :: matrix_multiplication2
   
    matrix_multiplication2 = 0.d0
    dims1 = SHAPE(m1)
    dims2 = SHAPE(m2)
    IF (test_DIMS(dims1,dims2)) THEN
       DO ii=1,dims1(1)
          DO jj=1,dims2(2)
             DO kk=1,dims1(2)
                matrix_multiplication2(ii,jj) = matrix_multiplication2(ii,jj) + m1(ii,kk)*m2(kk,jj) ! direct application of the fomula
             ENDDO
          ENDDO
       ENDDO
       RETURN
    ELSE
       PRINT*,"WRONG DIMENSIONS!!"
       RETURN
    END IF
  END FUNCTION matrix_multiplication2
      

  FUNCTION matrix_multiplication3(m1,m2)
! This function takes in input two real matrices,in double precision, 
      ! does the multiplication using the rule 'columns of the first matrix times the elements of the second one',
      ! and give the resulting matrix with the proper dimension.
    REAL*8, DIMENSION(:,:) :: m1,m2
    REAL*8, DIMENSION(SIZE(m1,1),SIZE(m2,2)) :: matrix_multiplication3
    
    matrix_multiplication3 = 0.d0
    IF (test_DIMS(dims1,dims2)) THEN
       do ii=1,dims2(2)
          do jj=1,dims1(2)
             matrix_multiplication3(:,ii) =  matrix_multiplication3(:,ii) + m1(:,jj)*m2(jj,ii) ! multiplication of the columns of the first matrix by the elements of the second one.
          enddo
       enddo
       RETURN
    ELSE
       PRINT*,"WRONG DIMENSIONS!!"
       RETURN
    END IF
  END FUNCTION  matrix_multiplication3
END MODULE matrix


PROGRAM EX4
  USE matrix
  USE DEBUGMOD
  IMPLICIT NONE

   
  INTEGER*4, DIMENSION(4) :: allsize  ! Variable to store the sizes of matrices

  LOGICAL :: manual_insert  ! Flag to decide input mode
  
  turn_debug_on = .TRUE.
  manual_insert = .FALSE.
  

  open(1,file="time1.txt",status='old',position="append",action='write')
  open(2,file="time2.txt",status='old',position="append",action='write')
  open(3,file="time3.txt",status='old',position="append",action='write')
  open(4,file="time4.txt",status='old',position="append",action='write')

  IF (manual_insert) THEN
     CALL DIMS(r1,c1,r2,c2)
  ELSE
     CALL DIM_FILE("dims.txt", allsize)
     r1 = allsize(1)
     c1 = allsize(2)
     r2 = allsize(3)
     c2 = allsize(4)


  END IF
  
  IF (test_DIMS( (/r1,c1/) ,(/r2,c2/) )) THEN
     ALLOCATE(matrix1(r1,c1),matrix2(r2,c2)) 
     ! allocating the appropriate memory and initializing the matrices
     
    ! matrices to be multiplied are filled with random numbers ~ U([0,1])
     call RANDOM_NUMBER(matrix1) 
     call RANDOM_NUMBER(matrix2)
     
     ! use interface to debug
     CALL DEBUG(turn_debug_on, SHAPE(matrix1), &
          (ALL(SHAPE(matrix1) .NE. (/r1,c1/))),"size m1")
     CALL DEBUG(turn_debug_on, SHAPE(matrix2), &
          (ALL(SHAPE(matrix2) .NE. (/r2,c2/))),"size m2")


     ! time performance of the differents multiplication methods 

     CALL CPU_TIME(start)
     res1=matrix_multiplication1(matrix1,matrix2)
     CALL DEBUG(turn_debug_on, SHAPE(res1), &
          (ALL(SHAPE(res1) .NE. (/r1,c2/))),"size result")
     CALL CPU_TIME(finish)
     print*, "Using the intrinsic function DOT_PRODUCT() takes ",finish-start," seconds"
     write(1,*) finish-start, r1*c2
     
     CALL CPU_TIME(start)
     res2=matrix_multiplication2(matrix1,matrix2)
     CALL DEBUG(turn_debug_on, SHAPE(res2), &
          (ALL(SHAPE(res2) .NE. (/r1,c2/))),"size result!")
     CALL CPU_TIME(finish)
     print*, "Using the standard way takes ",finish-start," seconds"
     write(2,*) finish-start, r1*c2
      
     CALL CPU_TIME(start)
     res3=matrix_multiplication3(matrix1,matrix2)
     CALL DEBUG(turn_debug_on, SHAPE(res3), &
          (ALL(SHAPE(res3) .NE. (/r1,c2/))),"size result!")
     CALL CPU_TIME(finish)
     print*, "Using the 'columns' method takes ",finish-start," seconds"
     write(3,*) finish-start, r1*c2
     
     CALL CPU_TIME(start)
     res4=MATMUL(matrix1,matrix2)
     CALL DEBUG(turn_debug_on, SHAPE(res4), &
          (ALL(SHAPE(res4) .NE. (/r1,c2/))),"size result!")
     CALL CPU_TIME(finish)
     print*, "Using the MATMUL method takes ",finish-start," seconds"
     write(4,*) finish-start, r1*c2

  ELSE
     print*,"Dimension error: matrices can not be multiplicated"
  END IF
         
     DEALLOCATE(matrix1,matrix2,res1,res2,res3,res4)
     ! deallocation of the matrices   
STOP
     
close(1)
close(2)
close(3)
close(4)
END PROGRAM EX4
   

      
      
