!     to compile 'gfortran matrix.f90 -o matrix.x'
!     to see the output of the program  './matrix.x'

      MODULE matrix

      REAL*4, DIMENSION(:,:), ALLOCATABLE :: m1,m2,res
      REAL*4, DIMENSION(4) :: result
      INTEGER*4 ::  ii, jj, kk
      DOUBLE PRECISION :: start,finish

      CONTAINS

      FUNCTION matrix_multiplication1(m1,m2,n_rows1,cols1_rows2,n_cols2) RESULT(resu)
      
      IMPLICIT NONE
      INTEGER*4 :: n_rows1,cols1_rows2,n_cols2
      REAL*4, DIMENSION(n_rows1,cols1_rows2) :: m1
      REAL*4, DIMENSION(cols1_rows2,n_cols2) :: m2
     
      REAL*4, DIMENSION(n_rows1,n_cols2) :: resu

      resu = 0
      do ii=1,n_rows1
         do jj=1,n_cols2
            resu(ii,jj)= DOT_PRODUCT(m1(ii,:),m2(:,jj)) ! intrinsic function DOT_PRODUCT()
         enddo
      enddo
      END FUNCTION matrix_multiplication1
      
     FUNCTION matrix_multiplication2(m1,m2,n_rows1,cols1_rows2,n_cols2) RESULT(resu)

     IMPLICIT NONE
     INTEGER*4 :: n_rows1,cols1_rows2,n_cols2
     REAL*4, DIMENSION(n_rows1,cols1_rows2) :: m1
     REAL*4, DIMENSION(cols1_rows2,n_cols2) :: m2
     REAL*4, DIMENSION(n_rows1,n_cols2) :: resu
   
     resu = 0
     DO ii=1,n_rows1
         DO jj=1,n_cols2
            DO kk=1,cols1_rows2
               resu(ii,jj) = resu(ii,jj) + m1(ii,kk)*m2(kk,jj) ! direct application of the fomula
            ENDDO
         ENDDO
      ENDDO
      END FUNCTION matrix_multiplication2
      

      FUNCTION matrix_multiplication3(m1,m2,n_rows1,cols1_rows2,n_cols2) RESULT(resu)

      IMPLICIT NONE
      INTEGER*4 :: n_rows1,cols1_rows2,n_cols2
      REAL*4, DIMENSION(n_rows1,cols1_rows2) :: m1
      REAL*4, DIMENSION(cols1_rows2,n_cols2) :: m2
      REAL*4, DIMENSION(n_rows1,n_cols2) :: resu

      resu = 0
      do ii=1,n_cols2
         do jj=1,cols1_rows2
            resu(:,ii) = resu(:,ii) + m1(:,jj)*m2(jj,ii) ! multiplication of the columns of the first matrix by the elements of the second one.
         enddo
      enddo
      END FUNCTION  matrix_multiplication3
      END MODULE matrix

      PROGRAM EX3
      USE matrix
      IMPLICIT NONE
      INTEGER*4 :: r1,rc,c2

      open(1,file="time1.txt")
      open(2,file="time2.txt")
      open(3,file="time3.txt")
      open(4,file="time4.txt")

    
      do r1 = 50,700,300
         do rc = 50,700,200
            do c2 =50,700,200
               
               print*,"matrix1:",r1,"x",rc
               print*,"matrix2:",rc,"x",c2
         

!     allocating the appropriate memory and initializing the matrices
               ALLOCATE(m1(r1,rc),m2(rc,c2),res(r1,c2))
    
               call RANDOM_NUMBER(m1)
               call RANDOM_NUMBER(m2)

               ! print*,m1 CHECK THE MATRICES!!!
               ! print*,m2

              CALL CPU_TIME(start)
              res=matrix_multiplication1(m1,m2,r1,rc,c2)
              ! print*,res  CHECK THE MATRICES!!!
              CALL CPU_TIME(finish)
              print*, "Using the intrinsic function DOT_PRODUCT() takes ",finish-start," seconds"
              write(1,*) finish-start, r1*c2
             


               CALL CPU_TIME(start)
               res=matrix_multiplication2(m1,m2,r1,rc,c2)
               !print*,res
               CALL CPU_TIME(finish)
               print*, "Using the standard way takes ",finish-start," seconds"
               write(2,*) finish-start, r1*c2
         

               CALL CPU_TIME(start)
               res=matrix_multiplication3(m1,m2,r1,rc,c2)
               !print*,res
               CALL CPU_TIME(finish)
               print*, "Using the 'columns' method takes ",finish-start," seconds"
               write(3,*) finish-start, r1*c2

               CALL CPU_TIME(start)
               res=MATMUL(m1,m2)
               !print*,res
               CALL CPU_TIME(finish)
               print*, "Using the MATMUL method takes ",finish-start," seconds"
               write(4,*) finish-start, r1*c2


         
               DEALLOCATE(m1,m2,res)
            enddo
         enddo
      enddo
   
      
      STOP
     
      close(1)
      close(2)
      close(3)
      close(4)
      END PROGRAM EX3


      
      
