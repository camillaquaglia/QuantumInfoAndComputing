! to compile 'gfortran Ex2-Quaglia-CODE.f90 -o  Ex2-Quaglia-CODE.x'
! to run the executable './Ex2-Quaglia-CODE.x'

MODULE matrix
  IMPLICIT NONE

  TYPE cmatrix
     INTEGER*4, DIMENSION(2) ::  RC    ! RC is a vector that contains two integers, The dimensions of the matrix
     DOUBLE COMPLEX, DIMENSION(:,:), ALLOCATABLE :: m   ! m is a matrix that will be filled with complex numbers
     DOUBLE COMPLEX :: trace, det
    
  END type cmatrix

  INTERFACE OPERATOR (.INIT.)
     MODULE PROCEDURE f_init
  END INTERFACE OPERATOR (.INIT.)
  
  INTERFACE OPERATOR (.ADJ.)
     MODULE PROCEDURE  adj_cmatrix 
  END INTERFACE OPERATOR (.ADJ.)

  INTERFACE OPERATOR (.TRACE.)
     MODULE PROCEDURE trace_cmatrix 
  END INTERFACE OPERATOR (.TRACE.)


  
CONTAINS

  
  FUNCTION f_init(m_size)  ! function that initializes the type cmatrix
    INTEGER*4, DIMENSION(2), INTENT(IN) :: m_size
    TYPE(cmatrix) :: f_init
    if ((m_size(1) .ge. 1) .and. (m_size(2) .ge. 1)) then
       ALLOCATE(f_init%m(m_size(1),m_size(2)))
       f_init%RC = m_size
       f_init%m = 0.d0
       f_init%trace = 0.d0
       f_init%det = 0.d0
    else
       print*,'The dimensions of the matrix are non positive!!!'
       ALLOCATE(f_init%m(0,0))
       f_init%RC = (/ 0, 0 /)
       f_init%m = 0.d0
       f_init%trace = 0.d0
       f_init%det = 0.d0
    endif
    RETURN
  END FUNCTION f_init

  
  FUNCTION trace_cmatrix(c_mat)
    TYPE(cmatrix), INTENT(IN) :: c_mat
    DOUBLE COMPLEX :: trace_cmatrix
    INTEGER*4 :: ii
    if (c_mat%RC(1) .eq. c_mat%RC(2)) then
       trace_cmatrix = 0.d0
       do ii=1, c_mat%RC(1)
          trace_cmatrix = trace_cmatrix + c_mat%m(ii,ii)
       enddo
    else
       print*,  "Not a square matrix! Trace set to 0."
       trace_cmatrix = 0.d0
    endif
    RETURN
  END FUNCTION trace_cmatrix

  FUNCTION adj_cmatrix(c_mat)
    TYPE(cmatrix), INTENT(IN) :: c_mat
    TYPE(cmatrix) :: adj_cmatrix
    if (c_mat%RC(1) .ne. c_mat%RC(1)) print*, "WARNING: the matrix is not square."
    adj_cmatrix%trace = conjg(c_mat%trace)
    adj_cmatrix%det = conjg(c_mat%det)
    adj_cmatrix%RC = c_mat%RC(2:1:-1)
    adj_cmatrix%m = transpose(conjg(c_mat%m))
    RETURN
  END FUNCTION adj_cmatrix


  SUBROUTINE writetxt(ftxt, c_mat)
    CHARACTER*8 :: ftxt
    TYPE(cmatrix) :: c_mat
    INTEGER*4 :: ii
    open(2, file=ftxt, status="REPLACE", action="WRITE")
    write(2, *) "Dimensions: ", c_mat%RC
    write(2, *) ""
    write(2, *) "Trace: ", c_mat%trace
    write(2, *) ""
   ! write(12, *) "Determinant: ", c_mat%det
    write(2, *) ""
    write(2, *) "Matrix elements: "
    do ii=1, c_mat%RC(1)
       write(2, *) c_mat%m(ii,:)
    enddo
    RETURN
  END SUBROUTINE writetxt

  
END MODULE matrix



PROGRAM EX2
  USE matrix
  IMPLICIT NONE

  TYPE(cmatrix) :: A, Aadj
  INTEGER*4 :: ii, nrows, ncols
  INTEGER*4, DIMENSION(2) :: dims
  REAL*8, DIMENSION(:,:), ALLOCATABLE :: re,im


  ! setting the size of the matrix
  nrows=0
  ncols=0
  do while (nrows .le. 0)
     print*, "Insert the number of rows: "
     read*, nrows
     if (nrows .le. 0) print*, "non positive number of rows!!"
  enddo
  do while (ncols .le. 0)
     print*, "Insert the number of columns: "
     read*, ncols
     if (ncols .le. 0) print*,"non positive number of columns!!"
  enddo
  dims = (/ nrows, ncols /)
  ALLOCATE(re(nrows,ncols),im(nrows,ncols))
  call RANDOM_NUMBER(re) 
  call RANDOM_NUMBER(im)

  A=.INIT.(dims)
  A%m=cmplx(re,im)
  A%trace=.TRACE.(A)
  ! setting Aadj as the adjoint of A
  Aadj=.ADJ.(A)
  ! writing both to a txt file
  call writetxt("Amat.txt", A)
  call writetxt("Aadj.txt", Aadj)
  DEALLOCATE(A%m, Aadj%m)
  STOP
END PROGRAM EX2

