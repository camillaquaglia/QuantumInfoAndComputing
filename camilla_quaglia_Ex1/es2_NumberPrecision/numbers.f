!     to compile 'gfortran numbers.f -o numbers.x -fno-range-check'
!     to see the output of the program  './numbers.x'

      MODULE numbers
      
      INTEGER*2 n1_2,n2_2
      INTEGER*4 n1_4,n2_4
      REAL*4 n3a_4,n3b_4,n4a_4,n4b_4
      REAL*8 n3a_8,n3b_8,n4a_8,n4b_8
      
      END MODULE numbers

      

      PROGRAM test
      
      USE numbers               ! use the module 'numbers' defined before
      IMPLICIT NONE
      n1_2 = 2000000
      n1_4 = 2000000
      n2_2 = 1
      n2_4 = 1
      n3a_4 = ACOS(-1.)         ! Fortran does not have a built-in constant for PI
      n3a_8 = ACOS(-1.D0)
      n3b_4 = 10e32
      n3b_8 = 10e32

      n4a_4 = SQRT(2.)
      n4a_8 = SQRT(2.D0)
      n4b_4 = 10e21
      n4b_8 = 10e21
      
      
      print*,"With INTEGER*2, the sum 2000000 + 1 =", n1_2 + n2_2
      print*,"With INTEGER*4, the sum 2000000 + 1 =", n1_4 + n2_4
      print*,"With REAL*4, the sum PI*10^32 + sqrt(2)*10^21",n3a_4*n3b_4 + n4a_4*n4b_4
      print*,"With REAL*8, the sum PI*10^32 + sqrt(2)*10^21",n3a_8*n3b_8 + n4a_8*n4b_8
      
      STOP
      END PROGRAM test
