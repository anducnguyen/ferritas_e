SUBROUTINE Force_2Linear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Force_2Linear
   ! PARENT: F_Elast2Linear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate nodal force vector for a linear boundary element 
   !         for two-dimensional elastostatic and thermo-elastostatic problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: i,j,k
   
   ! Variables used for integration of linear straight boundary elements
   REAL(dp) :: Lx, Ly, L
   REAL(dp) :: trac(2,2)
   INTEGER  :: n(2)
   
   ! Global node numbers defining this element
   n(1) = Sid(eNum,1)
   n(2) = Sid(eNum,2)
   
   ! Length of this straight boundary element
   Lx = x(n(2)) - x(n(1))
   Ly = y(n(2)) - y(n(1))
   L = sqrt(Lx*Lx + Ly*Ly)
   
   trac = 0.0_dp
   
   DO i=1,2 ! Counting the nodes
      DO j=1,2 ! Counting the direction
         IF(bcc(eNum,i,j) == 1)  trac(i,j) = bcv(eNum,i,j) * L * thickness(eNum)
      END DO
   END DO
   
   DO j=1,2 ! Counting the direction
      Flinear(1,1,j) = trac(1,j)/3.0_dp 
      Flinear(1,2,j) = trac(2,j)/6.0_dp
      Flinear(2,1,j) = trac(1,j)/6.0_dp
      Flinear(2,2,j) = trac(2,j)/3.0_dp
   END DO
               
   RETURN
END SUBROUTINE Force_2Linear
