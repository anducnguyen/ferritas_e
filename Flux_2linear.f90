SUBROUTINE Flux_2Linear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Flux_2Linear
   ! PARENT: F_Poten2Linear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate nodal heat flux vector for a linear boundary element 
   !         for two-dimensional steady-state heat conduction problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
  
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: i,j,k
   
   ! Variables used for integration of linear straight boundary elements
   REAL(dp) :: Lx, Ly, L
   REAL(dp) :: flux(2)
   INTEGER  :: n(2)
   
   ! Global node numbers defining this element
   n(1) = Sid(eNum,1)
   n(2) = Sid(eNum,2)
   
   ! Length of this straight boundary element
   Lx = x(n(2)) - x(n(1))
   Ly = y(n(2)) - y(n(1))
   L = sqrt(Lx*Lx + Ly*Ly)
   
   flux = 0.0_dp
   
   DO i=1,2 ! Counting the nodes
      IF(bccode(eNum,i) == 1)  flux(i) = bcval(eNum,i) * L * thickness(eNum)
   END DO
   
   Flinear_p(1,1) = flux(1)/3.0_dp 
   Flinear_p(1,2) = flux(2)/6.0_dp
   Flinear_p(2,1) = flux(1)/6.0_dp
   Flinear_p(2,2) = flux(2)/3.0_dp
      
   RETURN
END SUBROUTINE Flux_2Linear
