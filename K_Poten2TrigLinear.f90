SUBROUTINE K_Poten2TrigLinear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   K_Poten2TrigLinear
   ! PARENT: Mat_Poten2TrigLinear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix a triangular linear element for 
   !         two-dimensional steady-state heat conduction problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
  
   INTEGER, INTENT(IN) :: eNum
   
   INTEGER :: i,j,k,l
   
   ! Variables used for integrations of triangular linear elements
   REAL(dp) :: a1, a2, a3, b1, b2, b3, c1, c2, c3, Delta, Delta2
   REAL(dp) :: coef_b1, coef_c1, coef_b2, coef_c2, coef_b3, coef_c3
   REAL(dp) :: a1_b1xg_c1yg, a2_b2xg_c2yg, a3_b3xg_c3yg
   REAL(dp) :: x1, x2, x3, y1, y2, y3
   REAL(dp) :: Dm(2,2)                           ! [D] matrix
   REAL(dp) :: Bm1(2),    Bm2(2),    Bm3(2)      ! [B] matrix
   REAL(dp) :: Dm_Bm1(2), Dm_Bm2(2), Dm_Bm3(2)   ! [D][B]
   REAL(dp) :: kx, ky
   INTEGER  :: p1, p2, p3
         
   Ktrig_p = 0.0
   
   kx  = lambda( emtype(eNum), 1 )  ! thermal conductivity in x-direction
   ky  = lambda( emtype(eNum), 2 )  ! thermal conductivity in y-direction
   
   ! [D] matrix
   !
   !       |  -kx     0   |
   ! [D] = |              |
   !       |   0     -ky  |
   Dm(1,1) = -kx
   Dm(1,2) = 0.0_dp
   Dm(2,1) = 0.0_dp
   Dm(2,2) = -ky
   
   Bm1 = 0.0
   Bm2 = 0.0
   Bm3 = 0.0
   
   ! Global node numbers of the element under consideration
   p1 = elm(eNum,1)
   p2 = elm(eNum,2)
   p3 = elm(eNum,3)
   
   ! Coordinates of node 1          
   x1 = x( p1 )
   y1 = y( p1 )
   
   ! Coordinates of node 2
   x2 = x( p2 )
   y2 = y( p2 )
   
   ! Coordinates of node 3
   x3 = x( p3 )
   y3 = y( p3 )
   
   a1 = x2*y3 - x3*y2;   b1 = y2 - y3;   c1 = x3 - x2
   a2 = x3*y1 - x1*y3;   b2 = y3 - y1;   c2 = x1 - x3
   a3 = x1*y2 - x2*y1;   b3 = y1 - y2;   c3 = x2 - x1
   
   Delta2 = a1 + a2 + a3
   Delta  = Delta2 * 0.5_dp
   
   !         |  ∂N1/∂x  |
   ! [B1] =  |          |
   !         |  ∂N1/∂y  |      
   Bm1(1) = b1
   Bm1(2) = c1
   
   !         |  ∂N2/∂x  |
   ! [B2] =  |          |
   !         |  ∂N2/∂y  |      
   Bm2(1) = b2
   Bm2(2) = c2
   
   !         |  ∂N3/∂x  |
   ! [B3] =  |          |
   !         |  ∂N3/∂y  |      
   Bm3(1) = b3
   Bm3(2) = c3
   
   Bm1 = Bm1/Delta2
   Bm2 = Bm2/Delta2
   Bm3 = Bm3/Delta2
      
   Dm_Bm1(1) = Dm(1,1) * Bm1(1) + Dm(1,2) * Bm1(2)
   Dm_Bm1(2) = Dm(2,1) * Bm1(1) + Dm(2,2) * Bm1(2)
   
   Dm_Bm2(1) = Dm(1,1) * Bm2(1) + Dm(1,2) * Bm2(2)
   Dm_Bm2(2) = Dm(2,1) * Bm2(1) + Dm(2,2) * Bm2(2)
   
   Dm_Bm3(1) = Dm(1,1) * Bm3(1) + Dm(1,2) * Bm3(2)
   Dm_Bm3(2) = Dm(2,1) * Bm3(1) + Dm(2,2) * Bm3(2)
   
   ! [K11] = [B1]^T [D] [B1]
   ! [K12] = [B1]^T [D] [B2]
   ! [K13] = [B1]^T [D] [B3]
   Ktrig_p(1,1) = ( Bm1(1) * Dm_Bm1(1) + Bm1(2) * Dm_Bm1(2) ) * Delta
   Ktrig_p(1,2) = ( Bm1(1) * Dm_Bm2(1) + Bm1(2) * Dm_Bm2(2) ) * Delta
   Ktrig_p(1,3) = ( Bm1(1) * Dm_Bm3(1) + Bm1(2) * Dm_Bm3(2) ) * Delta
   
   ! [K21] = [B2]^T [D] [B1]
   ! [K22] = [B2]^T [D] [B2]
   ! [K23] = [B2]^T [D] [B3]
   Ktrig_p(2,1) = ( Bm2(1) * Dm_Bm1(1) + Bm2(2) * Dm_Bm1(2) ) * Delta
   Ktrig_p(2,2) = ( Bm2(1) * Dm_Bm2(1) + Bm2(2) * Dm_Bm2(2) ) * Delta
   Ktrig_p(2,3) = ( Bm2(1) * Dm_Bm3(1) + Bm2(2) * Dm_Bm3(2) ) * Delta
   
   ! [K31] = [B3]^T [D] [B1]
   ! [K32] = [B3]^T [D] [B2]
   ! [K33] = [B3]^T [D] [B3]
   Ktrig_p(3,1) = ( Bm3(1) * Dm_Bm1(1) + Bm3(2) * Dm_Bm1(2) ) * Delta
   Ktrig_p(3,2) = ( Bm3(1) * Dm_Bm2(1) + Bm3(2) * Dm_Bm2(2) ) * Delta
   Ktrig_p(3,3) = ( Bm3(1) * Dm_Bm3(1) + Bm3(2) * Dm_Bm3(2) ) * Delta
   
   Ktrig_p = Ktrig_p * thickness(eNum)
   
   RETURN
END SUBROUTINE K_Poten2TrigLinear
