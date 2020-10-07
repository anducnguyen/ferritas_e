SUBROUTINE K_Elast2TrigLinear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   K_Elast2TrigLinear
   ! PARENT: Mat_Elast2TrigLinear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix a triangular linear element for 
   !         two-dimensional elastostatic and thermo-elastostatic problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   
   INTEGER :: i,j,k,l
   
   ! Variables used for integrations of triangular linear elements
   REAL(dp) :: a1, a2, a3, b1, b2, b3, c1, c2, c3, Delta, Delta2, Delta4
   REAL(dp) :: coef_b1, coef_c1, coef_b2, coef_c2, coef_b3, coef_c3
   REAL(dp) :: a1_b1xg_c1yg, a2_b2xg_c2yg, a3_b3xg_c3yg
   REAL(dp) :: x1, x2, x3, y1, y2, y3, xg, yg
   REAL(dp) :: Dm(3,3)
   REAL(dp) :: Bm1(3,2),      Bm2(3,2),      Bm3(3,2)
   REAL(dp) :: Bm1_T(2,3),    Bm2_T(2,3),    Bm3_T(2,3)
   REAL(dp) :: Bm1_T_Dm(2,3), Bm2_T_Dm(2,3), Bm3_T_Dm(2,3)
   
   REAL(dp) :: Yng, Pssn, coef
   INTEGER  :: n1, n2, n3
         
   Ktrig = 0.0
   
   Yng  = E( emtype(eNum) )  ! Young's modulus
   Pssn = v( emtype(eNum) )  ! Poisson's ration
   !       E
   !   ------------
   !   (1+v)(1-2v)
   coef = Yng / (( 1.0 + Pssn )*(1.0 - 2.0 * Pssn))
   
   ! [D] matrix
   !
   !            E      |  1-v     v      0      |
   ! [D] = ----------- |   v     1-v     0      |
   !       (1+v)(1-2v) |   0      0   (1-2v)/2  |
   Dm(1,1) = 1.0 - Pssn
   Dm(1,2) = Pssn
   Dm(1,3) = 0.0
   Dm(2,1) = Pssn
   Dm(2,2) = 1.0 - Pssn
   Dm(2,3) = 0.0
   Dm(3,1) = 0.0
   Dm(3,2) = 0.0
   Dm(3,3) = 0.5 - Pssn
   Dm = coef * Dm

   
   Bm1 = 0.0
   Bm2 = 0.0
   Bm3 = 0.0
   
   ! Global node numbers of the element under consideration
   n1 = elm(eNum,1)
   n2 = elm(eNum,2)
   n3 = elm(eNum,3)
   
   ! Coordinates of node 1            
   x1 = x( n1 )
   y1 = y( n1 )
   
   ! Coordinates of node 2
   x2 = x( n2 )
   y2 = y( n2 )
   
   ! Coordinates of node 3
   x3 = x( n3 )
   y3 = y( n3 )
   
   a1 = x2*y3 - x3*y2;   b1 = y2 - y3;   c1 = x3 - x2
   a2 = x3*y1 - x1*y3;   b2 = y3 - y1;   c2 = x1 - x3
   a3 = x1*y2 - x2*y1;   b3 = y1 - y2;   c3 = x2 - x1
   
   Delta2 = a1 + a2 + a3
   Delta  = Delta2 * 0.5_dp
   
   !         |  ∂N1/∂x    0     |
   ! [B1] =  |   0      ∂N1/∂y  |
   !         |  ∂N1/∂y  ∂N1/∂x  |      
   Bm1(1,1) = b1
   Bm1(2,2) = c1
   Bm1(3,1) = c1
   Bm1(3,2) = b1
   
   !         |  ∂N2/∂x    0     |
   ! [B2] =  |   0      ∂N2/∂y  |
   !         |  ∂N2/∂y  ∂N2/∂x  |      
   Bm2(1,1) = b2
   Bm2(2,2) = c2
   Bm2(3,1) = c2
   Bm2(3,2) = b2
   
   !         |  ∂N3/∂x    0     |
   ! [B3] =  |   0      ∂N3/∂y  |
   !         |  ∂N3/∂y  ∂N3/∂x  |      
   Bm3(1,1) = b3
   Bm3(2,2) = c3
   Bm3(3,1) = c3
   Bm3(3,2) = b3
   
   Bm1 = Bm1/Delta2
   Bm2 = Bm2/Delta2
   Bm3 = Bm3/Delta2
   
   ! transpositions of [B1], [B2], [B3]
   Bm1_T = TRANSPOSE(Bm1)
   Bm2_T = TRANSPOSE(Bm2)
   Bm3_T = TRANSPOSE(Bm3)
   
   Bm1_T_Dm = MATMUL(Bm1_T, Dm)
   Bm2_T_Dm = MATMUL(Bm2_T, Dm)
   Bm3_T_Dm = MATMUL(Bm3_T, Dm)
   
   ! [K11] = [B1]^T [D] [B1]
   ! [K12] = [B1]^T [D] [B2]
   ! [K13] = [B1]^T [D] [B3]
   Ktrig(1,1,:,:) = MATMUL( Bm1_T_Dm, Bm1 ) * Delta
   Ktrig(1,2,:,:) = MATMUL( Bm1_T_Dm, Bm2 ) * Delta
   Ktrig(1,3,:,:) = MATMUL( Bm1_T_Dm, Bm3 ) * Delta
   
   ! [K21] = [B2]^T [D] [B1]
   ! [K22] = [B2]^T [D] [B2]
   ! [K23] = [B2]^T [D] [B3]
   Ktrig(2,1,:,:) = MATMUL( Bm2_T_Dm, Bm1 ) * Delta
   Ktrig(2,2,:,:) = MATMUL( Bm2_T_Dm, Bm2 ) * Delta
   Ktrig(2,3,:,:) = MATMUL( Bm2_T_Dm, Bm3 ) * Delta
   
   ! [K31] = [B3]^T [D] [B1]
   ! [K32] = [B3]^T [D] [B2]
   ! [K33] = [B3]^T [D] [B3]
   Ktrig(3,1,:,:) = MATMUL( Bm3_T_Dm, Bm1 ) * Delta
   Ktrig(3,2,:,:) = MATMUL( Bm3_T_Dm, Bm2 ) * Delta
   Ktrig(3,3,:,:) = MATMUL( Bm3_T_Dm, Bm3 ) * Delta
   
   Ktrig = Ktrig * thickness(eNum)
   
   IF (pcode == 3) THEN
      xg = (x1 + x2 + x3) / 3.0_dp
      yg = (y1 + y2 + y3) / 3.0_dp
      coef = alpha( emtype(eNum) )*Yng / (1 - 2*Pssn)
      Delta4 = Delta2 + Delta2
      coef = coef / Delta4
      coef_b1 = coef * b1
      coef_c1 = coef * c1
      coef_b2 = coef * b2
      coef_c2 = coef * c2
      coef_b3 = coef * b3
      coef_c3 = coef * c3
      a1_b1xg_c1yg = a1 + b1*xg + c1*yg
      a2_b2xg_c2yg = a2 + b2*xg + c2*yg
      a3_b3xg_c3yg = a3 + b3*xg + c3*yg
      
      ! stored as Thtrig(node, direction, node)
      Thtrig(1,1,1) = coef_b1 * a1_b1xg_c1yg
      Thtrig(1,1,2) = coef_b1 * a2_b2xg_c2yg
      Thtrig(1,1,3) = coef_b1 * a3_b3xg_c3yg
      Thtrig(1,2,1) = coef_c1 * a1_b1xg_c1yg
      Thtrig(1,2,2) = coef_c1 * a2_b2xg_c2yg
      Thtrig(1,2,3) = coef_c1 * a3_b3xg_c3yg
      Thtrig(2,1,1) = coef_b2 * a1_b1xg_c1yg
      Thtrig(2,1,2) = coef_b2 * a2_b2xg_c2yg
      Thtrig(2,1,3) = coef_b2 * a3_b3xg_c3yg
      Thtrig(2,2,1) = coef_c2 * a1_b1xg_c1yg
      Thtrig(2,2,2) = coef_c2 * a2_b2xg_c2yg
      Thtrig(2,2,3) = coef_c2 * a3_b3xg_c3yg
      Thtrig(3,1,1) = coef_b3 * a1_b1xg_c1yg
      Thtrig(3,1,2) = coef_b3 * a2_b2xg_c2yg
      Thtrig(3,1,3) = coef_b3 * a3_b3xg_c3yg
      Thtrig(3,2,1) = coef_c3 * a1_b1xg_c1yg
      Thtrig(3,2,2) = coef_c3 * a2_b2xg_c2yg
      Thtrig(3,2,3) = coef_c3 * a3_b3xg_c3yg
   END IF
            
   RETURN
END SUBROUTINE K_Elast2TrigLinear
