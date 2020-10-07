SUBROUTINE K_Poten2RectLinear(eNum)
   USE PrecTypes
   USE GlobalData
   USE Gauss
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   K_Poten2RectLinear
   ! PARENT: Mat_Poten2RectLinear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix a rectangular linear element for 
   !         two-dimensional steady-state heat conduction problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   
   INTEGER :: i,j,k,l
   
   ! Variables used for integrations of rectilinear elements
   REAL(dp) :: Delta
   REAL(dp) :: x1, x2, x3, y1, y2, y3, x4, y4
   REAL(dp) :: xi_m1, xi_p1, eta_m1, eta_p1
   REAL(dp) :: xi, w_xi, eta, w_eta
   REAL(dp) :: xi4, eta4
   REAL(dp) :: N1, N2, N3, N4
   REAL(dp) :: DN1Dxi, DN1Deta, DN2Dxi, DN2Deta, DN3Dxi, DN3Deta, DN4Dxi, DN4Deta
   REAL(dp) :: DxDxi, DxDeta, DyDxi, DyDeta
   REAL(dp) :: invJ11, invJ12, invJ21, invJ22
   REAL(dp) :: DN1Dx, DN1Dy, DN2Dx, DN2Dy, DN3Dx, DN3Dy, DN4Dx, DN4Dy
   REAL(dp) :: det
   REAL(dp) :: Dm(2,2)                                      ! [D] matrix
   REAL(dp) :: Bm1(2),    Bm2(2),    Bm3(2),    Bm4(2)      ! [B] matrix
   REAL(dp) :: Dm_Bm1(2), Dm_Bm2(2), Dm_Bm3(2), Dm_Bm4(2)   ! [D][B]
   REAL(dp) :: Klocal(4,4), Klocal_eta(4,4)
   REAL(dp) :: kx, ky

   INTEGER  :: p1, p2, p3, p4
         
   Krect_p = 0.0_dp
   
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
   Bm4 = 0.0
   
   ! Global node numbers of the element under consideration
   p1 = elm(eNum,1)
   p2 = elm(eNum,2)
   p3 = elm(eNum,3)
   p4 = elm(eNum,4)

   ! Coordinates of node 1            
   x1 = x( p1 )
   y1 = y( p1 )
   
   ! Coordinates of node 2
   x2 = x( p2 )
   y2 = y( p2 )
   
   ! Coordinates of node 3
   x3 = x( p3 )
   y3 = y( p3 )
   
   ! Coordinates of node 4
   x4 = x( p4 )
   y4 = y( p4 )
   

   DO i=1, gausspoint
   
      xi   = zt(i)
      w_xi = wt(i)

      xi4  = 0.25_dp * xi    !  xi/4
      
      KlocaL = 0.0_dp        !  initialize the array for storing the integrated results for j-loop  
      
      DO j=1, gausspoint
      
         Klocal_eta = 0.0_dp
         
         eta   = zt(j)
         w_eta = wt(j)
         
         eta4 = 0.25_dp * eta    ! eta/4

         DN1Dxi  = -0.25_dp + eta4
         DN1Deta = -0.25_dp + xi4
         DN2Dxi  =  0.25_dp - eta4
         DN2Deta = -0.25_dp - xi4
         DN3Dxi  =  0.25_dp + eta4
         DN3Deta =  0.25_dp + xi4
         DN4Dxi  = -0.25_dp - eta4
         DN4Deta =  0.25_dp - xi4

         DxDxi  = DN1Dxi  * x1 + DN2Dxi  * x2 + DN3Dxi  * x3 + DN4Dxi  * x4
         DxDeta = DN1Deta * x1 + DN2Deta * x2 + DN3Deta * x3 + DN4Deta * x4
         DyDxi  = DN1Dxi  * y1 + DN2Dxi  * y2 + DN3Dxi  * y3 + DN4Dxi  * y4
         DyDeta = DN1Deta * y1 + DN2Deta * y2 + DN3Deta * y3 + DN4Deta * y4

         det = DxDxi * DyDeta - DyDxi * DxDeta
         Delta = ABS(det)

         invJ11 =  DyDeta
         invJ12 = -DyDxi
         invJ21 = -DxDeta
         invJ22 =  DxDxi
         DN1Dx = (invJ11 * DN1Dxi + invJ12 * DN1Deta)/det       !   ∂N1/∂x 
         DN1Dy = (invJ21 * DN1Dxi + invJ22 * DN1Deta)/det       !   ∂N1/∂y
         DN2Dx = (invJ11 * DN2Dxi + invJ12 * DN2Deta)/det       !   ∂N2/∂x
         DN2Dy = (invJ21 * DN2Dxi + invJ22 * DN2Deta)/det       !   ∂N2/∂y
         DN3Dx = (invJ11 * DN3Dxi + invJ12 * DN3Deta)/det       !   ∂N3/∂x
         DN3Dy = (invJ21 * DN3Dxi + invJ22 * DN3Deta)/det       !   ∂N3/∂y
         DN4Dx = (invJ11 * DN4Dxi + invJ12 * DN4Deta)/det       !   ∂N4/∂x
         DN4Dy = (invJ21 * DN4Dxi + invJ22 * DN4Deta)/det       !   ∂N4/∂y

         !         |  ∂N1/∂x  |
         ! [B1] =  |          |
         !         |  ∂N1/∂y  |      
         Bm1(1) = DN1Dx
         Bm1(2) = DN1Dy

         !         |  ∂N2/∂x  |
         ! [B2] =  |          |
         !         |  ∂N2/∂y  |      
         Bm2(1) = DN2Dx
         Bm2(2) = DN2Dy

         !         |  ∂N3/∂x  |
         ! [B3] =  |          |
         !         |  ∂N3/∂y  |      
         Bm3(1) = DN3Dx
         Bm3(2) = DN3Dy

         !         |  ∂N4/∂x  |
         ! [B4] =  |          |
         !         |  ∂N4/∂y  |      
         Bm4(1) = DN4Dx
         Bm4(2) = DN4Dy

         Dm_Bm1(1) = Dm(1,1) * Bm1(1) + Dm(1,2) * Bm1(2)
         Dm_Bm1(2) = Dm(2,1) * Bm1(1) + Dm(2,2) * Bm1(2)
         
         Dm_Bm2(1) = Dm(1,1) * Bm2(1) + Dm(1,2) * Bm2(2)
         Dm_Bm2(2) = Dm(2,1) * Bm2(1) + Dm(2,2) * Bm2(2)
         
         Dm_Bm3(1) = Dm(1,1) * Bm3(1) + Dm(1,2) * Bm3(2)
         Dm_Bm3(2) = Dm(2,1) * Bm3(1) + Dm(2,2) * Bm3(2)
         
         Dm_Bm4(1) = Dm(1,1) * Bm4(1) + Dm(1,2) * Bm4(2)
         Dm_Bm4(2) = Dm(2,1) * Bm4(1) + Dm(2,2) * Bm4(2)
   
         ! [K11] = [B1]^T [D] [B1]
         ! [K12] = [B1]^T [D] [B2]
         ! [K13] = [B1]^T [D] [B3]
         ! [K14] = [B1]^T [D] [B4]
         Klocal_eta(1,1) = ( Bm1(1) * Dm_Bm1(1) + Bm1(2) * Dm_Bm1(2) ) * Delta
         Klocal_eta(1,2) = ( Bm1(1) * Dm_Bm2(1) + Bm1(2) * Dm_Bm2(2) ) * Delta
         Klocal_eta(1,3) = ( Bm1(1) * Dm_Bm3(1) + Bm1(2) * Dm_Bm3(2) ) * Delta
         Klocal_eta(1,4) = ( Bm1(1) * Dm_Bm4(1) + Bm1(2) * Dm_Bm4(2) ) * Delta
         
         ! [K21] = [B2]^T [D] [B1]
         ! [K22] = [B2]^T [D] [B2]
         ! [K23] = [B2]^T [D] [B3]
         ! [K24] = [B2]^T [D] [B4]
         Klocal_eta(2,1) = ( Bm2(1) * Dm_Bm1(1) + Bm2(2) * Dm_Bm1(2) ) * Delta
         Klocal_eta(2,2) = ( Bm2(1) * Dm_Bm2(1) + Bm2(2) * Dm_Bm2(2) ) * Delta
         Klocal_eta(2,3) = ( Bm2(1) * Dm_Bm3(1) + Bm2(2) * Dm_Bm3(2) ) * Delta
         Klocal_eta(2,4) = ( Bm2(1) * Dm_Bm4(1) + Bm2(2) * Dm_Bm4(2) ) * Delta
   
         ! [K31] = [B3]^T [D] [B1]
         ! [K32] = [B3]^T [D] [B2]
         ! [K33] = [B3]^T [D] [B3]
         ! [K34] = [B3]^T [D] [B4]
         Klocal_eta(3,1) = ( Bm3(1) * Dm_Bm1(1) + Bm3(2) * Dm_Bm1(2) ) * Delta
         Klocal_eta(3,2) = ( Bm3(1) * Dm_Bm2(1) + Bm3(2) * Dm_Bm2(2) ) * Delta
         Klocal_eta(3,3) = ( Bm3(1) * Dm_Bm3(1) + Bm3(2) * Dm_Bm3(2) ) * Delta
         Klocal_eta(3,4) = ( Bm3(1) * Dm_Bm4(1) + Bm3(2) * Dm_Bm4(2) ) * Delta
   
         ! [K31] = [B3]^T [D] [B1]
         ! [K32] = [B3]^T [D] [B2]
         ! [K33] = [B3]^T [D] [B3]
         ! [K34] = [B3]^T [D] [B4]
         Klocal_eta(4,1) = ( Bm4(1) * Dm_Bm1(1) + Bm4(2) * Dm_Bm1(2) ) * Delta
         Klocal_eta(4,2) = ( Bm4(1) * Dm_Bm2(1) + Bm4(2) * Dm_Bm2(2) ) * Delta
         Klocal_eta(4,3) = ( Bm4(1) * Dm_Bm3(1) + Bm4(2) * Dm_Bm3(2) ) * Delta
         Klocal_eta(4,4) = ( Bm4(1) * Dm_Bm4(1) + Bm4(2) * Dm_Bm4(2) ) * Delta

         Klocal_eta =  Klocal_eta * w_eta
         
         Klocal = Klocal + Klocal_eta
                 
      END DO  ! end of loop for eta

      Krect_p = Krect_p + Klocal * w_xi
      
   END DO   ! end of loop for xi

   Krect_p = Krect_p * thickness(eNum)
               
   RETURN
END SUBROUTINE K_Poten2RectLinear
