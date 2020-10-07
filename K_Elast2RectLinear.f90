SUBROUTINE K_Elast2RectLinear(eNum)
   USE PrecTypes
   USE GlobalData
   USE Gauss
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   K_Elast2RectLinear
   ! PARENT: Mat_Elast2RectLinear
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix a rectangular linear element for 
   !         two-dimensional elastostatic and thermo-elastostatic problems.
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
   REAL(dp) :: Dm(3,3)
   REAL(dp) :: Bm1(3,2),      Bm2(3,2),      Bm3(3,2),      Bm4(3,2)
   REAL(dp) :: Bm1_T(2,3),    Bm2_T(2,3),    Bm3_T(2,3),    Bm4_T(2,3)
   REAL(dp) :: Bm1_T_Dm(2,3), Bm2_T_Dm(2,3), Bm3_T_Dm(2,3), Bm4_T_Dm(2,3)
   REAL(dp) :: Klocal(4,4,2,2), Klocal_eta(4,4,2,2)
   real(dp) :: Thlocal(4,2,4), Thlocal_eta(4,2,4)
   
   REAL(dp) :: Yng, Pssn, coef

   INTEGER  :: p1, p2, p3, p4
         
   Krect = 0.0_dp
   if (pcode == 3) Threct = 0.0_dp
   
   Yng  = E( emtype(eNum) )  ! Young's modulus
   Pssn = v( emtype(eNum) )  ! Poisson's ratio
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

      xi4  = 0.25_dp * xi     !  xi/4
      
      KlocaL = 0.0_dp         ! initialize the array for storing the integrated results for j-loop
      
      IF (pcode == 3) THEN               ! for a thermoelastic problem
         Thlocal = 0.0_dp                ! initialize the array for storing the integrated 
                                         ! results for j-loop
         xi_m1 = (1.0_dp - xi) * 0.5_dp  !  (1 - xi)/2
         xi_p1 = (1.0_dp + xi) * 0.5_dp  !  (1 + xi)/2
      END IF

      DO j=1, gausspoint
      
         Klocal_eta = 0.0_dp
         
         eta   = zt(j)
         w_eta = wt(j)
         if (pcode==3) then
            Thlocal_eta = 0.0_dp
            eta_m1 = (1.0_dp - eta) * 0.5_dp    !  (1 - eta)/2
            eta_p1 = (1.0_dp + eta) * 0.5_dp    !  (1 + eta)/2
         end if
         
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

         !         |  ∂N1/∂x    0     |
         ! [B1] =  |   0      ∂N1/∂y  |
         !         |  ∂N1/∂y  ∂N1/∂x  |      
         Bm1(1,1) = DN1Dx
         Bm1(2,2) = DN1Dy
         Bm1(3,1) = DN1Dy
         Bm1(3,2) = DN1Dx

         !         |  ∂N2/∂x    0     |
         ! [B2] =  |   0      ∂N2/∂y  |
         !         |  ∂N2/∂y  ∂N2/∂x  |      
         Bm2(1,1) = DN2Dx
         Bm2(2,2) = DN2Dy
         Bm2(3,1) = DN2Dy
         Bm2(3,2) = DN2Dx

         !         |  ∂N3/∂x    0     |
         ! [B3] =  |   0      ∂N3/∂y  |
         !         |  ∂N3/∂y  ∂N3/∂x  |      
         Bm3(1,1) = DN3Dx
         Bm3(2,2) = DN3Dy
         Bm3(3,1) = DN3Dy
         Bm3(3,2) = DN3Dx

         !         |  ∂N4/∂x    0     |
         ! [B4] =  |   0      ∂N4/∂y  |
         !         |  ∂N4/∂y  ∂N4/∂x  |      
         Bm4(1,1) = DN4Dx
         Bm4(2,2) = DN4Dy
         Bm4(3,1) = DN4Dy
         Bm4(3,2) = DN4Dx

         !   Bm1 = Bm1/Delta2
         !   Bm2 = Bm2/Delta2
         !   Bm3 = Bm3/Delta2
         !   Bm4 = Bm4/Delta2

         ! [B1], [B2], [B3], [B4] の転置
         Bm1_T = TRANSPOSE(Bm1)
         Bm2_T = TRANSPOSE(Bm2)
         Bm3_T = TRANSPOSE(Bm3)
         Bm4_T = TRANSPOSE(Bm4)

         Bm1_T_Dm = MATMUL(Bm1_T, Dm)
         Bm2_T_Dm = MATMUL(Bm2_T, Dm)
         Bm3_T_Dm = MATMUL(Bm3_T, Dm)
         Bm4_T_Dm = MATMUL(Bm4_T, Dm)

         ! [K11] = [B1]^T [D] [B1]
         ! [K12] = [B1]^T [D] [B2]
         ! [K13] = [B1]^T [D] [B3]
         ! [K14] = [B1]^T [D] [B3]
         Klocal_eta(1,1,:,:) = MATMUL( Bm1_T_Dm, Bm1 ) * Delta
         Klocal_eta(1,2,:,:) = MATMUL( Bm1_T_Dm, Bm2 ) * Delta
         Klocal_eta(1,3,:,:) = MATMUL( Bm1_T_Dm, Bm3 ) * Delta
         Klocal_eta(1,4,:,:) = MATMUL( Bm1_T_Dm, Bm4 ) * Delta

         ! [K21] = [B2]^T [D] [B1]
         ! [K22] = [B2]^T [D] [B2]
         ! [K23] = [B2]^T [D] [B3]
         ! [K24] = [B2]^T [D] [B4]
         Klocal_eta(2,1,:,:) = MATMUL( Bm2_T_Dm, Bm1 ) * Delta
         Klocal_eta(2,2,:,:) = MATMUL( Bm2_T_Dm, Bm2 ) * Delta
         Klocal_eta(2,3,:,:) = MATMUL( Bm2_T_Dm, Bm3 ) * Delta
         Klocal_eta(2,4,:,:) = MATMUL( Bm2_T_Dm, Bm4 ) * Delta

         ! [K31] = [B3]^T [D] [B1]
         ! [K32] = [B3]^T [D] [B2]
         ! [K33] = [B3]^T [D] [B3]
         ! [K34] = [B3]^T [D] [B4]
         Klocal_eta(3,1,:,:) = MATMUL( Bm3_T_Dm, Bm1 ) * Delta
         Klocal_eta(3,2,:,:) = MATMUL( Bm3_T_Dm, Bm2 ) * Delta
         Klocal_eta(3,3,:,:) = MATMUL( Bm3_T_Dm, Bm3 ) * Delta
         Klocal_eta(3,4,:,:) = MATMUL( Bm3_T_Dm, Bm4 ) * Delta

         ! [K41] = [B4]^T [D] [B1]
         ! [K42] = [B4]^T [D] [B2]
         ! [K43] = [B4]^T [D] [B3]
         ! [K44] = [B4]^T [D] [B4]
         Klocal_eta(4,1,:,:) = MATMUL( Bm4_T_Dm, Bm1 ) * Delta
         Klocal_eta(4,2,:,:) = MATMUL( Bm4_T_Dm, Bm2 ) * Delta
         Klocal_eta(4,3,:,:) = MATMUL( Bm4_T_Dm, Bm3 ) * Delta
         Klocal_eta(4,4,:,:) = MATMUL( Bm4_T_Dm, Bm4 ) * Delta

      !   Klocal = Klocal * w_eta
         Klocal_eta =  Klocal_eta * w_eta
         
         Klocal = Klocal + Klocal_eta
         
         IF (pcode == 3) THEN
            N1 = xi_m1 * eta_m1   ! (1 - xi)/2 * (1 - eta)/2
            N2 = xi_p1 * eta_m1   ! (1 + xi)/2 * (1 - eta)/2
            N3 = xi_p1 * eta_p1   ! (1 + xi)/2 * (1 + eta)/2
            N4 = xi_m1 * eta_p1   ! (1 - xi)/2 * (1 + eta)/2
            coef = alpha( emtype(eNum) )*Yng / (1 - 2*Pssn)   ! a * E/(1-2v)
            !------
            Thlocal_eta(1,1,1) = coef * DN1Dx * N1
            Thlocal_eta(1,1,2) = coef * DN1Dx * N2
            Thlocal_eta(1,1,3) = coef * DN1Dx * N3
            Thlocal_eta(1,1,4) = coef * DN1Dx * N4
            Thlocal_eta(1,2,1) = coef * DN1Dy * N1
            Thlocal_eta(1,2,2) = coef * DN1Dy * N2
            Thlocal_eta(1,2,3) = coef * DN1Dy * N3
            Thlocal_eta(1,2,4) = coef * DN1Dy * N4
            !------
            Thlocal_eta(2,1,1) = coef * DN2Dx * N1
            Thlocal_eta(2,1,2) = coef * DN2Dx * N2
            Thlocal_eta(2,1,3) = coef * DN2Dx * N3
            Thlocal_eta(2,1,4) = coef * DN2Dx * N4
            Thlocal_eta(2,2,1) = coef * DN2Dy * N1
            Thlocal_eta(2,2,2) = coef * DN2Dy * N2
            Thlocal_eta(2,2,3) = coef * DN2Dy * N3
            Thlocal_eta(2,2,4) = coef * DN2Dy * N4
            !------
            Thlocal_eta(3,1,1) = coef * DN3Dx * N1
            Thlocal_eta(3,1,2) = coef * DN3Dx * N2
            Thlocal_eta(3,1,3) = coef * DN3Dx * N3
            Thlocal_eta(3,1,4) = coef * DN3Dx * N4
            Thlocal_eta(3,2,1) = coef * DN3Dy * N1
            Thlocal_eta(3,2,2) = coef * DN3Dy * N2
            Thlocal_eta(3,2,3) = coef * DN3Dy * N3
            Thlocal_eta(3,2,4) = coef * DN3Dy * N4
            !------
            Thlocal_eta(4,1,1) = coef * DN4Dx * N1
            Thlocal_eta(4,1,2) = coef * DN4Dx * N2
            Thlocal_eta(4,1,3) = coef * DN4Dx * N3
            Thlocal_eta(4,1,4) = coef * DN4Dx * N4
            Thlocal_eta(4,2,1) = coef * DN4Dy * N1
            Thlocal_eta(4,2,2) = coef * DN4Dy * N2
            Thlocal_eta(4,2,3) = coef * DN4Dy * N3
            Thlocal_eta(4,2,4) = coef * DN4Dy * N4
            !------
            Thlocal = Thlocal + Thlocal_eta * w_eta * Delta
         END IF
         
      END DO  ! end of loop for eta

      Krect = Krect + Klocal * w_xi
      IF (pcode == 3) Threct = Threct + Thlocal * w_xi
      
   END DO   ! end of loop for xi

   Krect = Krect * thickness(eNum)
   IF (pcode == 3)  Threct = Threct * thickness(eNum)
               
   RETURN
END SUBROUTINE K_Elast2RectLinear
