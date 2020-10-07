SUBROUTINE SS_2RectLinear(eNum)
   USE PrecTypes
   USE GlobalData
   USE Gauss
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   SS_2RectLinear
   ! PARENT: StressStrain
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate strain, stress components, and von Mises stress for a 
   !         rectanglular linear element.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum ! element number
   INTEGER :: i,j,k,l
   REAL(dp) :: dudx, dudy      ! displacement gradients
   REAL(dp) :: dvdx, dvdy      ! displacement gradients
   REAL(dp) :: u1,v1           ! displacement components of point 1 of the quadrilateral element
   REAL(dp) :: u2,v2           ! displacement components of point 2 of the quadrilateral element
   REAL(dp) :: u3,v3           ! displacement components of point 3 of the quadrilateral element
   REAL(dp) :: u4,v4           ! displacement components of point 4 of the quadrilateral element
   REAL(dp) :: N1, N2, N3, N4  ! shape functions
   REAL(dp) :: eta, xi, eta4, xi4
   REAL(dp) :: DN1Dxi, DN1Deta, DN2Dxi, DN2Deta, DN3Dxi, DN3Deta, DN4Dxi, DN4Deta
   REAL(dp) :: DxDxi, DxDeta, DyDxi, DyDeta
   REAL(dp) :: invJ11, invJ12, invJ21, invJ22
   REAL(dp) :: DN1Dx, DN1Dy, DN2Dx, DN2Dy, DN3Dx, DN3Dy, DN4Dx, DN4Dy
   REAL(dp) :: det, Delta

   REAL(dp) :: coef, coef2
   REAL(dp) :: Yng, Pssn, a      ! Young's modulus, Poisson's ratio, linear thermal expansion coefficient of this element
   INTEGER  :: p1, p2, p3, p4    ! Node numbers defining the element
   REAL(dp) :: x1, y1, x2, y2, x3, y3, x4, y4, xp, yp, Tp
   REAL(dp) :: ex, ey, exy       ! Direct strains in x and y directions and engineering shear strain
    REAL(dp) :: sx, sy, sxy, sz  ! Direct stresses in x and y directions and shear stress
  
   Yng  = E( emtype(eNum) )      ! Young's modulus
   Pssn = v( emtype(eNum) )      ! Poisson's ratio
   IF (pcode == 3) a = alpha( emtype(eNum) )
   p1 = elm(eNum,1)
   p2 = elm(eNum,2)
   p3 = elm(eNum,3)
   p4 = elm(eNum,4)
   
   ! The coordinates of node 1 of the triangular element            
   x1 = x( p1 )
   y1 = y( p1 )
   
   ! The coordinates of node 2 of the triangular element
   x2 = x( p2 )
   y2 = y( p2 )
   
   ! The coordinates of node 3 of the triangular element
   x3 = x( p3 )
   y3 = y( p3 )
   
   ! The coordinates of node 4 of the triangular element
   x4 = x( p4 )
   y4 = y( p4 )
   
   ! Displacement components of each vertex point of the quadrilateral element
   u1 = u(p1,1)
   v1 = u(p1,2)
   u2 = u(p2,1)
   v2 = u(p2,2)
   u3 = u(p3,1)
   v3 = u(p3,2)
   u4 = u(p4,1)
   v4 = u(p4,2)
      
   DO k=1,etype(eNum)
   
      IF (k==1) THEN
         xi  = -1.0_dp
         eta = -1.0_dp
      ELSE IF (k==2) THEN
         xi  =  1.0_dp
         eta = -1.0_dp
      ELSE IF (k==3) THEN
         xi  =  1.0_dp
         eta =  1.0_dp
      ELSE IF (k==4) THEN
         xi  = -1.0_dp
         eta =  1.0_dp
      END IF
   
      xi4  = 0.25_dp * xi     ! xi/4
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
      
      dudx = DN1Dx * u1 + DN2Dx * u2 + DN3Dx * u3 + DN4Dx * u4
      dudy = DN1Dy * u1 + DN2Dy * u2 + DN3Dy * u3 + DN4Dy * u4
      dvdx = DN1Dx * v1 + DN2Dx * v2 + DN3Dx * v3 + DN4Dx * v4
      dvdy = DN1Dy * v1 + DN2Dy * v2 + DN3Dy * v3 + DN4Dy * v4

      ! strain components
      strain(eNum,k,1) = dudx
      strain(eNum,k,2) = dvdy
      strain(eNum,k,3) = dudy + dvdx

      ex  = strain(eNum,k,1)
      ey  = strain(eNum,k,2)
      exy = strain(eNum,k,3)

      ! stress components
      coef = Yng / (1.0_dp - 2.0_dp*Pssn)  !  <=  E / (1-2v)
      coef2 = coef / (1.0_dp+Pssn)         !  <=  E / ((1-2v)*(1+v))
      stress(eNum,k,1) = coef2 * ( (1.0_dp-Pssn)*ex + Pssn*ey )
      stress(eNum,k,2) = coef2 * ( Pssn*ex + (1.0_dp-Pssn)*ey )
      stress(eNum,k,3) = 0.5_dp * Yng / (1.0_dp+Pssn) * exy
      ! Ou-of-plane stress σz is also calculated for plane strain case.
      IF (plane == 1) THEN
         stress(eNum,k,4) = coef2*Pssn*(ex+ey)
      END IF

      ! -------------------------------------------------
      ! Effect of the thermal strain is also added for linear thermoelastic problems
      IF (pcode == 3) THEN
         N1 = 0.25_dp * (1.0_dp - xi) * (1.0_dp - eta)
         N2 = 0.25_dp * (1.0_dp + xi) * (1.0_dp - eta)
         N3 = 0.25_dp * (1.0_dp + xi) * (1.0_dp + eta)
         N4 = 0.25_dp * (1.0_dp - xi) * (1.0_dp + eta)
         Tp = N1 * T(elm(eNum,1)) + N2 * T(elm(eNum,2)) &
            + N3 * T(elm(eNum,3)) + N4 * T(elm(eNum,4))

         stress(eNum,k,1) = stress(eNum,k,1) - a*coef*Tp
         stress(eNum,k,2) = stress(eNum,k,2) - a*coef*Tp
         
         ! For plane strain case, the effect of the thermal strain to the out-of-plane stress is calculated.
         IF (plane == 1) THEN
            stress(eNum,k,4) = stress(eNum,k,4) - a*coef*Tp
         END IF
      END IF


      ! -------------------------------------------------
      ! Out-of-plane strain εz is also calculated for plane stress case.
      IF (plane == 2) THEN
      
         !　εz = - v/(1-2v) *（εx＋εy）
         coef = Pssn/(1.0_dp - 2.0_dp*Pssn) 
         strain(eNum,k,4) = - coef * (strain(eNum,k,1) + strain(eNum,k,2))
         
         IF (pcode == 3) THEN
            !　εz = - v/(1-2v) *（εx＋εy）+ (1+v)/(1-2v)*α*T
            strain(eNum,k,4) = strain(eNum,k,4) + (1.0_dp+Pssn)/(1-2.0_dp*Pssn)*a*Tp
         END IF 
         
      END IF
               
      ! von Mises stress
      ! Plane strain case
      IF (plane==1) THEN
         sx  = stress(eNum,k,1)
         sy  = stress(eNum,k,2)
         sxy = stress(eNum,k,3)
         sz  = stress(eNum,k,4)
         Mises(eNum,k) = sx*sx+sy*sy+sz*sz+3.0_dp*sxy*sxy-sx*sy-sx*sz-sy*sz
         Mises(eNum,k) = sqrt( Mises(eNum,k) )
      ! Plane stress case
      ELSE IF (plane==2) THEN
         sx  = stress(eNum,k,1)
         sy  = stress(eNum,k,2)
         sxy = stress(eNum,k,3)
         Mises(eNum,k) = sx*sx+sy*sy+3.0_dp*sxy*sxy-sx*sy
         Mises(eNum,k) = sqrt( Mises(eNum,k) )
      END IF
 
   END DO
   
   RETURN
END SUBROUTINE SS_2RectLinear
