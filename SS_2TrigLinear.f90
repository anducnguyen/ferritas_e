SUBROUTINE SS_2TrigLinear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   SS_2TrigLinear
   ! PARENT: StressStrain
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate strain, stress components, and von Mises stress for a 
   !         trianglular linear element.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum         ! element number
   REAL(dp) :: dudx, dudy              ! displacement gradients
   REAL(dp) :: dvdx, dvdy              ! displacement gradients
   REAL(dp) :: u1,v1                   ! displacement components of point 1 of the triangle
   REAL(dp) :: u2,v2                   ! displacement components of point 2 of the triangle
   REAL(dp) :: u3,v3                   ! displacement components of point 3 of the triangle
   REAL(dp) :: a1, a2, a3
   REAL(dp) :: b1, b2, b3
   REAL(dp) :: c1, c2, c3
   REAL(dp) :: Delta, Delta2
   REAL(dp) :: coef, coef2
   REAL(dp) :: Yng, Pssn, a            ! Young's modulus, Poisson's ratio, linear thermal expansion coefficient of this element
   INTEGER  :: n1, n2, n3              ! Node numbers defining the element
   REAL(dp) :: x1, y1, x2, y2, x3, y3
   REAL(dp) :: xg, yg, Tg              ! The coordinates and the temperature of the center of geometry of the element
   REAL(dp) :: ex, ey, exy             ! Direct strains in x and y directions and engineering shear strain
   REAL(dp) :: sx, sy, sxy, sz         ! Direct stresses in x and y directions and shear stress
   
   Yng  = E( emtype(eNum) )            ! Young's modulus
   Pssn = v( emtype(eNum) )            ! Poisson's ratio
   
   IF (pcode == 3) a = alpha( emtype(eNum) )    ! Linear thermal expansion coefficient
   
   n1 = elm(eNum,1)
   n2 = elm(eNum,2)
   n3 = elm(eNum,3)
   
   ! The coordinates of node 1 of the triangular element
   x1 = x( n1 )
   y1 = y( n1 )
   
   ! The coordinates of node 2 of the triangular element
   x2 = x( n2 )
   y2 = y( n2 )
   
   ! The coordinates of node 3 of the triangular element
   x3 = x( n3 )
   y3 = y( n3 )

   a1 = x2*y3 - x3*y2;   b1 = y2 - y3;   c1 = x3 - x2
   a2 = x3*y1 - x1*y3;   b2 = y3 - y1;   c2 = x1 - x3
   a3 = x1*y2 - x2*y1;   b3 = y1 - y2;   c3 = x2 - x1
   
   Delta2 = a1 + a2 + a3     ! Twice of the area of the triangular element
   Delta  = Delta2 * 0.5_dp  ! Area of the triangular element
   
   ! Displacement components of each vertex point of the triangle
   u1 = u(n1,1)
   v1 = u(n1,2)
   u2 = u(n2,1)
   v2 = u(n2,2)
   u3 = u(n3,1)
   v3 = u(n3,2)
   
   dudx = (b1*u1 + b2*u2 + b3*u3) / Delta2
   dvdy = (c1*v1 + c2*v2 + c3*v3) / Delta2
   dudy = (c1*u1 + c2*u2 + c3*u3) / Delta2
   dvdx = (b1*v1 + b2*v2 + b3*v3) / Delta2
   
   ! Strain components
   strain(eNum,1,1) = dudx
   strain(eNum,1,2) = dvdy
   strain(eNum,1,3) = dudy + dvdx

   ex  = strain(eNum,1,1)
   ey  = strain(eNum,1,2)
   exy = strain(eNum,1,3)

   ! Stress components
   coef  = Yng / (1.0_dp - 2.0_dp*Pssn)                        !  <=  E / (1-2v)
   coef2 = coef / (1.0_dp+Pssn)                                !  <=  E / ((1-2v)*(1+v))
   stress(eNum,1,1) = coef2 * ( (1.0_dp-Pssn)*ex + Pssn*ey )
   stress(eNum,1,2) = coef2 * ( Pssn*ex + (1.0_dp-Pssn)*ey )
   stress(eNum,1,3) = 0.5_dp * Yng / (1.0_dp+Pssn) * exy
   
   ! Ou-of-plane stress σz is also calculated for plane strain case.
   IF (plane == 1) THEN
      stress(eNum,1,4) = coef2*Pssn*(ex+ey)
   END IF
   
   ! -------------------------------------------------
   ! Effect of the thermal strain is also added for linear thermoelastic problems
   IF (pcode == 3) THEN
      xg = (x1 + x2 + x3)/3.0_dp
      yg = (y1 + y2 + y3)/3.0_dp
      Tg = T(n1)*(a1 + b1*xg + c1*yg) &
         + T(n2)*(a2 + b2*xg + c2*yg) &
         + T(n3)*(a3 + b3*xg + c3*yg) 
      Tg = Tg / Delta2 
      stress(eNum,1,1) = stress(eNum,1,1) - a*coef*Tg
      stress(eNum,1,2) = stress(eNum,1,2) - a*coef*Tg
      
      ! For plane strain case, the effect of the thermal strain to the out-of-plane stress is calculated.
      IF (plane == 1) THEN
         stress(eNum,1,4) = stress(eNum,1,4) - a*coef*Tg
      END IF
   END IF
   
   
   ! -------------------------------------------------
   ! Out-of-plane strain εz is also calculated for plane stress case.
   IF (plane == 2) THEN
   
      !　εz = - v/(1-2v) *（εx＋εy）
      coef = Pssn/(1.0_dp - 2.0_dp*Pssn) 
      strain(eNum,1,4) = - coef * (strain(eNum,1,1) + strain(eNum,1,2))
      
      IF (pcode == 3) THEN
         xg = (x1 + x2 + x3)/3.0_dp
         yg = (y1 + y2 + y3)/3.0_dp
         Tg = T(n1)*(a1 + b1*xg + c1*yg) &
            + T(n2)*(a2 + b2*xg + c2*yg) &
            + T(n3)*(a3 + b3*xg + c3*yg) 
         Tg = Tg / Delta2 
         !　εz = - v/(1-2v) *（εx＋εy）+ (1+v)/(1-2v)*α*T
         strain(eNum,1,4) = strain(eNum,1,4) + (1.0_dp+Pssn)/(1-2.0_dp*Pssn)*a*Tg
      END IF 
      
   END IF
   
   ! von Mises stress
   ! Plane strain case
   IF (plane==1) THEN
         sx  = stress(eNum,1,1)
         sy  = stress(eNum,1,2)
         sxy = stress(eNum,1,3)
         sz  = stress(eNum,1,4)
         Mises(eNum,1) = sx*sx+sy*sy+sz*sz+3.0_dp*sxy*sxy-sx*sy-sx*sz-sy*sz
         Mises(eNum,1) = sqrt( Mises(eNum,1) )
   ! Plane stress case
   ELSE IF (plane==2) THEN
         sx  = stress(eNum,1,1)
         sy  = stress(eNum,1,2)
         sxy = stress(eNum,1,3)
         Mises(eNum,1) = sx*sx+sy*sy+3.0_dp*sxy*sxy-sx*sy
         Mises(eNum,1) = sqrt( Mises(eNum,1) )
   END IF

   RETURN
END SUBROUTINE SS_2TrigLinear
