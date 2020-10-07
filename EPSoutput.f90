SUBROUTINE EPSoutput
   USE PrecTypes
   USE GlobalData
   USE EPS_sharedModule
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   EPSoutput
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Output the mesh, temperature (potential) distribution, 
   !         deformation, stress distribution diagrams to EPS (Encapsulated 
   !         PostScript) files.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
     
   INTEGER :: i,j,ii,jj
   REAL(dp) :: s1, s2
   
   ! Attribute file used for creating output image
   OPEN (UNIT=edev, FILE=attrFileName, STATUS='OLD', ACTION='READ', ERR=100)
 
   READ(edev,*) displayrelative
   !   write(*,'(a,e15.8,1x,e15.8)') 'displayrelative=',displayrelative
 
   READ(edev,*) gtmin, gtmax
   !   write(*,'(a,e15.8,1x,e15.8)') 'gtmin, gtmax=',gtmin, gtmax
      
   READ(edev,*) gmisesmin, gmisesmax
   !   write(*,'(a,e15.8,1x,e15.8)') 'gmisesmin, gmisesmax=',gmisesmin, gmisesmax
      
   READ(edev,*) scaledUmax, scaledVmax
   !   write(*,'(a,e15.8,1x,e15.8)') 'scaledUmax, scaledVmax=',scaledUmax, scaledVmax
      
   READ(edev,*) ox, oy
   !   write(*,'(a,e15.8,1x,e15.8)') 'ox, oy=',ox, oy
      
   READ(edev,*) scaledWidth, scaledHeight
   !   write(*,'(a,e15.8,1x,e15.8)') 'scaledWidth, scaledHeight=',scaledWidth, scaledHeight
      
   READ(edev,*) thinLine
   !   write(*,'(a,f5.3)') 'thinLine=',thinLine
      
   READ(edev,*) midLine
   !   write(*,'(a,f5.3)') 'midLine=',midLine
      
   READ(edev,*) thickLine
   !   write(*,'(a,f5.3)') 'midLine=',midLine
      
   READ(edev,*) offxg, offyg
   !   write(*,'(a,e15.8,1x,e15.8)') 'offxg, offyg=',offxg, offyg
      
   READ(edev,*) offx, offy
   !   write(*,'(a,e15.8,1x,e15.8)') 'offx, offy=',offx, offy
      
   READ(edev,*) meshLineThickness
   !   write(*,'(a,f5.3)') 'meshLineThickness=',meshLineThickness
      
   READ(edev,*) elementNumberSwitch
   !   write(*,'(a,I1)') 'elementNumberSwitch=',elementNumberSwitch
      
   READ(edev,*) nodeNumberSwitch
   !   write(*,'(a,I1)') 'nodeNumberSwitch=',nodeNumberSwitch
      
   READ(edev,*) elementColorSwitch
   !   write(*,'(a,I1)') 'elementColorSwitch=',elementColorSwitch
      
   READ(edev,*) malElementColorSwitch
   !   write(*,'(a,I1)') 'malElementColorSwitch=',malElementColorSwitch

   READ(edev,*) (elementColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'elementColor=',elementColor
      
   READ(edev,*) (malElementColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'malElementColor=',malElementColor
      
   READ(edev,*) (meshLineColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'meshLineColor=',meshLineColor

   READ(edev,*) elNumberPhontSize
   !   write(*,'(a,I2)') 'elNumberPhontSize=',elNumberPhontSize
      
   READ(edev,*) (elNumberPhontColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'elNumberPhontColor=',elNumberPhontColor
     
   READ(edev,*) ndNumberPhontSize
   !   write(*,'(a,I2)') 'ndNumberPhontSize=',ndNumberPhontSize
      
   READ(edev,*) (ndNumberPhontColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'ndNumberPhontColor=',ndNumberPhontColor
      
   READ(edev,*) colorChartPhontSize
   !   write(*,'(a,I2)') 'colorChartPhontSize=',colorChartPhontSize
      
   READ(edev,*) dispMeshShowSwitch
   !   write(*,'(a,I1)') 'dispMeshShowSwitch=',dispMeshShowSwitch
      
   READ(edev,*) dispMeshLineThickness
   !   write(*,'(a,I1)') 'dispMeshLineThickness=',dispMeshLineThickness
      
   READ(edev,*) dispLineThickness
   !   write(*,'(a,I1)') 'dispLineThickness=',dispLineThickness
      
   READ(edev,*) (dispElmColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'dispElmColor=',dispElmColor
      
   READ(edev,*) (dispMeshLineColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'dispMeshLineColor=',dispMeshLineColor
      
   READ(edev,*) (dispLineColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'dispLineColor=',dispLineColor
      
   READ(edev,*) mizesMeshShowSwitch
   !   write(*,'(a,I1)') 'mizesMeshShowSwitch=',mizesMeshShowSwitch
      
   READ(edev,*) (elNumberColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'elNumberColor=',elNumberColor
      
   READ(edev,*) (ndNumberColor(i),i=1,3)
   !   write(*,'(a,3(e15.8,1x))') 'ndNumberColor=',ndNumberColor
      
   READ(edev,*) (pt(i),i=1,11)
   !   write(*,'(a,6(e15.8,1x))') 'pt=',pt
      
   READ(edev,*) (red(i),i=1,11)
   !   write(*,'(a,6(e15.8,1x))') 'red=',red
      
   READ(edev,*) (grn(i),i=1,11)
   !   write(*,'(a,6(e15.8,1x))') 'grn=',grn
      
   READ(edev,*) (blu(i),i=1,11)
   !   write(*,'(a,6(e15.8,1x))') 'blu=',blu
      
   READ(edev,*) overred, overgrn, overblu
   !   write(*,'(a,3(e15.8,1x))') 'overred, overgrn, overblu=',overred, overgrn, overblu
      
   READ(edev,*) underred, undergrn, underblu
   !   write(*,'(a,3(e15.8,1x))') ' underred, undergrn, underblu=', underred, undergrn, underblu

   ! When opening the attribute file fails, it means that there is no user defined attribute file. 
   ! Default parameters are used in this case.
   100 CONTINUE
   
   ! Generate a list for saving node numbers which define the edges of each element
   ALLOCATE(eline(ne,4,3))
   eline = 0
   DO i=1,ne
      DO j=1,etype(i)
         eline(i,j,1) = elm(i,j)
         IF (j<etype(i)) THEN
            eline(i,j,2) = elm(i,j+1)
         ELSE
            eline(i,j,2) = elm(i,1)
         END IF
      END DO
   END DO

   ! Center of geometry of each element
   ALLOCATE(xg(ne),yg(ne),pxg(ne),pyg(ne))
   DO i=1,ne
      IF (etype(i)==3) THEN
         xg(i) = ( x(elm(i,1)) + x(elm(i,2)) + x(elm(i,3)) ) / 3.0_dp
         yg(i) = ( y(elm(i,1)) + y(elm(i,2)) + y(elm(i,3)) ) / 3.0_dp
      ELSE IF (etype(i)==4) THEN
         xg(i) = ( x(elm(i,1)) + x(elm(i,2)) + x(elm(i,3)) + x(elm(i,4)) ) / 4.0_dp
         yg(i) = ( y(elm(i,1)) + y(elm(i,2)) + y(elm(i,3)) + y(elm(i,4)) ) / 4.0_dp
      END IF
   END DO
 
   ! Determine the minimum and maximum coordinate values
   xmin = x(1)
   ymin = y(1)
   xmax = x(1)
   ymax = y(1)
   DO i=1,node
      IF (x(i) < xmin) xmin = x(i)
      IF (y(i) < ymin) ymin = y(i)
      IF (x(i) > xmax) xmax = x(i)
      IF (y(i) > ymax) ymax = y(i)
   END DO
   
   ! Determine the width and height of the computation model
   objectWidth  = xmax - xmin
   objectHeight = ymax - ymin

!   Calculate the scaling factor to magnify/shrink to the size of the image area
!   IF (objectWidth <= objectHeight) THEN
!   
!      IF (scaledWidth <= scaledHeight) THEN
!         xscale = scaledWidth / objectHeight
!      ELSE IF (scaledWidth > scaledHeight) THEN
!         xscale = scaledHeight / objectHeight
!      END IF
!      
!   ELSE IF (objectWidth > objectHeight) THEN
!   
!      IF (scaledWidth <= scaledHeight) THEN
!         xscale = scaledWidth / objectWidth
!      ELSE IF (scaledWidth > scaledHeight) THEN
!         xscale = scaledHeight / objectWidth
!      END IF
!      
!   END IF

!   if (objectWidth >= objectHeight) then
!      xscale = scaledWidth / objectWidth
!   else
!      xscale = scaledHeight / objectHeight
!   end if


   xscale = min( scaledWidth / objectWidth, scaledHeight / objectHeight)

   
   ! Allocation of the arrays to store the coordinates after the scaling is applied
   ALLOCATE( px(node), py(node) )
   
   ! Scaling the coordinates
   DO i=1,node
      px(i)  = xMargin + ox + xscale * (x(i)  - xmin)
      py(i)  = yMargin + oy + xscale * (y(i)  - ymin)
   END DO
   DO i=1,ne
      pxg(i) = xMargin + ox + xscale * (xg(i) - xmin) + offxg
      pyg(i) = yMargin + oy + xscale * (yg(i) - ymin) + offyg
   END DO

   ! Two-dimensional linear elastostatic and thermo-eastostatic cases
   IF (meshCheck/=1 .AND. (pcode==2 .OR. pcode==3)) THEN
   
      ! Allocation of the arrays to store the sum of the nodal coordinates and the displacement values
      ALLOCATE( ux(node), uy(node), pux(node), puy(node) )
      
      ! Calculate the maximum absolute values of the displacements
      umax = abs(u(1,1))
      vmax = abs(u(1,2))
      DO i=1,node
         IF ( umax < abs(u(i,1)) ) umax = abs(u(i,1))
         IF ( vmax < abs(u(i,2)) ) vmax = abs(u(i,2))
      END DO

      ! Calculate the magnification scale of the displacements
      ! The actual maximum displacement values in the deformation diagram are 'scaledUmax' or 'scalesVmax'
      IF(umax >= vmax) THEN
         uscale = scaledUmax / umax
      ELSE
         uscale = scaledVmax / vmax
      END IF
      
      ! Scaling factors of the nodal coordinates when the deformation is taken into consideration
      
      s1 = (scaledWidth - scaledUmax)/objectWidth
      s2 = (scaledHeight - scaledVmax)/objectHeight

      xscale = min(s1,s2)
      
      ! Calculate the scaled displacement vectors and the nodal coordinates, and their summations
      DO i=1,node
         ux(i) = uscale * u(i,1)
         uy(i) = uscale * u(i,2)
         px(i) = xMargin + ox + xscale*(x(i)-xmin)
         py(i) = yMargin + oy + xscale*(y(i)-ymin)
         pux(i) = px(i) + ux(i)
         puy(i) = py(i) + uy(i)
      END DO
      DO i=1,ne
         pxg(i) = xMargin + ox + xscale * (xg(i) - xmin) + offxg
         pyg(i) = yMargin + oy + xscale * (yg(i) - ymin) + offyg
      END DO
   END IF
   
   ! Determine the drawn image size
   boxWidth  = xMargin * 2.0 + ox + scaledWidth
   boxHeight = yMargin * 2.0 + oy + scaledHeight
   
   ! Output of mesh diagram
   CALL EPS_mesh
   
   IF (meshCheck==1) RETURN
   
   ! Steady-state heat conduction problem
   IF (pcode==1) THEN
   
      ! Output of emperature distribution diagram
      CALL EPS_temp
   
   ! Linear elstostatic and thermo-elastostatic problem
   ELSE IF (pcode==2 .OR. pcode==3) THEN
   
      ! Output of deformation diagram
      CALL EPS_deformation
      
      ! Output of von Mises stress diagram
      CALL EPS_mises

   END IF

   
   RETURN
      
END SUBROUTINE EPSoutput

!------------------------------------------------------------------
! External utility function
!------------------------------------------------------------------
! A function used for blending RGB color defining a color scheme
FUNCTION fn(color,pt,x)
   USE PrecTypes
   IMPLICIT NONE
   REAL(dp), INTENT(IN) :: color(11),pt(11)
   REAL(dp), INTENT(IN) :: x
   REAL(dp) :: fn
   REAL(dp) :: F1, F2, F3
   INTEGER :: i1, i2, i3
   
   
   IF (x >= pt(1) .AND. x<=pt(3)) THEN
      i1 = 1
      i2 = 2
      i3 = 3
   ELSE IF (x > pt(3) .AND. x <= pt(5)) THEN
      i1 = 3
      i2 = 4
      i3 = 5
   ELSE IF (x > pt(5) .AND. x <= pt(7)) THEN
      i1 = 5
      i2 = 6
      i3 = 7
   ELSE IF (x > pt(7) .AND. x <= pt(9)) THEN
      i1 = 7
      i2 = 8
      i3 = 9
   ELSE IF (x > pt(9) .AND. x <= pt(11)) THEN
      i1 = 9
      i2 = 10
      i3 = 11
   END IF
   
   F1 = (x-pt(i2))*(x-pt(i3)) / ( (pt(i1)-pt(i2))*(pt(i1)-pt(i3)) )
   F2 = (x-pt(i1))*(x-pt(i3)) / ( (pt(i2)-pt(i1))*(pt(i2)-pt(i3)) )
   F3 = (x-pt(i1))*(x-pt(i2)) / ( (pt(i3)-pt(i1))*(pt(i3)-pt(i2)) )
   
   fn = color(i1) * F1 + color(i2) * F2 + color(i3) * F3
  
   RETURN
END FUNCTION fn
