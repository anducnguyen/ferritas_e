SUBROUTINE EPS_temp
   USE PrecTypes
   USE GlobalData
   USE EPS_sharedModule
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   EPS_temp
   ! PARENT: EPSoutput
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Output the temperature (potential) distribution diagram to an EPS
   !         (Encapsulated PostScript) file.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
     
   INTEGER :: i,j,ii,jj
   INTEGER(i4b) :: p1, p2, p3, pp1, pp2, p, pp(3)
   CHARACTER(len=50) :: chr, chr1, chr2, chr3, chr4
   REAL(dp) :: rd, gr, bl, tempmax, tempmin
   REAL(dp) :: dt, tmp
   REAL(dp) :: colorscaleAreaWidth=80.0, colorscaleWidth=10.0, colorscaleHeight
   REAL(dp) :: xb,yb,xt,yt,dyt,y0
   REAL(dp) :: tlow,thigh,deltaT,tmp1,tmp2,rd1,gr1,bl1,rd2,gr2,bl2
   REAL(dp), ALLOCATABLE :: Telm(:)
         
   ! Output the data for contour plotting
   ALLOCATE(Telm(ne)) 
   DO i=1,ne
      IF (etype(i)==3) THEN
         Telm(i) = (T(elm(i,1))+T(elm(i,2))+T(elm(i,3)))/3.0_dp
      ELSE IF (etype(i)==4) THEN
         Telm(i) = (T(elm(i,1))+T(elm(i,2))+T(elm(i,3))+T(elm(i,4)))*0.25_dp
      END IF
   END DO
   
   OPEN (UNIT=cdev, FILE=contourDataFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=100)
   WRITE(cdev,'(I6)') pcode
   WRITE(cdev,'(I6)') node
   DO i=1,node
      WRITE(cdev,'(I7,2(2X,E15.8))') i, x(i), y(i)
   END DO
   WRITE(cdev,'(I5)') ne
   DO i=1,ne
      WRITE(cdev,'(I7,1X,I3,3X,4(1X,I7))') i, etype(i), (elm(i,j),j=1,etype(i))
   END DO
   DO i=1,node
      WRITE(cdev,'(I7,1X,E15.8)') i, T(i)
   END DO
   DO i=1,ne
      WRITE(cdev,'(I7,1X,E15.8)') i, Telm(i)
   END DO
   CLOSE(cdev)
   DEALLOCATE(Telm)
   ! -------------------------------------------   


   gtmax = 0.0
   gtmin = -10.0
      
   DO i=1,ne
      DO j=1,etype(i)
         eline(i,j,3) = 0
      END DO
   END DO
   
   tmax = T(1)
   tmin = T(1)
   DO i=1,node
      IF (tmax < T(i)) tmax = T(i)
      IF (tmin > T(i)) tmin = T(i)
   END DO
   tempmax = tmax
   tempmin = tmin
   
   if (displayrelative /= 1) then
      tmax = gtmax
      tmin = gtmin
   end if

   dt = tmax - tmin
   
   ! Opening a file for output
   OPEN (UNIT=edev, FILE=tempFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=200)
   
   ! Bounding box of the image
   boundLBx = 0.0_DP
   boundLBy = 0.0_DP
   boundTRx = boxWidth + colorscaleAreaWidth
   boundTRy = boxHeight
   
   ! EPS header and macro commands
   WRITE(edev,'(A)') '%!PS-Adobe-3.1 EPSF-3.0'
   WRITE(chr1,*) boundLBx
   WRITE(chr2,*) boundLBy
   WRITE(chr3,*) boundTRx
   WRITE(chr4,*) boundTRy
   WRITE(edev,'(A)') '%%BoundingBox: ' // trim(adjustl(chr1)) // ' ' // &
   & trim(adjustl(chr2)) // ' ' // trim(adjustl(chr3)) // ' ' // trim(adjustl(chr4))
   WRITE(edev,'(A)') '%%Title: displacement.eps'
   WRITE(edev,'(A)') '%%Creator: Feritas'
   WRITE(edev,'(A)') '%%DocumentNeededResources: font Times-Roman'
   WRITE(edev,'(A)') '%%DocumentFonts: Times-Roman'
   WRITE(edev,'(A)') '%%Copyright: Copyright (C) 2009 Toshiro Matsumoto.  All Rights Reserved.'
   WRITE(edev,'(A)') '%%EndComments'
   WRITE(edev,'(A)') '/S {stroke} def'
   WRITE(edev,'(A)') '/NP {newpath} def'
   WRITE(edev,'(A)') '/SL {setlinewidth} def'
   WRITE(edev,'(A)') '/C {setrgbcolor} def'
   WRITE(edev,'(A)') '/SG {setgray} def'
   WRITE(edev,'(A)') '/MT {moveto} def'
   WRITE(edev,'(A)') '/LT {lineto} def'
   WRITE(edev,'(A)') '/RT {rlineto} def'
   WRITE(edev,'(A)') '/A {arc} def'
   WRITE(edev,'(A)') '/F {fill} def'
   WRITE(edev,'(A)') '/LJ {setlinejoin} def'
   WRITE(edev,'(A)') '/CL {closepath} def'
   WRITE(edev,'(A)') '/SP {showpage} def'
   WRITE(edev,'(A)') '/SW {show} def'
   WRITE(edev,'(A)') '/AP {aload pop} def'
   WRITE(edev,'(A)') '/SF {shfill} def'
   WRITE(edev,'(A,1X,I2,1X,A)') '/DC { 0 360 A S } def'  ! Macro command for drawing a circle
   WRITE(edev,'(A,1X,I2,1X,A)') '/FC { 0 360 A F } def'  ! Macro command for painting a circle 
   WRITE(edev,'(A)') '0.3  SL'         ! Line width   
   WRITE(edev,'(1X,I2,1X,A)') linejoin, 'LJ'
   
   ! Drawing the temperature distribution
   DO i=1,ne
      IF (etype(i)==3 .OR. etype(i)==4) THEN
         WRITE(edev,'(A)') 'NP'
         WRITE(edev,'(A)') '<<'
         WRITE(edev,'(A)') '/ShadingType 4'
         WRITE(edev,'(A)') '/ColorSpace [ /DeviceRGB ]'
         WRITE(edev,'(A)') '/DataSource ['

         DO j=1,3
            p = elm(i,j)
            IF (dt==0.0) THEN 
               tmp = 0.0
            ELSE
               tmp = (T(p) - tmin) / dt
            END IF
            if (tmp > 1.0) then
               rd = overred
               bl = overblu
               gr = overgrn
            else if (tmp < 0.0) then
               rd = underred
               bl = underblu
               gr = undergrn
            else
               rd = fn(red,pt,tmp)
               gr = fn(grn,pt,tmp)
               bl = fn(blu,pt,tmp)
            end if
               
            WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',px(p), py(p), ' ] AP',rd,gr,bl
         END DO 
         WRITE(edev,'(A)') '] >>'
         WRITE(edev,'(A)') 'SF'
      END IF
      IF (etype(i)==4) THEN
         WRITE(edev,'(A)') 'NP'
         WRITE(edev,'(A)') '<<'
         WRITE(edev,'(A)') '/ShadingType 4'
         WRITE(edev,'(A)') '/ColorSpace [ /DeviceRGB ]'
         WRITE(edev,'(A)') '/DataSource ['

         pp(1) = elm(i,1)
         pp(2) = elm(i,3)
         pp(3) = elm(i,4)
         DO j=1,3
            IF (dt==0.0) THEN 
               tmp = 0.0
            ELSE
               tmp = (T(pp(j)) - tmin) / dt
            END IF
            if (tmp > 1.0) then
               rd = overred
               bl = overblu
               gr = overgrn
            else if (tmp < 0.0) then
               rd = underred
               bl = underblu
               gr = undergrn
            else
               rd = fn(red,pt,tmp)
               gr = fn(grn,pt,tmp)
               bl = fn(blu,pt,tmp)
            end if
               
            WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ', &
            & px(pp(j)), py(pp(j)), ' ] AP',rd,gr,bl
         END DO 
         WRITE(edev,'(A)') '] >>'
         WRITE(edev,'(A)') 'SF'
      END IF
   END DO
   
   ! Color scale is drawn
   xb = boxWidth
   xt = boxWidth + colorscaleWidth
   colorscaleHeight = scaledHeight*0.6

   dyt = colorscaleHeight/10.0_dp
   y0 = yMargin + oy + dyt

   tlow = tmin
   deltaT = dt/10.0
   
   yb = y0 - dyt
   yt = yb + dyt
   tlow = gtmin
      
   WRITE(edev,'(A)') 'NP'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'MT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, yb,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, yt,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yt,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'LT'
   WRITE(edev,'(A)') 'CL'
   WRITE(edev,'(3(F6.3,1X),A)') underred,undergrn,underblu,'C'
   WRITE(edev,'(A)') 'F'
   WRITE(edev,'(A)') 'S'

   do i=1,10
      yb = y0 + float(i-1)*dyt
      yt = yb + dyt
      tmp1 = 0.1 * float(i-1)
      tmp2 = 0.1 * float(i)
      rd1 = fn(red,pt,tmp1)
      gr1 = fn(grn,pt,tmp1)
      bl1 = fn(blu,pt,tmp1)
      rd2 = fn(red,pt,tmp2)
      gr2 = fn(grn,pt,tmp2)
      bl2 = fn(blu,pt,tmp2)
      WRITE(edev,'(A)') '<<'
      WRITE(edev,'(A)') '/ShadingType 4'
      WRITE(edev,'(A)') '/ColorSpace [ /DeviceRGB ]'
      WRITE(edev,'(A)') '/DataSource ['
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xb, yb, ' ] AP',rd1,gr1,bl1
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xt, yb, ' ] AP',rd1,gr1,bl1
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xt, yt, ' ] AP',rd2,gr2,bl2
      WRITE(edev,'(A)') '] >>'
      WRITE(edev,'(A)') 'SF'
      WRITE(edev,'(A)') '<<'
      WRITE(edev,'(A)') '/ShadingType 4'
      WRITE(edev,'(A)') '/ColorSpace [ /DeviceRGB ]'
      WRITE(edev,'(A)') '/DataSource ['
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xb, yb, ' ] AP',rd1,gr1,bl1
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xt, yt, ' ] AP',rd2,gr2,bl2
      WRITE(edev,'(A,E11.4,1X,E11.4,A,1X,3(F6.3,1X))') '0 [ ',xb, yt, ' ] AP',rd2,gr2,bl2
      WRITE(edev,'(A)') '] >>'
      WRITE(edev,'(A)') 'SF'
   
   end do
   
   ! Elements are drawn
   DO i=1,ne
      DO j=1,etype(i)
         eline(i,j,3) = 0
      END DO
   END DO

   WRITE(edev,'(A)') 'NP'
   WRITE(edev,'(F6.3,1X,A)') meshLineThickness, 'SL'        ! line thickness   
   DO i=1,ne
      DO j=1,etype(i)
         p1 = elm(i,j)
         IF (j<etype(i)) THEN 
            p2 = elm(i,j+1)
         ELSE
            p2 = elm(i,1)
         END IF
         IF (eline(i,j,3)==0) THEN
            WRITE(edev,'(E15.8,2X,E15.8,2X,A)') px(p1), py(p1), 'MT'
            WRITE(edev,'(E15.8,2X,E15.8,2X,A)') px(p2), py(p2), 'LT'
            eline(i,j,3) = 1
            SEARCH: DO ii=1,ne
               DO jj=1,etype(ii)
                  pp1 = elm(ii,jj)
                  IF (jj<etype(ii)) THEN 
                     pp2 = elm(ii,jj+1)
                  ELSE
                     pp2 = elm(ii,1)
                  END IF
                  IF (pp1==p2 .AND. pp2==p1) THEN
                     eline(ii,jj,3) = 1
                     EXIT SEARCH
                  END IF
               END DO
            END DO SEARCH
         END IF
      END DO 
   END DO
   WRITE(edev,'(A)') 'S'
 
   ! Color scale is drawn
   yb = colorscaleHeight + yMargin + oy +dyt
   yt = yb + dyt
   WRITE(edev,'(A)') 'NP'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'MT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, yb,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, yt,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yt,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'LT'
   WRITE(edev,'(A)') 'CL'
   WRITE(edev,'(3(F6.3,1X),A)') overred,overgrn,overblu,'C'
   WRITE(edev,'(A)') 'F'
   WRITE(edev,'(A)') 'S'
   ! Color scale frame is drawn
   WRITE(edev,'(A)') 'NP'
   WRITE(edev,'(A)') '0.0 SG'
   DO i=1,13
      WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, y0+float(i-2)*dyt,'MT'
      IF (i==1 .OR. i==13) THEN 
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, y0+float(i-2)*dyt,'LT'
      ELSE
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt+5.0, y0+float(i-2)*dyt,'LT'
      END IF
      WRITE(edev,'(A)') '0.75 SL'     
   END DO
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, y0-dyt,'MT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, y0-dyt,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt, y0+dyt*11.0,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, y0+dyt*11.0,'LT'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, y0-dyt,'LT'
   WRITE(edev,'(A)') 'CL'
   WRITE(edev,'(A)') 'S'
  
   ! Numbers of the color scale are drawn
   WRITE(edev,'(A,1X,I2,1X,A)') '/Courier findfont 7 scalefont setfont'
   WRITE(edev,'(A)') '0.0 SG'
   
   do i=1,10
      tlow = tmin + deltaT * float(i-1)
      WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt+8.0, y0+float(i-1)*dyt-5.0*point,'MT'
      WRITE(edev,'(A,E11.4,A)') '(',tlow,') SW'
      if (i==10) then
         thigh = tlow + deltaT
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xt+8.0, y0+float(i)*dyt-5.0*point,'MT'
         WRITE(edev,'(A,E11.4,A)') '(',thigh,') SW'
      end if
   end do
   
   ! Displaying the maximum and minimum value
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yMargin+oy+scaledHeight-20.0*point,'MT'
   WRITE(edev,'(A,E11.4,A)') '(Max.=',tempmax,') SW'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yMargin+oy+scaledHeight-20.0*point-30.0*point,'MT'
   WRITE(edev,'(A,E11.4,A)') '(Min.=',tempmin,') SW'
  
   WRITE(edev,'(A)') 'SP'

   CLOSE(edev)
   
   ! Returning when normally ended
   RETURN

   ! Process when an error occurred -------------------------------------------------
   ! File opening error occurred here
   100 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', contourDataFileName
   STOP
   ! Process when an error occurred -------------------------------------------------
   ! File opening error occurred here
   200 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', tempFileName
   STOP
   
END SUBROUTINE EPS_temp
