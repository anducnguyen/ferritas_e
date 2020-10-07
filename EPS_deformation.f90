SUBROUTINE EPS_deformation
   USE PrecTypes
   USE GlobalData
   USE EPS_sharedModule
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   EPS_deformation
   ! PARENT: EPSoutput
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Output the deformation diagram to an EPS (Encapsulated PostScript)
   !         file.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
     
   INTEGER :: i,j,k,ii,jj
   INTEGER(i4b) :: p1, p2, p3, p4, pp1, pp2
   CHARACTER(len=50) :: chr,chr1,chr2,chr3,chr4
   REAL(dp) :: annotationAreaWidth=130.0, dy=30.0
   REAL(dp) :: xb, yb
  
   ! Outputting the data for deformation diagram -------------------
   OPEN (UNIT=ddev, FILE=deformDataFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=100)
   WRITE(ddev,'(I6)') -pcode
   WRITE(ddev,'(I6)') node
   DO i=1,node
      WRITE(ddev,'(I7,2(2X,E15.8))') i, x(i), y(i)
   END DO
   WRITE(ddev,'(I5)') ne
   DO i=1,ne
      WRITE(ddev,'(I7,1X,I3,4(1X,I7))') i, etype(i), (elm(i,j),j=1,etype(i))
   END DO
   DO i=1,node
      WRITE(ddev,'(I5,2X,2(E15.8,1X))')   i,(u(i,j),j=1,dir)
   END DO
   CLOSE(ddev)
   ! -------------------------------------------        
      
   DO i=1,ne
      DO j=1,etype(i)
         eline(i,j,3) = 0
      END DO
   END DO
   
   ! Opening a file for output
   OPEN (UNIT=edev, FILE=dispFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=200)
   
   ! Bounding box of the image
   boundLBx = 0.0_DP
   boundLBy = 0.0_DP
   boundTRx = boxWidth + annotationAreaWidth
   boundTRy = boxHeight
   ! EPS headers and macro commands
   chr1=''
   chr2=''
   chr3=''
   chr4=''
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
   WRITE(edev,'(A,1X,I2,1X,A)') '/DC { 0 360 A S } def'    ! Macro command for drawing a circle
   WRITE(edev,'(A,1X,I2,1X,A)') '/FC { 0 360 A F } def'    ! Macro command for painting a circle
   WRITE(edev,'(1X,I2,1X,A)') linejoin, 'LJ'

   
   ! Drawing the elements before deformation
   IF (dispMeshShowSwitch == 1) THEN
      ! First, paint all the elements
      WRITE(edev,'(A)') 'NP'
      DO i=1,ne
         p1 = elm(i,1)
         p2 = elm(i,2)
         p3 = elm(i,3)
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p1),py(p1),'MT'
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p2),py(p2),'LT'
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p3),py(p3),'LT'
         if (etype(i) == 4) then
            p4 = elm(i,4)
            WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p4),py(p4),'LT'
         end if
         WRITE(edev,'(A)') 'CL'
         WRITE(edev,'(3(F6.3,1X),A)') dispElmColor(1),dispElmColor(2),dispElmColor(3),'C'
         WRITE(edev,'(A)') 'F'
      end do
      WRITE(edev,'(A)') 'S'

      ! Then, draw all the lines of the element shape
      DO i=1,ne
         DO j=1,etype(i)
            eline(i,j,3) = 0
         END DO
      END DO

      WRITE(edev,'(A)') 'NP'
      WRITE(edev,'(F6.3,1X,A)') dispMeshLineThickness, 'SL'         ! line thickness    
      WRITE(edev,'(3(F6.3,1X),A)') (dispMeshLineColor(k),k=1,3), 'C'
      DO i=1,ne
         DO j=1,etype(i)
            p1 = elm(i,j)
            IF (j<etype(i)) THEN 
               p2 = elm(i,j+1)
            ELSE
               p2 = elm(i,1)
            END IF
            IF (eline(i,j,3)==0) THEN
               WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p1), py(p1), 'MT'
               WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(p2), py(p2), 'LT'
               eline(i,j,3) = 1
               SEARCH2: DO ii=1,ne
                  DO jj=1,etype(ii)
                     pp1 = elm(ii,jj)
                     IF (jj<etype(ii)) THEN 
                        pp2 = elm(ii,jj+1)
                     ELSE
                        pp2 = elm(ii,1)
                     END IF
                     IF (pp1==p2 .AND. pp2==p1) THEN
                        eline(ii,jj,3) = 1
                        EXIT SEARCH2
                     END IF
                  END DO
               END DO SEARCH2
            END IF
         END DO 
      END DO
      WRITE(edev,'(A)') 'S'
   END IF
   
   ! Drawing the shapes after deformation
   DO i=1,ne
      DO j=1,etype(i)
         eline(i,j,3) = 0
      END DO
   END DO

   WRITE(edev,'(F6.3,1X,A)') dispLineThickness, 'SL'         ! line thickness    
   WRITE(edev,'(3(F6.3,1X),A)') (dispLineColor(k),k=1,3), 'C'
        
   DO i=1,ne
      DO j=1,etype(i)
         p1 = elm(i,j)
         IF (j<etype(i)) THEN 
            p2 = elm(i,j+1)
         ELSE
            p2 = elm(i,1)
         END IF
         IF (eline(i,j,3)==0) THEN
            WRITE(edev,'(E11.4,1X,E11.4,1X,A)') pux(p1), puy(p1), 'MT'
            WRITE(edev,'(E11.4,1X,E11.4,1X,A)') pux(p2), puy(p2), 'LT'
            eline(i,j,3) = 1
            SEARCH3: DO ii=1,ne
               DO jj=1,etype(ii)
                  pp1 = elm(ii,jj)
                  IF (jj<etype(ii)) THEN 
                     pp2 = elm(ii,jj+1)
                  ELSE
                     pp2 = elm(ii,1)
                  END IF
                  IF (pp1==p2 .AND. pp2==p1) THEN
                     eline(ii,jj,3) = 1
                     EXIT SEARCH3
                  END IF
               END DO
            END DO SEARCH3
         END IF
      END DO 
   END DO
   
   WRITE(edev,'(A)') 'S'
   
   ! Displaying the maxmum and minimum absolute values of the displacement
      xb = boxWidth
      yb = yMargin + oy + scaledHeight - 20.0*point
      dy = dy * point

   WRITE(edev,'(A)') '/Helvetica findfont 7 scalefont setfont'
   WRITE(edev,'(A)') '0.0 SG'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'MT'
   WRITE(edev,'(A,E11.4,A)') '(Rate of magnification: (x)',uscale,') SW'
   
   yb = yb - 1.5*dy
   
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb,'MT'
   WRITE(edev,'(A,E11.4,A)') '(Max.Abs.u=',umax,') SW'
   WRITE(edev,'(E11.4,1X,E11.4,1X,A)') xb, yb-dy,'MT'
   WRITE(edev,'(A,E11.4,A)') '(Max.Abs.v=',vmax,') SW'
   
   WRITE(edev,'(A)') 'SP'
   
   CLOSE(edev)
         
   ! Returning when normally ended
   RETURN

   ! Process when an error occurred -------------------------------------------------
   ! File opening error occurred here
   100 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', deformDataFileName
   STOP
   200 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', dispFileName
   STOP
      
END SUBROUTINE EPS_deformation
