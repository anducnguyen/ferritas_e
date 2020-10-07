SUBROUTINE EPS_mesh
   USE PrecTypes
   USE GlobalData
   USE EPS_sharedModule
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   EPS_mesh
   ! PARENT: EPSoutput
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Output the mesh diagram to an EPS (Encapsulated PostScript) file.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
     
   INTEGER :: i,j,k,ii,jj
   INTEGER(i4b) :: p1, p2, p3, pp1, pp2
   REAL(dp) :: a1, a2, a3
   REAL(dp), ALLOCATABLE :: area(:)
   CHARACTER(len=50) :: chr, chr1, chr2, chr3, chr4
   INTEGER :: meshcode = 0

   ! Outputting the data for mesh plotting-------------------   
   OPEN (UNIT=cdev, FILE=meshDataFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=100)
   WRITE(cdev,'(I6)') meshcode
   WRITE(cdev,'(I6)') node
   DO i=1,node
      WRITE(cdev,'(I7,2(2X,E15.8))') i, x(i), y(i)
   END DO
   WRITE(cdev,'(I5)') ne
   DO i=1,ne
      WRITE(cdev,'(I7,1X,I3,3X,4(1X,I7))') i, etype(i), (elm(i,j),j=1,etype(i))
   END DO

   CLOSE(cdev)
   ! -------------------------------------------   
    
   ! Outputting the mesh diagram
   ! Opening a file for output
   OPEN (UNIT=edev, FILE=meshFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=200)
   ! Bounding box of the image
   boundLBx = 0.0_DP
   boundLBy = 0.0_DP
   boundTRx = boxWidth
   boundTRy = boxHeight
   ! EPS headers and macro commands
   WRITE(edev,'(A)') '%!PS-Adobe-3.1 EPSF-3.0'
   WRITE(chr1,*) boundLBx
   WRITE(chr2,*) boundLBy
   WRITE(chr3,*) boundTRx
   WRITE(chr4,*) boundTRy
   WRITE(edev,'(A)') '%%BoundingBox: ' // trim(adjustl(chr1)) // ' ' // &
   & trim(adjustl(chr2)) // ' ' // trim(adjustl(chr3)) // ' ' // trim(adjustl(chr4))
   WRITE(edev,'(A)') '%%Title: mesh.eps'
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
   
   WRITE(edev,'(F6.3,1X,A)') meshLineThickness, 'SL'        ! Line thickness      
   WRITE(edev,'(1X,I2,1X,A)') linejoin, 'LJ'
   
   ! Painting the elements
   IF (elementColorSwitch==1) THEN
   
      ! Memory allocation of an array in which element areas are stored
      ALLOCATE( area(ne) )

      DO i=1,ne
         ! Calculation of the are of this element
         ! Case of a triangle
         p1 = elm(i,1)
         p2 = elm(i,2)
         p3 = elm(i,3)
         a1 = x( p2 )*y( p3 ) - x( p3 )*y( p2 )
         a2 = x( p3 )*y( p1 ) - x( p1 )*y( p3 )
         a3 = x( p1 )*y( p2 ) - x( p2 )*y( p1 )
         area(i) = (a1 + a2 + a3) * 0.5_dp
         ! Case of a rectangle: adding the area of additional triangle
         IF (etype(i)==4) THEN
            p1 = elm(i,1)
            p2 = elm(i,3)
            p3 = elm(i,4)
            a1 = x( p2 )*y( p3 ) - x( p3 )*y( p2 )
            a2 = x( p3 )*y( p1 ) - x( p1 )*y( p3 )
            a3 = x( p1 )*y( p2 ) - x( p2 )*y( p1 )
            area(i) = area(i) + (a1 + a2 + a3) * 0.5_dp
         END IF
      END DO
      
      WRITE(edev,'(A)') 'NP'

      DO i=1,ne
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(elm(i,1)), py(elm(i,1)), 'MT'
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(elm(i,2)), py(elm(i,2)), 'LT'
         WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(elm(i,3)), py(elm(i,3)), 'LT'
         IF (etype(i) == 4) THEN
            WRITE(edev,'(E11.4,1X,E11.4,1X,A)') px(elm(i,4)), py(elm(i,4)), 'LT'
         END IF
         IF (area(i) > 0) THEN
            ! Setting the painting color when the area of the element 
            ! is positive (case of correct node numbering)
            WRITE(edev,'(3(F6.3,1X),A)') (elementColor(k),k=1,3), 'C'
         ELSE 
            ! Setting the different painting color when the area of the element 
            ! is negative (case of wrong node numbering)
            IF (malElementColorSwitch==1) THEN
            
               ! Color setting for the wrong numbering case
               WRITE(edev,'(3(F6.3,1X),A)') (malElementColor(k),k=1,3), 'C'
               
            ! Setting the same painting color also when the area of the element 
            ! is negative (case of wrong node numbering)
           ELSE
            
            ! Case when the same painting color is used for any case
               WRITE(edev,'(3(F6.3,1X),A)') (elementColor(k),k=1,3), 'C'
            END IF
         END IF
         WRITE(edev,'(A)') 'F'
      END DO
      
      WRITE(edev,'(A)') 'S'
         
      DEALLOCATE( area )

   END IF
  
   ! Drawing the elements
   WRITE(edev,'(A)') 'NP'
   WRITE(edev,'(3(F6.3,1X),A)') (meshLineColor(k),k=1,3), 'C'
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
   
   ! Writing the element numbers
   IF (elementNumberSwitch==1) THEN
      WRITE(edev,'(A,1X,F6.3,1X,A)') '/Times-Bold findfont',elNumberPhontSize,'scalefont setfont'
      WRITE(edev,'(3(F7.4,1X),A)') (elNumberPhontColor(k),k=1,3),'setrgbcolor'
      DO i=1,ne
         WRITE(chr,'(I8)') i
         WRITE(edev,'(E11.4,1X,E11.4,1X,A,2X,A,2X,A)') pxg(i), pyg(i), 'MT', &
         & '(' // trim(adjustl(chr)) // ')', 'SW'   
      END DO
   END IF
   
   ! Writing the node numbers
   IF (nodeNumberSwitch==1) THEN
      WRITE(edev,'(A,1X,F6.3,1X,A)') GothicOblique // ' findfont', &
      & ndNumberPhontSize, 'scalefont setfont'
      WRITE(edev,'(3(F7.4,1X),A)') (ndNumberPhontColor(k),k=1,3),'setrgbcolor'
      DO i=1,node
         WRITE(chr,'(I8)') i
         WRITE(edev,'(E11.4,1X,E11.4,1X,A,1X,A,1X,A)') px(i)+offx, py(i)+offy,'MT', &
         & '(' // trim(adjustl(chr)) // ')', 'SW'   
      END DO
   END IF
   
   WRITE(edev,'(A)') 'SP'
   
   CLOSE(edev)
   
   ! Returning when normally ended
   RETURN

   ! Process when an error occurred -------------------------------------------------
   ! File opening error occurred here
   100 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error cannot open file: ', meshDataFileName
   STOP
   200 CONTINUE
   ! Outputting an error report
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error cannot open file: ', meshFileName
   STOP
      
END SUBROUTINE EPS_mesh
