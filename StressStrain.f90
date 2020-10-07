SUBROUTINE StressStrain
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   StressStrain
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate strain, stress components, and von Mises stresses.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
      
   INTEGER :: i,j,gp
   
   strain = 0.0_dp
   stress = 0.0_dp
   Mises  = 0.0_dp
   
   SELECT CASE (dim)
   CASE (2) ! Two-dimansion problem
   
      DO i=1,ne   ! Repeat for all the elements
      
         SELECT CASE (etype(i))
         CASE (3) ! Triangular linear elment (constant strain element)
            CALL SS_2TrigLinear(i)
            
         CASE (4) ! Quadrilateral linear element
            CALL SS_2RectLinear(i)
         END SELECT
         
      END DO
   
   CASE (3) ! Three-dimensional problem (for future extension)
   END SELECT
                  
! For debugging------------
!write(*,'(a)') 'Strain'
!do i=1,ne
!   if (etype(i) == 3) then
!      gp = 1
!   else if (etype(i) == 4) then
!      gp = gausspoint*gausspoint
!   end if
!
!   do j=1,gp
!      if (plane == 1) then       !　Plane strain case
!         write(*,'(I3,X,I3,X,4(E15.8,X))') i,j,strain(i,j,1), strain(i,j,2), strain(i,j,3)
!      else if (plane == 2) then  !　Plane stress case
!         write(*,'(I3,X,I3,X,4(E15.8,X))') i,j,strain(i,j,1), strain(i,j,2), strain(i,j,3), strain(i,j,4)
!      end if
!   end do
!end do
!
!write(*,'(a)') 'Stress'
!do i=1,ne
!   if (etype(i) == 3) then
!      gp = 1
!   else if (etype(i) == 4) then
!      gp = gausspoint*gausspoint
!   end if
!         
!   do j=1,gp
!      if (plane == 1) then       !　Plane strain case
!         write(*,'(I3,X,I3,X,4(E15.8,X))') i,j,stress(i,j,1), stress(i,j,2), stress(i,j,3), stress(i,j,4)
!      else if (plane == 2) then  !　Plane stress case
!         write(*,'(I3,X,I3,X,4(E15.8,X))') i,j,stress(i,j,1), stress(i,j,2), stress(i,j,3)
!      end if
!   end do
!end do
!! von Mises応力
!write(*,'(A)') 'von Mises stress'
!do i=1,ne
!   if (etype(i) == 3) then
!      gp = 1
!   else if (etype(i) == 4) then
!      gp = gausspoint*gausspoint
!   end if
!   do j=1,gp
!      write(*,'(I5,2X,I6,4X,E15.8)') i,j,Mises(i,j)
!   end do
!end do
   
   RETURN
END SUBROUTINE StressStrain
