SUBROUTINE F_Elast2Linear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   F_Elast2Linear
   ! PARENT: Matrix
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate nodal force vector for a linear boundary element 
   !         for two-dimensional elastostatic and thermo-elastostatic problems
   !         and store them into the global nodal force vector.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: j,k
   INTEGER, PARAMETER :: Nodes = 3
   INTEGER :: n(10), ir, ic, jc, F_known

   ! Global node numbers of this boundary element are stored into n(j)
   DO j=1,Stype(eNum)
      n(j) = Sid(eNum,j) 
   END DO
   
   F_known = 0   !  Setting a flug showing if nodal force has to be calculated
                 !  when the traction is known. (0: not necessary) 
                 
   DO j=1,Stype(eNum)  ! Counting the node number of this boundary element
      DO k=1,dir
         IF(uflag(n(j),k) == 0) F_known = 1   ! Changing the flag to 1 (needed to calculate) 
                                              ! because the node at which the displacement is unknown.
      END DO
   END DO
   
   ! When the traction is not given at any  node of this element, there is no need to 
   ! calculate the nodal force. Move to the next boundary element.
   IF (F_known == 0) RETURN  
   
   ! Following is the process for the case when nodal forces have to be calculated.
   ! Calculation of the boundary integral for a straight boundary element
   CALL Force_2Linear(eNum) ! The results are returned from this subroutine 
                            ! in the array Flinear(2,2,direction) (node numbers × node numbers, direction)
   
   ! Obtaining the global node numbers of this boundary element
   
   DO ir=1,Stype(eNum) ! Repeating for the number of nodes defining this boundary element
      DO jc=1,dir      ! Repeating for the number of dimension
         
          ! For the weight function in jc-direction of the node n(ir)
         IF (uflag(n(ir),jc) == 0) THEN

            DO ic=1,Stype(eNum)    ! Repeating for the number of nodes defining this boundary element
               
               Fv( table(n(ir),jc) ) = Fv( table(n(ir),jc) ) + Flinear(ir,ic,jc)
               f(n(ir),jc) = f(n(ir),jc) + Flinear(ir,ic,jc)  ! Copying the nodal heat flow obtained 
                                                    ! from the boundary condition               
            END DO
      
         END IF
      END DO    ! <-- jc=1,dir
   END DO       ! <-- ir=1,Stype(i)
  
   RETURN
END SUBROUTINE F_Elast2Linear
