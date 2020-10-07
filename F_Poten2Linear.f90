SUBROUTINE F_Poten2Linear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   F_Poten2Linear
   ! PARENT: Matrix
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate nodal heat flux vector for a linear boundary element 
   !         for two-dimensional steady-state heat conduction problems and 
   !         store them into the global nodal heat flux vector.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
  
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: j,k
   INTEGER, PARAMETER :: Nodes = 3
   INTEGER :: n(10), ir, ic, Q_known

   ! Global node numbers of this boundary element are stored into n(j)
   DO j=1,Stype(eNum)
      n(j) = Sid(eNum,j) 
   END DO
   
   Q_known = 0   ! Setting a flug showing if nodal heat flow rate has to be calculated 
                 ! when the heat flux is known. (0: not necessary) 
   
   DO j=1,Stype(eNum)  ! Counting the node number of this boundary element
      IF(tflag(n(j)) == 0) Q_known = 1   ! Changing the flag to 1 (needed to calculate) 
                                         ! because the node at which the temperature is unknown.
   END DO
   
   ! When the heat flux is not given at any  node of this element, there is no need to 
   ! calculate the nodal heat flow. Move to the next boundary element.
   IF (Q_known == 0) RETURN  
   
   ! Following is the process for the case when nodal heat flows have to be calculated.
   ! Calculation of the boundary integral for a straight boundary element
   CALL Flux_2Linear(eNum) ! The results are returned from this subroutine 
                           ! in the array Flinear_p(2,2) (node numbers × node numbers)
   
   ! Obtaining the global node numbers of this boundary element
   
   DO ir=1,Stype(eNum)    ! Repeating for the number of nodes defining this boundary element
   
      !  For the weight function in jc-direction of the node n(ir)
      IF (tflag(n(ir)) == 0) THEN

         DO ic=1,Stype(eNum)    ! Repeating for the number of nodes defining this boundary element
            
            Fv( ptable(n(ir)) ) = Fv( ptable(n(ir)) ) + Flinear_p(ir,ic)
            Q(n(ir)) = Q(n(ir)) + Flinear_p(ir,ic)  ! Copying the nodal heat flow obtained 
                                                    ! from the boundary condition
            
         END DO
   
      END IF
         
   END DO       ! <-- ir=1,Stype(i)
  
   RETURN
END SUBROUTINE F_Poten2Linear
