SUBROUTINE Mat_Poten2TrigLinear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Mat_Poten2TrigLinear
   ! PARENT: Matrix
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix for a triangular linear element for 
   !         two-dimensional steady-state heat conduction problems and store 
   !         them into the global stiffness matrix and nodal heat flux vector.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: j,k
   INTEGER, PARAMETER :: Nodes = 3
   INTEGER :: n(10), row, column, ir, ic

   ! Calculation of element stiffness matrix
   CALL K_Poten2TrigLinear(eNum)
   
   ! Save the element stiffness matrix into the whole stiffness matrix
      
   ! Global node numbers corresponding to the local node number of the element is obtained
   DO j=1,Nodes
      n(j) = elm(eNum,j)
   END DO
   
   DO ir=1,Nodes    ! counting the element nodes in row direction
   
      row = ptable(n(ir))  ! row number of the matrix corresponding to the node n(ir)
      
      DO ic=1,etype(eNum) ! counting the element nodes in column direction
      
         column = ptable(n(ic)) ! column number of the matrix corresponding to the node n(ic)
         
         ! When the temperature is unknown and should be stored on the left-hand side
         IF (row > 0) then
         
            IF(column > 0) then   ! should be stored on the left-hand side
               Km(row, column) = Km(row, column) + Ktrig_p(ir,ic)   
            ELSE                  ! should be stored on the right-hand side by multiplying the known quantities
               Fv(row) = Fv(row) - Ktrig_p(ir,ic) * T(n(ic))
            END IF

         ! When the temperature is known and the nodal heat flux is unknown
         ELSE ! When row < 0 row is stored as a negative value.
              ! After the nodal temperatures are all obtained, they are used to calculate 
              ! the unknown nodal fluxes.
              ! Therefore, the column number for storing can be that correspoinding to the node number.
            Kmm(-row, elm(eNum,ic)) = Kmm(-row, elm(eNum,ic)) + Ktrig_p(ir,ic)
 
         END IF

      END DO ! <-- ic=1,etype(eNum)
     
   END DO 
  
   RETURN
END SUBROUTINE Mat_Poten2TrigLinear
