SUBROUTINE Mat_Elast2RectLinear(eNum)
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Mat_Elast2RectLinear
   ! PARENT: Matrix
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Calculate stiffness matrix for a rectangular linear element for 
   !         two-dimensional elastostatic and thermo-elastostatic problems and 
   !         store them into the global stiffness matrix.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER, INTENT(IN) :: eNum
   INTEGER :: j,k
   INTEGER, PARAMETER :: Nodes = 4
   INTEGER :: n(10), row, column, ir, jr, ic, jc

   ! Calculation of element stiffness matrix
   CALL K_Elast2RectLinear(eNum)
   
   
   ! Save the element stiffness matrix into the whole stiffness matrix
      
   ! Global node numbers corresponding to the local node number of the element is obtained
   DO j=1,Nodes
      n(j) = elm(eNum,j)
   END DO

   DO ir=1,Nodes    ! counting the element nodes in row direction
   DO jr=1,dir      ! repetition for dimension numbers: 2 or 3.
   
      row = table(n(ir), jr)   ! row number corresponding to jr component of the node n(ir)
      
      DO ic=1,etype(eNum) ! counting the element nodes in column direction
      DO jc=1,dir         ! repetition for dimension numbers: 2 or 3.
      
         column = table(n(ic), jc)     ! column number corresponding to jc component of the node n(ic)
         
         ! should be stored on the left-hand side because the displacement is unknown
         IF (row > 0) then
         
            IF(column > 0) then        ! should be stored on the left-hand side
               Km(row, column) = Km(row, column) + Krect(ir,ic, jr,jc)   
            ELSE                       ! should be moved to the right-hand side by multiplying the known quantities
               Fv(row) = Fv(row) - Krect(ir,ic, jr,jc) * u(n(ic), jc)
            END IF

         ! When the displacement is known and the nodal force is unknown
         ELSE ! When row < 0 row is stored as a negative value.
              ! After the nodal displacements are all obtained, they are 
              ! used to calculate the unknown nodal forces.
              ! Therefore, the column number for storing can be that correspoinding to the node number.
            Kmm(-row, dir*(elm(eNum,ic)-1)+jc) = Kmm(-row, dir*(elm(eNum,ic)-1)+jc) + Krect(ir,ic, jr,jc)
 
         END IF
         
      END DO ! <-- jc=1,dir
         
            ! Adding the effect of thermal strains
            IF (pcode == 3) THEN
               ! When the displacement is unknown
               IF (row > 0) THEN
                  Fv(row)   = Fv(row)   + Threct(ir,jr,ic) * T(n(ic))               
               ! When the displacement is known and the nodal force is unknown
               ELSE ! row < 0 のとき
                  Fvv(-row) = Fvv(-row) + Threct(ir,jr,ic) * T(n(ic))
               END IF
            END IF

      END DO ! <-- ic=1,etype(eNum)
     
   END DO 
   END DO 
  
   RETURN
END SUBROUTINE Mat_Elast2RectLinear
