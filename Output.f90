SUBROUTINE Output
   USE PrecTypes
   USE GlobalData
   
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Output
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Output the results to files.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER :: i,j,k,l
   INTEGER :: gp
   CHARACTER(LEN=128) :: fmt
   
   OPEN (UNIT=odev, FILE=oFileName, STATUS='UNKNOWN', ACTION='WRITE', ERR=100)

   ! Problem dimensionality (2-d or 3-d but 3-d is reserved for future extension)
   WRITE(odev,'(I1,A)') dim,'-dimensional problem'
   !--------------------------------------
   ! Problem code (1: steady-state heat conduction, 2: linear elastostatic, 3: linear thermo-elastostatic)
   IF (pcode==1) THEN
      WRITE(odev,'(A)') 'Steady-state heat conduction problem, or potential problem'
   ELSE IF (pcode==2) THEN
      WRITE(odev,'(A)') 'Linear elastostatic problem'
   ELSE IF (pcode==3) THEN
      WRITE(odev,'(A)') 'Linear thermo-elastostatic problem'
   END IF
   
   IF(pcode==2 .OR. pcode==3) THEN
      IF (dim== 2 .AND. plane==1) then      !  Plane strain
         WRITE(odev,'(A)') 'Plane strain problem'
      ELSE IF (dim==2 .AND. plane==2) THEN  !  Plane stress
         WRITE(odev,'(A)') 'Plane stress problem'
      END IF
   END IF
   
   !--------------------------------------------------------------------
   ! Total number of materials
   WRITE(odev, '(A,I2)') 'Total number of materials :',nm
   !--------------------------------------------------------------------
   ! Output of material properties
   SELECT CASE(pcode)   
      ! Potential problem
      CASE (1) 
         WRITE(odev,'(A)') 'Thermal conductivity'
         WRITE(odev,'("Material",3X,"kx",15X,"ky")')    
         DO i=1,nm  ! Output of thermal conductivities
            WRITE(odev,'(I8,3(2X,E15.8))') i, (lambda(i,k),k=1,dim)
         END DO
         WRITE(odev,'("Material",3X,"Young''s modulus",2X,"Poisson''s ratio")')    
      
      ! Linear elastostatic problem
      CASE (2)
         WRITE(odev,'("Material",3X,"Young''s modulus",2X,"Poisson''s ratio")')    
         DO i=1,nm  ! Output of Young's moduli and Poisson's ratios
            WRITE(odev,'(I8,3X,2(E15.8,2X))') i, E(i), v(i)
         END DO

      ! Linear thermo-elastostatic problem
      CASE (3)         
         WRITE(odev,'("Material",3X,"Young''s modulus",2X,"Poisson''s ratio",2X,"Thermal expansion coef")')    
         DO i=1,nm  ! Output of Young's modulus, Poisson's ratio, linear thermal expansion coefficient
            WRITE(odev,'(I8,3X,3(E15.8,2X))') i, E(i), v(i), alpha(i)
         END DO

   END SELECT
   !--------------------------------------------------------------------
   WRITE(odev,'(A,I6)') 'Total number of nodes :',node  ! Total number of nodes
   !--------------------------------------------------------------------
   ! Output of nodal coordinates
   SELECT CASE (dim) 
      CASE(2) ! Two-dimensional problem
         WRITE(odev,'(3X,A4,3X,A1,17X,A1)') 'Node','x','y'
         DO i=1,node
            WRITE(odev,'(I7,2(2X,E15.8))') i, x(i), y(i)
         END DO
      CASE(3) ! Three-dimensional problem
         WRITE(odev,'(1X,A4,2X,A1,17X,A1)') 'Node','x','y'
         DO i=1,node
            WRITE(odev,'(I5,3(2X,E15.8))') i, x(i), y(i),z(i)
         END DO
   END SELECT
   !--------------------------------------------------------------------
   ! Output of total number of elements
   WRITE(odev,'(A,I5)') 'Total number of element :',ne

   !--------------------------------------------------------------------
   ! For the case of two-dimensional linear elastostatic and linear thermo-elastostatic cases, 
   ! element thicknesses are also written out.
      WRITE(odev,'(A,2X,A,2X,A,4(2X,A),4X,A)') 'Element', 'Type', 'Material', &
      & 'Node1', 'Node2', 'Node3', 'Node4', 'Thickness'
      DO i=1,ne
         IF(etype(i) == 3) then
            WRITE(odev,'(I7,2X,I4,2X,I8,2X,3(I5,2X),7X,2X,E15.8)') i, etype(i), emtype(i), &
            & (elm(i,j),j=1,etype(i)), thickness(i)
         ELSE IF(etype(i) == 4) then
            WRITE(odev,'(I7,2X,I4,2X,I8,2X,4(I5,2X),2X,E15.8)') i, etype(i), emtype(i), &
            & (elm(i,j),j=1,etype(i)), thickness(i)
         END IF
      END DO

   !--------------------------------------------------------------------
   ! Output of the total number of element groups
   WRITE(odev,'(A,I3)') 'Total number of element groups :',ng
   
   !--------------------------------------------------------------------
   DO i=1,ng
      ! Number of elements belonging to the element group 'i'
      WRITE(odev,'(A,I3,A,I4)') 'Number of elements for group',i,' :', egrp(i)%ne
      WRITE(odev,'(A,3X,A)') 'Number','Element'
      DO j=1,egrp(i)%ne
         ! Element number of each element belonging to the element group 'i'
         WRITE(odev,'(I6,3X,I7)') j,egrp(i)%e(j)
      END DO
   END DO
   !--------------------------------------------------------------------
   ! Total number of boundary elements
   WRITE(odev,'(A,I4)') 'Total number of boundary elements : ',ns
   !--------------------------------------------------------------------
   WRITE(odev,'(A,4(2X,A))') 'Number','Type','Node1','Node2','Node3'
   DO i=1,ns
      WRITE(odev,'(I6,2X,I4,3(2X,I5))') i, stype(i), (sid(i,j),j=1,stype(i))
   END DO
   !--------------------------------------------------------------------
   ! Boundary condition codes and corresponding boundary condition values
   WRITE(odev,'(A,4(2X,A))') 'Boundary conditions'
   IF(pcode==1) THEN
      WRITE(odev,'(A)') 'Number, code_1, value_1, code_2, value_2y, ...'
      DO i=1,ns
         WRITE(odev,'(I5,2X,3(I5,2X,E15.8))') i, (bccode(i,j), bcval(i,j), j=1,stype(i))
      END DO
   ELSE IF (pcode==2 .OR. pcode==3) THEN
      WRITE(odev,'(A)') 'Number, code_x, value_x, code_y, value_y, code_x, value_x, code_y, value_y, ...'
      DO i=1,ns
         WRITE(odev,'(I6,2X,6(I5,2X,E15.8,1X))') i, ((bcc(i,k,l), bcv(i,k,l),l=1,dir), k=1,stype(i))
      END DO
   END IF
   !-------------------------------------------------------
   ! Nodal temperature output for the case of linear thermo-elastostatic problem
   IF (pcode==3) THEN
   
      WRITE(odev,'(A)') 'Prescribed nodal temperatures'
      WRITE(odev,'(1X,A,2X,A)') 'Node','Temperature'
      DO i=1,node
         WRITE(odev,'(I5,2X,E15.8)') i, T(i)
      END DO
   END IF

   !--------------------------------------------------------------------
   ! Output of computed results
   
   ! Steady-state heat conduction problem
   IF (pcode==1) THEN
      WRITE(odev,'(A)') 'Solutions for nodal temperatures'
      WRITE(odev,'(1X,A,2X,A)') 'Node','Temperature'
      DO i=1,node
         WRITE(odev,'(I5,2X,E15.8)')   i,T(i)
      END DO
      WRITE(odev,'(A)') 'Boundary nodal heat flow'
      WRITE(odev,'(1X,A,2X,A)') 'Node', 'Heat flow'
      DO i=1,node
         IF(bNodeFlag(i) == 1) THEN
            WRITE(odev,'(I5,2X,E15.8)') i,Q(i)
         END IF
      END DO
      
   ! Linear elastostatic problem and linear thermo-elastostatic problems
   ELSE IF (pcode==2 .OR. pcode==3) THEN
   
      ! Output of nodal displacements
      WRITE(odev,'(A)') 'Solutions for Nodal displacements'
      IF (dim==2) THEN 
         WRITE(odev,'(1X,A,3X,A,15X,A)') 'Node','ux','uy'
         DO i=1,node
            WRITE(odev,'(I5,2X,2(E15.8,2X))')   i,(u(i,j),j=1,dir)
         END DO
      ELSE IF (dim==3) THEN
         WRITE(odev,'(1X,A,3X,A,15X,A,15X,A)') 'Node','ux','uy','uz'
         DO i=1,node
            WRITE(odev,'(I5,2X,3(E15.8,2X))')   i,(u(i,j),j=1,dir)
         END DO
      END IF
     
      ! Output of equivalent nodal forces
      WRITE(odev,'(A)') 'Boundary equivalent nodal forces'
      IF (dim==2) THEN 
         WRITE(odev,'(1X,A,3X,A,15X,A)') 'Node','Fx','Fy'
         DO i=1,node
            IF(bNodeFlag(i) == 1) THEN
               WRITE(odev,'(I5,2X,2(E15.8,2X))')   i,(F(i,j),j=1,dir)
            END IF
         END DO
      ELSE IF (dim==3) THEN
         WRITE(odev,'(1X,A,3X,A,15X,A,15X,A)') 'Node','Fx','Fy','Fz'
         DO i=1,node
            IF(bNodeFlag(i) == 1) THEN
               WRITE(odev,'(I5,2X,3(E15.8,2X))')   i,(F(i,j),j=1,dir)
            END IF
         END DO
      END IF
      
      ! Output of strain components
      WRITE(odev,'(A)') 'Strain components'
      IF (plane == 1) THEN       !　Plane strain
         WRITE(odev,'(A,2X,A,6X,A,15X,A,15X,A)') 'Element','Element_or_node','ex','ey','gxy'
         DO i=1,ne
            IF (etype(i) == 3) THEN
               gp = 1
            ELSE IF (etype(i) == 4) THEN
               gp = 4
            END IF
            DO j=1,gp
               WRITE(odev,'(I7,8X,I6,8X,3(E15.8,2X))') i,j,strain(i,j,1), strain(i,j,2), strain(i,j,3)
            END DO
         END DO
      ELSE IF (plane == 2) THEN  !　Plane stress
         WRITE(odev,'(A,2X,A,2X,4X,A,15X,A,14X,A,15X,A)') 'Element','Element_or_node', &
         & 'ex','ey','gxy','ez'
         DO i=1,ne
            IF (etype(i) == 3) THEN
               gp = 1
            ELSE IF (etype(i) == 4) THEN
               gp = 4
            END IF
            DO j=1,gp
               WRITE(odev,'(I7,8X,I6,8X,4(E15.8,2X))') i,j, &
               & strain(i,j,1), strain(i,j,2), strain(i,j,3), strain(i,j,4)
            END DO
         END DO
      END IF
      
      ! Output of stress components
      WRITE(odev,'(A)') 'Stress componensts'
      IF (plane == 1) THEN       !　Plane strain
         WRITE(odev,'(A,2X,A,6X,A,15X,A,15X,A,14X,A)') 'Element','Element_or_node', &
         & 'sx','sy','sxy','sz'
         DO i=1,ne
            IF (etype(i) == 3) THEN
               gp = 1
            ELSE IF (etype(i) == 4) THEN
               gp = 4
            END IF
            DO j=1,gp
               WRITE(odev,'(I7,8X,I6,8X,4(E15.8,2X))') i,j, &
               & stress(i,j,1), stress(i,j,2), stress(i,j,3), stress(i,j,4)
            END DO
         END DO
      ELSE IF (plane == 2) THEN  !　Plane stress
         WRITE(odev,'(A,2X,A,6X,A,15X,A,15X,A)') 'Element','Element_or_node','sx','sy','sxy'
         DO i=1,ne
            IF (etype(i) == 3) THEN
               gp = 1
            ELSE IF (etype(i) == 4) THEN
               gp = 4
            END IF
            DO j=1,gp
               WRITE(odev,'(I7,8X,I6,8X,3(E15.8,2X))') i,j, &
               & stress(i,j,1), stress(i,j,2), stress(i,j,3)
            END DO
         END DO
      END IF
      
      ! Output of von Mises stress
      WRITE(odev,'(A)') 'von Mises stresses'
      WRITE(odev,'(A,2X,A,6X,A)') 'Element','Element_or_node','stress_value'
      DO i=1,ne
            IF (etype(i) == 3) THEN
               gp = 1
            ELSE IF (etype(i) == 4) THEN
               gp = 4
            END IF
         DO j=1,gp
            WRITE(odev,'(I7,8X,I6,8X,E15.8)') i,j,Mises(i,j)
         END DO
      END DO
   
   END IF

   CLOSE(odev)  ! Closing the file used for outputting the results
   
   ! Returned from here for normal finishing
   RETURN

   ! Process for file opining errors -------------------------------------------------
   ! When jumped here, there was a file opening error.
   100 CONTINUE
   ! The error report is written out and the execution is terminated.
   WRITE(*,'(A)') 'Error in opening output file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', oFileName
   STOP
      
END SUBROUTINE Output
