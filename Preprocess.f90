SUBROUTINE Preprocess
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Preprocess
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Set flags, calculate number of knowns and unknowns, allocate arrays.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   INTEGER :: i,j,k,l
   
   ! Linear heat conduction problem:
   ! When the temperature is known, the flag is set to 1, otherwise set to 0.
   !
   ! Linear elastostatic and thermo-elastostatic problem:
   ! When the displacement is each direction is known, the flag in that direction
   ! is set to 1, otherwise set to 0.
   !
   IF (pcode==1) THEN
   
      tflag = 0   ! The flag to identify when the temperature are known initialized.
      T = 0.0     ! Nodal temperatures are initialized.
      bNodeFlag=0 ! The flag which is set to 1 when the node is on the boundary and 0 when in the domain.
                  ! Initally all the flags are set to zero.     
      DO i=1,ns
         DO j=1,Stype(i)   ! Repeat for the number of nodes composing each element
         
            IF(bNodeFlag(Sid(i,j)) == 0) THEN 
               bNodeFlag(Sid(i,j)) = 1
            END IF
            
            IF (bccode(i,j) == uKNOWN) THEN   ! The case when the temperature is known
               tflag( Sid(i,j)) = 1           ! The flag is set so that the temperature is known
               T( Sid(i,j) ) = bcval(i,j)     ! The boundary condition value is set as the nodal temperature
            END IF
            
         END DO
      END DO
      
      
! For debugging ------------------
!DO i=1,node
!   WRITE(*,'(I2,2x,I1,2x,E15.8)') i, tflag(i), T(i)
!END DO
! ----------------------------
   
   ELSE IF (pcode==2 .OR. pcode==3) THEN
   
      uflag = 0   ! The flag to identify when the displacement component is known is initialized.
      u = 0.0     ! Nodal displacements are initialized.
      bNodeFlag=0 ! The flag which is set to 1 when the node is on the boundary and 0 when in the domain.
                  ! Initally all the flags are set to zero. 
      
      DO i=1,ns
         DO j=1,Stype(i)   ! Repeat for the number of nodes composing each element
         
            IF(bNodeFlag(Sid(i,j)) == 0) THEN 
            
               bNodeFlag(Sid(i,j)) = 1
               
            END IF
            
            DO k=1,dir     ! Repeat for x, y, (and z)
            
               IF (bcc(i,j,k) == uKNOWN) THEN     ! The case when the displacement is known
               
                  uflag( Sid(i,j), k ) = 1        ! The flag is set so that the displacement is known
                  u( Sid(i,j), k ) = bcv(i,j,k)   ! The boundary condition value is set as the nodal displacement
                  
               END IF
               
            END DO
            
         END DO
      END DO
      
      
! For debugging ------------------
!DO i=1,node
!   WRITE(*,'(I2,2x,I1,x,I1,2x,E15.8,x,E15.8)') i, (uflag(i,j),j=1,dir), (u(i,j),j=1,dir)
!END DO
! ----------------------------
   
   END IF
   
   N_unknowns = 0    ! Total number of unknowns
   N_knowns = 0      ! Total number of knowns (set as a negative integer number)
   
   ! Steady-state heat conduction problem
   IF (pcode==1) THEN

      ! In 'ptable(i)', the temperature of node 'i' is saved
      DO i=1,node
      
         IF (tflag(i) == 0) THEN
         
            N_unknowns = N_unknowns + 1
            ptable(i) = N_unknowns
            
         ELSE IF (tflag(i) == 1) THEN
         
            N_knowns = N_knowns - 1
            ptable(i) = N_knowns
            
         END IF
         
      END DO

! For debugging ------------------
!write(*,'(a,I5)') 'Number of unknowns =',N_unknowns
!DO i=1,node
!write(*,'("ptable ",I2,x,I2)') i,ptable(i)
!END DO
! ----------------------------

   
   ! Linear elastostatic and thermo-elastostatic problem
   ELSE IF (pcode==2 .OR. pcode==3) THEN
   
      ! In 'table(i,j)', the displacement in 'j' direction of the node 'i' is saved
      DO i=1,node
         DO j=1,dir
         
            IF (uflag(i,j) == 0) THEN
            
               N_unknowns = N_unknowns + 1
               table(i,j) = N_unknowns
               
            ELSE IF (uflag(i,j) == 1) THEN
            
               N_knowns = N_knowns - 1
               table(i,j) = N_knowns
               
            END IF
            
         END DO
      END DO

! For debugging ------------------
!      write(*,'(a,I5)') 'Number of unknowns =',N_unknowns
!      DO i=1,node
!         DO j=1,dir
!            write(*,'("table ",I2,x,I2,3x,i3)') i,j,table(i,j)
!         END DO
!      END DO
! ----------------------------
      
   END IF

   ! Allocation of arrays of stiffness matrix and right-hand-side equivalent shear force vector
   ALLOCATE(Km(N_unknowns,N_unknowns), Fv(N_unknowns))

   ! Allocation of arrays of stiffness matrix for calculating unknown nodal fluxes 
   ! and unknown nodal reaction forces, and their vector themselves
   IF(N_knowns<0) THEN
      ALLOCATE( Kmm(-N_knowns,dir*node), Fvv(-N_knowns) )
   ELSE
      ALLOCATE( Kmm(3,3), Fvv(3) )
   END IF

   RETURN
END SUBROUTINE Preprocess
