SUBROUTINE Solve

   ! -------------------------------------------------------------------------
   ! NAME:   Solve
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Solve the system of algebraic equations and rearrange the obtained
   !         solutions to appropriate arrays to store the unknows.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   USE PrecTypes
   USE GlobalData

   IMPLICIT NONE

   INTEGER(i4b) :: i,j,k,l
   INTEGER(i4b) :: returnCode
   INTEGER(i4b), ALLOCATABLE :: iwk(:)
   REAL(dp), ALLOCATABLE ::  wk(:)
   REAL(dp), PARAMETER :: epsilon = 1.0e-14_dp

   ALLOCATE( wk(N_unknowns) )
   ALLOCATE( iwk(N_unknowns) )

   ! Solve the system [K]{u} = {F} by LU-decomposition method
   CALL LUfact(Km, N_unknowns, N_unknowns, Fv, epsilon, wk, iwk, returnCode)

   WRITE(*,'(A,I2)') '  Return from LU factorization: returnCode =',returnCode

   DEALLOCATE( wk )
   DEALLOCATE( iwk )
   
   SELECT CASE(pcode)
   
   ! Steady-state heat conduction problem
   CASE(1)
      ! ------------------------------------
      ! Copy the obtained temperature into the nodal temperature vector
      DO i=1,node
         IF(ptable(i) > 0) then
            T(i) = Fv(ptable(i))
         END IF
      END DO
         
      ! ------------------------------------
      ! Computation of unknown nodal heat fluxes
      DO i=1,node
         IF(ptable(i) < 0) then
            DO k=1,node
               Q(i) = Q(i) + Kmm(-ptable(i), k) * T(k)
            END DO
         END IF
      END DO

	! Output for debugging
	! write(*,'(a)') 'Nodal temperature'
	! do i=1,node
	!    write(*,'(I3,2x,3(E15.8,2x))')   i,T(i)
	! end do
	! write(*,'(a)') 'Boundary equivalent nodal heat flow'
	! do i=1,node
	!    if(bNodeFlag(i) == 1) then
	!       write(*,'("Q(",I3,")=",E15.8)') i,Q(i)
	!    end if
	! end do
   
   
   ! Linear elastostatic and thermo-elastostatic cases
   CASE(2,3)
      ! ------------------------------------
      ! Copy the obtained displacements into the nodal displacement vector
      DO i=1,node
         DO j=1,dir
            IF(table(i,j) > 0) then
               u(i,j) = Fv(table(i,j))
            END IF
         END DO
      END DO
         
      ! ------------------------------------
      ! Computation of unknown nodal reaction forces
      DO i=1,node
         DO j=1,dir
            IF(table(i,j) < 0) then
               DO k=1,node
                  DO l=1,dir
                     F(i,j) = F(i,j) + Kmm(-table(i,j), 2*k-2+l)*u(k,l)
                  END DO
               END DO
            END IF
            IF(table(i,j) < 0) F(i,j) = F(i,j) - Fvv(-table(i,j))
         END DO
      END DO

   ! Output for debugging
   !write(*,'(a)') 'Nodal displacements'
   !do i=1,node
   !   write(*,'(I3,2x,3(E15.8,2x))')   i,(u(i,j),j=1,dir)
   !end do
   !write(*,'(a)') 'Boundary equivalent nodal forces'
   !do i=1,node
   !   if(bNodeFlag(i) == 1) then
   !      do j=1,dir
   !         write(*,'("F(",I3,",",I3,")=",E15.8)') i,j,F(i,j)
   !      end do
   !   end if
   !end do
      
      ! ------------------------------------
      ! Computation of stresses and strains
      CALL StressStrain
      
   END SELECT
      
   RETURN
    
END SUBROUTINE Solve
