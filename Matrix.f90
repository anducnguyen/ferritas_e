SUBROUTINE Matrix
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Matrix
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Construct stiffness matrix and nodal force vector.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------
  
   INTEGER :: i,j
   
   ! -------------------------------------------------------------------------
   ! Matrix map (only for debugging)
   character(len=1), allocatable :: im(:,:)
   ! -------------------------------------------------------------------------

   Km = 0.0_dp  ! Initialize the stiffness matrix
   Fv = 0.0_dp  ! Initialize the equivalent nodal force vector
   Kmm = 0.0_dp ! Initialize the sub stiffness matrix for calculating the nodal reaction force
   Fvv = 0.0_dp ! Initialize the equivalent nodal force vector for calculating the nodal reaction force

   SELECT CASE(pcode)
   
      ! Potential problem --------------------
      CASE (1) 
         
         DO i=1,ne  ! Counting the current element

            ! Computation of element stiffness matrix            
            SELECT CASE( etype(i) )
               CASE (3) ! Triangular linear element
            
                  ! Computation of element stiffness matrix of triangular linear elmenet
                  CALL Mat_Poten2TrigLinear(i)
                  
               case (4) ! Rectilinear element
            
                  ! Computation of element stiffness matrix of triangular rectilinear elmenet
                  CALL Mat_Poten2RectLinear(i)
               
            END SELECT

         END DO ! <-- i=1,ne

         ! Just for debugging ----------------
         if (node <= 60) then
            allocate(im(node,node))
            do i=1,node
               do j=1,node
                  if(Km(i,j) == 0.0_dp) then
                     im(i,j) = '.'
                  elseif(Km(i,j) > 0.0_dp) then
                     im(i,j) = 'o'
                  else 
                     im(i,j) = 'x'
                  end if
               end do
            end do
            
            do i=1,N_unknowns
               write(*,'(18(1x,a))') (im(i,j),j=1,N_unknowns)
            end do
         end if
         !---------------------------
         
         ! Equivalent nodal heat vector on the right-hand side of the system of equations
         DO i=1,ns   ! Repeating for the number of boundary elements
         
            SELECT CASE( Stype(i) )
               ! Straight linear elements (corresponding to the edges of triangles and rectangles)
               CASE (2)
                  ! Computations of right-hand side vector for linear boundary edge elements
                  CALL F_Poten2Linear(i)
               
            END SELECT
             
         END DO  ! <-- i=1,ns
      
      ! Linear elastostatic problems-------------------------
      CASE (2,3) 
         
         DO i=1,ne  ! Counting the current element

            ! Computation of element stiffness matrix              
            SELECT CASE( etype(i) )
               CASE (3) ! Triangular linear element
            
                  ! Computation of element stiffness matrix of triangular linear elmenet
                  CALL Mat_Elast2TrigLinear(i)
                  
               CASE (4) ! Rectilinear element
            
                  ! Computation of element stiffness matrix of triangular rectilinear elmenet
                  CALL Mat_Elast2RectLinear(i)
               
            END SELECT

         END DO ! <-- i=1,ne

         ! Just for debugging ----------------
         if (node<=60) then
            allocate(im(dir*node,dir*node))
            do i=1,dir*node
               do j=1,dir*node
                  if(Km(i,j) == 0.0_dp) then
                     im(i,j) = '.'
                  elseif(Km(i,j) > 0.0_dp) then
                     im(i,j) = 'o'
                  else 
                     im(i,j) = 'x'
                  end if
               end do
            end do
            
            do i=1,N_unknowns
               write(*,'(18(1x,a))') (im(i,j),j=1,N_unknowns)
            end do
         end if
         !---------------------------
         
         ! Equivalent nodal force vector on the right-hand side of the system of equations
         DO i=1,ns   ! Repeating for the number of boundary elements
         
            SELECT CASE( Stype(i) )
               ! Straight linear elements (corresponding to the edges of triangles and rectangles)
               CASE (2)
                  ! Computations of right-hand side vector for linear boundary edge elements
                  CALL F_Elast2Linear(i)
               
            END SELECT
             
         END DO  ! <-- i=1,ns

   END SELECT
   
   RETURN
END SUBROUTINE Matrix
