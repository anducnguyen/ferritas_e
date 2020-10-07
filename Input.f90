SUBROUTINE Input
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   Input
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Read geometry data, material parameters, boundary conditions.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   INTEGER :: i,j,k,l
   
   OPEN (UNIT=idev, FILE=iFileName, STATUS='OLD', ACTION='READ', ERR=100)

   ! For checking connectivities of elements, set as 1. This works only for two-dimensional problems.
   READ(idev,*) meshCheck

   ! Problem dimensionality: 2 or 3 (but the code does not solve 3-d problems)
   READ(idev,*) dim 

   !--------------------------------------
   ! Problem code (1: steady-state heat conduction, 2: elastostatic, 3: thermo-elastostatic)
   READ(idev,*) pcode 
   IF(pcode==1) THEN
      dir = 1
   ELSE IF(pcode==2 .OR. pcode==3) THEN
      dir = dim
      IF (dim == 2) then     ! For two-dimensional problem
         READ(idev,*) plane  ! 1 for plane strain state, 2 for plane stress state
      END IF
   END IF
   
   !--------------------------------------------------------------------
   READ(idev,*) nm                ! Total number of materials
   !--------------------------------------------------------------------
   ! Read material properties
   SELECT CASE(pcode)  
    
      ! Potential problem
      CASE (1)
         ! Memory allocation for the array storing thermal conductivities
         ALLOCATE(lambda(nm,dim)) 
         
         ! Read thermal conductivities
         DO i=1,nm
            READ(idev,*) j, (lambda(j,k),k=1,dim)
         END DO
      
      ! Elastostatic problem
      CASE (2)
         ! Memory allocation for the array storing Young's moduli and Poisson's ratios
         ALLOCATE(E(nm),v(nm))
         
         ! Read Young's moduli and Poisson's ratios
         DO i=1,nm
            READ(idev,*) j, E(j), v(j)
            
            ! When a wrong Poisson's ratio is read, the process is terminated.
            IF(v(j) == 0.5_dp .OR. v(j) == 1.0_dp) THEN
               WRITE(*,10) "  *** Wrong value is specified for Poisson's ratio.", &
               'Material number: ', j, "Poisson's ration", v(j)
               10 format(/,A,/,A,I3,3X,A,E15.8)
               STOP
            END IF
            
            IF (plane == 2) THEN
               v(j) = v(j)/(1.0_dp+v(j))
               E(j) = (1.0_dp - v(j)*v(j))*E(j)
            END IF
         END DO

      ! Linear thermo-elastostatic problem
      CASE (3)
         ! Memory allocation for storing Young's moduli, Poisson's ratios, and linear 
         ! thermal expansion coefficients
         ALLOCATE(E(nm),v(nm),alpha(nm)) 
         
         ! Read Young's moduli, Poisson's ratios, and linear thermal expansion coefficients
         DO i=1,nm
            READ(idev,*) j, E(j), v(j), alpha(j)
            ! For plane stress state, Poisson's ratios and Young's moduli are converted.
            IF (plane == 2) THEN
               v(j) = v(j)/(1.0_dp+v(j))
               E(j) = (1.0_dp - v(j)*v(j))*E(j)
               alpha(j) = alpha(j)/(1+v(j))
            END IF
         END DO

   END SELECT
   !--------------------------------------------------------------------
   READ(idev,*) node                     ! Total number of nodes
   
   ALLOCATE(x(node), y(node), z(node))
   z=0.0                                 ! Just for safety for 2D case.
   
   ! Steady-state heat conduction problem
   IF (pcode==1) THEN
      ALLOCATE( T(node), tflag(node), ptable(node), bNodeFlag(node), Q(node) )
      tflag     =   0
      T         =   0.0_dp
      bNodeFlag =   0
      Q         =   0.0_dp
   ! Linear elastostatic problem
   ELSE IF (pcode==2) THEN
      ALLOCATE( u(node,dir), uflag(node,dir), table(node,dir), bNodeFlag(node), F(node,dir) )
      uflag     =   0
      u         =   0.0_dp
      bNodeFlag =   0
      F         =   0.0_dp
   ! Linear thermo-elastostatic problem
   ELSE IF (pcode==3) THEN
      ALLOCATE( u(node,dir), uflag(node,dir), table(node,dir), bNodeFlag(node), F(node,dir), T(node) )
      uflag     =   0
      u         =   0.0_dp
      bNodeFlag =   0
      F         =   0.0_dp
      T         =   0.0_dp
   END IF

   !--------------------------------------------------------------------
   ! Read nodal coordinates
   SELECT CASE (dim) 
      CASE(2)   ! For two-dimensional case
         DO i=1,node
            READ(idev,*) j, x(j), y(j)
         END DO
      CASE(3)   ! For three-dimensional case
         DO i=1,node
            READ(idev,*) j, x(j), y(j), z(j)
         END DO
   END SELECT
   !--------------------------------------------------------------------
   ! Read total number of elements
   READ(idev,*) ne
   
   ALLOCATE( etype(ne), emtype(ne), elm(ne,maxnode) )
   ALLOCATE( thickness(ne) )
   
   ! For two-dimensional case
   IF (dim == 2) THEN
      ALLOCATE( strain(ne,maxnode,4) )
      ALLOCATE( stress(ne,maxnode,4) )
      ALLOCATE( Mises(ne,maxnode)    )
      
   ! For three-dimensional case (not installed yet, just for future extension)
   ELSE IF (dim == 3) THEN
      ALLOCATE( strain(ne,maxnode,6) )
      ALLOCATE( stress(ne,maxnode,6) )
      ALLOCATE( Mises(ne,maxnode)    )
   END IF
   !--------------------------------------------------------------------
   ! Two-dimensional elastostatic or thermo-elastostatic case. 
   ! Element thicknesses are also inputted. 
   IF((pcode == 2 .OR. pcode == 3) .AND. dim == 2) THEN
   
      DO i=1,ne
         READ(idev,*) j,etype(j), emtype(j), (elm(j,k),k=1,etype(j)), thickness(j)
      END DO
      
   ! Two-dimensional steady-state heat conduction. 
   ! Element thicknesses are also inputted. 
   ELSE IF (pcode == 1) THEN                  
   
      DO i=1,ne
         READ(idev,*) j,etype(j), emtype(j), (elm(j,k),k=1,etype(j)), thickness(j)
      END DO
      
   END IF
   !--------------------------------------------------------------------
   ! Read total number of element groups
   READ(idev,*) ng
   
   IF (ng > 0) THEN
      ! Memory allocation of an array in which the number of elements for each element group.
      ALLOCATE(egrp(ng)) 
      !--------------------------------------------------------------------
      DO i=1,ng
         READ(idev,*) egrp(i)%ne          ! Read the number of elements belonging to element group 'i'
         
         ALLOCATE( egrp(i)%e( egrp(i)%ne ) ) 
         
         DO j=1,egrp(i)%ne
            READ(idev,*) k,egrp(i)%e(k)   ! Read element numbers for the element group 'i'
         END DO
      END DO
   END IF
   !--------------------------------------------------------------------
   ! Total number of boundary elements
   READ(idev,*) ns
   ! Memory allocation for the arrays storing boundary element types 
   ! and node numbers constituting the boundary elements.
   ! Note that the maximum number of nodes constituting each boundary elements.
   ALLOCATE(stype(ns), sid(ns,4))
   
   ! Steady-state heat conduction problem        　
   IF (pcode==1) THEN
      ! Memory allocation for boundary condition codes and boundary condition values                   
      ALLOCATE(bccode(ns,4), bcval(ns,4)) 
      
   ! Elastostatic or thermo-elastostatic case
   ELSE IF (pcode==2 .OR. pcode==3) THEN 
   
      ! Memory allocation for boundary condition codes and boundary condition values                   
      ALLOCATE(bcc(ns,4,dir), bcv(ns,4,dir))
   END IF
   !--------------------------------------------------------------------
   DO i=1,ns
      READ(idev,*) j, stype(j), (sid(j,k),k=1,stype(j))
   END DO
   !--------------------------------------------------------------------
   ! When only a mesh connectivity checking, the execution is terminated here.
   IF (meshCheck==1) RETURN
   
   !--------------------------------------------------------------------
   ! Boundary condition codes and boundary condition values are read.
   IF(pcode==1) THEN
      DO i=1,ns
         READ(idev,*) j, (bccode(j,k), bcval(j,k), k=1,stype(j))
      END DO
   ELSE IF (pcode==2 .OR. pcode==3) THEN
      DO i=1,ns
         READ(idev,*) j, ((bcc(j,k,l), bcv(j,k,l),l=1,dir), k=1,stype(j))
      END DO
   END IF
   !-------------------------------------------------------
   ! Nodal temperatures are read for thermo-elastostatic problem.
    IF (pcode==3) THEN
      DO i=1,node
         READ(idev,*) j, T(j)
      END DO
   END IF
   !--------------------------------------------------------------------
      
   ! Returned from this subroutine here for normal execution.
   CLOSE(idev)  ! 入力ファイルをクローズ
      
   RETURN

   ! Processing when file opening errors take place
   100 CONTINUE
   ! Reporting the error.
   WRITE(*,'(A)') 'Error in opening input file'
   WRITE(*,'(A,A)') 'Error, cannot open file: ', iFileName
   STOP
   
END SUBROUTINE Input
