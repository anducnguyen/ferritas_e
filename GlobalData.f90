MODULE GlobalData
   USE PrecTypes
   IMPLICIT NONE
   
   ! -------------------------------------------------------------------------
   ! NAME:   GlobalData
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Globally defined variables used by "Feritas"
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   ! Input/output unit numbers
   INTEGER, PARAMETER :: idev=7, odev=8, edev=9, ddev=10, cdev=11
   
   ! Input file name
   CHARACTER(len=9)   :: iFileName='input.txt'
    
   ! Output file name                  
   CHARACTER(len=10)  :: oFileName='output.txt'
   
   ! File for storing deformation diagram               
   CHARACTER(len=10)  :: deformDataFileName='deform.txt'
   
   ! File for outputting mesh data
   CHARACTER(len=8)   :: meshDataFileName='mesh.txt'
   
   ! File for outputting temperature and stress distributions data
   CHARACTER(len=11)  :: contourDataFileName='contour.txt'
   
   ! File for outputting mesh diagram
   CHARACTER(len=8)   :: meshFileName='mesh.eps'
   
   ! EPS file for outputting temperature distribution diagram
   CHARACTER(len=15)  :: tempFileName='temperature.eps'
   
   ! EPS file for outputting deformation diagram
   CHARACTER(len=16)  :: dispFileName='displacement.eps'
   
   ! EPS file for outputting von Mises stress distribution diagram
   CHARACTER(len=9)   :: misesFileName='mises.eps'
   
   ! File for storing attribute data for drawing diagrams
   CHARACTER(len=13)  :: attrFileName='attribute.txt'
 
   INTEGER :: meshCheck ! Element numbering check is done when meshCheck=1
   INTEGER :: nm                         ! Number of materials
   REAL(dp), ALLOCATABLE :: lambda(:,:)  ! Thermal conductivity X materials
   REAL(dp), ALLOCATABLE :: E(:)         ! Young's modulus X materials
   REAL(dp), ALLOCATABLE :: v(:)         ! Poisson's ratio X materials
   REAL(dp), ALLOCATABLE :: alpha(:)     ! Linear thermal expansion coefficients
   
   ! Problem code (1: steady-state heat conduction, 2: elastostatics, 3: thermoelastostatic)
   INTEGER :: pcode
   INTEGER :: plane    ! Plane strain: 1, Plane stress 2

   INTEGER :: dim      ! Problem dimension (2 for 2d, 3 for 3d)
   INTEGER :: dir      ! dimension of function u (1 for potential, 2 or 3 for elasticity)
   INTEGER :: node     ! Number of nodes
   REAL(dp), ALLOCATABLE, dimension(:) :: x    ! x coordinate of nodes
   REAL(dp), ALLOCATABLE, dimension(:) :: y    ! y coordinate of nodes
   REAL(dp), ALLOCATABLE, dimension(:) :: z    ! z coordinate of nodes

   INTEGER :: ne                         ! Total number of elements
   INTEGER, PARAMETER :: maxnode = 4     ! Maximum number of nodes of each element
   INTEGER,  ALLOCATABLE :: elm(:,:)     ! List of node numbers constituting each element
   REAL(dp), ALLOCATABLE :: thickness(:) ! Thickness of each element
   INTEGER,  ALLOCATABLE :: emtype(:)    ! Material type for each element
   
   ! Element type of each element (depending on the node numbers: 3, 4, 6, 8, 9, ...)
   INTEGER,  ALLOCATABLE :: etype(:)     
   
   INTEGER :: ng                         ! Total number of element groups
   TYPE ElementGroup                     ! Type definition of element group
      INTEGER :: ne                      ! Total number of elements belonging to this element group
      INTEGER, ALLOCATABLE :: e(:)       ! Element number list belonging to this element group
   END TYPE elementGroup
   TYPE (elementGroup), ALLOCATABLE :: egrp(:)  ! An element group object

   INTEGER :: ns                         ! Total number of boundary elements
   INTEGER,  ALLOCATABLE :: Stype(:)     ! Boundary element type code
   INTEGER,  ALLOCATABLE :: Sid(:,:)     ! Surface element ID and nodes belonging to the element
   INTEGER,  ALLOCATABLE :: bcc(:,:,:)   ! Boundary condition code (element number，local node number, direction)
   REAL(dp), ALLOCATABLE :: bcv(:,:,:)   ! Boundary condition value (element number，local node number, direction)
   INTEGER,  ALLOCATABLE :: bccode(:,:)  ! Boundary condition code (element number，local node number)
   REAL(dp), ALLOCATABLE :: bcval(:,:)   ! Boundary condition value (element number，local node number) 
   REAL(dp), ALLOCATABLE :: u(:,:)       ! Nodal displacement (node number, direction)
   INTEGER,  ALLOCATABLE :: uflag(:,:)   ! Flag to indicate that the nodal displacement is known or unknown
!   REAL(dp), ALLOCATABLE :: u(:)        ! Nodal displacement or potential: just for a possibility to modify
   INTEGER,  ALLOCATABLE :: tflag(:)     ! Flag for indicating if the nodal temperature is known or unknown
   REAL(dp), ALLOCATABLE :: T(:)         ! Nodal temperature
   REAL(dp), ALLOCATABLE :: F(:,:)       ! Nodal force
   REAL(dp), ALLOCATABLE :: Q(:)         ! Nodal heat flux
   INTEGER, ALLOCATABLE  :: bNodeFlag(:) ! Flag for indicating if the node is on the boundary or not
   ! Table indicating the column number of K and M matrices for elastostatic and thermo-elastostatic cases
   INTEGER,  ALLOCATABLE :: table(:,:)
   ! Table indicating the column number of K matrix for steady-state heat conduction case
   INTEGER,  ALLOCATABLE :: ptable(:)
   INTEGER :: N_unknowns, N_knowns       ! Number of unknowns and number of knowns
   
   REAL(dp), ALLOCATABLE :: stress(:,:,:) ! Stress components of each element (node number, number of nodes, components)
                                          ! 　σx, σy, σz, τxy, τyz, τzx (three-dimensional case)
                                          ! 　σx, σy, τxy, σz (plane strain state)
                                          ! 　σx, σy, τxy (plane stress state)
   REAL(dp), ALLOCATABLE :: strain(:,:,:) ! Strain components of each element (node number, number of nodes, components)
                                          ! 　εx, εy, εz, γxy, γyz, γzx (three-dimensional case)
                                          ! 　εx, εy, γxy (plane strain state)
                                          ! 　εx, εy, γxy, εz (plane stress state)
   REAL(dp), ALLOCATABLE :: Mises(:,:)    ! von Mises stress (element number, number of points for output from the element)
   
   INTEGER, PARAMETER :: uKNOWN=0        !, tKNOWN=1
   INTEGER, PARAMETER :: gausspoint=2    ! Number of Gauss points used for numerical integration

   REAL(dp), ALLOCATABLE :: Km(:,:)      ! Stiffness matrix [K]
   REAL(dp), ALLOCATABLE :: Mm(:,:)      ! Mass matrix [M]
   REAL(dp), ALLOCATABLE :: Cm(:,:)      ! Damping matrix [C]
   REAL(dp), ALLOCATABLE :: Fv(:)        ! Equivalent shear force vector {F}
   
   REAL(dp) :: Ktrig_p(3,3)              ! Element stiffness matrix for triangular linear element (potential problem)
   REAL(dp) :: Krect_p(4,4)              ! Element stiffness matrix for rectilinear element (potential problem)
   REAL(dp) :: Ktrig(3,3,2,2)            ! Element stiffness matrix for triangular linear element (elastostatic problem)
   REAL(dp) :: Krect(4,4,2,2)            ! Element stiffness matrix for rectilinear element (elastostatic problem)
   
   REAL(dp) :: Thtrig(3,2,3)             ! Matrix for storing the effect by thermal strains for triangular linear element
   REAL(dp) :: Threct(4,2,4)             ! Matrix for storing the effect by thermal strains for rectilinear element
   
   REAL(dp) :: Flinear_p(2,2)            ! Local nodal force vector for straight linear element (potential problem)
   REAL(dp) :: Fquad_p(3,3)              ! Local nodal force vector for straight quadratic element (potential problem)
   REAL(dp) :: Flinear(2,2,2)            ! Local nodal force vector for straight linear element (elastostatic problem)
   REAL(dp) :: Fquad(3,3,2)              ! Local nodal force vector for straight quadratic element (elastostatic problem)
   
   REAL(dp), ALLOCATABLE :: Kmm(:,:)     ! Sub stiffness matrix for calculating the reaction nodal forces
   REAL(dp), ALLOCATABLE :: Fvv(:)       ! Reaction force vector to store the effect of thermal strains
   
   INTEGER, ALLOCATABLE :: bandmap(:,:)
    
END MODULE GlobalData
