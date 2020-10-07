MODULE EPS_sharedModule
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! Name:   EPS_sharedModule
   ! Parent: EPSoutput, EPS_mesh, EPS_temp, EPS_disp, EPS_mises
   ! Status: Current
   ! Owner:  Toshiro Matsumoto
   ! Text:   Module to define various control parameters to generate 
   !         illustrations in PostScript format.
   ! Revision Date: 16-Jul-2009
   ! -------------------------------------------------------------------------
     
   REAL(dp), PARAMETER  :: inch = 25.4_DP                  !   1.0 inch = 25.4 mm
   REAL(dp), PARAMETER  :: point = 0.352778_DP             !   1.0 point = 1/72 inch = 0.352778 mm
   REAL(dp), PARAMETER  :: deg = 0.0174532925199433_DP     !   1 degree in radian
   REAL(dp), PARAMETER  :: rad2deg = 57.2957795130824_DP   !   1 radian in degree
   REAL(dp) :: xMargin = 20.0_DP                           !   Margin of viewport
   REAL(dp) :: yMargin = 20.0_DP                           !   Margin of viewport
   CHARACTER(len=13) :: RomanItalic='/Times-Italic'        !   Default italic font
   CHARACTER(len=12) :: Roman='/Times-Roman'               !   Default roman font
   CHARACTER(len=11) :: RomanBold='/Times-Bold'            !   Default roman boldface font
   CHARACTER(len=18) :: GothicOblique='/Helvetica-Oblique' !   Default oblique sanserif font
   CHARACTER(len=10) :: Gothic='/Helvetica'                !   Default sanserif font
   CHARACTER(len=15) :: GothicBold='/Helvetica-Bold'       !   Default sanserif boldface font
   INTEGER(i4b) :: ItalicFontSize=9                        !   Default font size of italic font
   INTEGER(i4b) :: RomanFontSize=9                         !   Default font size of roman font
   INTEGER(i4b) :: BoldFontSize=9                          !   Default font size of roman boldface font

   REAL(dp) :: xmin, ymin, xmax, ymax                      ! Max and min values of x and y coordinates
   REAL(dp) :: xscale                                      ! Scale of the coordinates when the nodes are drawn
   REAL(dp) :: umax, vmax                                  ! Max absolute value of displacements
   REAL(dp) :: uscale                                      ! Scale of the displacement components when drawn
   REAL(dp) :: scaledUmax=30.0, scaledVmax=30.0            ! Max value of dispalcement when drawn
   REAL(dp) :: objectWidth, objectHeight                   ! Actual sizes of width and height of the object
   REAL(dp) :: ox=10.0, oy=10.0                            ! offset of the origin when the object is drawn
   REAL(dp) :: scaledWidth=400.0, scaledHeight=400.0       ! Sizes of width and height of the object when drawn
   REAL(dp) :: boxWidth, boxHeight                         ! Width and height of the bounding box of the EPS output
   REAL(dp), ALLOCATABLE :: px(:), py(:)                   ! Coordinates of the nodes when they are drawn
   REAL(dp), ALLOCATABLE :: ux(:), uy(:)                   ! Coordinates of the displacements of the nodes when they are drawn
   REAL(dp), ALLOCATABLE :: pux(:), puy(:)                 ! Coordinates of the nodes after deformation when they are drawn
   REAL(dp), ALLOCATABLE :: xg(:), yg(:)                   ! Actual coordinates of the centers of geometry of elements
   REAL(dp), ALLOCATABLE :: pxg(:), pyg(:)                 ! Coordinates of the centers of geometry of elements when drawn
   REAL(dp) :: tmax, tmin, misesmax, misesmin              ! Max and min value of the temperature and von Mises stress
   INTEGER :: displayRelative = 1                          ! Displayed values become relative when this parameter is 1
   REAL(dp) :: gtmax=100.0, gtmin=0.0                      ! Max and min values of the temperature corresponding to color range
   REAL(dp) :: gmisesmax=1.0e7, gmisesmin=0.0              ! Max and min values of the Mises stress corresponding to color range
   INTEGER :: boundLBx, boundLBy, boundTRx, boundTRy       ! Coordinates of the bounding box
   REAL(dp) :: thinLine = 0.25                             ! Line thickness of thin line
   REAL(dp) :: midLine = 0.50                              ! Line thickness of medium line
   REAL(dp) :: thickLine = 0.50                            ! Line thickness of thick line
   REAL(dp) :: offxg = -2.0,  offyg = -5.0                 ! Offset of element number when it is drawn
   REAL(dp) :: offx  =  5.0,  offy  = -7.0                 ! Offset of node number when it is drawn
   REAL(dp) :: meshLineThickness = 0.5                     ! Line thickness used for drawing element shapes
   INTEGER(i4b) :: elementNumberSwitch = 1                 ! Switch for displaying element numbers (1 or 0)
   INTEGER(i4b) :: nodeNumberSwitch = 1                    ! Switch for displaying node numbers (1 or 0)
   INTEGER(i4b) :: elementColorSwitch = 1                  ! Switch for filling elements (1 or 0)
   INTEGER(i4b) :: malElementColorSwitch = 1               ! Switch for filling elements (1 or 0) when numbered wrongly
   REAL(dp) :: elementColor(3) =(/0.9, 0.9, 0.9/)          ! Element color used for mesh diagram
   REAL(dp) :: malElementColor(3) =(/0.5, 0.0, 0.0/)       ! Element color used when numbered wrongly
   REAL(dp) :: meshLineColor(3) =(/0.0, 0.0, 0.0/)         ! Line color used for element shape
   REAL(dp) :: elNumberPhontSize = 8.0                     ! Font size used for element numbering
   REAL(dp) :: elNumberPhontColor(3) =(/0.9, 0.0, 0.0/)    ! Font color used for element numbering
   REAL(dp) :: ndNumberPhontSize = 7.0                     ! Font size used for node numbering
   REAL(dp) :: ndNumberPhontColor(3) =(/0.0, 0.0, 0.9/)    ! Font color used for node numbering
   REAL(dp) :: colorChartPhontSize = 8.0                   ! Font size used for color chart
   INTEGER(i4b) :: dispMeshShowSwitch = 1                  ! Switch if mesh is displayed together with deformation (1 or 0) 
   REAL(dp) :: dispMeshLineThickness = 0.5                 ! Line thickness of the mesh used for displaying deformation diagram
   REAL(dp) :: dispLineThickness = 1.0                     ! Line thickness used for displaying deformation
   REAL(dp) :: dispElmColor(3) =(/0.9, 0.9, 0.9/)          ! Color used for filling elements before defomation
   REAL(dp) :: dispMeshLineColor(3) =(/0.5, 0.5, 0.5/)     ! Color used for displaying mesh of the deformation diagram
   REAL(dp) :: dispLineColor(3)     =(/0.9, 0.0, 0.0/)     ! Color used for the deformed shape
   INTEGER(i4b) :: mizesMeshShowSwitch = 1                 ! Switch if the mesh is displayed on the Mises diagram (1 or 0)          
   REAL(dp) :: elNumberColor(3) =(/0.9, 0.0, 0.0/)         ! Color used for showing element numbers
   REAL(dp) :: ndNumberColor(3) =(/0.0, 0.0, 0.9/)         ! Color used for showing node numbers
   INTEGER(i4b) :: linejoin=2  ! Treatment of corners (0: miter joint, 1: round joint, 2: bevel joint)                           
   ! Stores node numbers of each edge and flag if the edge has been drawn or not
   INTEGER(i4b), ALLOCATABLE :: eline(:,:,:)
   
   ! Point values (0.0 --- 1.0) which define the color scheme          
   REAL(dp) :: pt(11)  = (/0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0/)  

   ! Color scheme
   ! Blending values of RED color from minimum to maximum
   REAL(dp) :: red(11) = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 1.0, 1.0, 1.0, 1.0/)   
   ! Blending values of GREEN color from minimum to maximum
   REAL(dp) :: grn(11) = (/0.0, 0.0, 0.2, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.8, 0.0/)   
   ! Blending values of BLUE color from minimum to maximum
   REAL(dp) :: blu(11) = (/0.5, 1.0, 1.0, 1.0, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/)   
   REAL(dp) :: overred  = 1.0
   REAL(dp) :: overgrn  = 0.5
   REAL(dp) :: overblu  = 1.0
   REAL(dp) :: underred = 0.1
   REAL(dp) :: undergrn = 0.1
   REAL(dp) :: underblu = 0.1
   
   INTERFACE 
      FUNCTION fn(color,pt,x)
         USE PrecTypes
         IMPLICIT NONE
         REAL(dp), INTENT(IN) :: color(11),pt(11)
         REAL(dp), INTENT(IN) :: x
         REAL(dp) :: fn
      END FUNCTION fn
   END INTERFACE


END MODULE EPS_sharedModule
