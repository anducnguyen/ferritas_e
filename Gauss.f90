MODULE Gauss

   USE PrecTypes
   IMPLICIT NONE 

   ! -------------------------------------------------------------------------
   ! Name:   Gauss
   ! Parent: General
   ! Status: Current
   ! Owner:  Toshiro Matsumoto
   ! Text:   Coordinates and weights of Gaussian quadrature formula.
   ! Revision Date: 16-Jul-2009
   ! -------------------------------------------------------------------------
   
   ! Offset numbers used in the arrays zt() and wt()
   INTEGER(I4B) :: ngs(6) =(/ 0, 0, 2, 5, 9, 14 /)
   
   ! Coordinates of the integration points
   REAL(DP) :: zt(20) = (/   &
      !
      ! 2-point formula
      -0.577350269189626E0_DP,  &  ! = zt( 1 + ngs(2) )
       0.577350269189626E0_DP,  &  ! = zt( 2 + ngs(2) )
      !
      ! 3-point formula
      -0.774596669241483E0_DP,  &  ! = zt( 1 + ngs(3) )
       0.000000000000000E0_DP,  &  ! = zt( 2 + ngs(3) )
       0.774596669241483E0_DP,  &  ! = zt( 3 + ngs(3) )
      !
      ! 4-point formula
      -0.861136311594053E0_DP,  &  ! = zt( 1 + ngs(4) )
      -0.339981043584856E0_DP,  &  ! = zt( 2 + ngs(4) )
       0.339981043584856E0_DP,  &  ! = zt( 3 + ngs(4) )
       0.861136311594053E0_DP,  &  ! = zt( 4 + ngs(4) )
      !
      ! 5-point formula
      -0.906179845938664E0_DP,  &  ! = zt( 1 + ngs(5) )
      -0.538469310105683E0_DP,  &  ! = zt( 2 + ngs(5) )
       0.000000000000000E0_DP,  &  ! = zt( 3 + ngs(5) )
       0.538469310105683E0_DP,  &  ! = zt( 4 + ngs(5) )
       0.906179845938664E0_DP,  &  ! = zt( 5 + ngs(5) )
      !
      ! 6-point formula
      -0.932469514203152E0_DP,  &  ! = zt( 1 + ngs(6) )
      -0.661209386466265E0_DP,  &  ! = zt( 2 + ngs(6) )
      -0.238619186083197E0_DP,  &  ! = zt( 3 + ngs(6) )
       0.238619186083197E0_DP,  &  ! = zt( 4 + ngs(6) )
       0.661209386466265E0_DP,  &  ! = zt( 5 + ngs(6) )
       0.932469514203152E0_DP   &  ! = zt( 6 + ngs(6) )
   /)

   !  Weights to multiply to the function values at the integration points
   REAL(DP) :: wt(20) =(/   &
      ! 2-point formula
      0.100000000000000E1_DP,  &  ! = wt( 1 + ngs(2) )
      0.100000000000000E1_DP,  &  ! = wt( 2 + ngs(2) )
      !
      ! 3-point formula
      0.555555555555556E0_DP,  &  ! = wt( 1 + ngs(3) )
      0.888888888888889E0_DP,  &  ! = wt( 2 + ngs(3) )
      0.555555555555556E0_DP,  &  ! = wt( 3 + ngs(3) )
      !
      ! 4-point formula
      0.347854845137454E0_DP,  &  ! = wt( 1 + ngs(4) )
      0.652145154862546E0_DP,  &  ! = wt( 2 + ngs(4) )
      0.652145154862546E0_DP,  &  ! = wt( 3 + ngs(4) )
      0.347854845137454E0_DP,  &  ! = wt( 4 + ngs(4) )
      !
      ! 5-point formula
      0.236926885056189E0_DP,  &  ! = wt( 1 + ngs(5) )
      0.478628670499366E0_DP,  &  ! = wt( 2 + ngs(5) )
      0.568888888888889E0_DP,  &  ! = wt( 3 + ngs(5) )
      0.478628670499366E0_DP,  &  ! = wt( 4 + ngs(5) )
      0.236926885056189E0_DP,  &  ! = wt( 5 + ngs(5) )
      !
      ! 6-point formula
      0.171324492379170E0_DP,  &  ! = wt( 1 + ngs(6) )
      0.360761573048139E0_DP,  &  ! = wt( 2 + ngs(6) )
      0.467913934572691E0_DP,  &  ! = wt( 3 + ngs(6) )
      0.467913934572691E0_DP,  &  ! = wt( 4 + ngs(6) )
      0.360761573048139E0_DP,  &  ! = wt( 5 + ngs(6) )
      0.171324492379170E0_DP   &  ! = wt( 6 + ngs(6) )
   /)

END MODULE Gauss
