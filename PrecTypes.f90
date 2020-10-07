MODULE PrecTypes
   IMPLICIT NONE
   
   ! -------------------------------------------------------------------------
   ! Name:   PrecTypes
   ! Status: Current
   ! Owner:  Public
   ! Text:   selected real/integer kind variables
   ! Revision Date: 16-Jul-2009
   ! -------------------------------------------------------------------------

   ! sigle precision real number
   INTEGER, PARAMETER :: sp  = SELECTED_REAL_KIND(p=6, r=37)
   
   ! double precision real number
   INTEGER, PARAMETER :: dp  = SELECTED_REAL_KIND(p=15, r=307)
   
   ! integer of 4-bytes
   INTEGER, PARAMETER :: i4b = SELECTED_INT_KIND (9)
   
   ! double precision complex number
   INTEGER, PARAMETER :: dpc = KIND((1.0_dp,1.0_dp))
   
   ! logical type
   INTEGER, PARAMETER :: lgt = KIND(.true.)

END MODULE PrecTypes
