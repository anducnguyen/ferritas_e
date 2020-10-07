SUBROUTINE CheckTime(stime,etime,ctime)
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   CheckTime
   ! STATUS: Current
   ! OWNER:  General
   ! TEXT:   Print CPU time.
   !         When called for the first time, stime should be set to 0.0 before
   !         calling.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   REAL :: stime   ! Starting time
   REAL :: ctime   ! Current time
   REAL :: etime   ! Elapsed time
   
   CALL CPU_TIME(ctime)
   
   IF(stime<=0.0) then
      stime = ctime
   ELSE
      etime = ctime - stime
      WRITE(*,'(A,E15.8)') '  Elasped time:', etime
   END IF
   
   RETURN
END SUBROUTINE CheckTime
   