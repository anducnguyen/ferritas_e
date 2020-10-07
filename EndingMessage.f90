SUBROUTINE EndingMessage

   ! -------------------------------------------------------------------------
   ! NAME:   EndingMessage
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Display ending messages.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   IMPLICIT NONE
   CHARACTER(LEN=8)  :: date
   CHARACTER(LEN=10) :: time
   CHARACTER(LEN=5)  :: zone
   CHARACTER(LEN=3),DIMENSION(12) :: &
      mon=(/&
         'Jan','Feb','Mar','Apr','May','Jun', &
         'Jul','Aug','Sep','Oct','Nov','Dec'  &
      /)
   CHARACTER(len=2) :: day, hor, min, sec
   INTEGER :: val(8)
   
   
   CALL DATE_AND_TIME(date,time,zone,val)
   
   WRITE(*,'(/,A)') 'End of job.'
   
   IF(val(3)>9) then
      WRITE(day,'(I2)') val(3)
   ELSE
      WRITE(day,'(A1,I1)') '0',val(3)
   END IF

   ! Hours are set in 2 digits.
   IF(val(5)>9) then
      WRITE(hor,'(I2)') val(5)
   ELSE
      WRITE(hor,'(A1,I1)') '0',val(5)
   END IF
   
   ! Minutes are set in 2 digits.
   IF(val(6)>9) then
      WRITE(min,'(I2)') val(6)
   ELSE
      WRITE(min,'(A1,I1)') '0',val(6)
   END IF
   
   ! Seconds are set in 2 digits.
   IF(val(7)>9) then
      WRITE(sec,'(I2)') val(7)
   ELSE
      WRITE(sec,'(A1,I1)') '0',val(7)
   END IF
   
   WRITE(*,'(A,A2,"/",A3,"/",I4,1X,A2,A,A2,A,A2,A,1X,A,A)') &
      'Computation ended at ', &
      day, mon(val(2)), val(1),  &
      hor,'h:',min,'m:',sec,'s', zone,' GMT.'

   WRITE(*,'(/A/)') 'Good-bye!'
   return
END SUBROUTINE EndingMessage
