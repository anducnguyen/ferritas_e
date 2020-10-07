SUBROUTINE StartingMessage
   IMPLICIT NONE

   ! -------------------------------------------------------------------------
   ! NAME:   StartingMessage
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Display starting messages in the console window.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   CHARACTER(LEN=8)  :: date      ! Date
   CHARACTER(LEN=10) :: time      ! Time
   CHARACTER(LEN=5)  :: zone      ! Zone for the Greenwich mean time
   CHARACTER(LEN=3),DIMENSION(12) :: & 
      mon=(/&
         'Jan','Feb','Mar','Apr','May','Jun', &
         'Jul','Aug','Sep','Oct','Nov','Dec'  &
      /)
   CHARACTER(len=2) :: day, hor, min, sec
   INTEGER :: val(8)
   
   ! Catch the date
   CALL DATE_AND_TIME(date,time,zone,val)
   
   WRITE(*,'(/,A)')   '======================================================'
   WRITE(*,'(/,A)')   '                   F E R I T A S '
   WRITE(*,'(A,/)')   '                      Ver.1.0'
   WRITE(*,'(A)')     'Welcome to FERITAS!'
   WRITE(*,'(A)')     'FERITAS is a finite element method solver developed by'
   WRITE(*,'(A)')     'Toshiro Matsumoto at Nagoya University.'
   WRITE(*,'(A)')     'Copyright 2009 Toshiro Matsumoto. All rights reserved.'
   WRITE(*,'(A,/)')   '======================================================'
   
   ! Setting the day in two digits
   IF(val(3)>9) then
      WRITE(day,'(I2)') val(3)
   ELSE
      ! Append '0' in front of the figure of the date when it is in one digit.
      WRITE(day,'(A1,I1)') '0',val(3) 
   END IF

   ! Setting the hours in two digits
   IF(val(5)>9) then
      WRITE(hor,'(I2)') val(5)
   ELSE
      WRITE(hor,'(A1,I1)') '0',val(5)
   END IF
   
   ! Setting the minutes in two digits
   IF(val(6)>9) then
      WRITE(min,'(I2)') val(6)
   ELSE
      WRITE(min,'(A1,I1)') '0',val(6)
   END IF
   
   ! Setting the seconds in two digits
   IF(val(7)>9) then
      WRITE(sec,'(I2)') val(7)
   ELSE
      WRITE(sec,'(A1,I1)') '0',val(7)
   END IF
   
   WRITE(*,'(A,A2,"/",A3,"/",I4,1X,A2,A,A2,A,A2,A,1X,A,A)') &
      'Computation started at ', &
      day, mon(val(2)), val(1),  &
      hor,'h:',min,'m:',sec,'s', zone,' GMT.'

   return
END SUBROUTINE StartingMessage
