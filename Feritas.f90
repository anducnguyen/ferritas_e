PROGRAM Feritas

   USE PrecTypes     !  Types of variables are defined
   USE GlobalData    !  All the global variables are defined
   IMPLICIT NONE
   ! -------------------------------------------------------------------------
   ! NAME:   Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   !         Department of Mechanical Science and Engineering
   !         Nagoya Univerisity
   !         Furo-cho, Chikusa-ku, Nagoya 464-8603, Japan
   !         E-mail: t.matsumoto@nuem.nagoya-u.ac.jp
   !         Copyright 2009 Toshiro Matsumoto. All rights reserved.
   ! TEXT:   Main program of FEM code Feritas for solutions of two-dimensional
   !         steady-state heat conduction problems and linear elastostatic and
   !         thermo-elastostatic problems.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   INTEGER :: i,j
   REAL :: stime,etime,ctime
   
   ! Show a starting message
   CALL StartingMessage
  
   ! Show the starting time
   stime = 0.0
   CALL CheckTime(stime,etime,ctime)
   
   ! Read input data
   WRITE(*,'(/,A)') '* Entry to [Input]'
   CALL Input
   CALL CheckTime(stime,etime,ctime)
      
   IF (meshCheck==1) THEN
   
      WRITE(*,'(/,A)') '* Entry to [EPSoutput] for generating mesh diagram'
      CALL EPSoutput
   
   ELSE
   
      ! Presprocessing (setting up flags, allocation of stiffness matrix, etc.)
      WRITE(*,'(/,A)') '* Entry to [Preprocess]'
      CALL Preprocess   
      CALL CheckTime(stime,etime,ctime)
   
     ! Calculate the band width
      WRITE(*,'(/,A)') '* Entry to [BandWidth]'
      CALL BandWidth
      CALL CheckTime(stime,etime,ctime)
      
      ! Construct the stiffness matrix
      WRITE(*,'(/,A)') '* Entry to [Matrix]'
      CALL Matrix
      CALL CheckTime(stime,etime,ctime)
      
      ! Solve the system of linear algebraic equations
      WRITE(*,'(/,A)') '* Entry to [Solve]'
      CALL Solve
      CALL CheckTime(stime,etime,ctime)
      
      ! Output the solutions
      WRITE(*,'(/,A)') '* Entry to [Output]'
      CALL Output
      CALL CheckTime(stime,etime,ctime)
      
      ! Graphical results are outputted to EPS files
      IF (dim==2) THEN
         WRITE(*,'(/,A)') '* Entry to [EPSoutput]'
         CALL EPSoutput
         CALL CheckTime(stime,etime,ctime)
      END IF
   
   END IF
   
   ! Show an ending message
   CALL EndingMessage
   
END PROGRAM Feritas
