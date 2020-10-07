SUBROUTINE BandWidth
   USE PrecTypes
   USE GlobalData
   IMPLICIT NONE
   ! -------------------------------------------------------------------------
   ! NAME:   BandWidth
   ! PARENT: Feritas
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Upper and lower band widths of the coefficient matrices of the 
   !         system of algebraic equations obtained before applying the 
   !         boundary conditions are calculated.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   INTEGER :: i, j, k, l, n
   INTEGER :: kmin, kmax
   INTEGER :: wmax, wmin, width

   wmax = 0
   
   DO i=1,node   ! Count the nodes
      kmin = dir * i
      kmax = 0
      DO j=1,ne   ! Count the element numbers
     
         ! Repeat for the number of nodes defining the element
         LOOP:DO k=1,etype(j)
            !  When the current node belongs to one of the nodes defining the current element
            IF(i==elm(j,k)) THEN 
               DO l=1,etype(j)
                  n = elm(j,l)
                  IF(kmin > dir*(n-1)+1)  kmin = dir*(n-1) + 1
                  IF(kmax < dir*n)        kmax = dir*n
               END DO 
               EXIT LOOP
            END IF
         END DO LOOP
         
      END DO

      width = kmax - kmin + 1

      IF(i==1) wmin = width
      IF(wmin < width) wmin = width
      IF(wmax > width) wmax = width
    
   END DO
     
   RETURN
   END SUBROUTINE BandWidth