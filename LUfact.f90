SUBROUTINE LUfact(a,n,ndim,b,epsilon,work,ipiv,returnCode)

   ! -------------------------------------------------------------------------
   ! NAME:   LUfact
   ! PARENT: Solve
   ! STATUS: Current
   ! OWNER:  Toshiro Matsumoto
   ! TEXT:   Called from subroutine 'Solve'. Solve systems of linear algebraic
   !         equations by LU factorization method.
   ! REVISION DATE: 16-Jul-2009
   ! -------------------------------------------------------------------------

   USE PrecTypes
   IMPLICIT NONE
   
   INTEGER, INTENT(IN) :: n,ndim
   INTEGER, INTENT(INOUT) :: ipiv(*)
   REAL(dp), INTENT(INOUT) :: epsilon
   REAL(dp), INTENT(INOUT) :: a(ndim,*),b(*),work(*)
   REAL(dp) :: ABSa, wk, aa, ba, minusb, wmax
   INTEGER, INTENT(OUT) :: returnCode
   INTEGER :: i, ii, j, k, kk, ir, iir

   ! Checking the arguments
   IF (ndim < n) THEN

      ! Error of the input argument
      WRITE(*,'("(LUfact) Invalid argument.",/,"n=",I5, /,"ndim =",I5)') n,ndim
      returnCode = 3
      RETURN

   ELSE IF (ndim >= n) THEN
      IF (n < 1) THEN
         ! Error of the input argument
         WRITE(6,10) n,ndim
         10 FORMAT('(LUfact) Invalid argument.',/,'n=',I5, /,'ndim =',I5)
         returnCode = 3
         RETURN

      ELSE IF (n == 1) THEN

         ! When the matrix size is one
         IF (a(1,1) == 0.0_dp) THEN
            ! Matrix singular case
            WRITE(*,'("(LUfact) Matrix is singular.")')
            returnCode = 1
            RETURN
         ELSE
            returnCode = 0
            b(1) = b(1)/a(1,1)
            ipiv(1) = 1
            RETURN
         END IF

      ELSE ! if n > 1
         IF (epsilon < 0.0_dp) epsilon = 1.0D-14
      END IF
   END IF

   ! 初期化
   DO i=1,n
      work(i) = ABS(a(i,1))
      ipiv(i) = i
   END DO

   ! Finding the maximum component of each row
   DO j=2,n
      DO i=1,n
         ABSa = ABS(a(i,j))
         wk = work(i)
         IF (wk < ABSa) work(i) = ABSa
      END DO
   END DO
  
   DO i=1,n
      wk = work(i)
      IF ( wk == 0.0_dp) THEN
         WRITE(*,'("(LUfact) Matrix is singular.")')
         returnCode = 1
         RETURN
      ELSE 
         work(i) = 1.0_dp/work(i)
      END IF
   END DO

   ! Starting the LU-decomposition
   DO k=1,n
      IF (k /= 1) THEN

         DO j=1,k-1
            ir = ipiv(j)
            aa = - a(ir,k) * a(ir,j)
            a(ir,k) = - aa
            DO ii=j+1,n
               i = ipiv(ii)
               a(i,k) = a(i,k) + a(i,j)*aa
            END DO
         END DO
      END IF

      ! Finding the maximum component of the k-th column
      wmax = 0.0_dp
      DO ii=k,n
         i = ipiv(ii)
         wk = ABS(a(i,k))*work(i)
         IF (wmax < wk) THEN
            iir = ii
            wmax = wk
         END IF
      END DO

   ! Checking if the matrix is singular or not
      IF (wmax <= epsilon) THEN
         IF (wmax == 0.0_dp) THEN
            WRITE(*,'("(LUfact) Matrix is singular.")')
            returnCode = 1
         ELSE
            WRITE(*,'("(LUfac) Matrix became singular at the factorizing step :", I5)') k
            returnCode = 2
         END IF
         RETURN
      END IF

      ir = ipiv(iir)
      ipiv(iir) = ipiv(k)
      ipiv(k) = ir
      a(ir,k) = 1.0_dp/a(ir,k)
   END DO

   IF (n < 1) THEN
      WRITE(*,'("(LUfact) Invalid argument.",/,"n=",I5, /,"ndim =",I5)') n,ndim
      returnCode = 3
      RETURN
   ELSE IF (n == 1) THEN
      ! When the matrix size is one
      IF (a(1,1) == 0.0_dp) THEN
         ! Matrix singular case
         WRITE(*,'("(LUfact) Matrix is singular.")')
         returnCode = 1
         RETURN
      ELSE
         returnCode = 0
         b(1) = b(1)/a(1,1)
         ipiv(1) = 1
         RETURN
      END IF
   else ! if n > 1

      ! Forward substitution
      DO k=1,n-1
         ir = ipiv(k)
         ba = -b(ir)*a(ir,k)
         b(ir) = -ba
         DO ii=k+1,n
            i = ipiv(ii)
            b(i) = a(i,k)*ba+b(i)
         END DO
      END DO
      b(i) = b(i)*a(i,n)

      ! Backward substitution
      DO kk=2,n
         k = n-kk+2
         minusb = -b(i)
         DO ii=1,k-1
            i = ipiv(ii)
            b(i) = a(i,k)*minusb+b(i)
         END DO
      END DO
      DO k=1,n
         work(k) = b(k)
      END DO
      DO k=1,n
         i = ipiv(k)
         b(k) = work(i)
      END DO
      returnCode = 0
      
   END IF
   
   RETURN

END SUBROUTINE LUfact
