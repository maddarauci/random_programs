! ------------------------------------------------------------------
!       Compute the area of a triangle using Heron's formula
! ------------------------------------------------------------------

PROGRAM HeronFormula
      IMPLICIT NONE

      REAL      :: a, b, c      ! three sides.
      REAL      :: s            ! triangle area 
      LOGICAL   :: cond_1, cond_2       ! two logical conditions 


      READ(*,*) a, b, c

      WRITE(*,*) "a = ", a 
      WRITE(*,*) "b = ", b
      WRITE(*,*) "c = ", c
      WRITE(*,*)

      cond_1 = (a > 0.) .AND. (b > 0.) .AND. (c > 0.0)
      cond_2 = (a + b > c) .AND. (a + c > b) .AND. (b + c > a)

      IF (cond_1 .AND. cond_2) THEN
              s = (a + b + c) / 2.0
              Area = SQRT(s * (s - a) * (s - b) * (s - c))
              WRITE(*,*) "Triangel area = ", Area
        ELSE
          WRITE(*,*) "ERROR: this is not a trianlge"
        END IF

END PROGRAM HeronFormula
