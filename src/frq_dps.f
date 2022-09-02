C
C====+==================================================================+==
C
      REAL FUNCTION FRQ_DPS(IFY, I)
C
C     Function retuns the actual radio frequency used for sounding,
C     including the adjustments for frequency search, if enabled.
C     NOTE: This does NOT exactly work for closely spaced frequency mode
C     (aka Precision Group Height mode) 
C
C     ISSUE:  The documentation is inconsistent with itself and with
C             experience.  Actual values observed are 0,1,2 and F
C     11Apr02  TWB
C             I* = F is an indicator that the frequency is restricted
C             This was a Processor hack to get info on restricted frequencies
C             into the MMM format Prelude.  Actual frequency used is
C             lost.  A value of 0 is chosen.
C
C     28Jul04 TWB Adapted for the DPS MMM format data which does not have
C                 this info in the preface.
C
      INTEGER I,II,J, IFY(4)
      REAL FREQ,DELF,DFTABLE(0:4)
      DATA DFTABLE /-20.0, -10.0, 0.0, +10.0, +20.0/
      SAVE DFTABLE
C
C     II is the actual value of I used for this frequency
      II = 0
      IF ((I.GT.0).AND.(I.LT.5))  II = I
C

      FREQ=0.0
      DO J=1,-2,-1
         FREQ=FREQ + REAL(10.0**J*IFY(2-J))
      ENDDO
C
      DELF = 0.001*DFTABLE(II)
C
      FRQ_DPS = FREQ + DELF
cdb      write(*,'(A,F8.5,1X,I2,1X,I2)') 'F,I,I*= ', FRQ_DPS,I,II
      RETURN
      END









