C
C====+==================================================================+==
C
      REAL FUNCTION FRQ_D256(PREFACE)
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
      INTEGER*1 PREFACE(57)
      INTEGER I,II,J
      REAL FREQ,DELF
C
      I = PREFACE(56)
C     II is the actual value of I used for this frequency
      II = PREFACE(52)
      IF (II.EQ.15) II = 0
C
C     Frequency digits in in preface locations 20-25
C     PREFACE(20) = 10.0    MHz
C     PREFACE(21) =  1.0    MHz
C     PREFACE(22) =  0.1    MHz (100 kHz)
C     PREFACE(23) =  0.01   MHz ( 10 kHz)
C     PREFACE(24) =  0.001  MHz (  1 kHz)
C     PREFACE(25) =  0.0001 MHz (100 Hz)

      FREQ=0.0
      DO J=1,-4,-1
         FREQ=FREQ + REAL(10.0**J*PREFACE(21-J))
      ENDDO
C
      DELF = 0.00250
      IF (I.EQ.0) THEN
         CONTINUE
      ELSE IF (I.EQ.1) THEN
         DELF = DELF + 0.020
      ELSE IF (I.EQ.2) THEN
         DELF = DELF + 0.010*REAL(II)
      ELSE IF (I.EQ.3) THEN
         DELF = DELF + 0.020*REAL(II)
      ENDIF
C
      FRQ_D256 = FREQ + DELF
cdb      write(*,'(A,F8.5,1X,I2,1X,I2)') 'F,I,I*= ', FRQ_D256,I,II
      RETURN
      END









