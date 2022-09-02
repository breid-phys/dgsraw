C
C====+==================================================================+==
C
      INTEGER*1 FUNCTION P_BCD2(IBYTE)
C
C     Packs an integer into a 2-digit packed BCD 
C
C     28Jul04   T. Bullett  AFRL
C
C
C====+==================================================================+==
C

      INTEGER IBYTE,IBX,I1,I10
C
      IBX= MIN(99,MAX(IBYTE,0))
      IF (IBX.NE.IBYTE) WRITE(0,*) 'p_bcd2 --> Range Error ',IBYTE
C
      I10 = IBX/10
      I1  = IBX - 10*I10
C
      P_BCD2 = 16*I10+I1
cdb      write(*,'(I3,Z3)') IBYTE,P_BCD2 
C
      RETURN
      END
C
C====+==================================================================+==
C
