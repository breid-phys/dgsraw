C
C====+==================================================================+==
C
      INTEGER FUNCTION UNP_BCD4(IBYTE1,IBYTE2)
C
C     Unpacks a 4-digit packed BCD character out of IBYTE1 and IBYTE2
C
C     28Jul04   T. Bullett  AFRL
C
C
C====+==================================================================+==
C

      INTEGER*1 IBYTE1, IBYTE2
C
      INTEGER*1 M4L,M4H
      PARAMETER (M4L=15,M4H=240)
C

      UNP_BCD4=1000*IAND(IBYTE1,M4H)/16 + 100*IAND(IBYTE1,M4L) + 
     +           10*IAND(IBYTE2,M4H)/16 +     IAND(IBYTE2,M4L)
      RETURN
      END
C
C====+==================================================================+==
C
