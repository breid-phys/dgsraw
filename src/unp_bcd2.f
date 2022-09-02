C
C====+==================================================================+==
C
      INTEGER FUNCTION UNP_BCD2(IBYTE)
C
C     Unpacks a 2-digit packed BCD character out of IBYTE
C
C     28Jul04   T. Bullett  AFRL
C
C
C====+==================================================================+==
C

      INTEGER*1 IBYTE
C
      INTEGER*1 M4L,M4H
      PARAMETER (M4L=15,M4H=240)
C
      UNP_BCD2=10*IAND(IBYTE,M4H)/16 + IAND(IBYTE,M4L)
      RETURN
      END
C
C====+==================================================================+==
C
