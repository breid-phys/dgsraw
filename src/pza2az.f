C
C====+==================================================================+==
C
      REAL FUNCTION PZA2AZ(PZA)
C
C     Compute the Azimuth Angle from the PZA byte.
C
C     Azimuth is in the low 4 bits of the 0PVZAAAA bits in PZA

      INTEGER PZA, IA
C
      PZA2AZ = 0.0
      IA = IAND(PZA,15)
C
C     Check for special cases.
      IF (IA.EQ.0) THEN
         PZA2AZ = 0.0
      ELSE IF (IAND(PZA,32).NE.0) THEN
C        This is overhead
         PZA2ZN = 0.0
      ELSE IF (IA.LT.13) THEN
         PZA2AZ = 30.0 * REAL(IA-1)
      ELSE
         WRITE(0,*) 'Error in PZA value ', PZA
      ENDIF
      RETURN
      END
C
C
