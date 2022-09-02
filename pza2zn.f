C====+==================================================================+==
C
      REAL FUNCTION PZA2ZN(PZA)
C
C     Compute the Zenith Angle from the PZA byte.
C
C     Zenith is in the 16's and 32's bit of the 0PVZAAAA bits in PZA
C
      INTEGER PZA
C
      PZA2ZN = 0.0
C
      IF (IAND(PZA,32).NE.0) THEN
C        This is overhead
         PZA2ZN = 0.0
      ELSE IF (IAND(PZA,16).EQ.0) THEN
C        Small zenith angles
         PZA2ZN = 11.0
      ELSE IF (IAND(PZA,16).EQ.0) THEN
C        Large zenith angles
         PZA2ZN = 22.0
      ELSE
         WRITE(0,*) 'Error in Zenith value of PZA byte...'
         PZA2ZN = 0.0
      ENDIF
      RETURN
      END
C
