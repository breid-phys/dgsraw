C
C====+==================================================================+==
C
      REAL FUNCTION PZA2POL(PZA)
C
C     Calculate the polarization based on the PZA byte
C     The numerical values of polarization are set to +-90.0 to be
C     consistent with the 'chirality' factor of the Dynasonde
C        (J.W. Wright, private communication)
C
C     Polarizaion is in the 64's bit of the 0PVZAAAA bits in PZA
      INTEGER PZA
C
      IF (IAND(PZA,64).EQ.0) THEN
C        O Polarization
         PZA2POL = -90.0
      ELSE
C        X polarization
         PZA2POL = +90.0
      ENDIF
      RETURN
      END
