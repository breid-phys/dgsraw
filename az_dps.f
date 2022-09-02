C
C====+==================================================================+==
C
      REAL FUNCTION AZ_DPS(NAZ,IRXCONF)
C
C
C     Compute the Doppler shift of Doppler Number NDOP for the DPS ionogram
C     with PREFACE
C
C 07Jun10 TWB - Updated from a dummy routine to have some meaning
C
C
C====+==================================================================+==
C
      INTEGER NAZ
      INTEGER IRXCONF

      AZ_DPS = 0.0
      IF ((NAZ.LT.1).OR.(NAZ.GT.6)) RETURN
C
C     Standard Configuration
      IF (IRXCONF.EQ.3) THEN
         AZ_DPS = 90 + 60.0*(NAZ-1)
C     Mirror
      ELSE IF (IRXCONF.EQ.4) THEN
         AZ_DPS = 270 - 60*(NAZ-1)
C     Rotated (D256)
      ELSE IF (IRXCONF.EQ.1) THEN
         AZ_DPS = 60 + 60.0*(NAZ-1)
      ELSE
C     Unknown or undefined
         AZ_DPS = 90 + 60.0*(NAZ-1)
      ENDIF

      IF (AZ_DPS.GT. 360.0) THEN AZ_DPS=AZ_DPS-360.0
      IF (AZ_DPS.LT. 000.0) THEN AZ_DPS=AZ_DPS+360.0  

      RETURN
      END
C
C====+==================================================================+==
C
