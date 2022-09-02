C
C====+==================================================================+==
C
      REAL FUNCTION AGC_DPS(IG,IGS,HINOISE)
C
C     Compute the receiver attenuation of a DPS based on G from the preface
C     and on the G* (IGS) value from the prelude.
C
C     25Jul04  TWB  Initial routine based on data from I. Galkin, UML
C
C====+==================================================================+==
C
      INTEGER IG, IGS, HINOISE
      REAL GAINTABLE(0:7),HNTABLE(0:2),GAIN
C
C
      DATA GAINTABLE/0,6,12,18,24,30,36,42/
      DATA HNTABLE/0, 6, 12/
C
      IF (IG.LT.8) THEN
         GAIN = GAINTABLE(IG)
      ELSE
         GAIN = GAINTABLE(IGS/2)
      ENDIF
C
      AGC_DPS = GAIN + HNTABLE(HINOISE)
cdb      write(*,*) IG,IGS,HINOISE,AGC_DPS
      RETURN
      END
