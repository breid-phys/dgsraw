C
C  =======================================================================
C
      REAL FUNCTION PRF_D256(IR)
C
C     Determine the Pulse Repetition Frequency (PRF) of a D256
C     from the R preface parameter

      INTEGER IR,IR3
C
      IR3 = IAND(IR,3)
C
      IF (IR3.EQ.0) THEN
         PRF_D256 = 50.0
      ELSE IF (IR3.EQ.1) THEN
         WRITE(0,*) "Illegal PRF R= ",IR
         PRF_D256 = 100
      ELSE IF (IR3.EQ.2) THEN
         PRF_D256 = 100.0
      ELSE
         PRF_D256 = 200.0
      ENDIF
      RETURN
      END
