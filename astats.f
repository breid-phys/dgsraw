C
C====+==================================================================+==
C
      SUBROUTINE ASTATS(EAMPS,MAXRBIN,  AMPMIN,AMPMAX,AMPMPA)
C
C     Calculate the Minumim and Maximum amplitude value in the array.
C     Calculate the Amplitude Most Pobable Value as a proxy for the
C     noise floor of the Range-Doppler data in EAMPS
C     Data are expected in the range of 0 to 120 dB and binned in 
C     3/8dB bins for 16C data and 3dB bins for MMM.
C     The resolution, range and number of bins is hard-coded.  Sorry.
C
C     18Mar03 TWB Modified for MMM and 16C data from AMPV function.
C     28May04 TWB Bug Fixes
C     Created ASTATS for RSF data
C
      REAL EAMPS(512),AMPMIN,AMPMAX,AMPMPA
      INTEGER MAXRBIN
C
C     REAL X,XMIN,XMAX,DX
      INTEGER I,J,NX(0:350),NS,NE,IX,NTOTAL
C
      AMPMIN =  1.0E30
      AMPMAX = -1.0E30
C
      XMIN = 0.0
      XMAX = 120.0
      DX = 3.0
C
C     Determine the bin #'s for the limits
      NS = INT(XMIN/DX)
      NE = INT(XMAX/DX)
C
C     Zero things
      DO I=0,350
         NX(I)=0
      ENDDO
C
C     Place the counts in the arrays
      NTOTAL=0
      DO I = 1, MAXRBIN
         X = EAMPS(I)
         IF (X.GT.AMPMAX) AMPMAX=X
         IF (X.LT.AMPMIN) AMPMIN=X
         IX = INT(X/DX)
         IX = MAX(MIN(IX,NE),NS)
         NX(IX) = NX(IX) + 1
         NTOTAL = NTOTAL + 1
      ENDDO
C
      IX = 0
      J = 0
      AMPV = 0.0
      DO I = NS,NE
cdb      write(*,'(F6.2,I6)') I*DX, NX(I)
         IF (NX(I).GT.J) THEN
            J = NX(I)
            IX = I
         ENDIF
      ENDDO
      AMPMPA = XMIN + REAL(IX)*DX
      RETURN
      END


