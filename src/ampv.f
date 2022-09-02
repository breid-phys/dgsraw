C
C====+==================================================================+==
C
      REAL FUNCTION AMPV(EAMPS,IC,NCHAN,MAPCH,MAXRBIN)
C
C     Calculate the Amplitude Most Pobable Value as a proxy for the
C     noise floor of the Range-Doppler data in EAMPS
C     Data are expected in the range of 0 to 120 dB and binned in 
C     3/8dB bins for 16C data and 3dB bins for MMM data .
C     The resolution, range and number of bins is hard-coded.  Sorry.
C
C     EAMPS -- Echo Amplitudes Array
C     IC    -- The single selected channel # for MMM data, for each range
C     NCHAN -- Number of Channels (1 or 16)
C     MAXRBIN -- Number of range bins
C     MAPCH  -- A binary map of channels to include in the analysis (16C only)
C               the 16 bits of the integer map to the 16 channels of the data.
C
C     18Mar03 TWB Modified for MMM and 16C data
C     29May03 TWB Added single&multi channel
C
      REAL EAMPS(256,16)
      INTEGER NCHAN,MAXRBIN,IC(256),MAPCH
C
C     REAL XMIN,XMAX,DX
      INTEGER I,J,NX(0:350),NS,NE,IX,NTOTAL
C
      XMIN = 0.0
      XMAX = 120.0
      IF(NCHAN.EQ.1) THEN
         DX = 3.0
      ELSE IF (NCHAN.EQ.16) THEN
         DX = 3.0/8.0
      ELSE
         DX = 1.0
         WRITE(0,*) 'AMPV -> Bad # of channels ',NCHAN
      ENDIF
C
C     Determine the bin #'s for the limits
      NS = INT(XMIN/DX)
      NE = INT(XMAX/DX)
C
C     Zero things
      DO I=0,350
         NX(I)=0
      ENDDO
      NTOTAL=0

C     Place the counts in the arrays
      DO J = 1, NCHAN
        IF (IAND(MAPCH, 2**J).NE.0) THEN 
          DO I = 1, MAXRBIN
             K=J
             IF (NCHAN.EQ.1) K=IC(I)
             IX = INT(EAMPS(I,K)/DX)
             IX = MAX(MIN(IX,NE),NS)
             NX(IX) = NX(IX) + 1
             NTOTAL = NTOTAL + 1
          ENDDO
       ENDIF
      ENDDO
C
      IX = 0
      J = 0
      AMPV = 0.0
      DO I = NS,NE
         IF (NX(I).GT.J) THEN
            J = NX(I)
            IX = I
         ENDIF
      ENDDO
      AMPV = XMIN + REAL(IX)*DX
      RETURN
      END
















