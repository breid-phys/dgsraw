      PROGRAM GAINCURVE
C
C     Compute the relative gain curve of a Digisonde as a function of
C     frequency, given a 'noiseplot' output file.
C
C     21Aug04   TWB
C
C
      INTEGER MFRQ,MAMP
      PARAMETER (MFRQ=300,MAMP=120)
C
      INTEGER MAXAMP(MFRQ,MAMP),NAMP(MFRQ),IOVAL,I,j,IFX,N90
      REAL F0,FM,DF,FRAC
      LOGICAL EOF
      CHARACTER*17 TIME
C
      F0 = 1.0
      FM = 30.0
      DF = 0.10
C
      XMAX =   0.0
      XMIN = 120.0
      DX   =   3.0
C
      DO I=1,MFRQ
         NAMP(I) = 0
         DO J=1,MAMP
            MAXAMP(I,J) = 0
         ENDDO
      ENDDO
C
C     Read the input file on stdin.  Create a buffer of max amplitude PDF for
C     each frequency step.
      EOF = .FALSE.
      DO WHILE (.NOT.EOF)
         READ(*,*,IOSTAT=IOVAL) TIME,FREQ,FMPA,ANOISE,AMIN,AMAX
         EOF = (IOVAL.LT.0)

         IF (EOF) CYCLE
         IFX=MIN(MFRQ,MAX(1,(FREQ-F0)/DF))
         NAMP(IFX) = MIN(MAMP, NAMP(IFX)+1)
         IX = INT(AMAX/DX)
         IX = MAX(MIN(IX,MAMP),1)
         MAXAMP(IFX,IX) = MAXAMP(IFX,IX) + 1
      ENDDO
C
      write(*,*) ((maxamp(i,j),i=1,mfrq),j=1,mamp)
C     Convert array to cumulative PDF
      DO IFX = 1, MFRQ
         FREQ = (IFX-1)*DF + F0
         DO IX = 2,MAMP
            MAXAMP(IFX,IX) = MAXAMP(IFX,IX)+MAXAMP(IFX,IX-1)
         ENDDO
C        Compute the 90th percentile, approximately.
         N90 = INT(0.90*REAL(NAMP(IFX)))
         DO IX=1,MAMP
            IF ((MAXAMP(IFX,IX).LE.N90).
     +                       AND.(MAXAMP(IFX,IX+1).GE.N90)) THEN
               FRAC = REAL(MAXAMP(IFX,IX)-N90)/REAL(N90)
               AMAX = DX*REAL(MAXAMP(IFX,IX)+FRAC)
            ENDIF
         ENDDO
         WRITE(*,102) FREQ,AMAX
      ENDDO

 101  FORMAT (A17,5F10.3)
 102  FORMAT (2F10.5)

      END
