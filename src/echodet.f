C
      PROGRAM ECHODET
C
C     Detects ionosphere echoes from Digisonde 256 16 channel data
C
C     This program is based on the DGS16C program by T. Bullett
C     It unpacks 16 channel data 
C     obtained from a UMass Lowell Digisonde 256 ionosonde
C     and uses a variety of techniques to detect ionosphere
C     echoes.
C
C     Terence Bullett
C     Air Force Research Laboratory
C     11 April 2002
C     linux g77
C
C     License:
C
C     THIS IS NOT FOR PUBLIC RELEASE  Distribution B
C
C     References:
C      DGS16C and references therein.
C
C     Useage:
C        echodet inputfilename {options}
C        decoded data are written in ASCII to stdout
C
C        The input file must exist and contain 16 channel D256 data.
C
C     Revision History
C     11Apr02 TWB - First version.  Threshold based detection from dgs16c.
C
C     
C     Bugs
C
C     Limitations
C        I can't reliably read binary files from stdin, at least not in any way
C        that is remotely portable.  Thus this program cannot be a pipe.
C        Sorry.
C
C        Blank Lines
C           The statment WRITE(*,*) ''  writes a line with 1 space, while
C           WRITE(*,*) writes a truly blank line.  At least in Linux g77.
C
C     To Do
C      - *** 256 range gate modes ***
C      - *** Precision Group Height modes ****
C      - Improve the use of custom antenna tables
C      - Allow for a range of frequencies to be extracted
C      - Test and Debug !!!!
C
C========================================================================
C
C      Variables
      CHARACTER*120 CLP,INFILE
      INTEGER I,K,NA,IU,NBEAM,NDOPP
      INTEGER*1 IBUF(4096)
      LOGICAL INFOK,EOF,FRQ_SEARCH,OK,NEWION
      LOGICAL DO_RDPB,DO_PRECISE,THRESH, GNUPLOT 
      REAL EPSILON,DELFRQ,WANTEDFRQ,VERSION,AMPTHR
C
      REAL FREQ, FREQL, EAMPS(256,16),EPHASE(256,16),RGT(256)
      CHARACTER*17 TIME
      INTEGER*1 PREFACE(57)
      INTEGER MAXRBIN
C
      REAL FRQ(16),APOL(16),HDOP(16),AZ(16),ZN(16)
      INTEGER ICS(16)
C
C     Detected Echoes
      INTEGER MAXECHO,NECHO
      PARAMETER (MAXECHO=100)
      REAL FNOISE
      REAL DFRQ(MAXECHO),DRNG(MAXECHO),DPOL(MAXECHO),DDOP(MAXECHO),
     +     DAZ(MAXECHO),DZN(MAXECHO),DAMP(MAXECHO),DPHASE(MAXECHO),
     +     DPHASV(MAXECHO),DCHSNR(MAXECHO)

C     Functions
      LOGICAL READ4KB, UNPACK_16C
      REAL AMPV
C
      PARAMETER (EPSILON=1.0E-6, DELFRQ=0.02, VERSION=0.10)

C
C      Parse command line options
C
C     Open input file
C     NOTE: This is operating system and compiler dependent.
C           You will likely have to edit this.
C     Pre-opened unit numbers under g77:  stdin=5, stdout=6, stderr=0
C        
C     Read the command line for arguments
      K = IARGC() 
C     Filename must exist
      IF (K.LT.1) THEN
         WRITE(0,*) "dgs16c-> ERROR: Input file must be specified"
         CALL EXIT(-1)
         RETURN
      ENDIF
C
C     Initialize variables
      INFILE='error'
      IU=5
      THRESH=.FALSE.
      DO_RDPB=.FALSE.
      GNUPLOT=.FALSE.
      FRQ_SEARCH=.FALSE.
      WANTEDFREQ=0.0
      FREQL=999.9
      NEWION=.TRUE.
 
C     Parse the command line.  Not very robust here, but it works. Mostly.
      DO NA = 1, K
        CALL GETARG(NA,CLP)
C
        IF (CLP.EQ.'-h') THEN
          WRITE(*,*)' useage: echodet {-fpg} {-f #} {-t #} filename'
          WRITE(*,*)' '
          WRITE(*,*)'Decode data from a Digisonde 256 16C file'
          WRITE(*,*)' Filename must be provided on the command line.'
          WRITE(*,*)' Command line options:'
          WRITE(*,*)'  -f  <freq>  extract data for <frequency>'
          WRITE(*,*)'  -t  <val> Threshold Detect, <val> above noise'
          WRITE(*,*)'  -b  Beam:Range:Phase:Doppler(BRPD) mode'
          WRITE(*,*)'  -p  Precison Mode (unimplimented)'
          WRITE(*,*)'  -g  Delimit with blanks for gnuplot'
          WRITE(*,*)' '      
          WRITE(*,*)' Returns exit status as follows:'
          WRITE(*,*)'  0:  No Problems'
          WRITE(*,*)' -1:  File unreadable or nonexistent'
          RETURN 
        ELSE IF (CLP.EQ.'-f') THEN
           FRQ_SEARCH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) WANTEDFRQ
        ELSE IF (CLP.EQ.'-b') THEN
           DO_RDPB=.TRUE.
        ELSE IF (CLP.EQ.'-g') THEN
           DO_PRECISE=.TRUE.
        ELSE IF (CLP.EQ.'-g') THEN
           GNUPLOT=.TRUE.
        ELSE IF (CLP.EQ.'-t') THEN
           THRESH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) AMPTHR
        ELSE    
          INFILE=CLP
        ENDIF
      ENDDO      
C
C     Initialize the ZTL Rx antenna tables.
      CALL ZTL_INIT()
C
C     Check if input file exists
      INQUIRE(FILE=INFILE,EXIST=INFOK)
      IF (.NOT.INFOK) THEN
         CALL EXIT(-1)
      ENDIF
C
C     Open input file.  This is for g77.  You mileage will vary.
      OPEN(UNIT=IU,FILE=INFILE, ACCESS='DIRECT', FORM='UNFORMATTED', 
     +     STATUS='OLD', RECL=4096 )
C

      EOF=READ4KB(IU,IBUF)
      DO WHILE (.NOT.EOF)
        OK=UNPACK_16C(IBUF,  
     +               TIME,PREFACE,FREQ,EAMPS,EPHASE,RGT,MAXRBIN)
C
        IF (.NOT.OK) THEN
           EOF=READ4KB(IU,IBUF)
           CYCLE
        ENDIF
C
        IF (FREQ.LT.FREQL) THEN
           NEWION=.TRUE.
           FREQL=999.9
        ENDIF
        FREQL = FREQ
C
C       Once per ionogram headers
        IF (NEWION) THEN
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
        ENDIF
C
        IF(FRQ_SEARCH.AND.(ABS(FREQ-WANTEDFRQ).GT.DELFRQ)) THEN
C          Read next record.  This will have to be more sophisticated for
C          Recordtype=  (2 frequencies per record, amplitude only)
           EOF=READ4KB(IU,IBUF)
           CYCLE
        ENDIF
C
        IF (THRESH) THEN

C          Fix the phases.
           CALL FIXPHASE(RGT,EPHASE,MAXRBIN)
C
C          Determine the channel meta data from the preface
           CALL CHANNEL_MAP (PREFACE, FRQ,APOL,HDOP,AZ,ZN,NBEAM,NDOPP)
C
C          Calculate the 'noise floor' using all channels
           FNOISE = AMPV(EAMPS,IC,16,-1,MAXRBIN)
C
C          Sort the channels to change the output order
           CALL SORT_SPECTRA(FRQ,APOL,HDOP,AZ,ZN,5,  ICS)
C
C          Write the data in an gnuplot splot compatable format
C          For each data point that passes the AMPTHR threshold:
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase'
           DO I=1, MAXRBIN
            DO J=1,16
             IF (EAMPS(I,ICS(J)).GT.(FNOISE+AMPTHR)) THEN 
                WRITE(*,111) TIME,FRQ(J),RGT(I),APOL(ICS(J)),FNOISE,
     +                       HDOP(ICS(J)),AZ(ICS(J)),ZN(ICS(J)),
     +                       EAMPS(I,ICS(J)),EPHASE(I,ICS(J))
             ENDIF
            ENDDO
            IF(GNUPLOT) WRITE(*,*) ''
           ENDDO
           IF(GNUPLOT) WRITE(*,*) ''
C
         ELSE IF (DO_RDPB) THEN
C          Implement "Burped" echo detector.
           IF (NEWION) WRITE(*,110) 'Time','Freq','Range','Polarize',
     +        'Noise','Doppler','Az','Zn','Ampltude','Phase',
     +        'ChSNR','PhRMS'
           CALL BRPD(TIME,PREFACE,FREQ,EAMPS,EPHASE,RGT,MAXRBIN,
     +         NECHO,FNOISE,DFRQ,DRNG,DPOL,DDOP,DAZ,DZN,DAMP,
     +         DPHASE,DCHSNR,DPHASV)
C
           DO I=1,NECHO
              WRITE(*,111) TIME,DFRQ(I),DRNG(I),DPOL(I),FNOISE,
     +                     DDOP(I),DAZ(I),DZN(I),DAMP(I),DPHASE(I),
     +                     DCHSNR(I),DPHASV(I)
           ENDDO
         ENDIF

C       Read the next block of data
        NEWION=.FALSE.
        EOF=READ4KB(IU,IBUF)
      ENDDO
 106  FORMAT ('# ',A17,F8.4,1X,11Z1,1X,8Z1,1X,6Z1,1X,7Z1,1X,5Z1,1X,3Z1,
     +        1X,3Z1,1X,4Z1,1X,3Z1,1X,3Z1,1X,4Z1)        
 110  FORMAT ('#',A16,12A10)
 111  FORMAT (A17,F10.4,12F10.3)
      END

C
C====+==================================================================+==
C
