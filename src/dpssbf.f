C
      PROGRAM DPSSBF
C
C     Digisonde Portable Sounder (DPS) SBF data decoder
C
C     This program forms the basis of a decoder for Routine Scientific Format
C     (SBF) data obtained from a UMass Lowell DPS ionosonde.
C     All these files might also be called .GRM files and mixed with a variety of 
C     scanning ionogram data formats (MMM, SBF,RSF, etc) depending on 
C     both their vintage and their source.  This code deals with only the MMM data
C
C     Terence Bullett
C     Air Force Research Laboratory
C     
C     28 Jul 2004
C     linux g77
C
C     License:
C     This is in the public domain.  Use it as you see fit.
C     Just don't take credit for my work.
C
C     This software was paid for by the Taxpayers of the
C     United States of America.
C     Thank one the next time you meet.
C
C     References:
C
C        Digisonde 256 -- General Description of the Compact Digital
C             Ionospheric Sounder", Bibl, Reinisch, Kitrosser, 1981 Revised
C              aka "Green Book"
C
C        "Digisonde 256 Operators Manual"  Terence Bullett  
C
C     Useage:
C        dpssbf inputfilename {options}
C        decoded data are written in ASCII to stdout
C
C        The input file must exist and contain SBF data from a DPS.
C
C     Revision History
C     28Jul04  Initial version from dpsmmm, version 0.43
C     15Dec04 TWB - Fixed Elevation vs Zenith angle inconsistency in favor 
C                   of using Zenith angle.
C
C     Notes:
C     There is some overhead in this code, especially in array sizes,
C     that is an artifact of this code being developed from the
C     16 channel decoder.  This is retained intentionally to allow
C     easier software maintenance.
C
C     Bugs
C
C     Limitations
C        I can't reliably read binary files from stdin, at least not in any way
C        that is remotely portable.  Thus this program cannot be a pipe.
C        Sorry.
C
C     To Do
C      - Test and Debug !!!!
C
C========================================================================
C
C      Variables
      CHARACTER*120 CLP,INFILE
      INTEGER I,K,NA,IU,NCHAN,HINOISE,IRXCONF
      INTEGER*1 IBUF(4096)
      LOGICAL VERBOSE,INFOK,EOF,FRQ_SEARCH,OK,BF
      LOGICAL WRSPL,FIXPHAS,THRESH,NOISEMODE,NEWION 
      REAL EPSILON,DELFRQ,WANTEDFRQ,VERSION,AMPTHR
C
      REAL FREQ, EAMPS(512),EPHASE(512),APOL(512),HDOP(512),AZ(512),
     +     ZN(512),RGT(512),FNOISE, ZENITH

      REAL FREQL
      REAL AMPMIN,AMPMAX,AMPMPA
      CHARACTER*17 TIME
      INTEGER*1 PREFACE(57)
      INTEGER MAXRBIN
C
C     Functions
      LOGICAL READ4KB, UNPACK_DPSSBF
C
      PARAMETER (EPSILON=1.0E-6, DELFRQ=0.02, VERSION=0.43)
      PARAMETER (NCHAN=1)
C
C      Parse command line options
C
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
         WRITE(0,*) "dpssbf-> ERROR: Input file must be specified"
         CALL EXIT(-1)
         RETURN
      ENDIF
C
C     Initialize variables
      INFILE='error'
      IU=5
      VERBOSE=.FALSE.
      FRQ_SEARCH=.FALSE.
      WANTEDFREQ=0.0
      WRSPL=.FALSE.
      FIXPHAS=.FALSE.
      THRESH=.FALSE.
      NOISEMODE=.FALSE.
      NEWION=.TRUE.
      FREQL=999.9
      BF=.FALSE.
      HINOISE=0
      IRXCONF=1
      ZENITH=30.0

C     Parse the command line.  Not very robust here, but it works. Mostly.
      DO NA = 1, K
        CALL GETARG(NA,CLP)
        IF (CLP.EQ.'-v') THEN
          VERBOSE = .TRUE.
        ELSE IF (CLP.EQ.'-h') THEN
          WRITE(*,*)' useage: dpsrsf {-vapsh} {-f #} filename'
          WRITE(*,*)' '
          WRITE(*,*)'	Decode SBF data from a DPS .SBF file'
          WRITE(*,*)' Filename must be provided on the command line.'
          WRITE(*,*)' Command line options:'
          WRITE(*,*)'  -v  Verbose.  Prints the status on stdout'
          WRITE(*,*)'  -f  <freq>  extract data for <frequency>'
          WRITE(*,*)'  -c  Correct phase based on range gate'
          WRITE(*,*)'  -s  Write data for gnuplot splot command'
          WRITE(*,*)'  -t  <val> Threshold Detect, <val> above noise'
          WRITE(*,*)'      output format set to -s'
          WRITE(*,*)'  -bf Output a blank after each frequency' 
          WRITE(*,*)'  -n  Noise mode; write signal & noise stats'
          WRITE(*,*)'  -H  <val>  Set the ARMENU HiNoise to <val>'
          WRITE(*,*)' '      
          WRITE(*,*)' Returns exit status as follows:'
          WRITE(*,*)'  0:  No Problems'
          WRITE(*,*)' -1:  File unreadable or nonexistent'
          RETURN 
        ELSE IF (CLP.EQ.'-f') THEN
           FRQ_SEARCH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) WANTEDFRQ
        ELSE IF (CLP.EQ.'-s') THEN
           WRSPL=.TRUE.    
        ELSE IF (CLP.EQ.'-c') THEN
           FIXPHAS=.TRUE.
        ELSE IF (CLP.EQ.'-t') THEN
           WRSPL = .FALSE.
           THRESH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) AMPTHR
        ELSE IF (CLP.EQ.'-H') THEN
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) HINOISE
        ELSE IF (CLP.EQ.'-n') THEN
           NOISEMODE=.TRUE.
        ELSE IF (CLP.EQ.'-bf') THEN
           BF = .TRUE.
        ELSE    
          INFILE=CLP
        ENDIF
      ENDDO      
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
C
        OK=UNPACK_DPSSBF(IBUF,  
     +       TIME,PREFACE,FREQ,FNOISE,EAMPS,EPHASE,APOL,HDOP,AZ,ZN,
     +       RGT,MAXRBIN,HINOISE,IRXCONF,ZENITH)
C

        IF (.NOT.OK) THEN
           EOF=READ4KB(IU,IBUF)
           CYCLE
        ENDIF

        IF ((FREQ-FREQL).LT. -0.1) THEN
           NEWION=.TRUE.
           FREQL=-1
           WRITE(*,*)
        ENDIF
        FREQL = FREQ
C
        IF(FRQ_SEARCH.AND.(ABS(FREQ-WANTEDFRQ).GT.DELFRQ)) THEN
           CYCLE
        ENDIF
C
C       Once per ionogram headers
        IF (NEWION) THEN
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
           IF (NOISEMODE) WRITE(*,110)'Time','Freq',
     +                       'FNOISE', 'MPA','Min','Max'
        ENDIF
C
C       Optionally fix the phases.
ctwb  Is this needed for SBF?
        IF (FIXPHAS) CALL FIXPHASE(RGT,EPHASE,MAXRBIN)
C
C
        IF (WRSPL) THEN
C          Write the data in an gnuplot splot compatable format
C          For each data point:
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase'
           DO I=1, MAXRBIN
              WRITE(*,111) TIME,FREQ,RGT(I),APOL(I),FNOISE,
     +                     HDOP(I),AZ(I),ZN(I),
     +                     EAMPS(I),EPHASE(I)

           ENDDO
           IF(BF) WRITE(*,*)
        ELSE IF (THRESH) THEN
C          Write the data in an gnuplot splot compatable format
C          For each data point that passes the AMPTHR threshold:
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase'
           DO I=1, MAXRBIN
             IF (EAMPS(I).GT.(FNOISE+AMPTHR)) THEN 
                WRITE(*,111) TIME,FREQ,RGT(I),APOL(I),FNOISE,
     +                       HDOP(I),AZ(I),ZN(I),
     +                       EAMPS(I),EPHASE(I)
             ENDIF
           ENDDO
           IF(BF) WRITE(*,*)
        ELSE IF (NOISEMODE) THEN
           CALL ASTATS(EAMPS,MAXRBIN, AMPMIN,AMPMAX,AMPMPA)
           WRITE(*,111) TIME,FREQ,FNOISE, AMPMPA,AMPMIN,AMPMAX      
C
        ENDIF
C
        NEWION=.FALSE.
      ENDDO
         
 100  FORMAT (25(1X,Z2.2))
 101  FORMAT (100(1X,Z1.1))
 102  FORMAT (F5.1,3X,16F6.1, ' # ', 16F6.1)
 103  FORMAT (F8.4,F8.1,32F8.1)
 104  FORMAT ('#',A15,64F8.2)
 105  FORMAT ('#',A15,64F8.3)
 107  FORMAT ('#',A15,64F8.4)
 106  FORMAT ('# ',A17,F7.4,1X,Z2.2,Z2,Z2.2,1X,2Z2.2,1X,3Z2.2,1X,
     +        6Z2.2,1X,2Z1,1X,3Z2.2,1X,2Z2.2,1X,3Z2.2,1X,2Z2.2,1X,
     +        Z2.2,1X,3Z1,1X,Z1,Z2.2,1X,2Z2.2,1X,Z1,1X,Z1,Z2.2,1X,
     +        2Z2.2,1X,Z2,6Z1,1X,2Z2.2,1X,2Z2.2,1X,Z2,1X,2Z2.2,1X,
     +        2Z2.2,1X,2Z2.2,1X,2Z2.2 )
 110  FORMAT ('#',A16,10A10)
 111  FORMAT (A17,F10.4,10F10.3)
      END
C
C====+==================================================================+==
