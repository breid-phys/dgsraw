C
      PROGRAM DGS16C
C
C     Digisonde 256 16 Channel data decoder
C
C     This program forms the basis of a decoder for 16 channel (16C) 
C     data obtained from a UMass Lowell Digisonde 256 ionosonde.
C
C     Terence Bullett
C     Air Force Research Laboratory
C     15 September 2001
C     linux g77
C
C     License:
C     This is in the public domain.  Use it as you see fit.
C     Just don't take credit for my work.
C
C     This software was paid for by the 
C     Taxpayers of the United States of America.
C     Thank one of them the next time you meet.
C
C     References:
C        GL-TR-09-0190 "ARTIST Tape Output Formats"  July 1990  J. Tang, et al
C
C        "Decoding of Status Information in MMM Digisonde Ionograms"
C                 I. Galkin  September 2001
C
C        Digisonde 256 -- General Description of the Compact Digital
C             Ionospheric Sounder", Bibl, Reinisch, Kitrosser, 1981 Revised
C              aka "Green Book"
C
C        "Digisonde 256 Operators Manual"  Terence Bullett  
C
C     Useage:
C        dgs16c inputfilename {options}
C        decoded data are written in ASCII to stdout
C
C        The input file must exist and contain 16 channel D256 data.
C
C     Revision History
C     17Jan02 TWB - Added status interpetation code.
C     12Feb02 TWB - Added output formats for gnuplot
C     03Mar02 TWB - Added simple threshold filtering
C     09Mar02 TWB - Corrected Doppler Table, improved implementation
C     10Mar02 TWB - Broke up the source code into seperate files for
C                   each subroutine.  Using make to compile.
C                 - Version 0.20 ready for testing
C     11Apr02 TWB - Modified to treat frequency as channel-dependent
C                   meta-data.  This is needed to deal with 'PGH'
C                   modes, which change frequency between channels.
C                 - Fixed frequency search related bug in frq_d256
C     02Apr03 TWB - Added NoiseMode
C                   Sync changes with dgsmmm
C     20Jun03 TWB - Implemment optional blank lines between data types
C                 - Mod for changes in ampv included for echodet
C     29Jun03 TWB - Add the feature of just listing the Prefaces in the file
C                   This is useful for getting time or frequency data to 
C                   select and control plots.
C     28Feb07 TWB - Added -R switch for Raw (0-255) amplitudes, without
C                   correction for G=x values
C                 - Added a record selection masking feature that allows to
C                   extract patterns of records from the 16C file
C                 - Modified loop logic to place one READ4K() call at the top.
C                 - Modified NEWIONogram detection
C                 - Added Ionogram mode, with O/X signals
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
      INTEGER I,K,NA,IU,NBEAM,NDOPP,NCHAN,MAPCH,JM,JO
      INTEGER*1 IBUF(4096)
      LOGICAL VERBOSE,INFOK,EOF,FRQ_SEARCH,OK,BS,BF,HEADERS,SKIP
      LOGICAL WRAMP,WRPHA,WRSPL,FIXPHAS,THRESH,NOISEMODE,NEWION,RAWAMP
      LOGICAL IONOGRAM
      REAL EPSILON,DELFRQ,WANTEDFRQ,VERSION,AMPTHR,DF
C
      REAL FREQ, FREQL, EAMPS(256,16), EPHASE(256,16), RGT(256)
      REAL FNOISE,AM
      CHARACTER*17 TIME
      INTEGER*1 PREFACE(57)
      INTEGER MAXRBIN,IC(256),MASKLEN,IBLOCK
      INTEGER*4 MASKBLOCK,IMA,IMB
C
      REAL FRQ(16),APOL(16),HDOP(16),AZ(16),ZN(16)
      INTEGER ICS(16)
C
C     Functions
      LOGICAL READ4KB, UNPACK_16C
      REAL AMPV
C
      PARAMETER (EPSILON=1.0E-6, DELFRQ=0.025, VERSION=0.50)
      PARAMETER (NCHAN=16)
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
         WRITE(0,*) "dgs16c-> ERROR: Input file must be specified"
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
      WRAMP=.FALSE.
      WRPHA=.FALSE.
      WRSPL=.FALSE.
      FIXPHAS=.FALSE.
      THRESH=.FALSE.
      NOISEMODE=.FALSE.
      HEADERS=.FALSE.
      NEWION=.TRUE.
      FREQL=999.9
      BF=.FALSE.
      BS=.FALSE.
      RAWAMP=.FALSE.
      MASKBLOCK=1
      MASKLEN=1
      SKIP=.FALSE.
      IONOGRAM=.FALSE.

C     Parse the command line.  Not very robust here, but it works. Mostly.
      DO NA = 1, K
        CALL GETARG(NA,CLP)
        IF (CLP.EQ.'-v') THEN
          VERBOSE = .TRUE.
        ELSE IF (CLP.EQ.'-h') THEN
          WRITE(*,*)' useage: dgs16c {-vcapsHnR bs bf {-f #} {-t #}'
          WRITE(*,*)'                   filename'
          WRITE(*,*)' '
          WRITE(*,*)' Version ', VERSION
          WRITE(*,*)'	Decode 16 channel data from a D256 16C file'
          WRITE(*,*)' Filename must be provided on the command line.'
          WRITE(*,*)' Command line options:'
          WRITE(*,*)'  -h  Help.  Print this message and exit'
          WRITE(*,*)'  -v  Verbose.  Prints the status on stdout'
          WRITE(*,*)'  -f  <freq>  extract data for <frequency>'
          WRITE(*,*)'  -c  Correct phase based on range gate'
          WRITE(*,*)'  -a  Write Amplitude Table'
          WRITE(*,*)'  -p  Write Phase Table'
          WRITE(*,*)'  -s  Write data for gnuplot splot command'
          WRITE(*,*)'  -t  <val> Threshold Detect, <val> above noise'
          WRITE(*,*)'      for -s and -i modes'
          WRITE(*,*)'  -n  Noise mode: write signal & noise stats'
          WRITE(*,*)'  -H  Headers: Write headers for each block'
          WRITE(*,*)'  -bs Output a blank after each spectra'
          WRITE(*,*)'  -bf Output a blank after each frequency'
          WRITE(*,*)'  -R  Output Raw (0-255) Amplitude and phase data'
          WRITE(*,*)'  -M m,n Use n bits of m to select records'
          WRITE(*,*)'  -i ionogram mode. Maximim signal each pixel'
          WRITE(*,*)' '      
          WRITE(*,*)' Returns exit status as follows:'
          WRITE(*,*)'  0:  No Problems'
          WRITE(*,*)' -1:  File unreadable or nonexistent'
          RETURN 
        ELSE IF (CLP.EQ.'-f') THEN
           FRQ_SEARCH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) WANTEDFRQ 
        ELSE IF (CLP.EQ.'-a') THEN
           WRAMP = .TRUE.
        ELSE IF (CLP.EQ.'-p') THEN
           WRPHA=.TRUE.
        ELSE IF (CLP.EQ.'-s') THEN
           WRSPL=.TRUE.    
        ELSE IF (CLP.EQ.'-c') THEN
           FIXPHAS=.TRUE.
        ELSE IF (CLP.EQ.'-i') THEN
           IONOGRAM=.TRUE.
        ELSE IF (CLP.EQ.'-t') THEN
           THRESH = .TRUE.
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) AMPTHR
        ELSE IF (CLP.EQ.'-n') THEN
           NOISEMODE = .TRUE.
           WRAMP = .FALSE.
           WRPHA = .FALSE.
           WRSPL = .FALSE.
        ELSE IF (CLP.EQ.'-H') THEN
           HEADERS = .TRUE.
        ELSE IF (CLP.EQ.'-bs') THEN
           BS = .TRUE.
        ELSE IF (CLP.EQ.'-bf') THEN
           BF = .TRUE.
        ELSE IF (CLP.EQ.'-R') THEN
           RAWAMP = .TRUE.
        ELSE IF (CLP.EQ.'-M') THEN
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) MASKBLOCK,MASKLEN
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
C     Check limits on block masking
      IF((MASKLEN.LT.1).OR.(MASKLEN.GT.32)) THEN
         WRITE(0,*) 'Mask block length must be between 1 and 32'
         CALL EXIT(-1)
      ENDIF
C
C     Open input file.  This is for g77.  You mileage will vary.
      OPEN(UNIT=IU,FILE=INFILE, ACCESS='DIRECT', FORM='UNFORMATTED', 
     +     STATUS='OLD', RECL=4096 )
C
C
C     Begin main program loop
      IBLOCK=0
      EOF=.FALSE.
      DO WHILE (.NOT.EOF)
        EOF=READ4KB(IU,IBUF)
        IBLOCK = IBLOCK + 1
        OK=UNPACK_16C(IBUF,RAWAMP,   
     +               TIME,PREFACE,FREQ,EAMPS,EPHASE,RGT,MAXRBIN)
C
        IF (.NOT.OK) THEN
           EOF=READ4KB(IU,IBUF)
           IBLOCK = IBLOCK + 1
           CYCLE
        ENDIF
C
C       TODO:  Use preface flags to set this.
        DF = FREQ-FREQL
        IF (DF.LT.-1.0) THEN
           NEWION=.TRUE.
           FREQL=999.9
           WRITE(*,*) 
        ENDIF
        FREQL = FREQ
C
        IF(FRQ_SEARCH.AND.(ABS(FREQ-WANTEDFRQ).GT.DELFRQ)) THEN
C          Read next record.  This will have to be more sophisticated for
C          Recordtype=  (2 frequencies per record, amplitude only)
           EOF=READ4KB(IU,IBUF)
           IBLOCK = IBLOCK + 1
           CYCLE
        ENDIF
C
C       Execute the block selection mask.  The length of the mask, in bits,
C       is MASKLEN, which can be up to 32 bits.  This is the largest pattern
C       of block selection that can be implemented.  The value of MASKBLOCK is
C       checked for the IBLOCK'th bit.  If this is 1 then the block is processed,
C       if the bit is 0 the the block is skipped.
C       For example, a MASKLEN=2 and MASKBLOCK=0x01 then even number blocks are
C       processed.  MASKBLOCK=0x02 looks at even block numbers.

        IMBIT=MOD(IBLOCK-1,MASKLEN)
        IMA=ISHFT(1,IMBIT)
        IMB=IAND(MASKBLOCK,IMA)
        SKIP=(IMB.EQ.0)
cdb        write(*,'(3i4,1x,z5.5,1x,z5.5,L5)') 
cdb     +            iblock,imbit,masklen,imask,maskblock,SKIP
C
        IF (SKIP) CYCLE

C       Headers
        IF (NEWION.OR.HEADERS) THEN
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
           IF (NOISEMODE) WRITE(*,110)'Time','Freq',
     +                       'FNOISE', 'MPA','Min','Max'
        ENDIF
C
C       Optionally fix the phases.
        IF (FIXPHAS) CALL FIXPHASE(RGT,EPHASE,MAXRBIN)
C
C       Determine the channel meta data from the preface
        CALL CHANNEL_MAP (PREFACE, FRQ,APOL,HDOP,AZ,EL,NBEAM,NDOPP)
C
C       Sort the channels to change the output order
        CALL SORT_SPECTRA(FRQ,APOL,HDOP,AZ,EL,5,  ICS)
C
C       Calculate the 'noise floor'.  16C data only
        MAPCH=-1
        FNOISE = AMPV(EAMPS,IC,16,MAPCH,MAXRBIN)
C
        IF (WRSPL) THEN
C          Write the data in an gnuplot splot compatable format
C          For each data point:
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase'
           DO I=1, MAXRBIN
            DO J=1,NCHAN
              WRITE(*,111) TIME,FRQ(J),RGT(I),APOL(ICS(J)),FNOISE,
     +                     HDOP(ICS(J)),AZ(ICS(J)),ZN(ICS(J)),
     +                     EAMPS(I,ICS(J)),EPHASE(I,ICS(J))
            ENDDO
            IF (BS) WRITE(*,*) 
           ENDDO
           IF (BF) WRITE(*,*) 
        ELSE IF (THRESH) THEN
C          Write the data in an gnuplot splot compatable format
C          For each data point that passes the AMPTHR threshold:
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase'
           DO I=1, MAXRBIN
            DO J=1,NCHAN
             IF (EAMPS(I,ICS(J)).GT.(FNOISE+AMPTHR)) THEN 
                WRITE(*,111) TIME,FRQ(J),RGT(I),APOL(ICS(J)),FNOISE,
     +                       HDOP(ICS(J)),AZ(ICS(J)),ZN(ICS(J)),
     +                       EAMPS(I,ICS(J)),EPHASE(I,ICS(J))
             ENDIF
            ENDDO
            IF (BS) WRITE(*,*) 
           ENDDO
           IF (BF) WRITE(*,*) 

        ELSE IF (WRAMP.AND.WRPHA) THEN
C          Write the amplitude and phase tables
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
           WRITE(*,107) 'Frequency',(FRQ(ICS(I)),I=1,NCHAN),
     +                              (FRQ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Polarize',(APOL(ICS(I)),I=1,NCHAN),
     +                             (APOL(ICS(I)),I=1,NCHAN)
           WRITE(*,105) 'Doppler',(HDOP(ICS(I)),I=1,NCHAN),
     +                            (HDOP(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Azimuth',(AZ(ICS(I)),I=1,NCHAN),
     +                            (AZ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Zenith',(ZN(ICS(I)),I=1,NCHAN),
     +                           (ZN(ICS(I)),I=1,NCHAN)
           DO I=1,MAXRBIN
            WRITE(*,103) FREQ,RGT(I),(EAMPS(I,ICS(J)),J=1,NCHAN),
     +                              (EPHASE(I,ICS(J)),J=1,NCHAN)
           ENDDO
           WRITE(*,*) 

C          Write both amplitude and phase tables
        ELSE IF (WRAMP) THEN
C          Write just the amplitude table
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
           WRITE(*,107) 'Frequency',(FRQ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Polarize',(APOL(ICS(I)),I=1,NCHAN)
           WRITE(*,105) 'Doppler',(HDOP(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Azimuth',(AZ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Zenith',(ZN(ICS(I)),I=1,NCHAN)
           DO I=1,MAXRBIN
            WRITE(*,103) FREQ,RGT(I),(EAMPS(I,ICS(J)),J=1,NCHAN)
           ENDDO
           WRITE(*,*) 
C
        ELSE IF (WRPHA) THEN
C          Write just the phase table
           WRITE(*,106) TIME, FREQ, (PREFACE(I),I=1,57)
           WRITE(*,107) 'Frequency',(FRQ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Polarize',(APOL(ICS(I)),I=1,NCHAN)
           WRITE(*,105) 'Doppler',(HDOP(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Azimuth',(AZ(ICS(I)),I=1,NCHAN)
           WRITE(*,104) 'Zenith',(ZN(ICS(I)),I=1,NCHAN)
           DO I=1,MAXRBIN
            WRITE(*,103) FREQ,RGT(I),(EPHASE(I,ICS(J)),J=1,NCHAN)
           ENDDO
           WRITE(*,*) 
        ELSE IF (NOISEMODE) THEN
           CALL STATS(EAMPS,IC,NCHAN,MAXRBIN, AMPMIN,AMPMAX,AMPMPA)
           WRITE(*,111) TIME,FRQ(1),FNOISE, AMPMPA,AMPMIN,AMPMAX      
C
        ELSE IF (IONOGRAM) THEN
C          Write the data in an gnuplot splot compatable format
C          For each Freqency-Range bin, report the maximum spectral line
           WRITE(*,110) 'Time','Freq','Range','Polarize','Noise',
     +                   'Doppler','Az','Zn','Ampltude','Phase',
     +                   'OpPolAmp','OpPolPha'
           DO I=1, MAXRBIN
C           Determine the maximum spectral line
            AM=-999.0
            JM=0
            JO=0
            DO J=1,NCHAN
              IF (EAMPS(I,ICS(J)).GT.AM) THEN
                 AM=EAMPS(I,ICS(J))
                 JM=J
              ENDIF
            ENDDO
C           Opposite Polarization Index
            IF (JM.LT.8) then
               JO = JM + 8
            ELSE
               JO = JM - 8
            ENDIF

            WRITE(*,111) TIME,FRQ(JM),RGT(I),APOL(ICS(JM)),FNOISE,
     +                     HDOP(ICS(JM)),AZ(ICS(JM)),ZN(ICS(JM)),
     +                     EAMPS(I,ICS(JM)),EPHASE(I,ICS(JM)),
     +                     EAMPS(I,ICS(JO)),EPHASE(I,ICS(JO))
 
           ENDDO
           IF (BF) WRITE(*,*) 
C
C
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
 106  FORMAT ('# ',A17,F8.4,1X,11Z1,1X,8Z1,1X,6Z1,1X,7Z1,1X,5Z1,1X,3Z1,
     +        1X,3Z1,1X,4Z1,1X,3Z1,1X,3Z1,1X,4Z1)
 110  FORMAT ('#',A16,12A10)
 111  FORMAT (A17,F10.4,12F10.3)

      END
