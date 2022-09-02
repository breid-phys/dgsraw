C
      PROGRAM AVE_ASCAN
C
C     Averages A-scans from a dgsraw output file format
C
C
C     Terence Bullett
C     CIRES
C     17 July 2008
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
C     Revision History
C
C     Bugs:
C        Blank Lines
C           The statment WRITE(*,*) ''  writes a line with 1 space, while
C           WRITE(*,*) writes a truly blank line.  At least in Linux g77.
C
C
C========================================================================
C
C      Variables
      CHARACTER*120 CLP
      CHARACTER*512 LINE
      INTEGER I,K,NA
      REAL VERSION
      PARAMETER (VERSION=0.51)
      REAL F1,F2
      INTEGER ICH
      REAL FREQ,RGT,EAMPS(16)
      LOGICAL EOF, SKIP


C
C      Parse command line options
C
C
C     Open input file
C     NOTE: This is operating system and compiler dependent.
C           You will likely have to edit this.
C     Pre-opened unit numbers under g77:  stdin=5, stdout=6, stderr=0
C
C     Initialize variables
      SKIP=.FALSE.
      F1=0.0
      F2=999.9
      ICH=1

C     Read the command line for arguments
      K = IARGC() 
C
C     Parse the command line.        
C====+==================================================================+==
      DO NA = 1, K
        CALL GETARG(NA,CLP)
        IF (CLP.EQ.'-h') THEN
          WRITE(*,*)' useage: ave_ascan {-f1 #} {-f2 #} {-c #}'
          WRITE(*,*)'   < datafile '
          WRITE(*,*)' '
          WRITE(*,*)' Version ', VERSION
          WRITE(*,*)'	Average A-scans'
          WRITE(*,*)' Filename must be provided on the command line.'
          WRITE(*,*)' Command line options:'
          WRITE(*,*)'  -h  Help.  Print this message and exit'
          WRITE(*,*)'  '
          WRITE(*,*)'  -f1  <freq1>'  
          WRITE(*,*)'  -f2  <freq2> extract data for {f1 to f2}'
          WRITE(*,*)'  -c <i> Select Channel i '
          WRITE(*,*)' '      
          WRITE(*,*)' Returns exit status as follows:'
          WRITE(*,*)'  0:  No Problems'
          WRITE(*,*)' -1:  File unreadable or nonexistent'
          RETURN 
        ELSE IF (SKIP) THEN
           SKIP=.FALSE.
        ELSE IF (CLP.EQ.'-f1') THEN
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) F1
           SKIP=.TRUE.
        ELSE IF (CLP.EQ.'-f2') THEN
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) F2
           SKIP=.TRUE.
        ELSE IF (CLP.EQ.'-c') THEN
           CALL GETARG(NA+1,CLP)
           READ(CLP,*) ICH
           ICH=MAX(1, MIN(ICH,16))
           SKIP=.TRUE.

        ENDIF
      ENDDO      
C
C     Begin main program loop
      NCHAN = 16
      EOF=.FALSE.
      DO WHILE (.NOT.EOF)
         READ(*,101,IOSTAT=IOVAL) LINE
         EOF = (IOVAL.LT.0)
         IF(EOF) CYCLE
         LT=LEN_TRIM(LINE)
         IF (LINE(1:1).EQ.'#') THEN
            WRITE(*,101) LINE(1:LT)
            CYCLE
         ELSE IF (LT.EQ.0) THEN
            WRITE(*,*) LINE
            CYCLE 
         ENDIF
         READ(LINE,103) FREQ,RGT,(EAMPS(J),J=1,NCHAN)
         WRITE(*,103) FREQ,RGT,(EAMPS(J),J=1,NCHAN)
      ENDDO 
C
 101  FORMAT (A)
 103  FORMAT (F8.4,F8.1,32F8.1)

      END
