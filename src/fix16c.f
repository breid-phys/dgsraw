      PROGRAM FIX16C
C
C     Program to fix the impact of a Digisonde 256 error (in the 
C     FEND.EXE program) which caused no RECORDTYPE to be placed
C     in the data.
C     
C     10Mar02  Terence Bullett  Air Force Research Laboratory
C     Distribution:  Public Domain
C
C     CAUTION:  This code indescriminately places a RECORDTYPE=12
C     into the input file.
C     You Were Warned
C
      INTEGER*1 IBUFF(4096)
      INTEGER I,K,IU,OU
      CHARACTER*120 INFILE,OUTFILE
      LOGICAL EOF,ERROR,READ4KB, WRITE4KB
C
      IU = 10
      OU = 11
C
C     Read the command line for arguments
      K = IARGC() 
C     Both filenames must exist
      IF (K.LT.2) THEN
         WRITE(0,*) "fix16c-> ERROR: Input & Output files ",
     +               "must be specified"
         CALL EXIT(-1)
         RETURN
      ENDIF
C
C     Get the input and output file names.
      CALL GETARG(1,INFILE)
      CALL GETARG(2,OUTFILE)
C
C     Open input file.  This is for g77.  You mileage will vary.
      OPEN(UNIT=IU,FILE=INFILE, ACCESS='DIRECT', FORM='UNFORMATTED', 
     +     STATUS='OLD', RECL=4096 )
C
C     Open input file.  This is for g77.  You mileage will vary.
      OPEN(UNIT=OU,FILE=OUTFILE, ACCESS='DIRECT',  
     +      FORM='UNFORMATTED', RECL=4096 )

C
      EOF = .FALSE.
      ERROR = .FALSE.
C
      DO WHILE (.NOT.EOF)
         EOF = READ4KB(IU,  IBUFF)
         IF (EOF) CYCLE
C        Make the low bits of the first byte = 0xc =1100
         I=IOR(IAND(IBUFF(1),INT(240,1)), INT(12,1))
         IBUFF(1) = I
C        Make the lowest bits of the next 3 words = 011
         I=IOR(IAND(IBUFF(2),INT(254,1)), INT(0,1))
         IBUFF(2) = I
         I=IOR(IAND(IBUFF(3),INT(254,1)), INT(1,1))
         IBUFF(3) = I
         I=IOR(IAND(IBUFF(4),INT(254,1)), INT(1,1))
         IBUFF(4) = I
         ERROR = WRITE4KB(OU,IBUFF)
      ENDDO
      
      CLOSE(IU)
      CLOSE(OU)
      END









