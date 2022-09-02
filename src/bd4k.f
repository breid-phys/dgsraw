      PROGRAM BD4K
C
C     Block Dump 4K bytes with printing
      INTEGER IU,I
      CHARACTER*120 INFILE
      INTEGER*1 IBUF(4096)

      LOGICAL EOF,READ4KB

      IU=1
C     Parse the command line.  Not very robust here, but it works. Mostly.
      CALL GETARG(1,INFILE)

C     Open input file.  This is for g77.  You mileage will vary.
      OPEN(UNIT=IU,FILE=INFILE, ACCESS='DIRECT', FORM='UNFORMATTED', 
     +     STATUS='OLD', RECL=4096 )
C
      EOF=READ4KB(IU,IBUF)
      DO WHILE (.NOT.EOF)
         WRITE(*,101) (IBUF(I),i=1,20)
 101     format ('Read-> ',20(1X,Z2.2))
         EOF=READ4KB(IU,  IBUF)
      ENDDO

      END
C
C  ========================================================================
C
      LOGICAL FUNCTION READ4KB(IU,  IBUF)
C
C     Subroutine to read 4096 bytes from opened file unit IU
C     Returns the values in IBUF
C     Return value is TRUE if there is some problem, such as EOF
C     Specialized for Linux g77 compiler which does not easily support binary files
C
      INTEGER I,IER,IB
      INTEGER*1 IBUF(4096)
      DATA IB /0/
      SAVE IB
C
      IB = IB + 1
      DO I=1,4096
         IBUF(I)=0
      ENDDO
      READ(IU,IOSTAT=IER,REC=IB) (IBUF(I),I=1,4096)
      READ4KB = (IER.NE.0)

      RETURN
      END






