C
C  ========================================================================
C
      LOGICAL FUNCTION WRITE4KB(OU,IBUF)
C
C     Subroutine to write 4096 bytes from opened file unit OU
C     Uses the values in IBUF
C     Return value is TRUE if there is some problem, such as EOF
C     Specialized for Linux g77 compiler which does not easily support
C       binary files
C
      INTEGER I,IER,OB,OU
      INTEGER*1 IBUF(4096)
      DATA OB /0/
      SAVE OB
C
      OB = OB + 1

      WRITE(OU,IOSTAT=IER,REC=OB) (IBUF(I),I=1,4096)
      WRITE4KB = (IER.NE.0)
C
      RETURN
      END

