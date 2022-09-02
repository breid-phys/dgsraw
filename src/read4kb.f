C
C  ========================================================================
C
      LOGICAL FUNCTION READ4KB(IU,  IBUF)
C
C     Subroutine to read 4096 bytes from opened file unit IU
C     Returns the values in IBUF
C     Return value is TRUE if there is some problem, such as EOF
C     Specialized for Linux g77 compiler which does not easily support
C       binary files
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

cdb      write(*,101) IB, (IBUF(I),i=1,20)
cdb  101  format ('Read-> ',I4,'->',20(1X,Z2))
      RETURN
      END
