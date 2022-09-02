C
C====+==================================================================+==
C
      SUBROUTINE PREF_MMM(IBUF,  IPREF)
C
C     Extract the preface for MMM format data.
C
C
      INTEGER IP,IE,IRT,IPEND
      INTEGER*1 IBUF(4096),IPREF(57),ML
      INTEGER IRTYPE
      PARAMETER(ML=15)

      IRT=IRTYPE(IBUF)
      IF ((IRT.NE.9).AND.(IRT.NE.8)) THEN
         WRITE(0,*) 'pref_mmm --> Bad record type ',IRT
         RETURN
      ENDIF
C
cdb      write(*,'(50(1x,Z1))') (IBUF(I),I=1,50)
C     This should be 57
      IPEND=IBUF(2) - 3
      DO IP = 1,IPEND
         IPREF(IP) = 0
         IE = IP+3
         IPREF(IP)=IAND(IBUF(IE),ML)
      ENDDO
      RETURN
      END
C
C====+==================================================================+==




