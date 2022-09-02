C
C====+==================================================================+==
C
      SUBROUTINE PREF_GPP(IBUF,  IPREF)
C
C     Extract the 'General Purpose' (i.e. DPS specific) preface from
C     RSF and  SBF data
C
C
      INTEGER IP,IE,IRT,IPEND
      INTEGER*1 IBUF(4096),IPREF(57),ML
      INTEGER IRTYPE
      LOGICAL GOODIRT
      PARAMETER(ML=-1)
C
      IRT=IRTYPE(IBUF)
      GOODIRT=((IRT.EQ.7).OR.(IRT.EQ.6).OR.(IRT.EQ.2).OR.(IRT.EQ.3))
      IF (.NOT.GOODIRT) THEN
         WRITE(0,*) 'pref_GPP --> Bad record type ',IRT
         RETURN
      ENDIF
C
cdb      write(*,'(60I3)') (i,i=-2,57)
cdb      write(*,'(60(1x,Z2))') (IBUF(I),I=1,60)
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




