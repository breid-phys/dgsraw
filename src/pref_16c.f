C
C =============================================================================
C
      SUBROUTINE PREF_16C(IBUF,  IPREF)
C
C     Extract the preface encoded into the LSB of the amplitude data in the
C     16 channel data blocks.
C     Preface characters 1-31 found in bytes    5-128 for both data types
C     Preface characters 32-57 found in bytes 257-360 for IRTYPE=12
C     Preface characters 32-64 found in bytes 129-232 for IRTYPE=???


      INTEGER I,IP,IE,IRT,IPEND
      INTEGER*1 IBUF(4096),IPREF(57),M1L
      INTEGER IRTYPE
      PARAMETER(M1L=1)

      IRT=IRTYPE(IBUF)
      IPEND=57
      DO IP = 1,IPEND
         IPREF(IP) = 0
         IE = 4*IP+1
c         IF ((IP.GT.31).AND.(IRT.EQ.12)) IE = IE + 128
         IF ((IP.GT.31)) IE = IE + 128
         DO I = 0,3
            IPREF(IP)=IOR(IPREF(IP),ISHFT(IAND(IBUF(IE+I),M1L),I))
         ENDDO
      ENDDO
      RETURN
      END
