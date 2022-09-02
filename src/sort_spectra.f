C
C====+==================================================================+==
C
      SUBROUTINE SORT_SPECTRA(SORT1,SORT2,SORT3,SORT4,SORT5,NSRT, ISEQ)
C
C     This subroutine takes 5 tables of signal characteristics,
C     typically Frequency, Polarization, Zenith, Azimuth and Doppler,
C     and returns the sequence of channel numbers that sorts these
C     keys in increasing order, from min to max value.
C     NSRT is the number (1-4) of the SORT's to perform
C     This is a simple formatting trick.
C
C     ***WARNING***  This code uses a simple multiplication trick to
C     rank the values relative to each other.  This seems weak and
C     prone to failure.
C
      REAL SORT1(16),SORT2(16),SORT3(16),SORT4(16),SORT5(16),SORT(16)
      INTEGER ISEQ(16),NSRT,I
C
      DO I=1,16
         ISEQ(I) = I
      ENDDO
C
      IF ((NSRT.LT.1).OR.(NSRT.GT.5)) RETURN
      DO I=1,16
         SORT(I) = SORT1(I)
         IF(NSRT.GE.2) SORT(I)=400.*SORT(I)+SORT2(I)
         IF(NSRT.GE.3) SORT(I)=400.*SORT(I)+SORT3(I)
         IF(NSRT.GE.4) SORT(I)=400.*SORT(I)+SORT4(I)
         IF(NSRT.GE.5) SORT(I)=400.*SORT(I)+SORT5(I)
      ENDDO
      CALL INDEXX(16,SORT,  ISEQ)

      RETURN
      END


















