C
C====+==================================================================+==
C
      SUBROUTINE RG_RSF(PREFACE,  NRG, RGT)
C
C     Given the PREFACE, returns the number of range gates
C     and their values for an RSF format ionogram
C
C     28Jul04  T. Bullett
C
C====+==================================================================+==
C
      INTEGER*1 PREFACE(57)
      INTEGER NRG
      REAL RGT(512)
C
      REAL E, H
      INTEGER I, NOMRG

C     Functions
      INTEGER UNP_BCD2, UNP_BCD4

C
C     The start height E
      E = REAL(UNP_BCD4(PREFACE(33),PREFACE(34)))
C     The range increment H
      H = REAL(UNP_BCD2(PREFACE(35)))
      IF (H.EQ.2.0) H = 2.5
C     The nominal number of range bins.  Not all of these are stored
      NOMRG = UNP_BCD4(PREFACE(36),PREFACE(37))
      IF (NOMRG.EQ.128) NRG = 128
      IF (NOMRG.EQ.256) NRG = 249
      IF (NOMRG.EQ.512) NRG = 501
C     
      DO I = 1, NRG
         RGT(I) = E + REAL(I-1)*H
      ENDDO
C
      RETURN
      END
C
C====+==================================================================+==
C
