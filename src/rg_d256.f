C
C====+==================================================================+==
C
      SUBROUTINE RG_D256(PREFACE,  NRG, RGT)
C
C     Given the PREFACE, returns the number of range gates
C     and their values
C
      INTEGER*1 PREFACE(57)
      INTEGER NRG
      REAL RGT(256)
C
C
      INTEGER IH, IE, IBL
      REAL HTBL(0:15),ETBL(5)
C
C
      DATA HTBL /2.5, 5.0, 10.0, 2.5, 0.0, 0., 0., 0.,
     +           2.5, 5.0, 10.0, 2.5, 5.0, 0., 0., 0. /
      DATA ETBL /10.0, 60.0, 160.0, 380.0, 760.0/
C
C     The range gates are determined by preface parameters H and E
C     as defined in the Digisonde 256 "Green Book"
      IH = PREFACE(54)
      IE = PREFACE(55)
C
      IF ((IH.LT.0).OR.(IH.GT.15).OR.(IE.LT.1).OR.IE.GT.5) THEN
         WRITE(0,*) "UNDEFIEND RANGE GATES H=",IH," E=",IE
         NRG = 0
         RETURN
      ENDIF
C
      IF (IH.GE.8) THEN
         NRG = 256
      ELSE
         NRG = 128
      ENDIF
C
C     The standard linear range gate modes
      DO I=1,NRG
         RGT(I) = ETBL(IE) + REAL(I-1)*HTBL(IH)
      ENDDO
C
C     Now the bi-linear modes. IBL is the # of gates at the lower ranges
      IBL = 0
      IF (IH.EQ.3) THEN
         IBL = 40
      ELSE IF ((IH.EQ.11).OR.(IH.EQ.12)) THEN
         IBL = 128
      ENDIF
C
C     For all these modes the RG spacing is double for the upper gates
      IF (IBL.EQ.0) RETURN
      DO I = 1, IBL
         RGT(I) = ETBL(IE) + REAL(I-1)*HTBL(IH)
      ENDDO
      DO I = IBL+1, NRG
         RGT(I) = ETBL(IE) + 2.0*REAL(I-1)*HTBL(IH)
      ENDDO
C
      RETURN
      END
