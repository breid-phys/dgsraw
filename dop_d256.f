C      
C  =======================================================================
C
      SUBROUTINE DOP_D256(PREFACE,   NDOPP,DFR)
C
C     Determines the number and spacing (Doppler Frequency Resolutuion) of
C     the Ionogram Doppler bins.
C     Some principles:
C      - The maximum number of unique Doppler lines is 16 [-8..8]
C      - There is never a zero Doppler in a D256.
C      - The Doppler spectrum is always symmetric about zero

      INTEGER*1 PREFACE(57)
      INTEGER NDOPP,NDT(0:3,4),I,J,IT,IH,IX,IT2,IN,IR,IW
      INTEGER IDC,NB,NPB,ISM
      REAL CIT,DFR
      LOGICAL H8, X123, X04567, X8, IT4
      REAL PRF_D256
C
C     Preface Parameters
      IT = PREFACE(47)
      IT2 = IAND(IT,3)
      IH = PREFACE(54)
      IX = PREFACE(44)
      IN = PREFACE(48)
      IR = PREFACE(49)
      IW = PREFACE(50)
      IT4 = (IAND(IT,4).NE.0)
C
      NDOPP = 0
C
C     This is the enumeration of Table 5.7, p96 in the 'Green Book"
      DO I= 0, 3
         DO J=1,4
            NDT(I,J) = INT(16/2**(I+J))
         ENDDO
      ENDDO
C
C     Some logicals to help decide which column of NDT() to use
      H8 = (IH.GE.8)
      X8 = (IX.GE.8)
      X123 = (IX.EQ.1).OR.(IX.EQ.2).OR.(IX.EQ.3)
      X04567 = (IX.EQ.0).OR.((IX.GE.4).AND.(IX.LE.7))
C
      IF ((.NOT.H8).AND.X04567) THEN
C        H<8;X=0,4,5,6,7
         IDC = 1
      ELSE IF ((.NOT.X8.AND.X123) .OR. (X8.AND.X04567)) THEN
C        H<8;X=1,2,3 -or- H>=8;X=0,4,5,6,7 
         IDC = 2
      ELSE IF (.NOT.(H8.OR.X8).OR.(H8.AND.X123)) THEN
C        X<8;X>=8 or H>=8;X=1,2,3
         IDC = 3
      ELSE IF(H8.AND.X8) THEN
C        H>=8; X>=8
         IDC=4
      ELSE
         IDC = 1
         WRITE(0,*) 'Doppler Table Error..'
      ENDIF
C
      NDOPP = 2*NDT(IT2,IDC)
C
C     NB is the 'Number of Beams' or simultaneous interlaced signals
      IF (NDOPP.EQ.0) THEN
         NB = 0
      ELSE
         NB = 16/NDOPP
      ENDIF
C     NPB is the Number of Pulses per Beam
      NPB = 2**(IN+1)
C     ISM is the Number of Sample adjuster for the W parameter
C                     **** untested ****
      IF (IAND(IW,2).EQ.0) THEN
         ISM = 1
      ELSE
         ISM = 2
      ENDIF

C     Determine the Coherent Integration Time (CIT,sec)
      CIT=REAL(NB*NPB*ISM)/PRF_D256(IR)
C     Now the Doppler Frequency Resolution, which is also the spacing.
      DFR = 1.0
      IF (CIT.NE.0.0) DFR = 0.50/CIT
      IF (IT4) DFR = 2.0*DFR

      RETURN
      END
