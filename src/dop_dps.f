C
C====+==================================================================+==
C
      REAL FUNCTION DOP_DPS(NDOP,PREFACE)
C
C
C     Compute the Doppler shift of Doppler Number NDOP for the DPS ionogram
C     with PREFACE.  Note the CIT value in the Preface is not useful for 
C     this purpose.  It is said to be OK in drift modes, but not ionograms.
C
C     10Aug04  TWB
C
C====+==================================================================+==
C
      INTEGER NDOP
      INTEGER*1 PREFACE(57)
C
      REAL CIT,PRF
      INTEGER NFFT,NPOL,NCOMP,NSSTEP,IX,IN,IS
C
      REAL DOPTAB(0:7)
      SAVE DOPTAB
      DATA DOPTAB/ -7., -5., -3., -1., +1., +3., +5., +7./
C
C     Functions
      INTEGER UNP_BCD2,UNP_BCD4
C
C     Number of small steps
      IS = UNP_BCD2(PREFACE(27))
      NSSTEP = 1
      IF(IS.GT.1) NSSTEP = IS
C
C     Phase Code.  Only works for complementary and short pulse
      NCOMP = 1
      IX = UNP_BCD2(PREFACE(28))
      IF (IX.EQ.1) NCOMP = 2
C
C     Polarization
      NPOL = 2
      IA = UNP_BCD2(PREFACE(29))
      IF(IA.GT.7) NPOL=1
C
C     FFT Size
      IN = UNP_BCD2(PREFACE(30))
      NFFT = 2**IN
C
C     Pulse Repetition Frequency
      PRF = REAL(UNP_BCD4(PREFACE(31),PREFACE(32)))
      PRF = MAX(1.0,PRF)
C      
      CIT = REAL(NFFT*NPOL*NCOMP)/PRF

cdb      write(*,*) ix,ncomp,ia,npol,in,nfft,prf,cit

      DOP_DPS = DOPTAB(NDOP)/(2.0*CIT)
      RETURN
      END
C
C====+==================================================================+==
C
