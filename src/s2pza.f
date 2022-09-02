C
C====+==================================================================+==
C
      INTEGER FUNCTION S2PZA(PREFACE,ISTAT)
C
C     Convert Status to PZA byte using the ZT tables, Corrected for L
C
C
      INTEGER Z,T,L,ISTAT,TE,ISE,LR
      INTEGER*1 PREFACE(57)

C     Tables in a common block
      INTEGER IZT(0:7,0:7,8)
      COMMON /ZTTABLE/ IZT
C
C     Ignore the high bit on Z
      Z = IAND(PREFACE(46),INT(7,KIND(PREFACE)))
C
C     Ignore the 4 bit on T
      T = PREFACE(47)
      TE = IAND(T,3) + IAND(T,8)/2

C     The 'effective' status, ignoring the high bit and shifted to
C     the range 1-8
      ISE = IAND(ISTAT,7)+1
C
      S2PZA = IZT(Z,TE,ISE)
C
C     Correct for L
      L = PREFACE(45)
      IF ((L.EQ.0).OR.(L.GT.12)) THEN
C        These are odd single-antnena or non-beamforming modes
cdb         WRITE(0,*) 'Warning:  Value of L= ',L,' is unsupported.'
      ELSE
C        The impact of L is to increment the AAAA portion of the 
C        0PVZAAAA byte in S2PZA, with 13 wrapping back to 1, etc.
         LR = IAND(S2PZA,15)
         LR = LR + (L-1)
         IF (LR.GT.12) LR = LR - 12
         S2PZA = IAND(S2PZA,240) + LR
      ENDIF         
      RETURN
      END
