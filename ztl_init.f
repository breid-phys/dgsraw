C
C  ========================================================================
C
      SUBROUTINE ZTL_INIT()
C
C     Initialize the ZT table
C     This routine sets the values for the ZT table used for
C     determining Rx antenna beams, etc.  It is initialized this way
C     to allow for a custom table to be loaded early in the main program,
C     perhaps from a file.
C
C     The Meaning byte is defined as 0PVZAAAA where
C        P = Polarization (0=Ordinary, 1=eXtraordinary)
C        V = Vertical  (0=Not vertical, 1 = Vertical)
C        Z = Zenith Angles (0 for small(11deg), 1 for Large (22deg))
C        AAAA = Azimuth Value (0-12)
C
C     ToDo: Allow this to optionally read from an UMLCAR format ZT file.
C
C     Source: Green Book table 5.9, P.98, Revised 11Jun86,5Nov87,28Dec88
C

      INTEGER IZT(0:7,0:7,8)
      COMMON /ZTTABLE/ IZT
C
C    Z=0,8 T=0,1,2,3, 8,9,A,B
      DATA (IZT(0,0,I),I=1,8) /32, 32, 32, 32, 32, 32, 32, 32/
      DATA (IZT(0,1,I),I=1,8) /17, 17, 17, 17, 17, 17, 17, 17/
      DATA (IZT(0,2,I),I=1,8) /17, 18, 96, 32, 17, 18, 96, 32/
      DATA (IZT(0,3,I),I=1,8) /17, 18, 19, 20, 21, 22, 96, 32/
      DATA (IZT(0,4,I),I=1,8) /17, 17, 17, 17, 17, 17, 17, 17/
      DATA (IZT(0,5,I),I=1,8) /17, 18, 17, 18, 17, 18, 17, 18/
      DATA (IZT(0,6,I),I=1,8) /17, 18, 19, 20, 17, 18, 19, 20/
      DATA (IZT(0,7,I),I=1,8) /17, 18, 19, 20, 21, 22, 23, 24/
C    Z=1,9 T=0,1,2,3, 8,9,A,B
      DATA (IZT(1,0,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(1,1,I),I=1,8) /17,  1, 17,  1, 17,  1, 17,  1/
      DATA (IZT(1,2,I),I=1,8) /17, 17, 32, 32, 17, 17, 32, 32/
      DATA (IZT(1,3,I),I=1,8) /17, 19, 21, 23, 25, 27, 96, 32/
      DATA (IZT(1,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(1,5,I),I=1,8) /17, 19, 17, 19, 17, 19, 17, 19/
      DATA (IZT(1,6,I),I=1,8) /17, 17, 17, 17, 17, 17, 17, 17/
      DATA (IZT(1,7,I),I=1,8) /17, 17, 17, 17, 19, 19, 19, 19/
C    Z=2,A T=0,1,2,3, 8,9,A,B
      DATA (IZT(2,0,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(2,1,I),I=1,8) /17, 32, 17, 32, 17, 32, 17, 32/
      DATA (IZT(2,2,I),I=1,8) /81, 81, 17, 17, 81, 81, 17, 17/
      DATA (IZT(2,3,I),I=1,8) /81, 81, 81, 81, 17, 17, 17, 17/
      DATA (IZT(2,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(2,5,I),I=1,8) /17, 20, 17, 20, 17, 20, 17, 20/
      DATA (IZT(2,6,I),I=1,8) /17, 20, 23, 26, 17, 20, 23, 26/
      DATA (IZT(2,7,I),I=1,8) /17, 17, 17, 17, 32, 32, 32, 32/
C    Z=3,B T=0,1,2,3, 8,9,A,B
      DATA (IZT(3,0,I),I=1,8) /81, 81, 81, 81, 81, 81, 81, 81/
      DATA (IZT(3,1,I),I=1,8) /81, 17, 81, 17, 81, 17, 81, 17/
      DATA (IZT(3,2,I),I=1,8) /17, 23, 96, 32, 17, 23, 96, 32/
      DATA (IZT(3,3,I),I=1,8) /17, 17, 23, 23, 96, 96, 32, 32/
      DATA (IZT(3,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(3,5,I),I=1,8) /17, 23, 17, 23, 17, 23, 17, 23/
      DATA (IZT(3,6,I),I=1,8) /17, 23,  1,  7, 17, 21,  1,  7/
      DATA (IZT(3,7,I),I=1,8) /17, 17, 17, 17, 23, 23, 23, 23/
C    Z=4,C T=0,1,2,3, 8,9,A,B
      DATA (IZT(4,0,I),I=1,8) /96, 96, 96, 96, 96, 96, 96, 96/
      DATA (IZT(4,1,I),I=1,8) / 1,  1,  1,  1,  1,  1,  1,  1/
      DATA (IZT(4,2,I),I=1,8) / 1,  2, 96, 32,  1,  2, 96, 32/
      DATA (IZT(4,3,I),I=1,8) / 1,  2,  3,  4,  5,  6, 96, 32/
      DATA (IZT(4,4,I),I=1,8) / 1,  1,  1,  1,  1,  1,  1,  1/
      DATA (IZT(4,5,I),I=1,8) /17, 19, 17, 19, 17, 19, 17, 19/
      DATA (IZT(4,6,I),I=1,8) /17, 17, 17, 17, 17, 17, 17, 17/
      DATA (IZT(4,7,I),I=1,8) /17, 17, 17, 17, 19, 19, 19, 19/
C    Z=5,D T=0,1,2,3, 8,9,A,B
      DATA (IZT(5,0,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(5,1,I),I=1,8) /96, 32, 96, 32, 96, 32, 96, 32/
      DATA (IZT(5,2,I),I=1,8) /96, 96, 32, 32, 96, 96, 32, 32/
      DATA (IZT(5,3,I),I=1,8) / 1,  3,  5,  7,  9, 11, 96, 32/
      DATA (IZT(5,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(5,5,I),I=1,8) / 1,  3,  1,  3,  1,  3,  1,  3/
      DATA (IZT(5,6,I),I=1,8) / 1,  1,  1,  1,  1,  1,  1,  1/
      DATA (IZT(5,7,I),I=1,8) / 1,  1,  1,  1,  3,  3,  3,  3/
C    Z=6,E T=0,1,2,3, 8,9,A,B
      DATA (IZT(6,0,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(6,1,I),I=1,8) / 1, 32,  1, 32,  1, 32,  1, 32/
      DATA (IZT(6,2,I),I=1,8) /65, 65,  1,  1, 65, 65,  1,  1/
      DATA (IZT(6,3,I),I=1,8) /65, 65, 65, 65,  1,  1,  1,  1/
      DATA (IZT(6,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(6,5,I),I=1,8) / 1,  4,  1,  4,  1,  4,  1,  4/
      DATA (IZT(6,6,I),I=1,8) / 1,  4,  7, 10,  1,  4,  7, 10/
      DATA (IZT(6,7,I),I=1,8) / 1,  1,  1,  1, 17, 17, 17, 17/
C    Z=7,F T=0,1,2,3, 8,9,A,B
      DATA (IZT(7,0,I),I=1,8) /65, 65, 65, 65, 65, 65, 65, 65/
      DATA (IZT(7,1,I),I=1,8) /65,  1, 65,  1, 65,  1, 65,  1/
      DATA (IZT(7,2,I),I=1,8) / 1,  7, 96, 32,  1,  7, 96, 32/
      DATA (IZT(7,3,I),I=1,8) /96, 96, 96, 96, 32, 32, 32, 32/
      DATA (IZT(7,4,I),I=1,8) / 0,  0,  0,  0,  0,  0,  0,  0/
      DATA (IZT(7,5,I),I=1,8) / 1,  7,  1,  7,  1,  7,  1,  7/
      DATA (IZT(7,6,I),I=1,8) /32, 32, 32, 32, 32, 32, 32, 32/
      DATA (IZT(7,7,I),I=1,8) / 1,  1,  1,  1,  7,  7,  7,  7/
C
C     Insert code here to read from sdtin or a file

      RETURN
      END
