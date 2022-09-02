C
C====+==================================================================+==
C
      SUBROUTINE FIXPHASE(RGT,EPHASE,MAXRBIN)
C
C     The Digisondes all perform complex (In-phase and Quadrature)
C     sampling of the receiver output by measuring the 225 kHz IF
C     signal twice at time separation equal to 90 degrees of phase 
C     in the IF.  This is 1.11111 us.
C     A consequence of this is that if successive range gates are not
C     spaced at integer multiples of the IF period (4.444444us) then
C     there will be a phase difference induced by this sampling
C     at the IF.  For dR=1.000 km, this is a 6.6667us dt, or 1.50
C     IF cycles.  For dR=5.000 km, there are 7.500 IF cycles between
C     successive range gates, thus every other range gate is 180 degree
C     phase offset from its neighbors.
C
C     This routine corrects this, referencing all phases to the first
C     range gate and removing the 360 degree multiples.
C
C     29Jun03 TWB The 2.5km data show the phase increases with range thus
C                 needs to be subtracted.  For 5km data this did not matter.
C
      INTEGER I,J,MAXRBIN
      REAL R0,DP,EP
      REAL RGT(256),EPHASE(256,16)
C
      R0 = RGT(1)
      DO I=2,MAXRBIN
         DP = 540.0*(RGT(I)-R0)
         DO J=1,16
            EP  = EPHASE(I,J) - DP
            EPHASE(I,J) = 360.0*INT(EP/360.0) - EP
         ENDDO
      ENDDO
      RETURN
      END
C
