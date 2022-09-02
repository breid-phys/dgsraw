C
C====+==================================================================+==
C
      SUBROUTINE CHANNEL_MAP(PREFACE, FRQ,APOL,HDOP,AZ,EL,NBEAM,NDOPP)
C
C     T. Bullett, AFRL, 18 January 2002
C
C     This subroutine performs the rather complex task of supplying the
C     'meta-data' for the 16 channels of the 16 channel Digisonde 256 
C     data.  The meta-data, or data about data, provide information on the
C     context of these different channels.  For the Digisonde 256, the
C     'channel number' can specify Polarization, Doppler and Arrival Angle.
C     It is the precise values of these that this routine computes, based
C     instrument settings.
C     There are lots of caviats here, mostly falling in categoryies:
C       A) That particular setting was not coded or debugged
C       B) Your Digisonde may have different values in its PROMs or
C          was installed differently.
C     The numerical values of polarization are set to +-90.0 to be
C     consistent with the 'chirality' factor of the Dynasonde
C       -90.0  is Ordinary  ; +90.0 is eXtraordinary
C        (J.W. Wright, private communication)
C
C     Output tables are indexed by the channel number(1-16) and contain:
C       FRQ  - Sounding Frequency (MHz)
C       APOL - Rx Antenna Polarization Mode O (-90.0 )or X (+90.0)
C       HDOP - Doppler shift, Hz
C       AZ   - Rx antenna beam azimuth, degrees
C       EL   - Rx antenna beam elevation, degrees
C       PZA  - The PZA or 'Meaning' byte defined by UMLCAR (cf Galkin 2001) 
C
C     Internal Variables
C       IBM(4) - Beam Mask to determine which channels are different beams
C       IST(16) - Status Table as a function of channel #
C       IS2RD(4,16) - Status to Relative Doppler Shift
C       IRDOP(16) - Relative Doppler Shift, in 'bins'
C       IB  - Beam #
C       IDD - Doppler Number
C
C     11Apr02 TWB
C        Added an array for the sounding frequency of each channel.
C        This is needed to support Precision Group Height (PGH) modes.
C         *** CURRENTLY UNIMPLEMENTED DUE TO LACK OF TEST DATA **
C     24Jul08 TWB
C        Code review.  Some comments added.
C
C     Working with the 4 bit status number, the IDX upper bits
C     are for Doppler, 4-IDX lower bits are IB 'beams'.
C     The relative Doppler index is the upper IDX
C     bits of the Status.  The high bit of these is the sign
C     of the Doppler shift (1=negative).
C     This is embodied in the IS2RD table.
C     Example:  For a T=1 ionogram (O/X and 8 Dopplers), the
C               4 status bits are SDDB where
C               S  = Doppler sign
C               DD = Doppler Magnitude (kind of)
C               B  = Beam number (O or X)
C     
C     * * * W A R N I N G * * * THIS DOES NOT WORK FOR 256 HEIGHTS
C     I HAVE NOT BEEN ABLE TO FIGURE OUT HOW 256 HEIGHT DATA ARE
C     PLACED INTO THE 16 CHANNELS.  I THINK IT IS EVEN/ODD
C
C
C====+==================================================================+==
C

      INTEGER*1 PREFACE(57)
      INTEGER I,IC2S(4,16),NBEAM,NDOPP,IDX,IBM(4),IB,IDD,III
      INTEGER IST(16),IRDOP(16),IS2RD(4,0:15)
C
      REAL FRQ(16),APOL(16),HDOP(16),AZ(16),ZN(16),DFR
      INTEGER PZA(16),S2PZA
      REAL PZA2ZN,PZA2AZ,FRQ_D256

C
      DATA IBM /7,3,1,0/
C
C----- Channel to Status Table
C
C     T=3 (8 beams, 2 Dopplers)
      DATA (IC2S(1,I),I=1, 8) / 8, 9,10,11,12,13,14,15/
      DATA (IC2S(1,I),I=9,16) / 0, 1, 2, 3, 4, 5, 6, 7/
C     T=2 (4 beams, 4 Dopplers)
      DATA (IC2S(2,I),I=1, 8) / 8, 9,10,11,12,13,14,15/
      DATA (IC2S(2,I),I=9,16) / 4, 5, 6, 7, 0, 1, 2, 3/
C     T=1 (2 beams, 8 Dopplers)
      DATA (IC2S(3,I),I=1, 8) / 8, 9,10,11,12,13,14,15/
      DATA (IC2S(3,I),I=9,16) / 6, 7, 4, 5, 2, 3, 0, 1/
C     T=0 (1 beam, 16 Dopplers)
      DATA (IC2S(4,I),I=1, 8) / 8, 9,10,11,12,13,14,15/
      DATA (IC2S(4,I),I=9,16) / 7, 6, 5, 4, 3, 2, 1, 0/
C
C     Status to Relative Doppler number.  The first index is 
C     opposite in sense to the previous table.
C     T=0 (1 beam, 16 Dopplers)
      DATA (IS2RD(4,I),I=0, 7) /-8, -7, -6, -5, -4, -3, -2, -1/
      DATA (IS2RD(4,I),I=8,15) / 1,  2,  3,  4,  5,  6,  7,  8/
C     T=1 (2 beams, 8 Dopplers)
      DATA (IS2RD(3,I),I=0, 7) /-4, -4, -3, -3, -2, -2, -1, -1/ 
      DATA (IS2RD(3,I),I=8,15) / 1,  1,  2,  2,  3,  3,  4,  4/
C     T=2 (4 beams, 4 Dopplers)
      DATA (IS2RD(2,I),I=0, 7) /-2, -2, -2, -2, -1, -1, -1, -1/
      DATA (IS2RD(2,I),I=8,15) / 1,  1,  1,  1,  2,  2,  2,  2/
C     T=3 (8 beams, 2 Dopplers)
      DATA (IS2RD(1,I),I=0, 7) /-1, -1, -1, -1, -1, -1, -1, -1/
      DATA (IS2RD(1,I),I=8,15) / 1,  1,  1,  1,  1,  1,  1,  1/

      SAVE IC2S,IS2RD
C
C
C====+==================================================================+==
C
C     Get the number and spacing for the doppler lines.
      CALL DOP_D256(PREFACE,  NDOPP,DFR)
      NBEAM = 16/NDOPP
C
C     IDX gives us the Channel-to-Status Table and the 
C     Status-to-Relative-Doppler Table row number to use
C     What I do here is make IDX the log base 2 of NDOPP
      IDX=3
      DO I=1,4
         IF ((NDOPP/2**I).EQ.1) IDX = I
      ENDDO
C
      DO ICH=1,16
C        The 'status' value for each channel
         IST(ICH) = IC2S(IDX,ICH)
C        Beam # for this channel
         IB = IAND(IBM(IDX),IST(ICH))
C        Relative Doppler for this 'status'
         IDD = IS2RD(IDX, IST(ICH))
         IRDOP(ICH) = IDD
         HDOP(ICH) = DFR*REAL(IDD)

C        Doppler table is complete.
C
C        Now look up the non-Doppler metadata by the Beam#
C        using the 'PZA Byte' concept (c.f. Galkin)
         PZA(ICH) = S2PZA(PREFACE, IST(ICH))
         APOL(ICH) = PZA2POL(PZA(ICH))
C        **** Need to make a PZA2AZ and PZA2ZN
         AZ(ICH) = PZA2AZ(PZA)
         ZN(ICH) = PZA2ZN(PZA)
cdb      write(*,'(A,6I4,4F8.3)') 'Ch,St,DD,B,DOP,POL,AZ,ZN',
cdb     +   ICH,IST(ICH),IDD,IB,IRDOP(ICH),PZA(ICH),
cdb     +   HDOP(ICH),APOL(ICH),AZ(ICH),ZN(ICH)

      ENDDO
C
C
C====+==================================================================+==
C
C     Set the Frequencies.
      III = PREFACE(56)
      IF (III.LT.4) THEN
C        Normal sounding mode
         FREQNOM = FRQ_D256(PREFACE)
         DO I = 1,16
            FRQ(I) = FREQNOM
         ENDDO
      ELSE
C        PGH mode.  ****UNIMPLEMENTED****
C        This is just a copy of the normal mode.
         FREQNOM = FRQ_D256(PREFACE)
         DO I = 1,16
            FRQ(I) = FREQNOM
         ENDDO
      ENDIF
C
C
      RETURN
      END
C
C====+==================================================================+==
C
