C
C====+==================================================================+==
C
      INTEGER FUNCTION PRELUDE_SBF(IFRQ,PREFACE,IBUF,IPOL,FREQ,IGS)
C
C     Decodes the PRELUDE data for entry IFRQ in F data block IBUF.
C     Time data are inserted into the active PREFACE() array, which is
C     where they came from in the first place.
C
C     MPA is an extra value and this is returned as the function value.
C     A value of -1 is returned if a request is made beyond the end of data.
C 
C     Prelude is 6 bytes of High and Low nibbles:
C     1H :  1=D256 ; 2=X Polarization (DPS), 3=O polarization (DPS)     
C     1L :  Data Group size (1=134; 2=262 ; 3=504 ; 4=1004)
C     IFS: The I* frequency search setting
C     IGS: The G* AutoGain setting
C     ISEC: Seconds of the minute
C     MPA: Most Probable Amplitude (0-31)
C
C     16Mar03  TWB
C     28Jul04  TWB -Added polarization (IPOL) which is the high nibble.  
C                   This is required for RSF & SBF format where the block
C                   could be O or X polarization.
C                  -Adapted routine to handle either MMM or RSF preface.
C                  -Added FREQ return variable of the Prelude Freq setting
C                   because DPS does not have this in the PREFACE.
C     09Aug04  TWB -Spawned prelude_mmm, prelude_sbf and prelude_rsf 
C                   because of increasingly complex variables between
C                   the preludes of these formats
C
C     Requires:
C        UNP_BCD2 IRTYPE  FRQ_DPS
C
C====+==================================================================+==
C

      INTEGER IFRQ,IPOL
      INTEGER*1 PREFACE(57),IBUF(4096)
      REAL FREQ
C     
C     Local Varibales
      INTEGER IBL,IDT,IOFF,NFR,NPC,LSEC,IGS,NGS(4),MININC,IFY(4),II,
     +        IRG,IMIN,ISEC,MFR(4)
      INTEGER*1 M4L,M4H
      PARAMETER (M4L=15,M4H=240)
      DATA NGS /134,262,504,1008/
      DATA MFR /30, 15, 8, 4/
      DATA LSEC /-1/
      DATA MININC /0/
      SAVE LSEC,MININC,NGS,MFR
C
C     Functions
      INTEGER IRTYPE, UNP_BCD2
      INTEGER*1 P_BCD2
      REAL FRQ_DPS
C
C
      PRELUDE_SBF = -1

C     IDT is the Data Type 
      IDT = IRTYPE(IBUF)
C
C     NPC is the Number of Preface Characters
      NPC = IBUF(2)
C
      IBL=IAND(IBUF(NPC+1),M4L)
      IF((IBL.LT.1).OR.IBL.GT.4) THEN
         WRITE(0,*) 'Prelude-> Bad Block Type = ',IBL
         IBL = 0
         RETURN
      ENDIF
C
C     NFR is the Number of Frequencies per Record.  IRG are the number 
C     of range gates in this a-scan.  Assumes this cannot
C     change in the midst of a data block.
      IRG=NGS(IBL)
      NFR=MFR(IBL)
C
      IF (IFRQ.GT.NFR) THEN
         LSEC=-1
         RETURN
      ENDIF
C
C     Determine which section of the data block we need
      IOFF= IRG*(IFRQ-1) + NPC
C
cdb      write(*,*) irg,nfr,ioff,ibuf(ioff+1),mininc
cdb      write(*,102) 'Prelude= ', (ibuf(ioff+i),i=1,6)
C
C     Check to see that there are legit data in this section
C     The 0x00 value in this position is 'end of data' for SBF
      IF (IBUF(IOFF+1).EQ.0) THEN
         LSEC=-1
         RETURN
      ENDIF
C
C     Polarization
      IPOL=IAND(IBUF(IOFF+1),M4H)/16
C
C     Place the Prelude values in the correct Preface locations
C     for the new DPS or 'General Purpose' format prefaces.
C     Frequency (Does not go into the Preface)
      IFY(1)=IAND(IBUF(IOFF+2),M4H)/16 
      IFY(2)=IAND(IBUF(IOFF+2),M4L)
      IFY(3)=IAND(IBUF(IOFF+3),M4H)/16
      IFY(4)=IAND(IBUF(IOFF+3),M4L)
C     Frequency Search i
      II=IAND(IBUF(IOFF+4),M4H)/16
      FREQ = FRQ_DPS(IFY,II)  
C
C     AutoGain G*
      IGS= IAND(IBUF(IOFF+4),M4L)
C     The time is more of a problem, because we can roll over seconds.
C     Also, since we re-decode the preface after each frequency, we have
C     to save the time deltas over the whole block.
      IF (IFRQ.EQ.1) MININC=0
C     *******BUG********
C     This will NOT roll over minutes into hours, etc. so a measurement that
C     spans an hour in the middle of the data block will have bad time. 
C     by having more than 60 minutes in the hour.
C     Some decoders can handle this.
C
      IMIN=UNP_BCD2(PREFACE(7))
      ISEC=UNP_BCD2(IBUF(IOFF+5))

      IF (ISEC.LT.LSEC) MININC = MININC + 1
      PREFACE(7) = P_BCD2(IMIN+MININC)
      PREFACE(8) = P_BCD2(ISEC)
      LSEC=ISEC
C     The MPA
      PRELUDE_SBF=UNP_BCD2(IBUF(IOFF+6))
C
C
 102  FORMAT (A,6(1X,Z2.2))
C
      RETURN
      END
C
C====+==================================================================+==
