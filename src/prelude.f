C
C====+==================================================================+==
C
      INTEGER FUNCTION PRELUDE(IFRQ,PREFACE,IBUF,IBT,FREQ,IGS)
C
C     Decodes the PRELUDE data for entry IFRQ in MMM data block IBUF.
C     These data are inserted into the active PREFACE() array, which is
C     where they came from in the first place.  This makes the Preface
C     valid for MMM data just like 16C data.
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
C     28Jul04  TWB -Added BlockType (IBT) which is the high nibble.  
C                   This is required for RSF format where the block
C                   could be O or X polarization.
C                  -Adapted routine to handle either MMM or RSF preface.
C                  -Added FREQ return variable of the Prelude Freq setting
C                   because DPS does not have this in the PREFACE.
C
C     Requires:
C        UNP_BCD2 IRTYPE  FRQ_D256  FRQ_DPS
C
C====+==================================================================+==
C

      INTEGER IFRQ,IBT
      INTEGER*1 PREFACE(57),IBUF(4096)
      REAL FREQ
C     
C     Local Varibales
      INTEGER IBL,IDT,IOFF,NFR,NPC,LSEC,IGS,NGS(4),MININC,IFY(4),II,
     +        IRG
      INTEGER M4L,M4H
      PARAMETER (M4L=15,M4H=240)
      LOGICAL GPP
      DATA NGS /134,262,504,1004/
      DATA LSEC /-1/
      DATA MININC /0/
      SAVE LSEC,MININC
C
C     Functions
      INTEGER IRTYPE, UNP_BCD2
      REAL FRQ_D256, FRQ_DPS
C
C
      PRELUDE = -1

C     IDT is the Data Type 
      IDT = IRTYPE(IBUF)
C
C     GPP is the General Purpose Preface Flag.  If false, use the old 
C         MMM or D256 format preface.
      GPP=((IDT.EQ.7).OR.(IDT.EQ.6))
C
C     NPC is the Number of Preface Characters
      NPC = IBUF(2)
C
      IBL=IAND(IBUF(NPC+1),M4L)
      IF((IBL.LT.1).OR.IBL.GT.4) THEN
         WRITE(0,*) 'Prelude-> Bad Block Type = ',IBL
         RETURN
      ENDIF
C
C     NFR is the Number of Frequencies per Record.  IRG are the number 
C     of range gates in this a-scan.  Assumes this cannot
C     change in the midst of a data block.
      IRG=NGS(IBL)
      NFR=INT(4096/IRG)
C
      IF (IFRQ.GT.NFR) THEN
         LSEC=-1
         RETURN
      ENDIF
C
C     Determine which section of the data block we need
      IOFF= IRG*(IFRQ-1) + NPC
C
cdb      write(*,*) ioff,ibuf(ioff+1),mininc
cdb      write(*,102) 'Prelude= ', (ibuf(ioff+i),i=1,6)
C
C     Check to see that there are legit data in this section
C     The 0x0E value in this position is 'end of data'
      IF (IBUF(IOFF+1).EQ.14) THEN
         LSEC=-1
         RETURN
      ENDIF
C
      IF(GPP) THEN
C        Place the Prelude values in the correct Preface locations
C        for the new DPS or 'General Purpose' format prefaces.
C        Frequency (Does not go into the Preface)
         IFY(1)=IAND(IBUF(IOFF+2),M4H)/16 
         IFY(2)=IAND(IBUF(IOFF+2),M4L)
         IFY(3)=IAND(IBUF(IOFF+3),M4H)/16
         IFY(4)=IAND(IBUF(IOFF+3),M4L)
C        Frequency Search i
         II=IAND(IBUF(IOFF+4),M4H)/16
         FREQ = FRQ_DPS(IFY,II)
   
C        AutoGain G*
         IGS= IAND(IBUF(IOFF+4),M4L)
C        The time is more of a problem, because we can roll over seconds.
C        Also, since we re-decode the preface after each frequency, we have
C        to save the time deltas over the whole block.
         IF (IFRQ.EQ.1) MININC=0
C        *******BUG********
C        This will NOT roll over minutes into hours, etc. so a measurement that
C        spans an hour in the middle of the data block will have bad time. 
C        by having more than 60 minutes in the hour.
C        Some decoders can handle this.
C
         ISEC=UNP_BCD2(IBUF(IOFF+5))
         IF (ISEC.LT.LSEC) MININC = MININC + 1
         PREFACE(9) = PREFACE(9) + MININC
         IF(PREFACE(9).GT.9) THEN
            PREFACE(8)=PREFACE(8)+1
            PREFACE(9)=PREFACE(9)-10
         ENDIF
C
         PREFACE(10)=IAND(IBUF(IOFF+5),M4H)/16
         PREFACE(11)=IAND(IBUF(IOFF+5),M4L)
         LSEC=ISEC
C        The MPA
         PRELUDE=UNP_BCD2(IBUF(IOFF+6))
      
      ELSE
C        Place the Prelude values in the correct Preface locations
C        for the old D256 format prefaces.
C        Frequency
         PREFACE(20)=IAND(IBUF(IOFF+2),M4H)/16 
         PREFACE(21)=IAND(IBUF(IOFF+2),M4L)
         PREFACE(22)=IAND(IBUF(IOFF+3),M4H)/16
         PREFACE(23)=IAND(IBUF(IOFF+3),M4L)
C        The 1 kHz and 100 Hz positions are lost in MMM format.
         PREFACE(24)=0
         PREFACE(25)=0
         FREQ = FRQ_D256(PREFACE)
C        Frequency Search i
         PREFACE(52)=IAND(IBUF(IOFF+4),M4H)/16
C        AutoGain G*
         IGS= IAND(IBUF(IOFF+4),M4L)
         PREFACE(53)=IGS
C        The time is more of a problem, because we can roll over seconds.
C        Also, since we re-decode the preface after each frequency, we have
C        to save the time deltas over the whole block.
         IF (IFRQ.EQ.1) MININC=0
C        *******BUG********
C        This will NOT roll over minutes into hours, etc. so a measurement that
C        spans an hour in the middle of the data block will have bad time. 
C        by having more than 60 minutes in the hour.
C        Some decoders can handle this.
C
         ISEC=(10*IAND(IBUF(IOFF+5),M4H)/16) + IAND(IBUF(IOFF+5),M4L)
         IF (ISEC.LT.LSEC) MININC = MININC + 1
         PREFACE(9) = PREFACE(9) + MININC
         IF(PREFACE(9).GT.9) THEN
            PREFACE(8)=PREFACE(8)+1
            PREFACE(9)=PREFACE(9)-10
         ENDIF
C
         PREFACE(10)=IAND(IBUF(IOFF+5),M4H)/16
         PREFACE(11)=IAND(IBUF(IOFF+5),M4L)
         LSEC=ISEC
C        The MPA
         PRELUDE=IBUF(IOFF+6)
      ENDIF
      
C
C
 102  FORMAT (A,6(1X,Z2.2))
C
      RETURN
      END
C
C====+==================================================================+==









