C
C====+==================================================================+==
C
      LOGICAL FUNCTION UNPACK_MMM(IBUF,   
     +            TIME,PREFACE,FREQ,FNOISE,EAMPS,EPHASE,IC,RGT,MAXRBIN)
C
C     This subroutine takes a 4K block of MMM ionogram data and is built
C       on a similar routine for decoding 16 channel data.  There are some
C       features here unneeded for 16C data.
C
C     Digisonde 256 data provides the following output.
C        PREFACE - The decoded preface for this ionogram
C        TIME - Time of the observation: YYYY DDD HH:MM:SS
C        FREQ - The precise sounding frequency, in MHz.
C               *NOTE* This will be slightly wrong for PGH modes, but it 
C               is still the frequency in the preface.
C        MPA - The Most Probable Amplitude, or Noise, from Preface.
C        EAMPS(256,16) - Received amplitudes vs range gate and channel # ,
C                        in dB and corrected for all D256 processor settings
C                        but not for any antenna gains, cable losses, etc.
C        EPHASE(256,16)- Received phase vs range and channel #, in degrees
C        IC(256) -  The MMM channel number for the 
C        RGT(256) - Range Gate Table assigns a range in km to each range gate.
C        MAXRBIN -  Maximum range bin number (128 or 256)
C
C     Interpetation of the channel number in terms of polarization, Doppler,
C     and Rx antenna beam position (arrival angle) is left to another routine.
C
C     Revisions:
C     16Mar03 TWB - Initial version from the 16C base code.
C        --Since there are multiple frequencies in each block,
C          this code returns .FALSE. when a new block of data
C          needs to be read.
C
C     16Jan05 TWB - Fixed MPA=0 bug for most MMM data
C     12Jan08 TWB - Changed PRELUDE() to PRELUDE_MMM() 
C
C
C====+==================================================================+==
C
      REAL FREQ, EAMPS(256,16), EPHASE(256,16), RGT(256),FNOISE
      CHARACTER*17 TIME
      INTEGER*1 IBUF(4096),PREFACE(57)
      INTEGER MAXRBIN,MAXCHAN, IRTYPE,IC(256)
C
C     Local variables
      INTEGER I,IR,YYYY,ITMP,MASK,MASKL,MASKH,IFRQ,MAXFRQ,IPL,MPA
      INTEGER IPOL,IGS
      REAL DBSCALE, DEG8
C
C     Functions
      INTEGER PRELUDE_MMM
C
      PARAMETER (DEG8=360.0/256.0,MASK=255,MASKH=240,MASKL=15)
C
C     IFRQ is the index of which frequency in the block is being decoded
      SAVE IFRQ
      DATA IFRQ /0/
C
C     Zero the arrays
      DO IR=1,256
         RGT(IR)=0.0
         DO I=1,16
            EAMPS(IR,I)=0.0
            EPHASE(IR,I)=0.0
         ENDDO
      ENDDO
      TIME=""
      FREQ=0.0
      MAXRBIN=0
C
C     Check for MMM data type.  Exit if not
      ITMP = IRTYPE(IBUF)
      IF ((ITMP.NE.9).AND.(ITMP.NE.8)) THEN
         UNPACK_MMM = .FALSE.
         RETURN
      ENDIF

      CALL PREF_MMM(IBUF, PREFACE)
C
C     There are various block types for MMM data.  This code
C     is hardwired to the BlockType=1 data, because 99% of the
C     recorded Digisonde 256 data are in this format.
C     These can be computed from the first prelude. (See PRELUDE())
      MAXCHAN = 1
      MAXFRQ = 30
C
      IFRQ = IFRQ + 1
      IF (IFRQ.GT.MAXFRQ) THEN
         UNPACK_MMM=.FALSE.
         IFRQ = 0
         RETURN
      ENDIF
C
C     Set the range gate values
      CALL RG_D256(PREFACE,  MAXRBIN, RGT)
C
C     Get the prelude data for the IFRQ entry in IBUF.
C     This actually modifies the PREFACE() array, except for MPA
      MPA=PRELUDE_MMM(IFRQ,PREFACE,IBUF,IPOL,FREQ,IGS)
      IF (MPA.EQ.-1) THEN
         UNPACK_MMM=.FALSE.
         IFRQ = 0
         RETURN
      ENDIF

C
      YYYY=10*PREFACE(1)+PREFACE(2)
      IF (YYYY.LT.82) THEN
         YYYY=YYYY+2000
      ELSE
         YYYY=YYYY+1900
      ENDIF
      WRITE(TIME,117) YYYY, (PREFACE(I),I=3,11)
 117  FORMAT (I4.4,1X,3I1,1X,2I1,':',2I1,':',2I1)
C
C     Get the frequency value
      FREQ = FRQ_D256(PREFACE)
C
C     The amplitude scale on the ionograms, from
C     4 upper bits give 4 or 6 dB per count depending on Z<8
      DBSCALE = 4.0/16.0
      IF (PREFACE(46).LT.8) DBSCALE = 1.5*DBSCALE
C
      AUTOGAIN = 6.0*REAL(PREFACE(53))
C
C     The MPA is 0-31, or twice that of the 0-15 amplitudes.
C     but the amplitudes, as the upper 4 bits of each byte,
C     come out as 16x their 0-15 range, much like the 16C data.
C
C     BUG:  Many MMM data have an MPA of zero.
C           This is generally not possible
      IF (MPA.EQ.0) THEN
         FNOISE = 0.0
      ELSE
         FNOISE = REAL(8*MPA)*DBSCALE + AUTOGAIN
      ENDIF
cx      write(*,*) dbscale,MPA,fnoise,preface(53),autogain
      
C     This is the preface length (57), plus the 3 lead characters
C     plus the 6 prelude characters
      IPL=IBUF(2) + 6
C      
C     Extract the amplitudes and phases from the array.
C     They are unsigned 8 bit integers so we have to treat them carefully
C     NOTE:  This won't work for 256 range data
      DO IR=1,MAXRBIN
         DO I=1,MAXCHAN
            IAX=IPL + (MAXRBIN+6)*(IFRQ-1) + MAXRBIN*(I-1) + IR
            IC(IR)=IAND(MASKL,IBUF(IAX)) + 1
            ITMP=IAND(MASKH,IBUF(IAX))
            EAMPS(IR,IC(IR))=DBSCALE*REAL(ITMP) + AUTOGAIN
            EPHASE(IR,IC(IR))=0.0
cx            write(*,*) iax,itmp,eamps(ir,ic(ir)),ic(ir)
         ENDDO
      ENDDO
C
      UNPACK_MMM = .TRUE.
C
C     todo: Move data around for the 256 range gate modes.
C           Need to get some 256 range data for testing!
      RETURN
      END
C
C====+==================================================================+==
C
