C
C  =======================================================================
C
      LOGICAL FUNCTION UNPACK_16C(IBUF,RAWAMP,   
     +                    TIME,PREFACE,FREQ,EAMPS,EPHASE,RGT,MAXRBIN)
C
C     This subroutine takes a 4K block of 16 channel amplitude and phase
C     Digisonde 256 data provides the following output.
C
C        PREFACE - The decoded preface for this ionogram
C        TIME - Time of the observation: YYYY DDD HH:MM:SS
C        FREQ - The precise sounding frequency, in MHz.
C               *NOTE* This will be slightly wrong for PGH modes, but it 
C               is still the frequency in the preface.
C        EAMPS(256,16) - Received amplitudes vs range gate and channel # ,
C                        in dB and corrected for all D256 processor settings
C                        but not for any antenna gains, cable losses, etc.
C        EPHASE(256,16)- Received phase vs range and channel #, in degrees
C        RGT(256) - Range Gate Table assigns a range in km to each range gate.
C        MAXRBIN -  Maximum range bin number (128 or 256)
C
C     Interpetation of the channel number in terms of polarization, Doppler,
C     and Rx antenna beam position (arrival angle) is left to another routine.
C
C     RAWAMP is a control flag to disable amplitude scaling.
C
      REAL FREQ, EAMPS(256,16), EPHASE(256,16), RGT(256)
      CHARACTER*17 TIME
      INTEGER*1 IBUF(4096),PREFACE(57)
      INTEGER MAXRBIN,MAXCHAN, IRTYPE
      LOGICAL RAWAMP
C
C     Local variables
      INTEGER I,IR,IC,YYYY,ITMP
      INTEGER*1 MASK
      REAL DBSCALE, DEG8
C
C     FUNCTIONS
      REAL FRQ_D256
C
      PARAMETER (DEG8=360.0/256.0,MASK=255)
C
C     Zero the arrays
      DO IR=1,256
         RGT(IR)=0.0
         DO IC=1,16
            EAMPS(IR,IC)=0.0
            EPHASE(IR,IC)=0.0
         ENDDO
      ENDDO
      TIME=""
      FREQ=0.0
      MAXRBIN=0
C
C     Check for 16 channel data type.  Exit if not
      ITMP = IRTYPE(IBUF)
      IF ((ITMP.NE.12).AND.(ITMP.NE.13)) THEN
         UNPACK_16C = .FALSE.
         RETURN
      ENDIF

      CALL PREF_16C(IBUF, PREFACE)
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
C
C     Get the frequency value
      FREQ = FRQ_D256(PREFACE)
C
C     Set the range gate values
      CALL RG_D256(PREFACE,  MAXRBIN, RGT)
      MAXCHAN = 16
      IF (MAXRBIN.EQ.256) MAXCHAN=8

C     The amplitude scale on the ionograms, from
C     4 upper bits give 4 or 6 dB per count depending on Z<8
      DBSCALE = 4.0/16.0
      IF (PREFACE(46).LT.8) DBSCALE = 1.5*DBSCALE
C
      AUTOGAIN = 6.0*REAL(PREFACE(53))
C      
C     Extract the amplitudes and phases from the array.
C     They are unsigned 8 bit integers so we have to treat them carefully
C     NOTE:  This won't work for 256 range data
      DO IR=1,MAXRBIN
         DO IC=1,MAXCHAN
            IAX=256*(IC-1)+IR
            ITMP=IAND(MASK,IBUF(IAX))
            IF(RAWAMP) THEN
               EAMPS(IR,IC)=REAL(ITMP)
            ELSE
               EAMPS(IR,IC)=DBSCALE*REAL(ITMP) + AUTOGAIN
            ENDIF
            IPX=IAX+128
            ITMP=IAND(MASK,IBUF(IPX))
            IF (RAWAMP) THEN
               EPHASE(IR,IC)=REAL(ITMP)
            ELSE
               EPHASE(IR,IC)=DEG8*REAL(ITMP)
            ENDIF
         ENDDO
      ENDDO
C
      UNPACK_16C = .TRUE.
C
C     todo: Move data around for the 256 range gate modes.
C           Need to get some 256 range data for testing!
C           Modify to handle BLOCKTYPE=13 (Amplitude only, 2 freqs/block)
      RETURN
      END






















