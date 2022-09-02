C
C====+==================================================================+==
C
      LOGICAL FUNCTION UNPACK_DPSSBF(IBUF,   
     +            TIME,PREFACE,FREQ,FNOISE,EAMPS,EPHASE,APOL,HDOP,AZ,ZN,
     +            RGT,MAXRBIN,HINOISE,IRXCONF,ZENITH)
C
C     This subroutine takes a 4K block of DPS SBF ionogram data.
C     The SBF is somewhat easier than MMM and 16C because there are
C     fewer meta-data to be divined from the preface.  Info about
C     Beams and Doppler are data for each range gate.
C
C     The DPS SBF format data provides the following output.
C        PREFACE - The decoded preface for this ionogram
C        TIME - Time of the observation: YYYY DDD HH:MM:SS
C        FREQ - The precise sounding frequency, in MHz.
C        MPA - The Most Probable Amplitude, or Noise, from Preface.
C        EAMPS(512) - Received amplitudes vs range gate,
C                        in dB and corrected for all DPS processor settings
C                        but not for any antenna gains, cable losses, etc.
C        EPHASE(512)- Received phase vs range and channel #, in degrees
C        APOL(512)- Polarization.  The numerical values of polarization are 
C                   set to +-90.0 to be consistent with the 'chirality' factor 
C                   of the Dynasonde 
C                  (-90.0  is Ordinary  ; +90.0 is eXtraordinary
C        HDOP(512) - Doppler shift, Hz
C        AZ(512)   - Rx antenna beam azimuth, degrees
C        ZN(512)   - Rx antenna beam zenith angle, degrees
C        RGT(512)  - Range Gate Table assigns a range in km to each range gate.
C        MAXRBIN   - Maximum range bin number (128, 256, 512)
C        HINOISE   - The value from the ARMENU.DPS file that controls the gain
C                     of the first RF stage.
C        IRXCONF   - Reciver array configuration, from ARMENU.DPS file
C                     1 =  Standard
C                     2 =  Mirror
C                     3 =  Rotated
C        ZENITH    - Zenith angle, from ARMENU.DPS 
C                   
C
C     Each SBF range bin has 1 byte:
C       -------------------
C       | A A A A A D D D |     A = Amplitude, D = Doppler Number
C       -------------------
C
C     Requires:
C      INTEGER PRELUDE_SBF  UNP_BCD2  AGC_DPS  DOP_DPS  AZ_DPS  TIME_GPP 
C              RG_DPS
C
C     Revisions:
C     07Aug04 TWB - Initial version from UNPACK_DPSRSF v0.43; 
C                   parallel development
C     12Jan08 TWB - Changed PRELUDE() to PRELUDE_SBF()
C
C
C====+==================================================================+==
C
      REAL FREQ, EAMPS(512),EPHASE(512),APOL(512),HDOP(512),AZ(512),
     +     ZN(512),RGT(512),FNOISE, ZENITH
      CHARACTER*17 TIME
      INTEGER*1 IBUF(4096),PREFACE(57)
      INTEGER MAXRBIN,IRTYPE,HINOISE,IRXCONF
C
C     Local variables
      INTEGER*1 MASKL,MASKH
      INTEGER IR,ITMP,MASK,IFRQ,MAXFRQ,IPL,MPA,
     +        IG,IGS,IPOL
      REAL DBSCALE, DEG8,POLTAB(0:3)
C
C     Functions
      INTEGER PRELUDE_SBF, UNP_BCD2 
      REAL AGC_DPS,DOP_DPS 
      CHARACTER*17 TIME_GPP
C
C     MASKH are the upper 5 bits ; MASKL are the lower 3 
      PARAMETER (DEG8=360.0/256.0,MASK=255,MASKH=248,MASKL=7)
C
C     IFRQ is the index of which frequency in the block is being decoded
      SAVE IFRQ, POLTAB
      DATA IFRQ /0/
      DATA POLTAB/ 0.0, 0.0, 90.0, -90.0 / 

C
C     Zero the arrays
      DO IR=1,512
         EAMPS(IR)=0.0
         EPHASE(IR)=0.0
         APOL(IR)=0.0
         HDOP(IR)=0.0
         AZ(IR)=0.0
         ZN(IR)=0.0
         RGT(IR)=0.0
      ENDDO
      TIME=""
      FREQ=0.0
      MAXRBIN=0
      MAXFRQ=0
C
C     Check for SBF data type.  Exit if not
      ITMP = IRTYPE(IBUF)
cdb      write(*,*) 'irtype= ',ITMP
      IF ((ITMP.NE.3).AND.(ITMP.NE.2)) THEN
         UNPACK_DPSSBF = .FALSE.
         RETURN
      ENDIF
C
C     Extract the ionogram 'General Purpose' Preface.
      CALL PREF_GPP(IBUF, PREFACE)
C
C     The maximum number of frequencies in a 4K block.
      MAXFRQ = 30
C
      IFRQ = IFRQ + 1
      IF (IFRQ.GT.MAXFRQ) THEN
         UNPACK_DPSSBF=.FALSE.
         IFRQ = 0
         RETURN
      ENDIF
C
C     Set the range gate values
      CALL RG_SBF(PREFACE,  MAXRBIN, RGT)
C
Cpre     Get the prelude data for the IFRQ entry in IBUF.
      MPA=PRELUDE_SBF(IFRQ,PREFACE,IBUF, IPOL,FREQ,IGS)
cdb      write(*,*) ifrq,ipol,freq,igs,mpa
      IF (MPA.EQ.-1) THEN
         UNPACK_DPSSBF=.FALSE.
         IFRQ = 0
         RETURN
      ENDIF
C     Get the time from the GP Preface
      TIME=TIME_GPP(PREFACE)
C
C     Figure out the receiver gain.  IG is base gain.  IGS is G* or g,
C     the variable part from the prelude.

      IG = UNP_BCD2(PREFACE(40))
      HINOISE = UNP_BCD2(PREFACE(46))
      AUTOGAIN = AGC_DPS(IG,IGS,HINOISE)
      GAIN = AUTOGAIN
      DBSCALE = 3.0

      FNOISE=REAL(MPA)*DBSCALE + GAIN
cdb      write(*,*) dbscale,MPA,fnoise,ig,igs,autogain,hinoise
      
C     This is the preface length (57), plus the 3 lead characters
C     plus the 6 prelude characters
      IPL=IBUF(2) + 6
C      
C     Extract the Amplitudes Dopplers,Phases and Beams from the array.
C     There are 2 bytes pre range bin.
C     They are unsigned 8 bit integers so we have to treat them carefully
      DO IR=1,MAXRBIN
         IAX=IPL + (MAXRBIN+6)*(IFRQ-1) + IR
C        Amplitude
         ITMP=IAND(MASKH,IBUF(IAX))/8
         EAMPS(IR)=DBSCALE*REAL(ITMP) + GAIN
C        Doppler
         ITMP=IAND(MASKL,IBUF(IAX))
         HDOP(IR) = DOP_DPS(ITMP, PREFACE)
         APOL(IR) = POLTAB(IPOL)
      ENDDO
C
      UNPACK_DPSSBF = .TRUE.
C
      RETURN
      END
C
C====+==================================================================+==
C
