C
C====+==================================================================+==
C
      SUBROUTINE BRPD(TIME,PREFACE,FREQ,EAMPS,EPHASE,RGT,MAXRBIN,
     +            NECHO,FNOISE,DFRQ,DRNG,DPOL,DDOP,DAZ,DZN,DAMP,
     +            DPHASE,DCHSNR,DPHASV)
C
C
C     Terence Bullett, AFRL   11 April 2002
C     Distibution: Limited, B
C
C     Detect echos using consistency in Beam, Range, Phase and Doppler
C     (BRPD or Burped for short)
C
C     This routine accepts the fully decoded 16 channel data from a 
C     Digisonde 256 and performs a Range:Doppler:Phase:Beam echo
C     detection process.  The data must have consistenty in Range, 
C     Doppler and Phase.
C
C     ??? What to do about the multiple beams  ???
C     - Report all and let calling routine select them (for now)
C
C     All beams which meet these criteria and in the order of
C     Range, Doppler and Phase.
C
C     An Range gate is an echo if and only if:
C     1)It is an amplitude peak in range, the adjcent range gates are
C         monotonic decreasing and the difference between the peak
C         and the lowest amplitude within NARGT bins is at least AMPDIF
C
C       ISSUES: This detects peaks, not maximum slopes.  May need to adjust.
C               May fail in 2.5 km, 10 & 20 km range sampling modes!
C               Spread-F and dispersion?
C
C     2)It is a peak in Doppler, and at least one adjcent Doppler bins
C       has comparable (within 6 dB) signal and comparable phase.
C       ISSUES: Doppler spread clutter?
C               Same Doppler bin for each range gate? Small amp differences?
C
C     3)The variance of the received phase in the 6+ points used for 
C       Steps 1) and 2) must be less than an empirical threshold.
C       ISSUES: Number of points and the threshold value may vary with
C               SNR, and surely with range gate sampling. Perhaps all
C               adjcent points in the Range-Doppler plane above a given SNR?
C
C     The arrays DFRQ,etc.. contain data about the Detected echoes.
C     Array ARDB and PRDB are 3d arrays indexed on Range, Doppler and Beam
C     and contain Amplitude and Phase data respectively.  Echoes are
C     detected from these arrays.
C
C     29May03 TWB Added DCHSNR, the Channel SNR, with noise computed only from
C                 the channel in which the signal was detected.
C     29Jun03 TWB Modifed peak range detection to allow assymetry.
C
C     ToDo:
C       Make code independent of range gate spacing
C         !! There are many constants in this code that will need updating!!!
C       Return echo dispersion, phase variance, noise??
C       Test with :
C          O/X mode (T=1)
C          Multi-Beam mode(s) (T=2,3)
C          256 range gate data (maybe never)
C
C
C====+==================================================================+==
C
C
C     Output values: arrays of echo parameters.
      INTEGER MAXECHO,NECHO
      PARAMETER (MAXECHO=100)
      REAL FNOISE
      REAL DFRQ(MAXECHO),DRNG(MAXECHO),DPOL(MAXECHO),DDOP(MAXECHO),
     +     DAZ(MAXECHO),DZN(MAXECHO),DAMP(MAXECHO),DPHASE(MAXECHO),
     +     DPHASV(MAXECHO),DCHSNR(MAXECHO)

C     Internal variables
      INTEGER IB,ID,IR, ICS(16), IDE,IDJ
C
C     ADP, IDP, JDP are the Amplitude, Index and adjcent Index of the echo
C     at each range relative the the current one under test.
      INTEGER IDP(-10:10), JDP(-10:10)
      REAL ADP(-10:10)
C
      REAL ARDB(0:256,16,8), PRDB(0:256,16,8)
      REAL EAMPS(256,16),EPHASE(256,16),RGT(256)
      INTEGER IC(256)
      REAL FREQ, A1,PHRMS,DADIF,AMPMIN
      CHARACTER*17 TIME
      INTEGER*1 PREFACE(57)
      INTEGER MAXRBIN,NARGL,NARGH
      LOGICAL AMPOK, DOPOK, ECHO, AP, PH 
C
C     Functions
      REAL AMPV, WGTAVEDB
C
C     Meta-data from 16 channel datafiles
      REAL FRQ(16),APOL(16),HDOP(16),AZ(16),ZN(16)
C
C     Detection Tresholds
C     Maximum phase difference for adjcent Dop bin
      PHDIF = 30.0
C     AMPDIF is the Minimum Amplitude Difference of a real echo 
C            about the peak.  This might be a function of NARGT
      AMPDIF = 3.0
C     Maximim Doppler Amplitude Difference for adjcent Dop bin 
      DADIF = 6.0
C     Maximum RMS of phases over the whole echo
      PHRMS = 40.0
C
C     NARGT is the Number of Adjcent Range Gates which must have signal.
C     ToDo:  Make this a function of range gate spacing and Tx pulsewidth!
C     Consequence: No echoes can be detected within NARGT of the ends!
C     ToDo: Re-code to allow even numbers of gates.
C
      NARGL = 1
      NARGH = 2
C     ==============================================================
C
C     Fix the phases.
      CALL FIXPHASE(RGT,EPHASE,MAXRBIN)
C
C     Determine the channel meta data from the preface
      CALL CHANNEL_MAP (PREFACE, FRQ,APOL,HDOP,AZ,EL,NBEAM,NDOPP)
C
C     Sort the channels to change the order
      CALL SORT_SPECTRA(FRQ,APOL,HDOP,AZ,EL,5,  ICS)
C
C     Calculate the 'noise floor' from all the channels
      FNOISE = AMPV(EAMPS,IC,16,-1,MAXRBIN)
C
C     Place the amplitude and phase data into ARDB and PRDB arrays
C     Will probably need a Doppler table here.
C     The zero range index of ARDB(0,ID,IB) is the noise in that channel
      DO IB = 1, NBEAM
         DO ID = 1, NDOPP
            ICH = NDOPP*(IB-1) + ID
            MAPCH= 2**ICS(ICH)
            ARDB(0,ID,IB) = AMPV(EAMPS,IC,16,MAPCH,MAXRBIN)
cdb            write(*,*) 'MAPCH =', ARDB(0,ID,IB),FNOISE
            DO IR = 1, MAXRBIN
               ARDB(IR,ID,IB) =  EAMPS(IR,ICS(ICH))
               PRDB(IR,ID,IB) = EPHASE(IR,ICS(ICH))
            ENDDO
          ENDDO
      ENDDO
C
C     Start detecting echoes.
C
      NECHO = 0
C     IDE is the Index Doppler of the Echo
      IDE=1
C     IDJ is the adJecent Doppler Index.
      IDJ=1
      EP=0.0
      EPV=0.0
      ECHO=.FALSE.
C
C     For each Beam
      DO IB = 1, NBEAM         
C       Search in Range for potential echoes.
        DO IR = 1+NARGL, MAXRBIN-NARGH
          ECHO = .TRUE.
          write(0,*) ''
          write(0,'(A,F8.4,F8.1)') 'Check Freq,Range=',FREQ,RGT(IR)
          DO I = -NARGL, NARGH
            KR = IR + I
C           Determine the maximum doppler amplitude at this range, 
C           which adjcent bin is the one with greater signal, and
C           if the phase is consistent.
C           ISSUE: Look for relative peaks if more than 2 dopplers??
            ADP(I) = -999.0
            IDP(I) = 1
            JDP(I) = 1
        write(0,1)'Checking DopAmps ',(ARDB(KR,K,IB),K=1,NDOPP)
        write(0,1)'Checking DopPhas ',(PRDB(KR,K,IB),K=1,NDOPP)
 1      FORMAT (A,8F10.2, I4)

C           Find the maximum doppler amplitude
            DO ID=1,NDOPP
              IF (ARDB(KR,ID,IB).GT.ADP(I)) THEN
                 ADP(I) = ARDB(KR,ID,IB)
                 IDP(I) = ID
              ENDIF
            ENDDO
C           Determine if max dopper is a peak /w adjcent signal & 
C           constant phase (i.e. 2 doppler bins must have signal)
C           Boundary conditions at the ends of the spectra are handled
            IDE=IDP(I)
            IF (IDE.EQ.1) THEN
C             Adjecent can only be higher Doppler number.
              IDJ=IDE+1
              AP=((ARDB(KR,IDE,IB)-ARDB(KR,IDJ,IB)).LE.DADIF)
              PH=(ABS(PRDB(KR,IDE,IB)-PRDB(KR,IDJ,IB)).LE.PHDIF)
              DOPOK=(AP.AND.PH)
            ELSE IF (IDE.EQ.NDOPP) THEN
C             Adjecent can only be lower.
              IDJ=IDE-1
              AP=((ARDB(KR,IDE,IB)-ARDB(KR,IDJ,IB)).LE.DADIF)
              PH=(ABS(PRDB(KR,IDE,IB)-PRDB(KR,IDJ,IB)).LE.PHDIF)
              DOPOK=(AP.AND.PH)
            ELSE IF (ARDB(KR,IDE+1,IB).GT.ARDB(KR,IDE-1,IB)) THEN
C             Upper side is larger, use its Amp&Phase as adjcent
              IDJ=IDE+1
              AP=((ARDB(KR,IDE,IB)-ARDB(KR,IDJ,IB)).LE.DADIF)
              PH=(ABS(PRDB(KR,IDE,IB)-PRDB(KR,IDJ,IB)).LE.PHDIF)
              DOPOK=(AP.AND.PH)
            ELSE
C             Adjecent is the lower side, use this Amp&Phase
              IDJ=IDE-1
              AP=((ARDB(KR,IDE,IB)-ARDB(KR,IDJ,IB)).LE.DADIF)
               PH=(ABS(PRDB(KR,IDE,IB)-PRDB(KR,IDJ,IB)).LE.PHDIF)
              DOPOK=(AP.AND.PH)
            ENDIF
C           Save the index of the adjcent doppler line
            JDP(I)=IDJ
            ECHO=(ECHO.AND.DOPOK)

        write(0,*)'I,IB,IR,KR,IDE,IDJ,AP,PH,DOPOK= ',
     +             I,IB,IR,KR,IDE,IDJ,AP,PH,DOPOK
C
C         This goes to the next range bin (I) in the echo
          ENDDO

          IF (.NOT.ECHO) CYCLE
          write(0,*) 'Passed DopCheck #1'
C
C         If the ECHO variable comes out true, there are spectra
C         on all of the NARGL+NARGH+1 adjecent range gates.  Now check that
C         the peak and adjcent lines are the same one for all range gates.
C         Note: Noise can cause the peak and adjcent lines to swap, 
C         especially of the Doppler is very near zero, thus these lines are
C         very nearly equal.
C
          DOPOK = .TRUE.
          IDE = IDP(0)
          IDJ = JDP(0)
          DO I = -NARGL, NARGH
             DOPOK = DOPOK .AND.
     +            ( (IDP(I).EQ.IDE).AND.(JDP(I).EQ.IDJ) .OR. 
     +              (IDP(I).EQ.IDJ).AND.(JDP(I).EQ.IDE) )         
          ENDDO
          write(0,*) 'SameDopCheck= ', DOPOK
          ECHO = ECHO .AND. DOPOK
          IF (.NOT.ECHO) CYCLE
C
C         Now check that the amplitude at rangegate IR and
C         for doppler line IDE is the peak in range, and that
C         the amplitude falls off at least AMPDIF at one end.
          AMPOK = .TRUE.
C         Look above the peak
          DO I=0,NARGH-1
            A1 = ARDB(IR+I,IDE,IB) - ARDB(IR+I+1,IDE,IB)
            AMPOK = (AMPOK .AND.(A1.GE.0.0))
            write(0,*) I,A1,AMPOK
          ENDDO
C         Look below the peak
          DO I=0,1-NARGL
            A1 = ARDB(IR+I,IDE,IB) - ARDB(IR+I-1,IDE,IB)
            AMPOK = (AMPOK .AND.(A1.GE.0.0))
            write(0,*) I,A1,AMPOK
          ENDDO
C
C         Look for the minimum value over the adjcent rangegates
          AMPMIN=999.9
          DO I=-NARGL,NARGH 
             IF(ARDB(IR+I,IDE,IB).LT.AMPMIN) AMPMIN=ARDB(IR+I,IDE,IB)
          ENDDO
          AMPOK = AMPOK.AND.((ARDB(IR,IDE,IB)-AMPMIN).GE.AMPDIF)
          ECHO = ECHO.AND.AMPOK
          write(0,*) 'IsPeakAmp= ',AMPOK

          IF (.NOT.ECHO) CYCLE

C         Compute the mean and standard deviation of the phases
C         for both IDE and IDJ doppler lines and NARGH+NARGL+1 ranges.
C         --**BUG**--  Phases that wrap 360 degrees will fail!!
          EP = 0.0
          EPV = 0.0
          KC=0
          DO KR = IR-NARGL, IR+NARGH
             EP = EP + PRDB(KR,IDE,IB) + PRDB(KR,IDJ,IB)
             KC = KC + 1
          ENDDO
          IF (KC.NE.0) THEN
             EP = EP/REAL(2*KC)
          ELSE
             EP = 0.0
          ENDIF
C
          KC=0
          DO KR = IR-NARGL, IR+NARGH
             EPV = EPV + (EP - PRDB(KR,IDE,IB))**2 + 
     +                   (EP - PRDB(IR,IDJ,IB))**2
             KC = KC + 1
          ENDDO
          IF (KC.GT.1) THEN
             EPV = SQRT(EPV/REAL(2*KC-1))
          ELSE
             EPV = 0.0
          ENDIF
C
          write(0,*) ' Phase Mean and RMS = ',EP,EPV
          ECHO = (ECHO.AND.(EPV.LE.PHRMS))
          IF (.NOT.ECHO) CYCLE
C
C         At this point we have an echo at range IR, Doppler IDE, Beam IB
C         Add this echo to the echo list.
          write(0,*) 'Echo at IB,IR,IDE= ', IB,IR,IDE
          NECHO = NECHO + 1
          IF (NECHO.GT.MAXECHO) THEN
             WRITE(0,*) 'Maximum number of echoes exceeded'
             NECHO = MAXECHO
          ENDIF
C         Get the channel number
          ICH = ICS(NDOPP*(IB-1) + IDE)

          DFRQ(NECHO)   = FRQ(ICH)
          DRNG(NECHO)   = RGT(IR)
          DPOL(NECHO)   = APOL(ICH)
          DDOP(NECHO)   = HDOP(ICH)
          DAZ(NECHO)    = AZ(ICH)
          DZN(NECHO)    = ZN(ICH)
          DAMP(NECHO)   = ARDB(IR,IDE,IB)
          DPHASE(NECHO) = EP
          DPHASV(NECHO) = EPV
          DCHSNR(NECHO) = DAMP(NECHO)-ARDB(0,IDE,IB)
C       End of loop over range gates
        ENDDO
C     End of loop over beams
      ENDDO
C
C     THIS DOES NOT WORK WELL ENOUGH.
C     Adjust the Chialrity of echos by searching for an echo in the 
C     opposite polarization mode and making a weighted average.
C     This is mostly dealing with cross polarization leakage.
      DO I=1, NECHO-1
         DO J=I+1,NECHO
C           ToDo: Test additonal characteristics here.
            ECHO=(DRNG(I).EQ.DRNG(J))
            IF (ECHO) THEN
               DPOL(I)=WGTAVEDB(DPOL(I),DAMP(I),DPOL(J),DAMP(J))
               DPOL(J)=DPOL(I)
CC             Can we delete the weaker echo at this point as being leakaage?
            ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
C
C====+==================================================================+==
C

C
C====+==================================================================+==
C
      REAL FUNCTION WGTAVEDB(A1,W1,A2,W2)
C
C     Compute the weighted average of two values A1 and A2 that
C     are weighted by weights W1 and W2.  The weights are in dB

      REAL W1L,W2L
      W1L = 10.0**(W1/10.0)
      W2L = 10.0**(W2/10.0)
      IF((W1L+W2L).NE.0.0) THEN
         WGTAVEDB=(W1L*A1+W2L*A2)/(W1L+W2L)
      ELSE
         WRITE(0,*) 'WGTAVEDB -- Divide by zero'
         WGTAVEDB = 0.0
      ENDIF
      RETURN
      END
      











