C
C====+==================================================================+==
C
      CHARACTER*17 FUNCTION TIME_GPP(PREFACE)
C
C     Extracts a 17 character time string YYYY DDD HH:MM:SS from
C     DPS format General Purpose PREFACE.
C
C     28Jul04   T. Bullett  AFRL
C
C     Requires:
C        UNP_BCD2,UNP_BCD4
C
C====+==================================================================+==
C

      INTEGER*1 PREFACE(57)
C
      INTEGER M4L,M4H
      PARAMETER (M4L=15,M4H=240)
C
      INTEGER UNP_BCD2,UNP_BCD4
C
      INTEGER YYYY,DDD,HH,MM,SS

      YYYY=UNP_BCD2(PREFACE(1))
      DDD=UNP_BCD4(PREFACE(2),PREFACE(3))
      HH=UNP_BCD2(PREFACE(6))
      MM=UNP_BCD2(PREFACE(7))
      SS=UNP_BCD2(PREFACE(8))
       
      IF (YYYY.LT.82) THEN
         YYYY=YYYY+2000
      ELSE
         YYYY=YYYY+1900
      ENDIF

      WRITE(TIME_GPP,117) YYYY,DDD,HH,MM,SS
 117  FORMAT (I4.4,1X,I3.3,1X,I2.2,':',I2.2,':',I2.2)
      RETURN
      END
C
C====+==================================================================+==
C
