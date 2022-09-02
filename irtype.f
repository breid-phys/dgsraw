C
C   ======================================================================
C
	INTEGER FUNCTION IRTYPE(IBUF)
C
C	This function takes the Digisonde 256/DPS data block in IBUF and
C	determines the actual data type (Ionogram, Ionogram continuation,
C	Drift, Artist, Raw Ionogram) and returns it as the function value.
C	If the data block is not one of these types (ie unknown) the 
C	function returns a zero value for the recordtype.
C
C       NOTE: Due to a bug in the recording of 16 channel data in Artist3
C         circa 2001, where the record type is not encoded into the 4 lowest
C         bits of the first byte of every 8K block, this routine must be
C         specific to 16 channel data and also VERY FRAGILE!
C
C       Empirically, this function returns a recordtype of 1 for what is
C       supposed to be Recordtype 0x0C.
C       Unknown what happens for Recordtype 0x0D
C
C       Revision History
C       07Aug04 TWB - Added SBF and RSF block types.
C       12Jan08 TWB - Added FF as a valid value for the 3rd byte
C
	INTEGER I
	INTEGER*1 IRT, KRT
	INTEGER*1 M4L,M1L,ICC,IFE,IFF,IBUF(4096)
	LOGICAL OK
	PARAMETER (M4L=15,M1L=1,ICC=204,IFE=-2,IFF=-1)
C
	OK = .FALSE.
C
C  17Jan2002  TWB -  No record type in data at all!
C  Fugly Hacked around this bug, assume recordtype is 12
cfu        ok = .true.
cfu        IRTYPE = 12
cfu       RETURN
C
C
C	Make the first cut at the record type.
	IRT = IAND(IBUF(1),M4L)
cdb   	write(*,'(4Z4)') IRT,IFE,IFF,IBUF(3)
C	Eliminate false alarms.
C
        KRT=0
	IF ((IRT.EQ.9).OR.(IRT.EQ.8)) THEN
C	   Verify that this is MMM data.
C	   The third character in the block should be zero.
	   OK = (IBUF(3).EQ.0)
	ELSE IF ((IRT.EQ.10).OR.(IRT.EQ.12).OR.
     +           (IRT.EQ.13)) THEN
C	   For raw data types, extract the record type from the least
C	   significant bits of the first 4 words, and compare that with
C	   the record type. This is less absolute than the Ionogram method.
	   KRT = 0
	   DO I = 0, 3
	      KRT = IOR(KRT,ISHFT(IAND(IBUF(I+1),M1L),I))
	   ENDDO
	   OK = (IRT.EQ.KRT)
	ELSE IF ((IRT.EQ.3).OR.(IRT.EQ.2)) THEN
C	   Verify that this is SBF data.
C	   The third character in the block should be FE or FF.
	   OK = ((IBUF(3).EQ.IFE).OR.(IBUF(3).EQ.IFF))
	ELSE IF ((IRT.EQ.7).OR.(IRT.EQ.6)) THEN
C	   Verify that this is RSF data.
C	   The third character in the block should be FE or FF.
	   OK = ((IBUF(3).EQ.IFE).OR.(IBUF(3).EQ.IFF))
	ELSE IF (IRT.EQ.15) THEN
C	   For ARTIST, the 4th and 5th bytes should be Hex CC
	   OK = (IBUF(4).EQ.ICC).AND.(IBUF(5).EQ.ICC)
	   OK = (IBUF(3).EQ.IFE)
	ENDIF
C
cdb        write(*,*) 'irtype -> ',IRT,KRT
	IRTYPE = 0
	IF (OK) IRTYPE = IRT
C
	RETURN
	END







