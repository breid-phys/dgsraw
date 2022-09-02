#!/bin/sh
#
# snrplots   -- Make daily plots of Signal, Noise and SNR from
#               individual .16C files
STN='BC840'
TMP="$HOME/temp"
YYYY=2004
declare -i STARTD=123
declare -i STOPD=125
declare -i DAY=$STARTD
#
echo $STARTD $STOPD $DAY
 
while [ $DAY -le $STOPD ]; do
#  Convert to 3-digit day number
   DDD=$(echo $DAY | awk '{ printf "%3.3i",  $1 }')
   DATDIR='./16C/'$YYYY-$DDD/
   TMPFILE=$TMP/$STN-$YYYY-$DDD'.16C'
   echo $TMPFILE
   rm -f $TMPFILE
   cat $DATDIR* > $TMPFILE
   ./noiseplot16c $TMPFILE 
   rm -f $TMPFILE
   let DAY=$DAY+1
done

exit 0
