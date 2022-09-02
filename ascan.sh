#!/bin/sh
#
# ascan - Makes Amplitude vs Range+Doppler plots for each frequency
#         in a 16C ionogram data file.
#
GNUSCR=ascan.gnu
DS=2
DATFILE=$1

#
BFN=${DATFILE##*/}
#
# Get a list of frequencies to plot; strip comments and blank lines
FRQLIST=$(grep -v '^$' $DATFILE | grep -v '#' | colrm 1 20 | colrm 8 999 | uniq )
#
# Plot an a-scan for each frequency
for FREQ in $FRQLIST ; do 
    echo $FREQ
    OUTFILE=${BFN%.*}'_'$FREQ'.png'
cat <<EOF  > $GNUSCR
set xrange [60:160]
set xtics 10
set xlabel "Range+Doppler [km+Hz/$DS]"
set yrange [0:120]
set ylabel 'Amplitude [dB]'
set ytics 10
set grid 
set term png
set output "$OUTFILE"
plot "$DATFILE" using (\$4==$FREQ ? \$5+\$8/$DS : 1/0) : (\$6<0 ? \$11 : 1/0) title "$FREQ" with lines 1 
EOF
    gnuplot $GNUSCR
done
exit 0