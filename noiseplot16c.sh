#!/bin/sh
# noiseplot - makes a 3D gnuplot of noise from a 16C format
#             Digisonde datafile
#   T. Bullett  May 2004
#
# useage:   noiseplot {datafile}
#
# Requires gnuplot 3.8j or higher with pm3d and new PNG support
#
# 17Jul04  Clean up to eliminate GIFs 
#
C16FILE=$1
BFN=${C16FILE##*/}
NOIFILE=${BFN%.*}.dat
GNUFILE=script.gnu
TITLE=${BFN%.*}
#
# Convert the C16 file into a noise file
rm -f $NOIFILE
dgs16c -n  $C16FILE > $NOIFILE
rm -f $GNUFILE
#
# Plot the MPA
cat <<EOF > $GNUFILE
set pm3d map
set title "MPA (Noise) Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT" 
#set yrange [0:30]
set ylabel "Frequency [MHz]"
set zrange [0:120]
set cblabel "[dB]"
set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:6 title ''
EOF
#
IMGFILE=${BFN%.*}_MPA.png
gnuplot $GNUFILE > $IMGFILE

#
# Plot the Max Signal
cat <<EOF > $GNUFILE
set pm3d map
set title "Max Signal Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT"
#set yrange [0:30]
set ylabel "Frequency [MHz]"
set zrange [0:120]
set cblabel "[dB]"
set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:8 title '' 
EOF
#
IMGFILE=${BFN%.*}_MAX.png
gnuplot $GNUFILE > $IMGFILE
#
# Plot the Minumim Signal
cat <<EOF > $GNUFILE
set pm3d map
set title "Minimum Amplitude Plot for $TITLE" 
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT" 
#set yrange [0:30]
set ylabel "Frequency [MHz]"
set zrange [0:120]
set cblabel "[dB]"
set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:7 title ''
EOF
#
IMGFILE=${BFN%.*}_MIN.png
gnuplot $GNUFILE > $IMGFILE

# Plot the Signal to Noise(MPA) 
cat <<EOF > $GNUFILE
set pm3d map
set title "SNR Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT"
#set yrange [0:30]
set ylabel "Frequency [MHz]" 
set zrange [0:50]
set cblabel "[dB]"

set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:(\$8-\$6) title '' 
EOF

IMGFILE=${BFN%.*}_SNR.png
gnuplot $GNUFILE > $IMGFILE

exit 0







