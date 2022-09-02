#!/bin/sh
# noiseplot - makes a 3D gnuplot of noise from a MMM format
#             Digisonde datafile
#
# useage:   noiseplot {datafile}
#
# Requires gnuplot 3.8j or higher with pm3d and PNG support
#
# 17Jul04 TWB Adjust for Gnuplot 3.8j+ and 4.x+
# 09Apr05 TWB Most DGS256 have broken MPA in prelude.
#
MMMFILE=$1
BFN=${MMMFILE##*/}
NOIFILE=${BFN%.*}.dat
GNUFILE=script.gnu
TITLE=${BFN%.*}
FRQRNG='[0:12]'
#
# Convert the MMM file into a noise file
rm -f $NOIFILE
dgsmmm -n  $MMMFILE > $NOIFILE
rm -f $GNUFILE
#
# Plot the MPA that is recomputed.
cat <<EOF > $GNUFILE
set pm3d map corners2color c1
set title "MPA (Noise) Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT" 
set yrange $FRQRNG
set ylabel "Frequency [MHz]" 
set zrange [0:120]
set cbrange [20:120]
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
set pm3d map corners2color c1
set title "Max Signal Plot for $TITLE" 
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT"
set yrange $FRQRNG
set ylabel "Frequency [MHz]"
set zrange [0:120]
set cbrange [20:120]
set cblabel "[dB]"
set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:8 title ''
EOF
#
IMGFILE=${BFN%.*}_MAX.png
gnuplot $GNUFILE > $IMGFILE
#
# Plot the Minimim Signal
cat <<EOF > $GNUFILE
set pm3d map corners2color c1
set title "Minimum Amplitude Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT" 
set yrange $FRQRNG
set ylabel "Frequency [MHz]"
set zrange [0:120]
set cbrange [0:100]
set cblabel "[dB]"
set terminal png crop size 800,600
splot "$NOIFILE" using 1:4:7 title '' 
EOF
#
IMGFILE=${BFN%.*}_MIN.png
gnuplot $GNUFILE > $IMGFILE

# Plot the Signal to Noise(MPA) 
cat <<EOF > $GNUFILE
set pm3d map corners2color c1
set title "SNR Plot for $TITLE"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT"
set yrange $FRQRNG
set ylabel "Frequency [MHz]"
set zrange [-10:60]
set cbrange [0:60]
set cblabel "[dB]"
set terminal png crop  size 800,600
splot "$NOIFILE" using 1:4:(\$8-\$6) title '' 
EOF

IMGFILE=${BFN%.*}_SNR.png
gnuplot $GNUFILE > $IMGFILE

exit 0

