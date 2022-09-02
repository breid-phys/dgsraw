set pm3d map
set title "Noise Plot for AS00Q_2001149"
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT"
set yrange [0:30]
set ylabel "Frequency [MHz]"
set zrange [0:120]
set zlabel "[dB]"
set terminal gif size 800,600
splot "AS00Q_2001149.dat" using 1:4:5 
