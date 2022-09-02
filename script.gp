set pm3d map
set title "SNR Plot for AS00Q_2003084" 0,-10
set xdata time
set timefmt "%Y %j %H:%M:%S"
set format x "%H"
set xlabel "UT" 0,5
#set yrange [0:30]
set ylabel "Frequency [MHz]" 25,-4
set zrange [*:*]
set cblabel "[dB]"
set terminal gif size 800,600
splot "AS00Q_2003084.dat" using 1:4:($8-$6) 
