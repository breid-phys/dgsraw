set xrange [60:160]
set xtics 10
set xlabel "Range+Doppler [km+Hz/2]"
set yrange [0:120]
set ylabel 'Amplitude [dB]'
set ytics 10
set grid 
set term png
set output "WP937_2003171210305_11.9925.png"
plot "WP937_2003171210305.dat" using ($4==11.9925 ? $5+$8/2 : 1/0) : ($6<0 ? $11 : 1/0) title "11.9925" with lines 1 
