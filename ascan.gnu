set xrange [60:160]
set xtics 10
set xlabel "Range+Doppler [km+Hz/2]"
set yrange [0:120]
set ylabel 'Amplitude [dB]'
set ytics 10
set grid 
set term png
set output "WP937_2003176022505_/dgs16c.png"
plot "testdata/dgs16c/WP937_2003176022505.16C" using ($4==/dgs16c ? $5+$8/2 : 1/0) : ($6<0 ? $11 : 1/0) title "/dgs16c" with lines 1 
