#!/usr/local/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 3.8g patchlevel 0
#    	last modified Sat Jul 28 17:40:25 IST 2001
#    	System: Linux 2.4.17-k7
#    
#    	Copyright(C) 1986 - 1993, 1999 - 2001
#    	Thomas Williams, Colin Kelley and many others
#    
#    	This is a pre-version of gnuplot 4.0. Please refer to the documentation
#    	for command syntax changes. The old syntax will be accepted throughout
#    	the 4.0 series, but all save files use the new syntax.
#    
#    	Type `help` to access the on-line reference manual
#    	The gnuplot FAQ is available from
#    		http://www.gnuplot.org/faq/
#    
#    	Send comments and requests for help to <info-gnuplot-beta@dartmouth.edu>
#    	Send bugs, suggestions and mods to <info-gnuplot-beta@dartmouth.edu>
#    
# set terminal unknown
# set output 'inogram.png'
unset clip points
set clip one
unset clip two
set bar 1.000000
set border 31 lt -1 lw 1.000
set xdata
set ydata
set zdata
set x2data
set y2data
set boxwidth
set dummy x,y
set format x "% g"
set format y "% g"
set format x2 "% g"
set format y2 "% g"
set format z "% g"
set angles radians
set grid nopolar
set grid xtics ytics noztics nox2tics noy2tics nomxtics nomytics nomztics nomx2tics nomy2tics nocbtics nomcbtics lt 0 lw 1.000, lt 0 lw 1.000
set key title ""
set key left top Right noreverse box linetype -2 linewidth 1.000 samplen 4 spacing 1 width 0
unset label
unset arrow
unset style line
unset logscale
set offsets 0, 0, 0, 0
set pointsize 1
set encoding default
unset polar
unset parametric
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface
unset contour
set clabel '%8.3g'
set mapping cartesian
unset hidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size ratio 0 1,1
set origin 0,0
set style data points
set style function lines
set xzeroaxis lt -2 lw 1.000
set yzeroaxis lt -2 lw 1.000
set x2zeroaxis lt -2 lw 1.000
set y2zeroaxis lt -2 lw 1.000
set tics in
set ticslevel 0.5
set ticscale 1 0.5
set mxtics default
set mytics default
set mztics default
set mx2tics default
set my2tics default
set mcbtics default
set ztics border nomirror norotate autofreq 
set nox2tics
set noy2tics
set cbtics border mirror norotate autofreq 
set title "AS00Q 2001 089 15:00" 0.000000,0.000000  ""
set timestamp "" bottom norotate 0.000000,0.000000  ""
set rrange [ * : * ] noreverse nowriteback  # (currently [0.00000:10.0000] )
set trange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set urange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set vrange [ * : * ] noreverse nowriteback  # (currently [-5.00000:5.00000] )
set timefmt x "%d/%m/%y,%H:%M"
set timefmt y "%d/%m/%y,%H:%M"
set timefmt z "%d/%m/%y,%H:%M"
set timefmt x2 "%d/%m/%y,%H:%M"
set timefmt y2 "%d/%m/%y,%H:%M"
set timefmt cb "%d/%m/%y,%H:%M"
set xlabel "Frequency [MHz]" 0.000000,0.000000  ""
set x2label "" 0.000000,0.000000  ""
set xrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set x2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set ylabel "Range [km]" 0.000000,0.000000  ""
set y2label "" 0.000000,0.000000  ""
set yrange [ 0.00000 : 750.000 ] noreverse nowriteback
set y2range [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zlabel "" 0.000000,0.000000  ""
set zrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set cblabel "" 0.000000,0.000000  ""
set cbrange [ * : * ] noreverse nowriteback  # (currently [-10.0000:10.0000] )
set zero 1e-08
set lmargin -1
set bmargin -1
set rmargin -1
set tmargin -1
set locale "C"
set pm3d scansautomatic flush begin nohidden3d transparent
unset pm3d
set palette color rgbformulae 7,5,15 positive nops_allcF maxcolors 0
set colorbox default
set colorbox vertical origin 0.9,0.2 size 0.1,0.63 bdefault
set loadpath 
plot 'ionogram.dat' using 4:($6<0 ? $5 : 1/0) title 'O','ionogram.dat' using 4:($6>0 ? $5 : 1/0) title 'X'
#    EOF
