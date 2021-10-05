set xrange [0:3.1]
#set yrange [-3.5:0]

set size ratio 0.65
set ylabel "Ground State Energy (e) " font ",15" 
set xlabel "Lambda" font ",15"

set xtics font ",15"
set ytics font ",15"

set key top right
set key font ",12"

plot "resN00002.txt" u 1:2 title "N=2" w lp lw 2 pt 7,"resN00003.txt" u 1:2 title "N=3" w lp lw 2 pt 7 lc rgb "#2222ee", "resN00004.txt" u 1:2 title "N=4" w lp lw 2 pt 7 lc rgb "#00dddd", "resN00005.txt" u 1:2 title "N=5" w lp lw 2 pt 7, [0:2] (-1-x**2/4) w l lw 2 lc rgb "#000000" title "Theoretical result",  [2:3] -x w l lw 2 lc rgb "#000000" title ""

