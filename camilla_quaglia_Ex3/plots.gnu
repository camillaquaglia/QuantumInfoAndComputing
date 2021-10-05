set title "Performance of matrix-matrix multiplication algorithms " font ",17"
set terminal png size 800,600
set key right bottom font ",14"
set xrange [-10000:95000]
set output "plot.png"
set ylabel "log-time (s)" font ",17"
set xlabel "size of the resulting matrix" font ",17"
set logscale y
plot "time1.txt" using 2:1 title 'intrinsic function DOT PRODUCT()' w lp lw 1.7 pt 7 , \
     "time2.txt" using 2:1 title 'Direct calculus' w lp lw 1.7 pt 7 , \
     "time3.txt" using 2:1 title 'Columns method' w lp lw 1.7 pt 7 , \
     "time4.txt" using 2:1 title 'intrinsic function MATMUL' w lp lw 1.7 pt 7 
