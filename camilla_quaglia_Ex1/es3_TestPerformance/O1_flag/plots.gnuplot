set title "Performance of matrix-matrix multiplication algorithms (O1 flag)" font ",17"
set terminal png size 800,700
set key left top
set xrange [-10000:250000]
set output "timeO1.png"
set ylabel "log-time (s)" font ",17"
set xlabel "size of the resulting matrix" font ",17"
set logscale y
plot "time1.txt" using 2:1 title 'intrinsic function DOT PRODUCT()' with points pointtype 11  pointsize 1, \
     "time2.txt" using 2:1 title 'Direct calculus' pointtype 11 pointsize 1, \
     "time3.txt" using 2:1 title 'Columns method' pointtype 11 pointsize 1, \
     "time4.txt" using 2:1 title 'intrinsic function MATMUL' pointtype 11 pointsize 1 

