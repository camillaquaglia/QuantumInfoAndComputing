set title "Performance of matrix-matrix multiplication algorithms,fit " font ",17"
set terminal png size 800,600
set key left top font ",14"

set output "fit.png"
set ylabel "Time (s)" font ",17"
set xlabel "size of the resulting matrix" font ",17"



a1=0.1
b1=1e-10
c1=3

a2=0.1
b2=1e-10
c2=3

a3=0.1
b3=1e-10
c3=3

a4=0.1
b4=1e-10
c4=3

f(x) = a1+b1*x**c1
g(x) = a2+b2*x**c2
h(x) = a3+b3*x**c3
t(x) = a4+b4*x**c4
fit f(x) 'time1.txt' u 2:1 via a1,b1,c1
fit g(x) 'time2.txt' u 2:1 via a2,b2,c2
fit h(x) 'time3.txt' u 2:1 via a3,b3,c3
fit t(x) 'time4.txt' u 2:1 via a4,b4,c4

plot "time1.txt" using 2:1 title 'intrinsic function DOT PRODUCT()' w p pt 7 , f(x) , \
     "time2.txt" using 2:1 title 'Direct calculus' w p pt 7 , g(x) , \
     "time3.txt" using 2:1 title 'Columns method' w p pt 7, h(x) , \
     "time4.txt" using 2:1 title 'intrinsic function MATMUL' w p pt 7, t(x)

