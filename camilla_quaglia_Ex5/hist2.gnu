# to run the script ' gnuplot> load "hist2.gnu" '
set xlabel "s_i" font ",16"
set size ratio 0.47
set output "fit2.png"
set key right top font ",14"
set title "Normalized histogram of data" font ",17"

#  ' gnuplot> stats "s_iREAL.dat" '    to see the min and the max
Min =  0.0000
Max =  129.1357 
n = 95            #chose accordingly
width = (Max - Min)/n
hist(x,width) = width/2.0 + width*floor(x/width)
#set size sq
set table 'hist2.temp'
#plot 's_iREAL.dat' u (hist($1,width)):(1.0) smooth freq w boxes lc rgb "red" notitle
plot 's_iREAL.dat' u (hist($1,width)):(1.0) smooth fnormal w boxes lc rgb "red" notitle 
unset table
#plot 'hist2.temp' u 1:2

P(x) = a*(x**alpha)*exp(-b*(x**beta))
a = 3
alpha = 2
b = 1
beta = 2

fit P(x) 'hist2.temp' u 1:2 via a,alpha,b,beta
plot 's_iREAL.dat' u (hist($1,width)):(1.0) smooth fnormal w boxes lc rgb "red" notitle, P(x) lw 1.7

