set multiplot layout 1,1 rowsfirst title sprintf("t=%i",time) font ',20'
set grid 
############# 
set yrange[-1-(0.5*(2-1)):2]
set xlabel 'x' font ',17'
plot 'realpart.dat' u 1:(column(time+2)) with lines lw 2 lc  rgb 'orange' title 'Re[{/Symbol Y}(x)]', 'imaginarypart.dat' u 1:(column(time+2)) with lines lw 2 lc rgb 'red' title 'Im[{/Symbol Y}(x)]', 'potential.dat' u 1:(column(time+2))-(0.5*(2*idx-1)) with lines lw 2 dashtype 1 lc rgb 'yellow' title 'V(x)', 'prob.dat' u 1:(column(time+2)) with lines lw 2 lc rgb 'green' title '|{/Symbol Y}(x)|^2'
##############

unset multiplot

time = time + 1
if (time < tot_time) reread


