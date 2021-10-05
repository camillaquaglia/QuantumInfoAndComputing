#reset
# to run the script ' gnuplot> load "plot.gnu" '

# FIRST PLOT ###############################################

set size ratio 0.50
set key l
set xlabel 'n' font ",14"
set ylabel 'E_n' font ",14"
set title 'Eigenvalues' font ",16"
 
#plotting
pl 'eigenvalues.dat' lw 2
f(x) = a*(x**b + c)
c=1/2
fit f(x) 'eigenvalues.dat' via a,b,c
replot f(x) title 'f(x) = a*(x**b + c)'



# SECOND PLOT: #########################################

set size ratio 0.60
set xlabel 'x' font ",14"
set ylabel '{/Symbol Y}(x)' font ",14"
k=5
set title 'First '.(k-1).' (numerical) eigenfunctions' font ",16"

#plotting
pl for [ii=2:k] 'eigenvectors.dat' u 1:ii w l title '{/Symbol Y}_{'.(ii-1).'}'


# THIRD PLOT ###############################################

set size ratio 0.60
set key b
set xlabel 'x' font ",14"
set ylabel '{/Symbol Y}(x)' font ",14"
set title 'First '.(k-1).' eigenfunctions' font ",16"

#plotting
pl for [ii=2:k] 'eigenvectors.dat' u 1:ii w l title '{/Symbol Y}_{'.(ii-1).'}', for [ii=2:k] 'Analitycal_eigvects.dat' u 1:ii lc ii dashtype 2 lw 2 title '{/Symbol Y}^{analytic.}_{'.(ii-1).'}'

 
