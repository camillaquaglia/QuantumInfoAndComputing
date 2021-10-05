set xrange [0:3.1]

set size ratio 0.80
set ylabel "Energy (normalized w.r.t. N)" font ",15" #offset -3.5,0
set xlabel "Lambda" font ",15"

set xtics font ",15"
set ytics font ",15"

set key bottom left
set key font ",12"

plot "lambda_eigenvalues.txt" u 1:2 title "1st eigenvalue E0/N" w lp lw 2 pt 7, "lambda_eigenvalues.txt" u 1:3 title "2nd eigenvalue E1/N" w lp lw 2 pt 7 lc rgb "#2222ee", "lambda_eigenvalues.txt" u 1:4 title "3rd eigenvalue E2/N" w lp lw 2 pt 7 lc rgb "#00dddd", "lambda_eigenvalues.txt" u 1:5 title "4th eigenvalue E3/N" w lp lw 2 pt 7,"lambda_eigenvalues.txt" u 1:6 title "5th eigenvalue E4/N" w lp lw 2 pt 7, [0:2] (-1-x**2/4) w l lw 2 lc rgb "#000000" title "Theoritical result", [2:3] -x w l lw 2 lc rgb "#000000" title ""
