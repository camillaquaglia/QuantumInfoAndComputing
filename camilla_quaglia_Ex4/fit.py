# to run this code 'python3 fit.py'

import numpy as np
import subprocess as sub
import os


N_min = 1
N_max = 1000
    
dim = np.linspace(N_min,N_max,100,dtype='int')

file_name = 'dims.txt'

if os.path.exists(file_name): 
    os.remove(file_name)
 
    
for i in range(len(dim)):
    with open(file_name,"w+") as filename:
        filename.write(str(dim[i])+'\n'+str(dim[i])+'\n'+str(dim[i])+'\n'+str(dim[i])+'\n')
    filename.close()
    fortran_code ="Ex4-Quaglia-CODE.f90"
    exe = "Ex4-Quaglia-CODE.x"
    sub.run(["gfortran",fortran_code,'-o',exe]) 
    sub.run(["./"+exe])  

gnuplot_script = "fit.gnu"
sub.run(['gnuplot',gnuplot_script])


