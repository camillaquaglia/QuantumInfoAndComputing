# to run this code 'python3 running.py'

import numpy as np
import subprocess as sub
import os

N_min = 0
while (N_min<=0):
    try:
        N_min = int(input("insert the minimun dimension you want:"))
    except ValueError:
        print("The input is not an integer number!")
    if(N_min <= 0):
        print("The input is non positive, reinsert")

N_max = 0
while ((N_max<=0) or (N_max < N_min)):
    try:
        N_max = int(input("insert the maximum dimension you want:"))
    except ValueError:
        print("The input is not an integer number!")
    if(N_max <= 0):
        print("The input is non positive, reinsert")
    elif(N_max < N_min):
        print("The maximum dimension shoul be greater than the minimum one, that is",N_min)
    

dim = np.linspace(N_min,N_max,10,dtype='int') 

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

gnuplot_script = "plots.gnu"
sub.run(['gnuplot',gnuplot_script])


