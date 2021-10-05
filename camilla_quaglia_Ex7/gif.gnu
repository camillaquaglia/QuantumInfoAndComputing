reset
set term gif animate font "TeX Gyre Pagella, 15" size 600,500
set output "animate.gif"

#setting the number of frames
tot_time = 50
time=0

#creating the gif 
load "p.gnu"
set output


# convert a gif to frames -> type on terminal 'convert animate.gif -coalesce animate.png'

