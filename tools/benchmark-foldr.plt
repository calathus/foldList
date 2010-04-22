 
plot "new-loop.dat" using 1:2 title 'new-array-loop' with linespoints, \
"new-arrbuff.dat" u 1:2 t 'new-array-buffer' w linespoints, \
"iter-loop.dat" u 1:2 t 'iter-loop' w linespoints
