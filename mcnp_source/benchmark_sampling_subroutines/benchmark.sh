./grab_subroutine.py ../source_gamma.F90

gfortran -c mcnp_placeholder.F90 -g
gfortran -c sampling.F90 -g

gfortran bench_sampling.F90  -o _run_benchmark  mcnp_placeholder.o sampling.o -g

#./_run_benchmark
gdb _run_benchmark

rm *.o
rm *.mod
