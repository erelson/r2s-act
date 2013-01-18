./grab_subroutine.py ../source_refactor.F90

gfortran -c mcnp_placeholder.F90
gfortran -c alias_table.F90

gfortran bench_alias_table.F90  -o _run_benchmark  mcnp_placeholder.o alias_table.o

./_run_benchmark

rm *.o
rm *.mod
