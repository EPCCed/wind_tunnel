FC = mpif90
FFLAGS =-Wall -O3 -g -Mcuda=nordc # -g -fbounds-check #-ffixed-line-length-0 -ffpe-trap=invalid -fbounds-check


files =  solver.o setup.o navier_stokes.o poisson.o getv.o writetofile.o getvort.o get_params.o setup_gpu.o get_v_gpu.o navier_stokes_gpu.o 
modules = vars.o parallel.o poisson_cuda.o 


all : windtunnel tests


windtunnel : $(modules) $(files) windtunnel.o
	$(FC) $(FFLAGS) $(modules) $(files) windtunnel.o -o windtunnel
tests : $(modules) $(files) tests.o
	$(FC) $(FFLAGS) $(modules) $(files) tests.o -o tests


%.o : %.f90
	$(FC) -c $(FFLAGS) $< #-o $@

clean:
	rm -f *.o *.mod
