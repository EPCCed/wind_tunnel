# Windtunnel simulation
A toy model that solves the navier stokes equation for the flow around a wind.
## Compiling
You will need a fortran compiler and CMake installed. You will also need a MPI library. If building with cuda support, the mpi library needs to be cuda aware. In most cases , it should be enough to run 
```bash
mkdir build
cd build
FC=mpif90 cmake ../
```
in a shell.
If building with GPU support then you should define the USE_CUDA variable 
```bash
FC=mpif90 cmake ../ -DUSE_CUDA=ON
```
You may need to setup the `FC` variable in the Makefile to the name of the mpi fortran compiler wrapper of your choice.

## Running 
An example input file is present in the file `config.txt`. 
The main variables are the parameters `alpha`, the angle of attack, `m` the camber and  `t`, the thickness in the section `&SHAPEPARAMS` .
Additional variables in this section are the parameters of the ellipse ( `a` axis in the  x direction , `b` axis in y direction , `p` , `c` location of the maximum camber, `c` chord ).
The calculation can be launched on the GPU by setting `device = .TRUE.` 
You can then run the program with
```
export OMP_NUM_THREADS=${OMP_THREADS}
mpirun -np ${NUM_RANKS} windtunnel
```

The program will write the output binary files

- output.dat
- potential.dat 

The output files contain a binary dump of arrays containing grid and field information.
Also several information is printed out in the standard output, including drag and lift.

![Velocity](visualize/velocity.png)

## Wind shape
The shape of the wing is a cambered 4-digit NACA airfoil [https://en.wikipedia.org/wiki/NACA_airfoil ](https://en.wikipedia.org/wiki/NACA_airfoil) . An implementation is found in the `areofoil` routine at `vars.f90:159`.

