# Windtunnel simulation
A toy model that solves the navier stokes equation for the flow around a wind.
## Compiling
These instructions were tested on cirrus with
- cmake/3.25.2
- nvhpc/22.2
The nvhpc suite provides the fortran openmpi cuda aware implementation.

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


## Running 
An example input file is present in the file `config.txt`. 
The main variables are the parameters:

*  `ALPHA`, the angle of attack,
*  `M` the maximum camber and  
* `T` the thickness of the wing section (as a faction of the wing length)

in the section `&SHAPEPARAMS`.  Additional variables in this section are `nx_global` and `ny_global` the number of cells in the x and y direction respectively.

The calculation can be launched on the GPU by setting `device = .TRUE.` or `device= .FALSE.`.

If you are going to run the code from the `build` directory then you will need to do:

```bash
cp src/windtunnel .  # Copy the executable to the local directory
cp ../config.txt .   # Copy the configuration file to the local directory.
```

You can then run the program with where you can replace `OMP_THREADS` by an environment variable or an integer less or equal to the number of cores available on your machine and `NUM_RANKS` again will be set as an environment variable or an integer number.

```
export OMP_NUM_THREADS=${OMP_THREADS}
mpirun -np ${NUM_RANKS} windtunnel
```

The program will write the output binary files:

- output.dat
- potential.dat 

The output files contain a binary dump of arrays containing grid and field information.
Also several information is printed out in the standard output, including drag and lift.

![Velocity](visualize/velocity.png)

## Wind shape
The shape of the wing is a cambered 4-digit NACA airfoil [https://en.wikipedia.org/wiki/NACA_airfoil ](https://en.wikipedia.org/wiki/NACA_airfoil) . An implementation is found in the `areofoil` routine at `vars.f90:159`.

