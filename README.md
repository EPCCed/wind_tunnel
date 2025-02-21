# Windtunnel simulation
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A toy model that solves the Navier-Stokes equation for the flow around a wind.

## Contents

* [Compiling](#compiling)
* [Running](#running)
* [Wing shape](#wing-shape)

## Compiling
These instructions were tested on [cirrus](https://www.cirrus.ac.uk/) with:
- cmake/3.25.2
- nvhpc/22.2
The nvhpc suite provides the Fortran openmpi cuda aware implementation.

You will need a fortran compiler and CMake installed. You will also need a MPI library. If building with CUDA support, the MPI library needs to be CUDA aware. In most cases , it should be enough to run: 
```bash
mkdir build
cd build
FC=mpif90 cmake ../
```
in a shell.
If building with GPU support then you should define the USE_CUDA variable: 

```bash
FC=mpif90 cmake ../ -DUSE_CUDA=ON
```
If you are not going to use GPU support then specify `-DUSE_CUDA=OFF`.

Default `GPU_OPTS` is set to `ccnative,nordc`.

For compilation specifc to a GPU model set to ccXY. For example, for a Tesla V100:
```bash
FC=mpif90 cmake ../ -DUSE_CUDA=ON -DGPU_OPTS=cc70,nordc
```

`TESTS` variable is used to determine whether to build tests.

```bash
FC=mpif90 cmake ../ -DUSE_CUDA=ON -DGPU_OPTS=cc70,nordc -DTESTS=ON
```

This will generate a Makefile in the `build` directory. To compile the code run:

```bash
make
```

## Running 

An example input file is present in the file `config.txt`. 

The main variables are the parameters:

* `alpha` the angle of attack of the wing to the horizontal in the direction of travel,
* `m` the camber (the shape of the wing) and  
* `t` the thickness of the wing

in the `&SHAPEPARAMS` section.

Additional variables in this section are the parameters of the ellipse ( `a` axis in the  x direction , `b` axis in y direction , `p` , `c` location of the maximum camber, `c` chord ), `nx_global` and `ny_global` the grid size respectively in the `x` and `y` direction. 

The calculation can be launched on a GPU by setting `device = .TRUE.` 
You can then run the program with:

```bash
export OMP_NUM_THREADS=${OMP_THREADS}
mpirun -np ${NUM_RANKS} windtunnel
```

where `OMP_THREADS` and `NUM_RANKS` are environment variables you have set to indicate the number of threads and processes you are going to run the program on. The program will write the output binary files:

- output.dat
- potential.dat 

The output files contain a binary dump of arrays containing grid and field information.
Also several bits of information are printed out to standard output, including the wing drag and lift.

![Velocity](visualize/velocity.png)

## Wing shape
The shape of the wing is a cambered 4-digit NACA airfoil [https://en.wikipedia.org/wiki/NACA_airfoil ](https://en.wikipedia.org/wiki/NACA_airfoil) . An implementation is found in the `areofoil` routine at `vars.f90:159`.

