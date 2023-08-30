cmake_minimum_required(VERSION 3.2)
#cmake_policy( SET CMP0074 NEW)

project( micro_benchmarks LANGUAGES Fortran )

find_package(MPI REQUIRED)

message("${MPI_Fortran_INCLUDE_DIRS}")