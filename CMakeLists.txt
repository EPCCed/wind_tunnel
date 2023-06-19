cmake_minimum_required(VERSION 3.2)

project( wind_tunnel LANGUAGES Fortran )
cmake_policy(SET CMP0074 NEW )

find_package(OpenMP REQUIRED)


option(USE_CUDA "Enable using CUDA for optimization" ON )
add_subdirectory( src )
