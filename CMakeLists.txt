cmake_minimum_required(VERSION 3.2)

project( wind_tunnel LANGUAGES Fortran CUDA)
cmake_policy(SET CMP0074 NEW )
find_package(OpenMP REQUIRED)

option(USE_CUDA "Enable using CUDA for optimization" ON )
option(TESTS "Enable Tests" OFF )
if (NOT GPU_OPTS)
   set(GPU_OPTS ccnative,nordc)
endif()
add_subdirectory( src )
