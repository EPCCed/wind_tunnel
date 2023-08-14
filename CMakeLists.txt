cmake_minimum_required(VERSION 3.2)

cmake_policy(SET CMP0074 NEW )

option(USE_CUDA "Enable using CUDA for optimization" ON )
option(TESTS "Enable Tests" OFF )
if (NOT GPU_OPTS)
   set(GPU_OPTS ccnative,nordc)
endif()

if ( ${USE_CUDA} )
   set ( project_languages Fortran CUDA)
else()
   set ( project_languages Fortran )
endif()

project( wind_tunnel LANGUAGES ${project_languages} )
find_package(OpenMP REQUIRED)

add_subdirectory( src )

enable_testing()


add_subdirectory(tests)
