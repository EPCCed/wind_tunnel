cmake_minimum_required(VERSION 3.11)

cmake_policy(SET CMP0074 NEW )

set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." )

option(USE_CUDA "Enable using CUDA for optimization" ON )
option(TESTS "Enable Tests" OFF )

if ( ${USE_CUDA} )
   set(GPU_OPTS ccnative,nordc CACHE STRING "GPU flags for the current architecture")

   set ( project_languages Fortran CUDA )

else()
   set ( project_languages Fortran)
endif()


project( wind_tunnel LANGUAGES ${project_languages} )
find_package(OpenMP REQUIRED)
find_package(MPI REQUIRED)

include( tools.cmake )

add_subdirectory( src )

if ( ${TESTS} )

   enable_testing()
   add_subdirectory(tests)

endif()
