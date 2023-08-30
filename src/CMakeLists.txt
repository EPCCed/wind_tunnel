
cmake_minimum_required(VERSION 3.2)

function( configure_target target)
   target_compile_options( ${target} PUBLIC -Wall -O3 -g  )
   target_link_options( ${target} PUBLIC -Wall -O3 -g )
   target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
   target_link_libraries( ${target} PUBLIC MPI::MPI_Fortran )
   target_include_directories( ${target} PUBLIC ${MPI_FORTRAN_DIRS} )
   
   if ( ${USE_CUDA} )   
   
      set( GPU_FLAGS -gpu=${GPU_OPTS} -cuda -Mfree )
      target_compile_options( ${target} PUBLIC ${GPU_FLAGS} )
      target_link_options( ${target} PUBLIC ${GPU_FLAGS}  )

   endif()

endfunction()



set(SOURCE_FILES timing.f90 timing_cfd.f90 vars.f90 get_params.f90 getv.f90 navier_stokes.f90 parallel.f90 poisson.f90 setup.f90 solver.f90 writetofile.f90 getvort.f90 )

#configure_file (
#    "${PROJECT_SOURCE_DIR}/src/build_info.f90.in"
#    "${PROJECT_SOURCE_DIR}/src/build_info.f90"
#)

if ( ${USE_CUDA} )
    list(APPEND SOURCE_FILES navier_stokes_gpu.f90 poisson_cuda.f90 setup_gpu.f90 get_v_gpu.f90  )
    add_compile_definitions( USE_CUDA )

endif()


set_source_files_properties(
   ${SOURCE_FILES} windtunnel.f90 tests.f90 PROPERTIES Fortran_PREPROCESS ON
   )


add_executable( windtunnel windtunnel.f90 ${SOURCE_FILES})
configure_target( windtunnel )
   
if ( ${USE_CUDA} )
   if ( ${TESTS} )
      add_executable( tests tests.f90 ${SOURCE_FILES})
   endif()
endif()

install(TARGETS windtunnel )
