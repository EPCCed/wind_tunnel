cmake_minimum_required(VERSION 3.2)

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

add_library( windtunnel_lib ${SOURCE_FILES})
configure_target(windtunnel_lib)

add_executable( windtunnel windtunnel.f90 )
configure_target( windtunnel )
target_link_libraries(windtunnel PUBLIC windtunnel_lib)

if ( ${USE_CUDA} )
   if ( ${TESTS} )
      add_executable( tests tests.f90 )
      configure_target(tests)
      target_link_libraries(tests PUBLIC windtunnel_lib)


   endif()
endif()

install(TARGETS windtunnel )