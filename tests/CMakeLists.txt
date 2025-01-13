
cmake_minimum_required(VERSION 3.2)

set(TEST_RUN_COMMAND ${MPIEXEC_EXECUTABLE} CACHE STRING " Command to run test executables" )
set( TEST_PROCESSOR_FLAGS ${MPIEXEC_NUMPROC_FLAG} CACHE STRING "Flag to specify the number of processors" )


function(register_test target nProcessors)
  set ( TEST_MODS test_tools.f90)
  add_executable( ${target}_n${nProcessors} test_tools.f90 ${target}.f90  )
  configure_target(${target}_n${nProcessors} )
  target_link_libraries( ${target}_n${nProcessors} PUBLIC windtunnel_lib)
  target_include_directories( ${target}_n${nProcessors} PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/../src)

  add_test(
    NAME ${target}_n${nProcessors}
    COMMAND ${TEST_RUN_COMMAND} ${TEST_PROCESSOR_FLAGS} ${nProcessors} $<TARGET_FILE:${target}_n${nProcessors}>
  )
endfunction()

  register_test(test_timings 1)
  register_test(test_timings 3)
  register_test(test_timing 1)
  
  if ( ${USE_CUDA})

    register_test(test_vorticity_evolution 1)
    register_test( test_velocity 1 )
    register_test( test_vorticity 1)
    register_test( test_poisson_step 1)
    register_test( test_poisson_step 2)
    register_test (test_boundaries 2)

  endif()
