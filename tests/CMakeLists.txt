
cmake_minimum_required(VERSION 3.2)

function(configure_test target)
   target_compile_options( ${target} PUBLIC -Wall -O3 -g )
   target_link_libraries( ${target} PUBLIC OpenMP::OpenMP_Fortran )
endfunction()

add_executable( test_timing ../src/timing.f90 test_timing.f90  )
configure_test(test_timing)

add_test(
  NAME test_timing
  COMMAND $<TARGET_FILE:test_timing>
)
