
cmake_minimum_required(VERSION 3.2)

function(configure_test target )
   target_compile_options( ${target} PUBLIC -Wall -O3 -g )
   target_link_libraries( ${target} PUBLIC OpenMP::OpenMP_Fortran )
endfunction()

set (MODS ../src/timing.f90)

function(register_test target nProcessors)
  add_executable( ${target}_n${nProcessors} ${MODS} ${target}.f90  )
  configure_test(${target}_n${nProcessors} )
  add_test(
    NAME ${target}_n${nProcessors}
    COMMAND mpirun --oversubscribe -np ${nProcessors} $<TARGET_FILE:${target}_n${nProcessors}>
  )

endfunction()


register_test(test_timings 1)
register_test(test_timings 3)

register_test(test_timing 1)


