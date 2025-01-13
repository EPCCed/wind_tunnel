function( configure_target target)
   target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
   target_link_libraries( ${target} PUBLIC MPI::MPI_Fortran )
   target_compile_options( ${target} PUBLIC -cpp  )
   
   if ( ${USE_CUDA} )   
   
      set( GPU_FLAGS -gpu=${GPU_OPTS} -cuda -Mfree )
      target_compile_options( ${target} PUBLIC ${GPU_FLAGS} )
      target_link_options( ${target} PUBLIC ${GPU_FLAGS}  )
      
   endif()

endfunction()