# siberia src directory

file(GLOB src_files CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_SOURCE_DIR}/*.f90"
)

add_executable(${model_name}_${version} ${src_files})

install(
  TARGETS ${model_name}_${version}
  DESTINATION ${PROJECT_SOURCE_DIR}
)
