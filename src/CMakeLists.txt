# siberia src directory

file(GLOB src_files CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_SOURCE_DIR}/*.f90"
)

add_executable(${CMAKE_PROJECT_NAME} ${src_files})

install(
  TARGETS ${CMAKE_PROJECT_NAME}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
