# siberia tests directory

include(CTest)

find_program(BASH bash)

if(BASH)
  add_test(
    NAME test_run_default_parameters
    COMMAND ${BASH} test_run_default_parameters.sh)
endif(BASH)

file(
  COPY_FILE test.setup siberia.setup)
file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/siberia.setup
  DESTINATION ${CMAKE_BINARY_DIR}/tests)
file(
  REMOVE siberia.setup)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/default.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/tests)
file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test_run_default_parameters.sh
  DESTINATION ${CMAKE_BINARY_DIR}/tests)
