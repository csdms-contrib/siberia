# SIBERIA

cmake_minimum_required(VERSION 3.12)
project(siberia VERSION 8.33 LANGUAGES Fortran)

include(GNUInstallDirs)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

message(STATUS "Model name: ${CMAKE_PROJECT_NAME}")
message(STATUS "Model version: ${CMAKE_PROJECT_VERSION}")

add_subdirectory(src)
add_subdirectory(tests)
include(CTest)
