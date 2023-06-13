# SIBERIA

cmake_minimum_required(VERSION 3.12)
project(siberia Fortran)

set(model_name siberia)
set(version 8.33)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

message(STATUS "Model name: ${model_name}")
message(STATUS "Model version: ${model_version}")

add_subdirectory(src)
include(CTest)
