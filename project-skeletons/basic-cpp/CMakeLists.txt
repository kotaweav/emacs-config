cmake_minimum_required(VERSION 3.1)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules/")
set(CMAKE_CXX_STANDARD 14)

set(SRC
  src/main.cpp)

add_executable(__PROJECT-NAME__ ${SRC})

add_custom_target(run
  COMMAND __PROEJCT-NAME__
  DEPENDS __PROJECT-NAME__
  WORKING_DIRECTORY ${CMAKE_PROJECT_DIR})
