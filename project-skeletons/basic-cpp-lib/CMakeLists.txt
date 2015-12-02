cmake_minimum_required(VERSION 2.8)

set(CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS} -std=c++14)

set(LIBSRC
  src/main.cpp)

add_executable(__PROJECT-NAME__ ${SRC})
