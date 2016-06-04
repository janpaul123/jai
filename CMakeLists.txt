
cmake_minimum_required (VERSION 2.6)
project (JAI)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g")


file(GLOB jai_SRC
    "source/*.cpp"
)

file(GLOB driver_SRC
    "driver/*.cpp"
)

add_executable(jai ${driver_SRC} ${jai_SRC})
target_include_directories (jai PUBLIC include)
