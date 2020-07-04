cmake_minimum_required(VERSION 3.0)

# Define the project and executable
project(EasyCodeIt C)
add_executable(eci utils.c parse.c eci.c)

# Enable warnings
if(CMAKE_C_COMPILER_ID STREQUAL "GNU")
	add_compile_options(-Wall -Wpedantic -Wextra -Wshadow -Wno-maybe-uninitialized -Wno-parentheses)
endif()
