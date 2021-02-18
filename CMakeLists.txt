cmake_minimum_required(VERSION 3.0)

# Define the project and executable
project(EasyCodeIt C)
add_executable(eci utils.c parse.c eci.c)

# Enable warnings
if(CMAKE_COMPILER_IS_GNUCC)
	add_compile_options(-Wall -Wpedantic -Wextra -Wshadow -Wno-maybe-uninitialized -Wno-parentheses)
	if(CMAKE_BUILD_TYPE STREQUAL "Debug")
		message(STATUS "Adding address and undefined sanitzers")
		add_compile_options(-fsanitize=undefined,address)
	endif()
endif()
