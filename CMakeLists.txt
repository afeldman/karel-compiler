cmake_minimum_required(VERSION 3.20)
project(karel-compiler C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Find Flex and Bison
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# Find LLVM 15
find_package(LLVM 15 REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# Generate Lexer and Parser from BNFC output (C backend)
FLEX_TARGET(KarelLexer 
    ${CMAKE_SOURCE_DIR}/parser-c/karel.l 
    ${CMAKE_BINARY_DIR}/Lexer.c
    COMPILE_FLAGS "-Pkarel_"
    DEFINES_FILE ${CMAKE_BINARY_DIR}/Lexer.h)

BISON_TARGET(KarelParser 
    ${CMAKE_SOURCE_DIR}/parser-c/karel.y 
    ${CMAKE_BINARY_DIR}/Parser.c
    COMPILE_FLAGS "-t -pkarel_"
    DEFINES_FILE ${CMAKE_BINARY_DIR}/Bison.h)

ADD_FLEX_BISON_DEPENDENCY(KarelLexer KarelParser)

# Include directories
include_directories(
    ${CMAKE_SOURCE_DIR}/parser-c
    ${CMAKE_SOURCE_DIR}/src
    ${CMAKE_BINARY_DIR}
)

# Parser library (C code compiled as C++)
add_library(karel-parser STATIC
    parser-c/Absyn.c
    parser-c/Buffer.c
    parser-c/Printer.c
    parser-c/Skeleton.c
    ${BISON_KarelParser_OUTPUTS}
    ${FLEX_KarelLexer_OUTPUTS}
)
set_target_properties(karel-parser PROPERTIES LINKER_LANGUAGE CXX)

# Main executable
add_executable(karelc
    src/main.cpp
    src/CodeGen.cpp
)

# Link LLVM libraries
llvm_map_components_to_libnames(llvm_libs 
    Core
    Support
    Target
    Analysis
    TransformUtils
    ExecutionEngine
    MC
    MCParser
    Object
    Passes
)

target_link_libraries(karelc
    karel-parser
    ${llvm_libs}
)

# Installation
install(TARGETS karelc DESTINATION bin)
