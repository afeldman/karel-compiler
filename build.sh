#!/bin/bash

set -e

echo "Building Karel Compiler..."

# Generate parser with BNFC (output to src/)
echo "Generating parser with BNFC..."
bnfc -m --haskell -o src/ karel.cf

# Build runtime library
echo "Building runtime library..."
cd runtime
make clean
make
cd ..

# Build with Stack
echo "Building Haskell project..."
stack setup
stack build --work-dir .stack-work-build

# Copy executable to build directory
echo "Copying executable to build/..."
mkdir -p build
EXECUTABLE=$(stack path --work-dir .stack-work-build --local-install-root)/bin/karel-compiler
cp "$EXECUTABLE" ./build/karelc

echo "Build complete! Executable: ./build/karelc"
